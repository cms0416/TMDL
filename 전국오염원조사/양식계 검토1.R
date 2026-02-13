#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(data.table)
library(readxl)
library(writexl)

### 반올림 사용자 정의 함수 로드
source("Script_R/Function/round2func.R")

## 공통파일(단위유역별 점유율) 불러오기
share <- read_excel("전국오염원조사/단위유역별 점유율.xlsx")

##########  자료 정리  #########################################################

## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
dir <- ("전국오염원조사/양식계")
files <- list.files(dir)

# 데이터 불러오기 및 합치기
양식장현황_원본 <- tibble()

for (file in files) {
  print(file)
  temp <- read_excel(file.path(dir, file), skip = 3, col_names = F) %>%
    # str_sub(문자열, 시작위치, 끝위치) : 연도 추출
    mutate(연도 = str_sub(file, 1, 4), .before = 1)
  
  # 2021년 이전 자료의 경우 인허가관리번호 항목 3개 추가(1열 뒤)
  if (str_sub(file, 1, 4) < 2021) {
    temp %<>% mutate(
      어업양식업면허대장 = NA,
      양식업허가대장 = NA,
      새올행정시스템 = NA,
      .after = 2
    )
  } else {
    temp %<>% rename(어업양식업면허대장 = ...2, 
                     양식업허가대장 = ...3, 
                     새올행정시스템 = ...4)
  }
  
  양식장현황_원본 <- bind_rows(양식장현황_원본, temp)
}
## *****************************************************************************


## 데이터 정리
양식장현황_정리 <- 양식장현황_원본 %>%
  select(연도, 7:14, 18, 22, 25, 28, 30) %>%
  set_names(c(
    "연도", "업소명", "법정동코드", "도", "시군", "읍면동", "리", "본번", "부번", 
    "종류", "시설면적", "사료사용량", "방류하천", "휴업"
  )) %>% 
  # 본번에서 '산' 분리
  mutate(
    산 = str_extract(본번, "산"), 
    본번 = str_remove_all(본번, "산") %>% str_trim(.),
    .after = 리
  ) %>% 
  # 주소 합치기
  mutate(
    업소코드 = str_c(업소명, 법정동코드),
    주소 =
      str_c(
        "강원도", " ", 시군, " ", 읍면동, " ",
        ifelse(is.na(리), "", str_c(리, " ")),
        ifelse(is.na(산), "", str_c("산", " ")),
        본번,
        ifelse(부번 == 0 | is.na(부번), "", str_c("-", 부번))
      )
  ) %>% 
  filter(연도 == 2022)

  # left_join(
  #   도로명주소 %>%
  #     select(지번주소, 도로명주소, x좌표, y좌표) %>% 
  #     rename(주소 = 지번주소),
  #   by = "주소"
  # )

##########  주소 좌표 변환  ###############################################################

library(httr)
library(jsonlite) # fromJSON()
library(progress)

## REST API 키(https://developers.kakao.com/)
KAKAO_MAP_API_KEY <- "9e9a85a9ec8362e009da2f7bc4b3a09c"

## 수용가번호 및 대표주소만 추출
address_jibun <- 양식장현황_정리 %>%
  select(업소코드, 주소) %>%
  rowid_to_column(var = "ID") # %>% filter(ID > 105)

address_jibun_list <- address_jibun$주소

## progress bar 설정
pb_jibun <- progress_bar$new(
  format = " Progress: [:bar] :current / :total (:percent), Estimated completion time::eta",
  total = nrow(address_jibun), # 총 tick 개수 (default 100)
  clear = FALSE, # 종료 후에도 진행 경과 출력 결과 유지 (default TRUE)
  width = 80 # 진행 경과 막대 너비
)

result_jibun <- tibble()

for (i in 1:nrow(address_jibun)) {
  place_list <- GET(
    url = "https://dapi.kakao.com/v2/local/search/address.json",
    query = list(query = address_jibun_list[i]),
    add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY))
  ) %>%
    content(as = "text") %>%
    fromJSON()
  
  # 수용가번호 및 기존 주소 불러오기
  temp_addr_jibun <- address_jibun %>% filter(ID == i)
  
  temp <- bind_cols(place_list$documents$road_address, tibble(test = NA))
  
  ## 지번주소
  # temp_jibun <- bind_cols(temp_addr_jibun, place_list$documents$address)
  # place_list$documents$road_address$address_name
  
  if (is.na(temp[1, 1])) {
    temp_jibun <- bind_cols(temp_addr_jibun, place_list$documents$address)
  } else {
    temp_jibun <- bind_cols(
      temp_addr_jibun, place_list$documents$address,
      place_list$documents$road_address$address_name
    )
  }
  
  result_jibun <- bind_rows(result_jibun, temp_jibun)
  
  ## 진행상황 확인
  pb_jibun$tick()
}





##########  좌표 정리  #########################################################

library(rgdal)
library(sp)

## 좌표와 속성 분리
위경도 <- result_jibun %>% 
  filter(!is.na(x)) %>% 
  mutate(across(c(x, y), as.numeric)) %>% 
  select(x, y)

업소코드 <- result_jibun %>% filter(!is.na(x)) %>% select(업소코드)


## GRS80TM의 좌표계 설정
GRS80_CRS <- CRS("+proj=longlat +ellps=GRS80 +no_defs")

## 속성이 없는 spatialpoint 데이터 생성
위경도_point <- SpatialPoints(위경도, GRS80_CRS)

## 좌표를 EPSG:5174로 변경
CRS_5174 <- CRS("+proj=tmerc +lat_0=38 +lon_0=127.002890277778 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m +no_defs")
                 
위경도_point_5174 <- spTransform(위경도_point, CRS_5174)

## spatialpoint data에 속성을 추가하여 spatailpointsdataframe 생성
양식장_좌표 <- SpatialPointsDataFrame(위경도_point_5174, 업소코드)


## 단위유역 shp 파일 불러오기
단위유역 <- readOGR("D:/GIS/유역/강원도_유역도_최종(이티워터)/총량단위유역_강원도_1985M_20200924.shp")

## 양식장 단위유역 정리
양식장_유역 <- point.in.polygon(양식장_좌표, 단위유역)
