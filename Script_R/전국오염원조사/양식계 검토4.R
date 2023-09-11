#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(readxl)
library(writexl)

library(httr)
library(jsonlite)
library(progress)
library(sf)

### 반올림 사용자 정의 함수 로드
# source("Script_R/Function/round2func.R")
# 
# ## 공통파일(단위유역별 점유율) 불러오기
# share <- read_excel("전국오염원조사/단위유역별 점유율.xlsx")


##########  파일 불러오기  #####################################################

# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/양식계",
  pattern = "*.xls", full.names = T
)

## Excel 파일을 읽어와 데이터를 합치는 함수
read_and_combine_data <- function(files) {
  data <- files %>%
    map_dfr(~ {
      data <- read_excel(.x, skip = 3, col_names = FALSE) 
      year <- str_sub(basename(.x), 1, 4) %>% as.integer()
      data <- data %>% mutate(연도 = year, .before = 1) 
      # 2021년 이전 자료의 경우 인허가관리번호 항목 3개 추가(1열 뒤)
      if (year < 2021) {
        data %<>% mutate(
          어업양식업면허대장 = NA,
          양식업허가대장 = NA,
          새올행정시스템 = NA,
          .after = 2
        )
      } else {
        data %<>% rename(
          어업양식업면허대장 = ...2,
          양식업허가대장 = ...3,
          새올행정시스템 = ...4
        )
      }
      
    })
  return(data)
}


양식장현황_원본 <- read_and_combine_data(files)



##########  자료 정리  #########################################################

## 데이터 정리
양식장현황_정리 <- 양식장현황_원본 %>%
  select(연도, 7:14, 18, 22, 25, 28, 30) %>%
  set_names(c(
    "연도", "업소명", "법정동코드", "도", "시군", "읍면동", "리", "본번", "부번",
    "종류", "시설면적", "사료사용량", "방류하천", "휴업"
  )) %>%
  # 데이터 숫자 형태로 변경
  mutate(across(c(연도, 시설면적, 사료사용량), as.numeric)) %>%
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
      ),
    동리 = ifelse(is.na(리), 읍면동, 리),
    주소코드 = str_c(시군, 읍면동, 동리, sep = " ")
  ) %>%
  filter(연도 >= 2020)


# left_join(
#   도로명주소 %>%
#     select(지번주소, 도로명주소, x좌표, y좌표) %>%
#     rename(주소 = 지번주소),
#   by = "주소"
# )

##########  주소 좌표 변환  ###############################################################

## 업소코드 및 주소만 추출
address <- 양식장현황_정리 %>%
  select(연도, 업소코드, 주소) %>%
  rowid_to_column(var = "ID") # %>% filter(ID > 105)

address_list <- address$주소

## progress bar 설정
pb <- progress_bar$new(
  format = " Progress: [:bar] :current / :total (:percent), Estimated completion time::eta",
  total = nrow(address), # 총 tick 개수 (default 100)
  clear = FALSE, # 종료 후에도 진행 경과 출력 결과 유지 (default TRUE)
  width = 80 # 진행 경과 막대 너비
)

result <- tibble()

for (i in 1:nrow(address)) {
  place_list <- GET(
    url = "https://dapi.kakao.com/v2/local/search/address.json",
    query = list(query = address_list[i]),
    add_headers(Authorization = "KakaoAK 9e9a85a9ec8362e009da2f7bc4b3a09c")
  ) %>%
    content(as = "text") %>%
    fromJSON()

  # 수용가번호 및 기존 주소 불러오기
  temp_addr <- address %>% filter(ID == i)
  
  # 도로명주소 여부 확인 위해 도로명주소 항목과 test열(NA값) 결합
  temp <- bind_cols(place_list$documents$road_address, tibble(test = NA))
  
  # 도로명주소가 없는 경우 test열만 있기 때문에 1행 1열은 NA
  # 도로명주소가 있는 경우 test열은 마지막 열이 되므로 1행 1열은 NA가 아님
  if (is.na(temp[1, 1])) {
    temp <- bind_cols(temp_addr, place_list$documents$address)
  } else {
    temp <- bind_cols(
      temp_addr, place_list$documents$address,
      place_list$documents$road_address$address_name
    )
  }

  result <- bind_rows(result, temp)

  ## 진행상황 확인
  pb$tick()
}


##########  좌표 정리 및 유역 매칭  ############################################

## 경위도좌표 정리
경위도 <- result %>%
  select(연도, 업소코드, address_name) %>%
  # 연도별 주소 정리
  pivot_wider(
    names_from = 연도,
    values_from = address_name
  ) %>%
  # 가장 최신 자료를 기준으로 대표 주소 지정
  mutate(address_name = ifelse(is.na(`2022`),
    ifelse(is.na(`2021`),
      ifelse(is.na(`2020`), NA, `2020`),
      `2021`
    ),
    `2022`
  )) %>%
  # 대표 주소 기준 경위도좌표 데이터 결합
  left_join(result %>% select(업소코드, 주소, address_name, x, y),
    by = c("업소코드", "address_name")
  ) %>%
  mutate(across(c(x, y), as.numeric)) %>% 
  distinct()

## 경위도 point 자료 생성(경위도 좌표계(WGS84) 적용 - EPSG:4326)
경위도_point <- 경위도 %>%
  filter(!is.na(x)) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)

## 좌표를 EPSG:5174로 변경(단위유역 shp 파일과 좌표계 통일)
경위도_point_5174 <- sf::st_transform(경위도_point, crs = 5174)


## 단위유역 shp 파일 불러오기(sf객체)
단위유역 <- sf::st_read("D:/GIS/유역/강원도_유역도_최종(이티워터)/총량단위유역_강원도_1985M_20200924.shp")


## 양식장 단위유역 정리
양식장_유역 <- sf::st_join(경위도_point_5174, 단위유역) %>%
  select(업소코드, 주소, 주소검색결과 = address_name, 단위유역 = SW_NAME) %>%
  mutate(
    단위유역 = ifelse(is.na(단위유역), "기타", 단위유역),
    코드 = str_c(업소코드, 주소)
  ) %>%
  # 지리정보(geometry) 제거
  st_drop_geometry() %>%
  distinct(코드, .keep_all = TRUE)


##########  유역 매칭 확인  ####################################################
## 유역별 점유율 자료 기반 유역 확인
# 양식장_유역_확인 <- 양식장현황_정리 %>%
#   left_join(share %>% select(주소코드 = 주소, 단위유역), by = "주소코드") %>%
#   distinct()
# 
# ## 좌표 및 GIS 이용한 유역 자료와 비교 검토
# 양식장_유역_확인 %<>%
#   left_join(양식장_유역 %>% select(업소코드, 단위유역2 = 단위유역), by = "업소코드") %>%
#   group_by(업소코드) %>%
#   mutate(중복 = length(업소코드)) %>%
#   ungroup() %>%
#   mutate(유역확인 = ifelse(단위유역 == 단위유역2, "O", "X"))


##########  유역 자료 합치기  ##################################################
양식장현황_최종 <- 양식장현황_정리 %>%
  left_join(양식장_유역 %>% select(업소코드, 단위유역), by = "업소코드")




##########  시설면적 계산  #####################################################

## 연도 및 동리별 시설면적 합계 정리
양식장현황_면적2022 <- 양식장현황_최종 %>%
  filter(연도 == 2022) %>%
  filter(!is.na(단위유역)) %>%
  group_by(연도, 시군, 단위유역) %>%
  summarise(시설면적 = sum(시설면적), .groups = "drop") %>%
  rbind(
    양식장현황_최종 %>%
      filter(연도 == 2022) %>%
      filter(!is.na(단위유역)) %>%
      group_by(연도, 시군) %>%
      summarise(시설면적 = sum(시설면적), .groups = "drop") %>%
      mutate(단위유역 = "합계")
  )










