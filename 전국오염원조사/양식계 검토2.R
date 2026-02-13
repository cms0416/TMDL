#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(data.table)
library(readxl)
library(writexl)

library(httr)
library(jsonlite) # fromJSON()
library(progress)

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
address_jibun <- 양식장현황_정리 %>%
  select(연도, 업소코드, 주소) %>%
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
    add_headers(Authorization = "KakaoAK 9e9a85a9ec8362e009da2f7bc4b3a09c")
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



##########  좌표 정리 및 유역 매칭  ############################################

library(sf)

## 경위도좌표 정리
경위도 <- result_jibun %>% 
  filter(!is.na(x)) %>% 
  mutate(across(c(x, y), as.numeric)) %>% 
  select(연도, 업소코드, 주소, address_name, x, y)

## 경위도 point 자료 생성(경위도 좌표계(WGS84) 적용 - EPSG:4326)
경위도_point <- 경위도 %>% st_as_sf(coords = c('x', 'y'), crs = 4326)

## 좌표를 EPSG:5174로 변경(단위유역 shp 파일과 좌표계 통일)
경위도_point_5174 <- sf::st_transform(경위도_point, crs = 5174)


## 단위유역 shp 파일 불러오기(sf객체)
단위유역 <- sf::st_read("D:/GIS/유역/강원도_유역도_최종(이티워터)/총량단위유역_강원도_1985M_20200924.shp")


## 양식장 단위유역 정리
양식장_유역 <- sf::st_join(경위도_point_5174, 단위유역) %>% 
  select(연도, 업소코드, 주소, 주소검색결과 = address_name, 단위유역 = SW_NAME) %>% 
  mutate(단위유역 = ifelse(is.na(단위유역), "기타", 단위유역),
         코드 = str_c(업소코드, 주소)) %>% 
  # 지리정보(geometry) 제거
  st_drop_geometry() %>% 
  distinct(코드, .keep_all = TRUE)


##########  유역 매칭 확인  ####################################################
## 유역별 점유율 자료 기반 유역 확인
양식장_유역_확인 <- 양식장현황_정리 %>% 
  left_join(share %>% select(주소코드 = 주소, 단위유역), by = "주소코드") %>% 
  distinct()

## 좌표 및 GIS 이용한 유역 자료와 비교 검토
양식장_유역_확인 %<>% 
  left_join(양식장_유역 %>% select(업소코드, 단위유역2=단위유역), by = "업소코드") %>% 
  group_by(업소코드) %>%
  mutate(중복 = length(업소코드)) %>%
  ungroup() %>% 
  mutate(유역확인 = ifelse(단위유역 == 단위유역2, "O", "X"))


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













## 연도 및 동리별 시설면적 합계 정리
fishfarm_total <- 양식장현황_최종 %>%
  group_by(연도, 주소, 시군, 종류) %>%
  summarise(시설면적 = sum(시설면적), .groups = "drop")

## 각 동리별 단위유역 점유율 계산(소유역 점유율 합계)
share_fishfarm <- share %>%
  group_by(주소, 단위유역, 시군구) %>%
  summarise(양식계 = sum(양식계) / 100, .groups = "drop") %>%
  mutate(
    가두리 = "가두리",
    유수식 = "유수식",
    도전양식 = "도전양식",
    지수식 = "지수식"
  ) %>%
  pivot_longer(cols = 가두리:지수식, names_to = "종류", values_to = "임시") %>%
  select(-임시) %>% 
  rename(시군 = 시군구)

## 유역/시군 기준 시설면적 합계 연도별 정리
fishfarm_sum <- tibble()

for (i in 2022:2022) {
  temp <- share_fishfarm %>%
    mutate(연도 = i) %>%
    left_join(fishfarm_total, by = c("주소", "연도", "시군", "종류")) %>%
    # 동리별 시설면적 합계와 유역 점유율 계산
    mutate(시설면적 = round2(양식계 * 시설면적, 2)) %>%
    group_by(연도, 단위유역, 시군, 종류) %>%
    summarise(시설면적 = sum(시설면적, na.rm = T), .groups = "drop")
  
  fishfarm_sum <- bind_rows(fishfarm_sum, temp)
}

## *****  소계 계산  ***********************************************************
# 유역별 합계
fishfarm_subtotal_1 <- fishfarm_sum %>%
  group_by(연도, 단위유역, 종류) %>%
  summarise(시설면적 = sum(시설면적), .groups = "drop") %>%
  mutate(시군 = "합계") %>%
  select(연도, 단위유역, 시군, 종류, 시설면적)

# 유역별 합계 합치기
fishfarm_sum <- bind_rows(fishfarm_sum, fishfarm_subtotal_1)

# 시군및 유역별 소계
fishfarm_subtotal_2 <- fishfarm_sum %>%
  group_by(연도, 단위유역, 시군) %>%
  summarise(시설면적 = sum(시설면적), .groups = "drop") %>%
  mutate(종류 = "소계") %>%
  select(연도, 단위유역, 시군, 종류, 시설면적)

# 시군및 유역별 소계 합치기
fishfarm_sum <- bind_rows(fishfarm_sum, fishfarm_subtotal_2)

# 시군별 합계
fishfarm_subtotal_3 <- fishfarm_sum %>%
  group_by(연도, 시군, 종류) %>%
  summarise(시설면적 = sum(시설면적), .groups = "drop") %>%
  mutate(단위유역 = "합계") %>%
  select(연도, 단위유역, 시군, 종류, 시설면적)

# 시군별 합계 합치기
fishfarm_sum <- bind_rows(fishfarm_sum, fishfarm_subtotal_3)
## *****************************************************************************


## 권역추가, 단위유역, 시군 순서 설정 및 각 연도를 열로 변경(wide 포맷)
fishfarm_sum <- fishfarm_sum %>%
  mutate(
    단위유역 = factor(단위유역, levels = c(
      "합계", "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A",
      "섬강A", "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
      "홍천A", "한탄A", "한강B", "제천A", "한강D", "북한D", "한탄B", "임진A",
      "낙본A", "기타"
    )),
    시군 = factor(시군, levels = c(
      "합계", "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군",
      "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군",
      "양구군", "인제군", "고성군", "동해시", "속초시", "양양군"
    )),
    권역 = case_when(
      단위유역 == "골지A" | 단위유역 == "오대A" | 단위유역 == "주천A" |
        단위유역 == "평창A" | 단위유역 == "옥동A" | 단위유역 == "한강A" ~ "남한강",
      단위유역 == "섬강A" | 단위유역 == "섬강B" ~ "섬강",
      단위유역 == "북한A" | 단위유역 == "북한B" | 단위유역 == "소양A" |
        단위유역 == "인북A" | 단위유역 == "소양B" | 단위유역 == "북한C" ~ "북한강",
      단위유역 == "홍천A" ~ "홍천강",
      단위유역 == "한탄A" ~ "한탄강",
      단위유역 == "한강B" | 단위유역 == "제천A" | 단위유역 == "한강D" ~ "충청북도",
      단위유역 == "북한D" | 단위유역 == "한탄B" | 단위유역 == "임진A" ~ "경기도",
      단위유역 == "낙본A" ~ "낙동강",
      TRUE ~ "기타"
    ),
    종류 = factor(종류, levels = c(
      "가두리", "유수식", "도전양식", "지수식", "소계"
    ))
  ) %>%
  select(권역, everything()) %>%
  # 각 연도를 열로 변경(wide 포맷)
  pivot_wider(names_from = 연도, values_from = 시설면적) %>%
  arrange(단위유역, 시군, 종류)

## 시군 기준 정리
fishfarm_sum_s <- fishfarm_sum %>%
  arrange(시군, 단위유역, 종류) %>%
  select(시군, everything(), -권역)
# filter(시군 != "합계")
