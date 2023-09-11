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
share <- read_excel("전국오염원조사/단위유역별 점유율.xlsx") %>% 
  select(주소, 단위유역) %>% 
  distinct()

##########  자료 정리  #########################################################

## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
dir <- ("전국오염원조사/산업계")
files <- list.files(dir)

# 데이터 불러오기 및 합치기
산업계_원본 <- tibble()

for (file in files) {
  print(file)
  temp <- read_excel(file.path(dir, file), skip = 5, col_names = F) %>%
    # str_sub(문자열, 시작위치, 끝위치) : 연도 추출
    mutate(연도 = str_sub(file, 1, 4), .before = 1)
  
  # 2022년 이전 자료의 경우 부분위탁량(76열) 뒤에 폐기물처리 항목 추가
  if (str_sub(file, 1, 4) < 2022) {
    temp %<>% mutate(폐기물처리 = "", .after = 76)
  }
  
  산업계_원본 <- bind_rows(산업계_원본, temp)
}
## *****************************************************************************


## 데이터 정리
산업계_정리 <- 산업계_원본 %>%
  select(연도, 3, 4, 6, 9, 10, 11, 12, 13, 24, 38, 73, 82) %>%
  set_names(c(
    "연도", "관할기관", "휴업", "업소명", "시군", "읍면동", "리", "본번", "부번",
    "규모", "방류구수", "폐수발생량", "폐수방류량"
  )) %>% 
  # 휴업인 경우 삭제
  filter(is.na(휴업)) %>%
  # 폐수발생량, 폐수방류량, 연도 숫자로 지정 및 결측값 0으로 수정
  mutate_at(vars(폐수발생량, 폐수방류량, 연도), as.numeric) %>%
  mutate_at(vars(폐수발생량, 폐수방류량, 연도), ~ replace(., is.na(.), 0)) %>%
  # 폐수방류량이 음수인 경우 0으로 수정
  mutate(
    폐수방류량 = ifelse(폐수방류량 < 0, 0, 폐수방류량),
    # 지명 변경 반영
    # 양구군 남면 → 국토정중앙면 / 홍천군 동면 → 영귀미면 / 영월군 중동면 → 산솔면
    읍면동 = ifelse(시군 == "양구군" & 읍면동 == "남면", "국토정중앙면", 읍면동),
    읍면동 = ifelse(시군 == "홍천군" & 읍면동 == "동면", "영귀미면", 읍면동),
    읍면동 = ifelse(시군 == "영월군" & 읍면동 == "중동면", "산솔면", 읍면동),
    # 주소코드 추가("리"가 없는 "동"의 경우 "법정리"칸에 "동"으로 추가)
    주소 = str_c(시군, 읍면동, ifelse(is.na(리), 읍면동, 리), sep = " "),
    주소전체 =
      str_c(
        "강원도", " ", 시군, " ", 읍면동, " ",
        ifelse(is.na(리), "", str_c(리, " ")),
        본번,
        ifelse(부번 == 0 | is.na(부번), "", str_c("-", 부번))
      )
  ) %>% 
  select(-c(읍면동:부번))


##########  주소 좌표 변환  ###############################################################

## 업소명 및 주소 추출 후 ID 부여
address_ind <- 산업계_정리 %>%
  select(연도, 관할기관, 시군, 업소명, 주소전체) %>%
  filter(연도 == 2022) %>%
  rowid_to_column(var = "ID")

## 주소 목록 list 생성
address_list <- address_ind$주소전체

## progress bar 설정
pb <- progress_bar$new(
  format = " Progress: [:bar] :current / :total (:percent), Estimated completion time::eta",
  total = nrow(address_ind), # 총 tick 개수 (default 100)
  clear = FALSE, # 종료 후에도 진행 경과 출력 결과 유지 (default TRUE)
  width = 80 # 진행 경과 막대 너비
)

## tibble 사전 생성
result <- tibble()
temp <- tibble()

## 주소 검색 및 데이터 취합
for (i in 1:nrow(address_ind)) {
  # 카카오맵 검색
  place_list <- GET(
    url = "https://dapi.kakao.com/v2/local/search/address.json",
    query = list(query = address_list[i]),
    add_headers(Authorization = "KakaoAK 9e9a85a9ec8362e009da2f7bc4b3a09c")
  ) %>%
    content(as = "text") %>%
    fromJSON()
  
  # 업소명 및 기존 주소 불러오기
  temp_addr <- address_ind %>% filter(ID == i)
  
  ## 주소
  temp <- bind_cols(temp_addr, place_list$documents)
  result <- bind_rows(result, temp)
  
  ## 진행상황 확인
  pb$tick()
}

### 주소 검색 결과 정리
# 중복 주소 정리
# 기존 주소전체에서 카카오맵 검색결과 읍면동이 일치하지 않는 경우 삭제
result1 <- result %>%
  mutate(확인 = ifelse(is.na(x),
                     TRUE,
                     str_detect(주소전체, address$region_3depth_name)
  )) %>%
  filter(확인 == TRUE)


# 지번 주소 정리
result_jibun <- bind_cols(
  result1 %>% select(ID),
  result1$address) %>%
  select(
    ID, address_name, b_code, region_3depth_h_name,
    region_3depth_name, mountain_yn, main_address_no, sub_address_no, x, y
  ) %>%
  set_names(c("ID", "지번주소", "법정동코드", "읍면동", "동리", "산",
              "지번_본번", "지번_부번", "x", "y"))

# 도로명 주소 정리
result_doro <- bind_cols(
  result1 %>% select(ID),
  result1$road_address) %>%
  select(
    ID, address_name, road_name, main_building_no, sub_building_no,
    building_name, x, y
  ) %>% 
  set_names(c("ID", "도로명주소", "도로명", "도로_본번", "도로_부번", 
              "건물명", "도로_x", "도로_y"))

# 지번주소, 도로명 주소 합치기
result1 %<>%
  select(ID:주소전체) %>%
  left_join(result_jibun, by = "ID") %>% 
  left_join(result_doro, by = "ID") %>% 
  relocate(도로명주소, .after = 지번주소) %>% 
  relocate(c(x, y), .before = 도로_x) %>% 
  mutate(주소확인 = ifelse(is.na(지번주소) & is.na(도로명주소), "주소확인", ""))



##########  주소 검색 안되는 경우 업소명으로 재검색  ######################################

# 주소 확인 필요한 업소 정리
check_ind <- result1 %>% 
  filter(주소확인 == "주소확인") %>% 
  select(ID:주소전체) %>% 
  mutate(검색 = str_c(관할기관, 업소명, sep = " ")) %>% 
  rowid_to_column(var = "ID_2")

## 주소 목록 list 생성
address_list <- check_ind$검색

## progress bar 설정
pb <- progress_bar$new(
  format = " Progress: [:bar] :current / :total (:percent), Estimated completion time::eta",
  total = nrow(check_ind), # 총 tick 개수 (default 100)
  clear = FALSE, # 종료 후에도 진행 경과 출력 결과 유지 (default TRUE)
  width = 80 # 진행 경과 막대 너비
)

## tibble 사전 생성
result_chk <- tibble()

## 업소명 검색 및 데이터 취합
for (i in 1:nrow(check_ind)) {
  # 카카오맵 검색
  place_list <- GET(
    url = "https://dapi.kakao.com/v2/local/search/keyword.json",
    query = list(query = address_list[i]),
    add_headers(Authorization = "KakaoAK 9e9a85a9ec8362e009da2f7bc4b3a09c")
  ) %>%
    content(as = "text") %>%
    fromJSON()
  
  # 업소명 및 기존 주소 불러오기
  temp_addr <- check_ind %>% filter(ID_2 == i)
  
  ## 주소
  temp <- bind_cols(temp_addr, place_list$documents)
  result_chk <- bind_rows(result_chk, temp)
  
  ## 진행상황 확인
  pb$tick()
}

### 검색결과 정리
result_chk2 <- result_chk %>% 
  select(ID_2:주소전체, address_name, road_address_name, place_name, x, y) %>% 
  distinct(ID, .keep_all = TRUE)


### 시군 송부용 자료 정리
result_chk3 <- result_chk %>% 
  select(ID_2, 관할기관:주소전체, address_name, road_address_name, place_name) %>% 
  distinct(ID_2, .keep_all = TRUE) %>% 
  set_names(c("ID", "관할기관", "업소명", "기존주소", "검색_지번주소", 
              "검색_도로명주소", "검색_업소명")) %>% 
  mutate(관할기관 = str_remove(관할기관, "강원도 "))


##########  좌표 정리 및 유역 매칭  ############################################

library(sf)

## 경위도좌표 정리
경위도 <- result1 %>% 
  filter(시군 %in% c("춘천시", "철원군")) %>% 
  mutate(across(c(x, y), as.numeric)) %>% 
  select(연도, 시군, 업소명, 주소전체, 지번주소, x, y)

## 경위도 point 자료 생성(경위도 좌표계(WGS84) 적용 - EPSG:4326)
경위도_point <- 경위도 %>% st_as_sf(coords = c('x', 'y'), crs = 4326)

## 좌표를 EPSG:5174로 변경(단위유역 shp 파일과 좌표계 통일)
경위도_point_5174 <- sf::st_transform(경위도_point, crs = 5174)


## 단위유역 shp 파일 불러오기(sf객체)
단위유역 <- sf::st_read("D:/GIS/유역/강원도_유역도_최종(이티워터)/총량단위유역_강원도_1985M_20200924.shp")


## 양식장 단위유역 정리
산업계_유역 <- sf::st_join(경위도_point_5174, 단위유역) %>% 
  select(연도, 시군, 업소명, 주소전체, 지번주소, 단위유역 = SW_NAME) %>% 
  mutate(단위유역 = ifelse(is.na(단위유역), "기타", 단위유역),
         코드 = str_c(시군, 업소명)) %>% 
  # 지리정보(geometry) 제거
  st_drop_geometry() %>% 
  distinct(코드, .keep_all = TRUE)



##########  유역 자료 합치기  ##################################################
산업계_최종 <- 산업계_정리 %>%
  filter(연도 == 2022) %>% 
  filter(시군 %in% c("춘천시", "철원군")) %>% 
  # 휴업인 경우 삭제
  filter(is.na(휴업)) %>%
  left_join(산업계_유역 %>% select(시군, 업소명, 단위유역), by = c("시군", "업소명")) %>% 
  select(-c(휴업, 주소))
  

##########  오염원 현황 계산  ##################################################

## 연도 및 동리별 산업계 현황 정리
산업계_현황2022 <- 산업계_최종 %>%
  filter(!is.na(단위유역)) %>% 
  mutate(업소수 = 1) %>%
  group_by(연도, 시군, 단위유역) %>%
  summarise(
    업소수 = sum(업소수),
    폐수발생량 = sum(폐수발생량),
    폐수방류량 = sum(폐수방류량),
    .groups = "drop"
  ) %>% 
  rbind(
    산업계_최종 %>% 
      filter(!is.na(단위유역)) %>% 
      mutate(업소수 = 1) %>%
      group_by(연도, 시군) %>% 
      summarise(
        업소수 = sum(업소수),
        폐수발생량 = sum(폐수발생량),
        폐수방류량 = sum(폐수방류량),
        .groups = "drop"
      ) %>% 
      mutate(단위유역 = "합계")
  ) 


시군별유역현황 <- read_excel("전국오염원조사/시군별유역현황.xlsx") %>% 
  select(시군, 단위유역) %>% 
  filter(시군 %in% c("춘천시", "철원군"))


시군별_산업계_현황 <- 시군별유역현황 %>% 
  left_join(산업계_현황2022, by = c("시군", "단위유역")) %>% 
  pivot_longer(
    cols = 업소수:폐수방류량,
    names_to = "오염원",
    values_to = "2022"
  ) %>% 
  mutate(across(c(`2022`), ~replace(., is.na(.), 0))) %>% 
  select(-연도)








