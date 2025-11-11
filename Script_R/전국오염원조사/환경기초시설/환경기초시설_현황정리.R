#####  라이브러리 로드  ########################################################
library(tidyverse)
library(readxl)
library(writexl)


##**************************************************************************** ##
##############################  1. 시설 현황 확인  #############################
##**************************************************************************** ##


#####  1-1. 데이터 불러오기  ###################################################

## 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/기준배출수질/환경기초시설 데이터",
  pattern = "*.xls", full.names = T
)

## 경로지정된 파일 불러오기(파일이 여러개인 경우 병합)
시설현황_원본 <- files %>%
  # map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
  map_dfr(read_excel, sheet = "환경기초시설", skip = 3, col_names = F)

## 파일 불러오기
# 시설현황_원본 <- read_excel(
#   "전국오염원조사/환경기초시설/2023년기준_전국오염원_조사자료_환경기초시설.xlsx",
#   sheet = 1, skip = 3, col_names = F
# )

## 환경기초시설별 단위유역 현황 자료 불러오기
stp_유역 <- read_excel(
  "전국오염원조사/환경기초시설/환경기초시설 현황/환경기초시설_현황.xlsx"
) %>%
  select(시설코드, 단위유역)


#####  1-2. 데이터 정리  ###################################################

시설현황_정리 <- 시설현황_원본 %>%
  select(2:10, 18:20, 26) %>%
  set_names(c(
    "시설명", "시설코드", "구분", "시도", "시군", "읍면동", "리", 
    "본번", "부번", "물리", "생물", "고도", "가동개시"
  )) %>%
  # filter(!(시군 %in% c("동해시", "속초시", "양양군"))) %>%
  mutate(
    주소 =
      str_c(
        "강원특별자치도", " ", 시군, " ", 읍면동, " ",
        ifelse(is.na(리), "", str_c(리, " ")),
        본번,
        ifelse(부번 == 0 | is.na(부번), "", str_c("-", 부번))
      ),
    동리코드 = str_c(시군, 읍면동, ifelse(is.na(리), "", 리), sep = " ") %>%
      str_trim(),
    across(c(물리:고도), as.numeric),
    across(c(물리:고도), ~ replace(., is.na(.), 0)),
    용량 = 물리 + 생물 + 고도,
    가동개시 = str_sub(가동개시, 1, 4) %>% as.numeric(.),
    종류 = str_sub(시설코드, 7, 7),
    종류 = case_when(
        종류 == "W" ~ "하수처리시설", # 500톤 이상 공공하수처리시설
        종류 == "V" ~ "마을하수도", # 500톤 미만 소규모 하수처리시설
        종류 == "A" ~ "농공단지폐수처리시설",
        종류 == "I" ~ "산업단지폐수처리시설",
        종류 == "F" ~ "분뇨처리시설",
        종류 == "S" ~ "축산폐수처리시설",
        종류 == "H" ~ "오수처리시설"
      )
  ) %>%
  relocate(용량, .before = 물리) %>% 
  relocate(종류, .before = 구분) %>%
  left_join(stp_유역, by = "시설코드") %>%
  distinct()


##****************************************************************************##
######################  2. 단위유역 미확인 시설 유역 확인  #####################
##****************************************************************************##

## 주소 좌표 변환 관련 라이브러리 로드
library(httr)
library(jsonlite) # fromJSON()
library(progress)

##### 2-1. 동리별 유역 현황과 매칭 -------------------------------------------
# 동리별 유역현황 자료 중 유역 점유율이 100%인 지역만 선택
동리별_유역현황 <- read_excel("전국오염원조사/동리별_유역현황.xlsx") %>%
  filter(점유율 == 100) %>%
  select(동리코드, 동리별_유역 = 단위유역)

# 동리별 유역현황 자료와 매칭하여 유역 확인
시설현황_정리 %<>% left_join(동리별_유역현황, by = "동리코드") %>%
  mutate(확인 = ifelse(단위유역 == 동리별_유역, 0, 1)) %>%
  mutate(단위유역 = ifelse(is.na(단위유역), 동리별_유역, 단위유역))

# 유역 확인용 열 삭제
시설현황_정리 %<>% select(-c(동리별_유역, 확인))


##### 2-2. 주소 좌표 변환 ----------------------------------------------------
## 단위유역 미확인 시설 시설명 및 주소 추출 후 ID 부여
address_ind <- 시설현황_정리 %>%
  filter(is.na(단위유역)) %>%
  rowid_to_column(var = "ID")

## 주소 목록 list 생성
address_list <- address_ind$주소

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


## 주소 검색 결과 정리
# 중복 주소 정리
# 기존 주소에서 카카오맵 검색결과 읍면동이 일치하지 않는 경우 삭제
result %<>%
  mutate(확인 = ifelse(is.na(x),
                     TRUE,
                     str_detect(주소, address$region_3depth_name)
  )) %>%
  filter(확인 == TRUE)


#####  2-3. 유역 확인 --------------------------------------------------------

library(sf)

## 경위도좌표 정리
경위도 <- result %>%
  mutate(across(c(x, y), as.numeric)) %>%
  select(시설명:동리코드, x, y)

## 경위도 point 자료 생성(경위도 좌표계(WGS84) 적용 - EPSG:4326)
경위도_point <- 경위도 %>%
  filter(!is.na(x)) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)


## 좌표를 EPSG:5174로 변경(단위유역 shp 파일과 좌표계 통일)
경위도_point_5174 <- sf::st_transform(경위도_point, crs = 5174)


## 단위유역 shp 파일 불러오기(sf객체)
단위유역 <- sf::st_read("D:/GIS/유역/강원도_유역도_최종(이티워터)/총량단위유역_강원도_1985M_20200924.shp") %>%
  select(SW_NAME, geometry) %>%
  rename(단위유역 = SW_NAME)

## 단위유역 정리
stp_유역2 <- sf::st_join(경위도_point_5174, 단위유역) %>%
  # select(연도, 시군, 업소명, 주소전체, 지번주소, 단위유역 = SW_NAME) %>%
  mutate(단위유역 = ifelse(is.na(단위유역), "기타", 단위유역)) %>%
  # 지리정보(geometry) 제거
  sf::st_drop_geometry()

## 유역 확인된 시설 기존 정리자료와 합치기
시설현황_정리 %<>%
  filter(!is.na(단위유역)) %>%
  bind_rows(stp_유역2) %>%
  mutate(
    시군 = factor(시군, levels = c(
      "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군",
      "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군",
      "양구군", "인제군", "고성군", "동해시", "속초시", "양양군"
    ))
  ) %>%
  arrange(시군, 시설명)


##****************************************************************************##
############################  3. 정리자료 내보내기  ############################
##****************************************************************************##

##  파일 내보내기
write_xlsx(
  시설현황_정리,
  "전국오염원조사/환경기초시설/환경기초시설 현황/환경기초시설_현황.xlsx"
)
