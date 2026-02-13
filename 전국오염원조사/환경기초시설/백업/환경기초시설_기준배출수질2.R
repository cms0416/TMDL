#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(readxl)
library(writexl)
library(nortest)

library(httr)
library(jsonlite) # fromJSON()
library(progress)


##****************************************************************************##
##############################  0. 시설 현황 확인  #############################
##****************************************************************************##


#####  0-1. 데이터 불러오기  ###################################################

# 데이터 경로지정 및 데이터 목록
# files <- list.files(
#   path = "전국오염원조사/기준배출수질/환경기초시설 데이터",
#   pattern = "*.xls", full.names = T
# )

# 경로지정된 파일 합치기
# 시설현황_원본 <- files %>%
#   # map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
#   map_dfr(read_excel, sheet = 1, skip = 3, col_names = F)

시설현황_원본 <- read_excel(
  "전국오염원조사/환경기초시설/2023년기준_전국오염원_조사자료_환경기초시설_(가확정).xlsx",
  sheet = 1, skip = 3, col_names = F
)
  
stp_유역 <- read_excel(
  "전국오염원조사/환경기초시설/환경기초시설 현황/환경기초시설_현황(2023).xlsx"
) %>%
  select(2, 14)


#####  0-2. 데이터 정리  ###################################################

시설현황_정리 <- 시설현황_원본 %>%
  select(2:10, 26) %>%
  set_names(c(
    "처리시설명", "처리시설코드", "구분", "시도", "시군",
    "읍면동", "리", "본번", "부번", "가동개시"
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
    동리 = ifelse(is.na(리), 읍면동, 리),
    동리코드 = str_c(시군, 읍면동, 동리, sep = " "),
    가동개시 = str_sub(가동개시, 1, 4)
  ) %>%
  left_join(stp_유역, by = "처리시설코드") %>% 
  distinct()
#   left_join(share %>% rename(단위유역2 = 단위유역), by = "동리코드") %>%
#   mutate(단위유역 = ifelse(is.na(단위유역), 단위유역2, 단위유역))


#####  0-3. 단위유역 미확인 시설 유역 확인  ####################################

##### 0-3-1. 주소 좌표 변환 ----------------------------------------------------
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


#####  0-3-2. 유역 확인 --------------------------------------------------------

library(sf)

## 경위도좌표 정리
경위도 <- result %>% 
  mutate(across(c(x, y), as.numeric)) %>% 
  select(처리시설명:동리코드, x, y)

## 경위도 point 자료 생성(경위도 좌표계(WGS84) 적용 - EPSG:4326)
경위도_point <- 경위도 %>% st_as_sf(coords = c('x', 'y'), crs = 4326)

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
    ))) %>% 
  arrange(시군, 처리시설명)

## 정리자료 내보내기
write_xlsx(
  시설현황_정리, 
  "전국오염원조사/환경기초시설/환경기초시설 현황/환경기초시설_현황(2023).xlsx"
  )


##****************************************************************************##
###############################  1. 정규성 검증  ###############################
##****************************************************************************##


#####  1-1. 데이터 불러오기  ###################################################

# 데이터 경로지정 및 데이터 목록
# files <- list.files(
#   path = "전국오염원조사/기준배출수질/환경기초시설 데이터",
#   pattern = "*.xls", full.names = T
# )
# 
# # 경로지정된 파일 합치기
# data_원본 <- files %>%
#   # map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
#   map_dfr(~ {
#     data <- read_excel(.x, sheet = 5, skip = 3, col_names = F) %>% 
#       # 데이터 형식이 달라서 합쳐지지 않는 문제 해결
#       mutate(across(c(1:55), as.character))
#   })
    

data_원본 <- read_excel(
  "전국오염원조사/환경기초시설/2023년기준_전국오염원_조사자료_환경기초시설_(가확정).xlsx",
  sheet = 5, skip = 3, col_names = F
)

data_정리 <- data_원본 %>%
  select(1, 7, 10, 14) %>%
  set_names(c("처리시설명", "유량", "BOD", "TP")) %>%
  # 시군, 유역 추가
  left_join(시설현황_정리 %>% select(처리시설명, 시군, 단위유역),
    by = "처리시설명"
  ) %>%
  # 기타 수계 제외
  filter(단위유역 != "기타") %>%
  mutate(across(c(유량:TP), as.numeric))


#####  1-2. BOD 정규성 검증  ###################################################

## 데이터 정리
BOD_data <- data_정리 %>%
  select(처리시설명, BOD) %>%
  # BOD  결측값 및 0값 제거
  filter(!is.na(BOD), BOD != 0) %>%
  # BOD 수치 로그값 추가
  mutate(ln_BOD = log(BOD, base = exp(1)))

## 데이터 측정 횟수 확인
BOD_측정횟수 <- BOD_data %>%
  count(처리시설명) %>%
  rename(BOD_개수 = n)

## 측정 횟수 30회 이상만 필터(30회 미만은 최대값 적용)
BOD_data_2 <- BOD_data %>%
  left_join(BOD_측정횟수, by = "처리시설명") %>%
  filter(BOD_개수 >= 30) %>%
  select(-c(BOD, BOD_개수))

## 정규성 검증
BOD_정규성검증 <- BOD_data_2 %>%
  group_by(처리시설명) %>%
  reframe(
    KS = map(., ~ lillie.test(ln_BOD)) %>% map_dbl("p.value"),
    SW = map(., ~ shapiro.test(ln_BOD)) %>% map_dbl("p.value"),
    AD = map(., ~ ad.test(ln_BOD)) %>% map_dbl("p.value")
  ) %>%
  distinct_all() %>%
  mutate(BOD_정규성 = ifelse(KS > 0.05 | SW > 0.05 | AD > 0.05, "정규성", "비정규성"))


#####  1-2. T-P 정규성 검증  ###################################################

## 데이터 정리
TP_data <- data_정리 %>%
  select(처리시설명, TP) %>%
  # TP 결측값 및 0값 제거
  filter(!is.na(TP), TP != 0) %>%
  # TP 수치 로그값 추가
  mutate(ln_TP = log(TP, base = exp(1)))

## 데이터 측정 횟수 확인
TP_측정횟수 <- TP_data %>%
  count(처리시설명) %>%
  rename(TP_개수 = n)

## 측정 횟수 30회 이상만 필터(30회 미만은 최대값 적용)
TP_data_2 <- TP_data %>%
  left_join(TP_측정횟수, by = "처리시설명") %>%
  filter(TP_개수 >= 30) %>%
  select(-c(TP, TP_개수))

## 정규성 검증
TP_정규성검증 <- TP_data_2 %>%
  group_by(처리시설명) %>%
  reframe(
    KS = map(., ~ lillie.test(ln_TP)) %>% map_dbl("p.value"),
    SW = map(., ~ shapiro.test(ln_TP)) %>% map_dbl("p.value"),
    AD = map(., ~ ad.test(ln_TP)) %>% map_dbl("p.value")
  ) %>%
  distinct_all() %>%
  mutate(TP_정규성 = ifelse(KS > 0.05 | SW > 0.05 | AD > 0.05, "정규성", "비정규성"))



##****************************************************************************##
############################  2. 기준배출수질 산정  ############################
##****************************************************************************##


#####  2-1. 기준배출수질(가)  ##############################################

## BOD 기준배출수질(가)
BOD_기준배출수질_가 <- BOD_data %>%
  group_by(처리시설명) %>%
  reframe(
    mean_BOD = mean(ln_BOD),
    sd_BOD = sd(ln_BOD),
    BOD_가 = exp(1)^(mean_BOD + sd_BOD * 1.645)
  )

## TP 기준배출수질(가)
TP_기준배출수질_가 <- TP_data %>%
  group_by(처리시설명) %>%
  reframe(
    mean_TP = mean(ln_TP),
    sd_TP = sd(ln_TP),
    TP_가 = exp(1)^(mean_TP + sd_TP * 1.645)
  )


#####  2-2. 기준배출수질(나)  ##############################################

## BOD 기준배출수질(나)
BOD_기준배출수질_나 <- BOD_data %>%
  group_by(처리시설명) %>%
  arrange(BOD, .by_group = TRUE) %>%
  reframe(
    count = n(),
    NO_BOD = 1 + 0.95 * (count - 1),
    a_BOD = floor(NO_BOD),
    b_BOD = NO_BOD - a_BOD,
    Xa_BOD = nth(BOD, a_BOD),
    Xaa_BOD = nth(BOD, a_BOD + 1),
    BOD_나 = (1 - b_BOD) * Xa_BOD + b_BOD * Xaa_BOD # 환경기초시설별 산식으로 계산(비정규성분포)
  )

## TP 기준배출수질(나)
TP_기준배출수질_나 <- TP_data %>%
  group_by(처리시설명) %>%
  arrange(TP, .by_group = TRUE) %>%
  reframe(
    count = n(),
    NO_TP = 1 + 0.95 * (count - 1),
    a_TP = floor(NO_TP),
    b_TP = NO_TP - a_TP,
    Xa_TP = nth(TP, a_TP),
    Xaa_TP = nth(TP, a_TP + 1),
    TP_나 = (1 - b_TP) * Xa_TP + b_TP * Xaa_TP
  )


#####  2-3. 최대값 산정  #######################################################

## BOD 최대값
BOD_최대값 <- BOD_data %>%
  group_by(처리시설명) %>%
  reframe(BOD_최대 = max(BOD))

## TP 최대값
TP_최대값 <- TP_data %>%
  group_by(처리시설명) %>%
  reframe(TP_최대 = max(TP))


#####  2-4. 평균유량 산정  #####################################################
평균유량 <- data_정리 %>%
  select(시군, 단위유역, 처리시설명, 유량) %>%
  group_by(시군, 단위유역, 처리시설명) %>%
  reframe(평균유량 = round(mean(유량), 1))


#####  3. 최종 정리  ###########################################################

## 2030년 계획 자료 불러오기
최종년도계획 <- 
  read_excel("전국오염원조사/기준배출수질/환경기초시설_2030년_계획.xlsx") %>% 
  select(-c(준공연도, 처리시설코드))

## 기준배출 수질 확인 및 정리
# 기준배출수질_최종 <- 평균유량 %>%
#   left_join(최종년도계획, by = c("시군", "단위유역","처리시설명")) %>%

기준배출수질_최종 <- 최종년도계획 %>%
  full_join(평균유량, by = c("시군", "단위유역","처리시설명")) %>% 
  left_join(BOD_기준배출수질_가 %>% select(처리시설명, BOD_가), by = "처리시설명") %>%
  left_join(BOD_기준배출수질_나 %>% select(처리시설명, BOD_나), by = "처리시설명") %>%
  left_join(BOD_최대값, by = "처리시설명") %>%
  left_join(BOD_측정횟수, by = "처리시설명") %>%
  left_join(BOD_정규성검증 %>% select(처리시설명, BOD_정규성), by = "처리시설명") %>%
  left_join(TP_기준배출수질_가 %>% select(처리시설명, TP_가), by = "처리시설명") %>%
  left_join(TP_기준배출수질_나 %>% select(처리시설명, TP_나), by = "처리시설명") %>%
  left_join(TP_최대값, by = "처리시설명") %>%
  left_join(TP_측정횟수, by = "처리시설명") %>%
  left_join(TP_정규성검증 %>% select(처리시설명, TP_정규성), by = "처리시설명") %>%
  left_join(시설현황_정리 %>% select(처리시설명, 가동개시), by = "처리시설명") %>% 
  mutate(
    시군 = factor(시군, levels = c(
      "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군",
      "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군",
      "양구군", "인제군", "고성군", "동해시", "속초시", "양양군"
    )),
    단위유역 = factor(단위유역, levels = c(
      "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A",
      "섬강A", "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
      "홍천A", "한탄A", "한강B", "제천A", "한강D", "북한D", "한탄B", "임진A",
      "낙본A"
    )),
    BOD_정규성 = ifelse(is.na(BOD_정규성), "30회미만", BOD_정규성),
    TP_정규성 = ifelse(is.na(TP_정규성), "30회미만", TP_정규성),
    # 기준배출수질 : 정규성 가 방식, 비정규성 나 방식, 30회 미만 최대값
    BOD_기준 = case_when(
      BOD_정규성 == "정규성" ~ BOD_가,
      BOD_정규성 == "비정규성" ~ BOD_나,
      BOD_정규성 == "30회미만" ~ BOD_최대
    ) %>% round(., 1),
    TP_기준 = case_when(
      TP_정규성 == "정규성" ~ TP_가,
      TP_정규성 == "비정규성" ~ TP_나,
      TP_정규성 == "30회미만" ~ TP_최대
    ) %>% round(., 3),
    구분 = ifelse(is.na(구분), "신규", 구분)
  ) %>%
  relocate(c(평균유량, BOD_기준, TP_기준), .after = TP_30년) %>%
  relocate(가동개시, .after = 구분) %>% 
  arrange(시군, 단위유역, 구분, 처리시설명) %>% 
  filter(!단위유역 %in% c("북한D", "임진A"), 
         구분 != "미준공")


## 최종 정리 자료 내보내기
write_xlsx(기준배출수질_최종,
  path = "전국오염원조사/기준배출수질/Output/기준배출수질_2023년기준.xlsx"
)
