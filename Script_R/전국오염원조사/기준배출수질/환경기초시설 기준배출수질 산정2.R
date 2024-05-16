#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(readxl)
library(writexl)
library(nortest)


##****************************************************************************##
##############################  0. 시설 현황 확인  #############################
##****************************************************************************##


#####  0-1. 데이터 불러오기  ###################################################

# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/기준배출수질",
  pattern = "*.xls", full.names = T
)

# 경로지정된 파일 합치기
시설현황_원본 <- files %>%
  # map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
  map_dfr(read_excel, sheet = 1, skip = 3, col_names = F)

# 동리별 유역 점유율 자료
share <- read_excel("전국오염원조사/단위유역별 점유율.xlsx") %>%
  rename(면적점유율 = `면적점유율(%)`) %>%
  group_by(동리코드, 단위유역) %>%
  reframe(면적점유율 = sum(면적점유율)) %>%
  filter(면적점유율 > 0.98) %>%
  select(동리코드, 단위유역)

## 환경기초시설 유역 현황
stp_유역 <- read_excel(
  "전국오염원조사/환경기초시설/환경기초시설 현황/전오사 환경기초시설 현황 정리.xlsx",
  skip = 1
) %>%
  select(2, 3, 13) %>%
  mutate(단위유역 = str_remove_all(소유역명, "\\d"), .after = 처리시설코드) %>%
  select(-c(처리시설명, 소유역명))


#####  0-2. 데이터 정리  ###################################################

시설현황_정리 <- 시설현황_원본 %>%
  select(2:10) %>%
  set_names(c(
    "처리시설명", "처리시설코드", "구분", "시도", "시군",
    "읍면동", "리", "본번", "부번"
  )) %>%
  mutate(
    주소 =
      str_c(
        "강원특별자치도", " ", 시군, " ", 읍면동, " ",
        ifelse(is.na(리), "", str_c(리, " ")),
        본번,
        ifelse(부번 == 0 | is.na(부번), "", str_c("-", 부번))
      ),
    동리 = ifelse(is.na(리), 읍면동, 리),
    동리코드 = str_c(시군, 읍면동, 동리, sep = " ")
  ) %>%
  left_join(stp_유역, by = "처리시설코드") %>%
  left_join(share %>% rename(단위유역2 = 단위유역), by = "동리코드") %>%
  mutate(단위유역 = ifelse(is.na(단위유역), 단위유역2, 단위유역))



##****************************************************************************##
###############################  1. 정규성 검증  ###############################
##****************************************************************************##


#####  1-1. 데이터 불러오기  ###################################################

# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/기준배출수질",
  pattern = "*.xls", full.names = T
)

# 경로지정된 파일 합치기
data_원본 <- files %>%
  # map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
  map_dfr(read_excel, sheet = 5, skip = 3, col_names = F)

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
  select(시군, 처리시설명, 유량) %>%
  group_by(시군, 처리시설명) %>%
  reframe(평균유량 = round(mean(유량), 1))


#####  3. 최종 정리  ###########################################################

## 기준배출 수질 확인
기준배출수질_최종 <- 평균유량 %>%
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
  mutate(
    시군 = factor(시군, levels = c(
      "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군",
      "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군",
      "양구군", "인제군", "고성군", "동해시", "속초시", "양양군"
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
    ) %>% round(., 3)
  ) %>%
  relocate(c(BOD_기준, TP_기준), .after = 평균유량) %>%
  arrange(시군, 처리시설명)


## 최종 정리 자료 내보내기
write_xlsx(기준배출수질_최종,
  path = "전국오염원조사/기준배출수질/Output/기준배출수질.xlsx"
)
