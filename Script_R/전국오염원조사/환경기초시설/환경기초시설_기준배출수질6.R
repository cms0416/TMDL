#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(readxl)
library(writexl)
library(nortest)


##**************************************************************************** ##
###############################  1. 정규성 검증  ###############################
##**************************************************************************** ##


#####  1-1. 데이터 불러오기  ###################################################

## 환경기초시설 현황 자료 불러오기
시설현황 <- read_excel(
  "전국오염원조사/환경기초시설/환경기초시설 현황/환경기초시설_현황.xlsx"
)

## 환경기초시설 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/기준배출수질/환경기초시설 데이터",
  pattern = "*.xls", full.names = T
)

## 환경기초시설 방류량 자료 불러오기(파일이 여러개인 경우 병합)
data_원본 <- files %>%
  # map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
  map_dfr(~ {
    data <- read_excel(.x, sheet = "방류량", skip = 3, col_names = F)
    # 데이터 형식이 달라서 합쳐지지 않는 문제 해결
    # mutate(across(everything(), as.character))
  })

## 데이터 정리
data_정리 <- data_원본 %>%
  select(1, 7, 10, 14) %>%
  set_names(c("시설명", "유량", "BOD", "TP")) %>%
  # 시군, 유역 추가
  left_join(시설현황 %>% select(시설명, 시군, 단위유역),
    by = "시설명"
  ) %>%
  # 기타 수계 제외
  filter(단위유역 != "기타") %>%
  mutate(across(c(유량:TP), as.numeric))


#####  1-2. 정규성 검증  #######################################################

## 정규성 검증 함수 정의
run_normality_tests <- function(df, raw_col) {
  log_col <- paste0("ln_", raw_col)
  result_col <- paste0(raw_col, "_정규성")

  df %>%
    filter(!is.na(.data[[raw_col]]), .data[[raw_col]] > 0) %>%
    mutate(!!log_col := log(.data[[raw_col]])) %>%
    group_by(시설명) %>%
    # 정규성 검증
    # - Shapiro-wilk, Anderson-Darling, Kolmogorov-Smirnov 3가지 방법 중
    #   1개 이상 정규성이면 정규성으로 판단
    # - ks.test는 모집단 평균(μ), 표준편차(σ)를 “사전 고정”된 모수로 가정하므로,
    #   표본에서 추정한 μ̂, σ̂를 사용할 경우 p-value가 과대·과소 평가될 수 있음
    # - 따라서 모수추정 보정을 포함한 Lilliefors 테스트(nortest::lillie.test)를 사용
    summarise(
      # 최소 샘플 개수 미만 및 모든 값이 동일한 경우(분산 0) NA로 처리
      n = n(),
      sd_check = sd(.data[[log_col]], na.rm = TRUE),
      KS = if (n >= 5 && sd_check > 0) {
        tryCatch(lillie.test(.data[[log_col]])$p.value,
          error = function(e) NA
        )
      } else {
        NA
      },
      SW = if (n >= 3 && n <= 5000 && sd_check > 0) {
        tryCatch(shapiro.test(.data[[log_col]])$p.value,
          error = function(e) NA
        )
      } else {
        NA
      },
      AD = if (n >= 8 && sd_check > 0) {
        tryCatch(ad.test(.data[[log_col]])$p.value,
          error = function(e) NA
        )
      } else {
        NA
      },
      !!result_col := case_when(
        is.na(KS) & is.na(SW) & is.na(AD) ~ NA_character_,
        KS > 0.05 | SW > 0.05 | AD > 0.05 ~ "정규성",
        TRUE ~ "비정규성"
      ),
      .groups = "drop"
    ) %>%
    select(-sd_check)
}


## 정규성 검증
BOD_정규성검증 <- run_normality_tests(data_정리, raw_col = "BOD")
TP_정규성검증 <- run_normality_tests(data_정리, raw_col = "TP")



##**************************************************************************** ##
############################  2. 기준배출수질 산정  ############################
##**************************************************************************** ##


#####  2-1. 기준배출수질(가)  ##############################################

## 시행규칙 별표3 가 방식(로그정규분포) 기준배출수질 함수
calc_parametric_limit <- function(df, raw_col) {
  log_col    <- paste0("ln_", raw_col)
  mean_col   <- paste0(raw_col, "_mean")
  sd_col     <- paste0(raw_col, "_sd")
  result_col <- paste0(raw_col, "_가")
  
  df %>%
    # NA 및 0 값 제거, 로그 변환
    filter(!is.na(.data[[raw_col]]), .data[[raw_col]] != 0) %>%
    mutate(!!log_col := log(.data[[raw_col]])) %>%
    group_by(시설명) %>%
    summarise(
      !!mean_col   := mean(.data[[log_col]], na.rm = TRUE),
      !!sd_col     := sd(.data[[log_col]],   na.rm = TRUE),
      !!result_col := exp(
        mean(.data[[log_col]], na.rm = TRUE) +
          1.645 * sd(.data[[log_col]], na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

## 기준배출수질(가)
BOD_기준배출수질_가 <- calc_parametric_limit(data_정리, "BOD")
TP_기준배출수질_가 <- calc_parametric_limit(data_정리, "TP")


#####  2-2. 기준배출수질(나)  ##############################################

# 시행규칙 별표3 나 방식(비정규분포) 기준배출수질 계산 함수
calc_nonparametric_limit <- function(df, raw_col) {
  result_col <- paste0(raw_col, "_나")
  count_col <- paste0(raw_col, "_개수")
  max_col <- paste0(raw_col, "_최대")

  df %>%
    # NA 및 0 값 제거
    filter(!is.na(.data[[raw_col]]), .data[[raw_col]] != 0) %>%
    group_by(시설명) %>%
    arrange(.data[[raw_col]], .by_group = TRUE) %>%
    summarise(
      n = n(), # 데이터 개수
      a = floor(1 + 0.95 * (n - 1)), # 1+0.95×(측정횟수-1)의 정수부
      b = (1 + 0.95 * (n - 1)) - a, # 1+0.95×(측정횟수-1)의 소수부분
      Xa = nth(.data[[raw_col]], a), # nth(): 변수의 a번째 행 출력
      Xa1 = nth(.data[[raw_col]], a + 1), # nth(): 변수의 a+1번째 행 출력
      result = (1 - b) * Xa + b * Xa1, # 기준배출수질 = (1-b) × Xa + b × X(a+1)
      !!max_col := max(.data[[raw_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(
      !!count_col := n,
      !!result_col := result
    )
}

## 기준배출수질(나)
BOD_기준배출수질_나 <- calc_nonparametric_limit(data_정리, "BOD")
TP_기준배출수질_나 <- calc_nonparametric_limit(data_정리, "TP")


#####  2-3. 평균유량 산정  #####################################################
평균유량 <- data_정리 %>%
  select(시군, 단위유역, 시설명, 유량) %>%
  group_by(시군, 단위유역, 시설명) %>%
  reframe(평균유량 = round(mean(유량), 1))



##**************************************************************************** ##
################################  3. 최종 정리  ################################
##**************************************************************************** ##

## 2030년 계획 자료 불러오기
최종년도계획 <-
  read_excel("전국오염원조사/기준배출수질/환경기초시설_2030년_계획.xlsx") %>%
  select(-c(준공연도, 시설코드))

## 기준배출 수질 확인 및 정리
기준배출수질_최종 <- 최종년도계획 %>%
  full_join(평균유량, by = c("시군", "단위유역", "시설명")) %>%
  left_join(BOD_정규성검증 %>% select(시설명, BOD_정규성), by = "시설명") %>%
  left_join(BOD_기준배출수질_가 %>% select(시설명, BOD_가), by = "시설명") %>%
  left_join(BOD_기준배출수질_나 %>% select(시설명, BOD_나, BOD_최대, BOD_개수), by = "시설명") %>%
  left_join(TP_정규성검증 %>% select(시설명, TP_정규성), by = "시설명") %>%
  left_join(TP_기준배출수질_가 %>% select(시설명, TP_가), by = "시설명") %>%
  left_join(TP_기준배출수질_나 %>% select(시설명, TP_나, TP_최대, TP_개수), by = "시설명") %>%
  left_join(시설현황 %>% select(시설명, 가동개시, 용량), by = "시설명") %>%
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
    # 샘플 개수에 따라서 구분, 정규성 검정이 안된 경우 NA로 유지
    BOD_정규성 = case_when(
      is.na(BOD_정규성) ~ NA,
      BOD_개수 < 30 ~ "n<30",
      BOD_개수 >= 347 ~ "n>=347",
      .default = BOD_정규성
    ),
    TP_정규성 = case_when(
      is.na(TP_정규성) ~ NA,
      TP_개수 < 30 ~ "n<30",
      TP_개수 >= 347 ~ "n>=347",
      .default = TP_정규성
    ),
    # 기준배출수질 결정
    # - 가 방식 : 정규성인 경우
    # - 나 방식 : 비정규성 및 측정횟수 347회 이상
    # - 최대값 : 측정횟수 30회 미만
    BOD_기준 = case_when(
      BOD_정규성 == "정규성" ~ BOD_가,
      BOD_정규성 == "비정규성" ~ BOD_나,
      BOD_정규성 == "n>=347" ~ BOD_나,
      BOD_정규성 == "n<30" ~ BOD_최대
    ) %>% round(., 1),
    TP_기준 = case_when(
      TP_정규성 == "정규성" ~ TP_가,
      TP_정규성 == "비정규성" ~ TP_나,
      TP_정규성 == "n>=347" ~ TP_나,
      TP_정규성 == "n<30" ~ TP_최대
    ) %>% round(., 3),
    구분 = case_when(
      is.na(구분) ~ "신규", 
      구분 == "미준공" & is.na(가동개시) ~ "제외",
      is.na(가동개시) ~ "폐쇄",
      .default = "기존"
    )
    # 가동개시 = ifelse(is.na(가동개시), 준공연도, 가동개시)
  ) %>%
  relocate(c(평균유량, BOD_기준, TP_기준), .after = TP_30년) %>%
  relocate(c(가동개시, 용량), .after = 구분) %>%
  arrange(시군, 단위유역, 구분, 가동개시, 시설명) %>%
  filter(
    !단위유역 %in% c("북한D", "임진A"),
    구분 != "제외"
  )


### 최종 정리 자료 내보내기
write_xlsx(
  list(
    "기준배출수질_최종" = 기준배출수질_최종,
    "BOD_정규성검증" = BOD_정규성검증,
    "BOD_기준배출수질_가" = BOD_기준배출수질_가,
    "BOD_기준배출수질_나" = BOD_기준배출수질_나,
    "TP_정규성검증" = TP_정규성검증,
    "TP_기준배출수질_가" = TP_기준배출수질_가,
    "TP_기준배출수질_나" = TP_기준배출수질_나
  ),
  path = "전국오염원조사/기준배출수질/Output/기준배출수질_2024년기준.xlsx"
)

