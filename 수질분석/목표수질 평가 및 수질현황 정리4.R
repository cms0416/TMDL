##########  라이브러리 로드  ###################################################
library(tidyverse)
library(readxl)
library(writexl)


##########  파일 불러오기  #####################################################
### 반올림 사용자 정의 함수 로드
source("Script_R/Function/func_round2.R")

### 평가 연도 범위 설정(평가 시작 년도 +2 부터 당해 연도 까지)
final_year <- 2025
start_year <- final_year - 10
years <- (start_year + 2):final_year

### 데이터 불러오기 및 전처리(측정 자료에 목표수질, 권역, 강원도 여부 등 추가)
target <- read_excel("수질분석/목표수질.xlsx")

obs <- read_excel("수질분석/총량측정망_전체_2007_2025.xlsx") %>%
  left_join(target, by = "총량지점명") %>%
  filter(연도 >= start_year) %>% 
  select(-c(수온, pH, EC, DO, COD, SS, TN))



##########  연평균 산정  #######################################################

## 연평균 산정 함수 정의
calculate_mean <- function(data, var_name) {
  # 항목별 목표수질 열 이름 지정
  # substitute(x) : x가 무엇인지 참조한 뒤에 x에 들어있는 표현식을 그대로 반환
  # deparse() : 객체를 문자형 벡터로 변환
  # deparse(substitute(x)) : substitute의 결과인 표현식을 취해 문자형 벡터로 변환
  target_column <- str_c(deparse(substitute(var_name)), "_목표수질")
  # 소수점 자리수(decimal places)
  dp <- ifelse(substitute(var_name) == "TP", 3, 1)

  data %>%
    # sym() : 문자열을 심볼로 변환. 심볼은 변수나 칼럼 이름을 나타내는데 사용
    # !! : 심볼을 풀어서 해당 표현식을 실행하는데 사용
    # !!sym(var_nm) : 'var_nm' 변수의 값을 심볼로 변환하고, 함수(group_by)에 전달
    group_by(강원도, 권역, 총량지점명, !!sym(target_column), 연도) %>%
    summarise(mean_value = round2(mean({{ var_name }}, na.rm = TRUE), dp), .groups = "drop") %>%
    pivot_wider(names_from = 연도, values_from = mean_value)
}


## 각 항목별 연평균 산정
BOD_ymean <- calculate_mean(obs, BOD)
TP_ymean <- calculate_mean(obs, TP)
TOC_ymean <- calculate_mean(obs, TOC)


## BOD, T-P 통합 연평균 수질 자료 정리
BOD_TP_ymean <- obs %>%
  # 지점별, 연도별 분리
  group_by(강원도, 권역, 총량지점명, 연도) %>%
  # BOD 연평균(소수점 1자리로 표시)
  summarise(
    BOD = round2(mean(BOD, na.rm = TRUE), 1),
    TP = round2(mean(TP, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = 연도,
    values_from = c(BOD, TP),
    names_vary = "slowest"
  )



##########  달성률 산정  #######################################################

## 달성률 산정 함수 정의
calculate_achievement <- function(data, var_name, i) {
  target_column <- str_c(deparse(substitute(var_name)), "_목표수질")

  data %>%
    filter(
      평가방식 == "달성률",
      !is.na({{ var_name }}),
      연도 %in% c((i - 2):i)
    ) %>%
    mutate(
      달성여부 = ifelse({{ var_name }} <= !!sym(target_column), 1, 0)
    ) %>%
    group_by(총량지점명) %>%
    mutate(
      rank = rank({{ var_name }}, ties.method = "first"),
      Total = n(),
      달성률만족횟수 = ceiling(Total * 0.625),
      달성기준수질 = ifelse(rank == 달성률만족횟수, {{ var_name }}, 0),
      목표달성횟수 = sum(달성여부),
      달성률 = round2(목표달성횟수 / Total, 3),
      평가기간 = ifelse(달성기준수질 != 0, paste0(i - 2002, "~", i - 2000), 0)
    ) %>%
    filter(평가기간 != 0) %>%
    select(총량지점명, 평가기간, 달성률) %>%
    pivot_wider(names_from = 평가기간, values_from = 달성률)
}



## 각 항목별 달성률 산정
BOD_achievement <- years %>%
  map(~calculate_achievement(obs, BOD, .)) %>%
  reduce(left_join, by = "총량지점명")

TP_achievement <- years %>%
  map(~calculate_achievement(obs, TP, .)) %>%
  reduce(left_join, by = "총량지점명")

TOC_achievement <- years %>%
  map(~calculate_achievement(obs, TOC, .)) %>%
  reduce(left_join, by = "총량지점명")



##########  평가수질 산정  #####################################################

## 평가수질 산정 함수 정의
calculate_mean <- function(data, var_name, i) {
  # 항목별 자연로그 변환값 열 이름 지정
  var_name_ln <- str_c(deparse(substitute(var_name)), "_ln")  
  # 소수점 자리수(decimal places)
  dp <- ifelse(substitute(var_name) == "TP", 3, 1)
  
  data %>%
    filter(평가방식 == "변환평균",
           연도 %in% c((i - 2):i)) %>%
    select(총량지점명, 연도, BOD, TP, TOC) %>%
    # 수질 측정값 자연로그 변환
    mutate(across(c(BOD, TP, TOC), ~ log(.), .names = "{.col}_ln")) %>% 
    group_by(총량지점명) %>%
    summarise(value = round2(exp(mean(!!sym(var_name_ln), na.rm = TRUE) + 
                                   var(!!sym(var_name_ln), na.rm = TRUE) / 2), dp)) %>% 
    rename_with(~ str_c(i - 2002, "~", i - 2000), value)
}


## 각 항목별 평가수질 산정
BOD_assessment <- years %>%
  map(~calculate_mean(obs, BOD, .)) %>%
  reduce(left_join, by = "총량지점명")

TP_assessment <- years %>%
  map(~calculate_mean(obs, TP, .)) %>%
  reduce(left_join, by = "총량지점명")

TOC_assessment <- years %>%
  map(~calculate_mean(obs, TOC, .)) %>%
  reduce(left_join, by = "총량지점명")



##########  전체 자료 합치기  ##################################################

## 자료 합치기 함수 정의
bind_files <- function(name) {
  name_ymean <- str_c(deparse(substitute(name)), "_ymean")
  name_achievement <- str_c(deparse(substitute(name)), "_achievement")
  name_assessment <- str_c(deparse(substitute(name)), "_assessment")
  
  # get 함수 : 문자열로 된 객체 이름을 변수로 변환
  # 각 변수가 사용자 정의 함수 내에서 정의되어 있지 않은 경우 !!sym() 사용 불가
  get(name_ymean) %>% 
    left_join(
      bind_rows(get(name_achievement), get(name_assessment)),
      by = "총량지점명"
    ) %>%
    # 수계 순서에 맞춰 데이터 순서 조정
    mutate(총량지점명 = factor(총량지점명, levels = c(
      "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A",
      "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
      "홍천A", "한탄A", "제천A", "한강B", "한강C", "달천A", "달천B",
      "한강D", "청미A", "양화A", "복하A", "한강E", "흑천A", "가평A",
      "북한D", "조종A", "경안A", "경안B", "한강F", "왕숙A", "한강G",
      "탄천A", "중랑A", "안양A", "공릉A", "임진A", "영평A", "신천A",
      "한탄B", "문산A", "임진B", "한강H", "한강I", "굴포A", "낙본A"
    ))) %>%
    arrange(총량지점명)
}


## 각 항목별 전체 자료 합치기
BOD_total <- bind_files(BOD)
TP_total <- bind_files(TP)
TOC_total <- bind_files(TOC)


## BOD, TP 표 하나에 정리(업무보고 양식)
BOD_TP_total <- BOD_total %>% 
  select(-all_of(as.character(start_year:final_year))) %>% 
  # mutate(항목 = "BOD", .before = 1) %>% 
  left_join(TP_total %>% 
              select(-all_of(as.character(start_year:final_year))),
            by = c("강원도", "권역", "총량지점명")) %>%
  filter(강원도 == "강원도") %>% 
  select("강원도":"BOD_목표수질", "TP_목표수질", sort(names(.))) %>% 
  rename_with(~ str_replace(., ".x", "_BOD")) %>% 
  rename_with(~ str_replace(., ".y", "_TP"))



##########  파일 내보내기  #####################################################

## 강원도 자료 내보내기
write_xlsx(
  list(
    "BOD" = BOD_total %>% filter(강원도 == "강원도"),
    "TP" = TP_total %>% filter(강원도 == "강원도"),
    "TOC" = TOC_total %>% filter(강원도 == "강원도"),
    "BOD_TP_통합" = BOD_TP_total
  ),
  path = "수질분석/Output/강원도_수질현황.xlsx"
)


## 한강수계 전체 자료 내보내기
write_xlsx(list("BOD" = BOD_total, "TP" = TP_total, "TOC" = TOC_total),
  path = "수질분석/Output/한강수계전체_수질현황.xlsx"
)


##########  계절별 달성 현황 검토  #############################################

library(janitor)

## 데이터 정리
obs_정리 <- obs %>%
  mutate(
    전체 = 1,
    BOD_달성 = ifelse(BOD <= BOD_목표수질, 1, 0),
    TP_달성 = ifelse(TP <= TP_목표수질, 1, 0),
    계절 = case_when(
      월 >= 3 & 월 <= 5 ~ "봄",
      월 >= 6 & 월 <= 8 ~ "여름",
      월 >= 9 & 월 <= 11 ~ "가을",
      TRUE ~ "겨울"
    ),
    계절 = factor(계절, levels = c("봄", "여름", "가을", "겨울"))
  ) %>% 
  filter(강원도 == "강원도")

## 계절별 달성 현황
계절별달성 <- obs_정리 %>% 
  filter(!is.na(TP)) %>% 
  group_by(총량지점명, 연도, 계절) %>% 
  summarise(across(c(전체, BOD_달성, TP_달성), ~ sum(.)), .groups = "drop") %>% 
  group_by(총량지점명, 연도) %>%
  group_modify(~ .x %>%
                 adorn_totals(where = "row", fill = "소계", name = "소계", na.rm = TRUE)) %>% 
  mutate(BOD_달성률 = round2(BOD_달성 / 전체, 3), 
         TP_달성률 = round2(TP_달성 / 전체, 3),
         총량지점명 = factor(총량지점명, levels = c(
           "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A",
           "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
           "홍천A", "한탄A", "제천A", "한강B", "한강D", "북한D", "임진A", 
           "한탄B", "낙본A"
         ))) %>% 
  arrange(총량지점명) %>% 
  relocate(starts_with("TP"), .after = 전체)

계절별측정횟수 <- obs_정리 %>% 
  filter(!is.na(TP)) %>% 
  group_by(계절) %>%
  summarise(across(전체, ~ sum(.)), .groups = "drop") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns() 


## 파일 내보내기
write_xlsx(계절별달성, path = "수질분석/Output/총량측정망_계절별달성현황.xlsx")
