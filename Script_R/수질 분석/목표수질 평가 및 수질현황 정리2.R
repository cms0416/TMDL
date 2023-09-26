#####  라이브러리 로드  ########################################################
library(tidyverse)
library(readxl)
library(writexl)


##########  파일 불러오기  #####################################################
### 반올림 사용자 정의 함수 로드
source("Script_R/Function/round2func.R")

### Excel 파일 불러오기_readxl
obs <- read_excel("수질 분석/총량측정망0723_1.xlsx")

target <- read_excel("수질 분석/목표수질.xlsx")

### 측정 자료에 목표수질, 권역, 강원도 여부 등 추가
obs <- obs %>%
  left_join(target, by = "총량지점명") %>%
  filter(연도 >= 2014)


##########  연평균 산정  #######################################################

BOD_ymean <- obs %>%
  # 지점별, 연도별 분리
  group_by(강원도, 권역, 총량지점명, BOD_목표수질, 연도) %>%
  # BOD 연평균(소수점 1자리로 표시)
  summarise(BOD = round2(mean(BOD, na.rm = TRUE), 1), .groups = "drop") %>%
  # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)
  pivot_wider(names_from = 연도, values_from = BOD)

TP_ymean <- obs %>%
  # 지점별, 연도별 분리
  group_by(강원도, 권역, 총량지점명, TP_목표수질, 연도) %>%
  # TP 연평균(소수점 3자리로 표시)
  summarise(TP = round2(mean(TP, na.rm = TRUE), 3), .groups = "drop") %>%
  # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)
  pivot_wider(names_from = 연도, values_from = TP)

TOC_ymean <- obs %>%
  # 지점별, 연도별 분리
  group_by(강원도, 권역, 총량지점명, TOC_목표수질, 연도) %>%
  # TOC 연평균(소수점 1자리로 표시)
  summarise(TOC = round2(mean(TOC, na.rm = TRUE), 1), .groups = "drop") %>%
  # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)
  pivot_wider(names_from = 연도, values_from = TOC)

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

## obs_측정값 정리
obs_achievement <- obs %>%
  filter(평가방식 == "달성률") %>%
  # BOD열에서 결측치 제거(결측치가 아닌 값들만 필터)
  filter(!is.na(BOD)) %>%
  select(-c(수온, pH, EC, DO, COD, SS, TN)) %>%
  # 월 및 계절 추가_lubridate, dplyr
  mutate(
    월 = month(일자),
    계절 = ifelse(월 >= 3 & 월 <= 5, "봄",
      ifelse(월 >= 6 & 월 <= 8, "여름",
        ifelse(월 >= 9 & 월 <= 11, "가을", "겨울")
      )
    )
  ) %>%
  # '계절' 요인형(factor)으로 변환 후 계절 순서로 요인 순서 변경
  mutate(계절 = factor(계절, levels = c("봄", "여름", "가을", "겨울")))

## 목표수질 추가 및 달성여부 확인
BOD_achievement <- obs_achievement %>%
  mutate(달성여부 = ifelse(BOD <= BOD_목표수질, 1, 0)) %>%
  select(-TP, -TP_목표수질, -TOC, -TOC_목표수질)

TP_achievement <- obs_achievement %>%
  mutate(달성여부 = ifelse(TP <= TP_목표수질, 1, 0)) %>%
  select(-BOD, -BOD_목표수질, -TOC, -TOC_목표수질)

TOC_achievement <- obs_achievement %>%
  mutate(달성여부 = ifelse(TOC <= TOC_목표수질, 1, 0)) %>%
  select(-BOD, -BOD_목표수질, -TP, -TP_목표수질)



## 지점별 달성률 계산 및 정리 --------------------------------------------------
BOD_group <- tibble()
TP_group <- tibble()
TOC_group <- tibble()

for (i in 2016:2023) {
  ## BOD _______________________________________________________________________
  temp <- BOD_achievement %>%
    # 지점별 그룹지정
    group_by(총량지점명) %>%
    # 해당연도만 추출
    filter(연도 %in% c(i - 2, i - 1, i)) %>%
    mutate(
      # 데이터 순위 매기기(오름차순/동일값은 앞에 있는 값이 우선순위)
      BOD_RANK = rank(BOD, ties.method = c("first")),
      # 전체 데이터 갯수 세기
      Total = n(),
      # 달성률 62.5% 만족하는 데이터 순위 확인
      달성률만족횟수 = ceiling(Total * 0.625),
      # 달성률 62.5% 만족하는 데이터 입력(데이터가 없는 경우 0)
      달성기준수질 = ifelse(BOD_RANK == 달성률만족횟수, BOD, 0),
      목표달성횟수 = sum(달성여부),
      달성률 = round2(목표달성횟수 / Total, 3),
      # 데이터가 있는 행에만 평가기간 입력
      평가기간 = ifelse(달성기준수질 != 0, paste0(i - 2002, "~", i - 2000), 0)
    )

  BOD_group <- bind_rows(BOD_group, temp)

  ## TP ________________________________________________________________________
  temp <- TP_achievement %>%
    # 지점별 그룹지정
    group_by(총량지점명) %>%
    # 해당연도만 추출
    filter(연도 %in% c(i - 2, i - 1, i)) %>%
    mutate(
      # 데이터 순위 매기기(오름차순/동일값은 앞에 있는 값이 우선순위)
      TP_RANK = rank(TP, ties.method = c("first")),
      # 전체 데이터 갯수 세기
      Total = n(),
      # 달성률 62.5% 만족하는 데이터 순위 확인
      달성률만족횟수 = ceiling(Total * 0.625),
      # 달성률 62.5% 만족하는 데이터 입력(데이터가 없는 경우 0)
      달성기준수질 = ifelse(TP_RANK == 달성률만족횟수, TP, 0),
      목표달성횟수 = sum(달성여부),
      달성률 = round2(목표달성횟수 / Total, 3),
      # 데이터가 있는 행에만 평가기간 입력
      평가기간 = ifelse(달성기준수질 != 0, paste0(i - 2002, "~", i - 2000), 0)
    )

  TP_group <- bind_rows(TP_group, temp)

  ## TOC ________________________________________________________________________
  temp <- TOC_achievement %>%
    # 지점별 그룹지정
    group_by(총량지점명) %>%
    # 해당연도만 추출
    filter(연도 %in% c(i - 2, i - 1, i)) %>%
    mutate(
      # 데이터 순위 매기기(오름차순/동일값은 앞에 있는 값이 우선순위)
      TOC_RANK = rank(TOC, ties.method = c("first")),
      # 전체 데이터 갯수 세기
      Total = n(),
      # 달성률 62.5% 만족하는 데이터 순위 확인
      달성률만족횟수 = ceiling(Total * 0.625),
      # 달성률 62.5% 만족하는 데이터 입력(데이터가 없는 경우 0)
      달성기준수질 = ifelse(TOC_RANK == 달성률만족횟수, TOC, 0),
      목표달성횟수 = sum(달성여부),
      달성률 = round2(목표달성횟수 / Total, 3),
      # 데이터가 있는 행에만 평가기간 입력
      평가기간 = ifelse(달성기준수질 != 0, paste0(i - 2002, "~", i - 2000), 0)
    )

  TOC_group <- bind_rows(TOC_group, temp)
}
## _____________________________________________________________________________


## 자료 정리
BOD_ach.rate <- BOD_group %>%
  filter(평가기간 != 0) %>%
  select(총량지점명, 평가기간, 달성률) %>%
  pivot_wider(names_from = 평가기간, values_from = 달성률)

TP_ach.rate <- TP_group %>%
  filter(평가기간 != 0) %>%
  select(총량지점명, 평가기간, 달성률) %>%
  pivot_wider(names_from = 평가기간, values_from = 달성률)

TOC_ach.rate <- TOC_group %>%
  filter(평가기간 != 0) %>%
  select(총량지점명, 평가기간, 달성률) %>%
  pivot_wider(names_from = 평가기간, values_from = 달성률)

# 달성률 '%' 추가
# BOD_group$달성률 <- paste0(sprintf("%.1f", BOD_group$달성률), "%")
# TP_group$달성률 <- paste0(sprintf("%.1f", TP_group$달성률), "%")
# TOC_group$달성률 <- paste0(sprintf("%.1f", TOC_group$달성률), "%")




##########  평가수질 산정  #####################################################

## BOD, TP, TOC 자연로그(ln)값 추가_dplyr
obs_assessment <- obs %>%
  filter(평가방식 == "변환평균") %>%
  mutate(
    BOD_ln = log(BOD),
    TP_ln = log(TP),
    TOC_ln = log(TOC)
  ) %>%
  # 수계 순서에 맞춰 데이터 순서 조정
  mutate(총량지점명 = factor(총량지점명, levels = c(
    "청미A", "양화A", "복하A", "한강E", "흑천A", "가평A",
    "북한D", "조종A", "경안A", "경안B", "한강F", "왕숙A", "한강G",
    "탄천A", "중랑A", "안양A", "공릉A", "임진A", "영평A", "신천A",
    "한탄B", "문산A", "임진B", "한강H", "한강I", "굴포A", "낙본A"
  ))) %>%
  arrange(총량지점명)



## 지점별 변환평균 계산 및 정리 ------------------------------------------------
BOD_assessment <- target %>% filter(평가방식 == "변환평균") %>% select(총량지점명) 
TP_assessment <- target %>% filter(평가방식 == "변환평균") %>% select(총량지점명)
TOC_assessment <- target %>% filter(평가방식 == "변환평균") %>% select(총량지점명)


for (i in 2016:2023) {
  ## BOD _______________________________________________________________________
  temp <- obs_assessment %>%
    # 지점별 분리
    group_by(총량지점명) %>% 
    # 해당연도만 추출
    filter(연도 %in% c(i - 2, i - 1, i)) %>% 
    # BOD변환평균(소수점 1자리로 표시)
    summarise(ave_BOD = round2(exp(mean(BOD_ln, na.rm = TRUE) + var(BOD_ln, na.rm = TRUE) / 2), 1)) %>% 
    # 변수명 변환
    rename_with(~ str_c(i - 2002, "~", i - 2000), ave_BOD)

  BOD_assessment <- BOD_assessment %>%
    left_join(temp, by = "총량지점명")
    # arrange(총량지점명)

  ## T-P _______________________________________________________________________ 
  temp <- obs_assessment %>%
    # 지점별 분리
    group_by(총량지점명) %>% 
    # 해당연도만 추출
    filter(연도 %in% c(i - 2, i - 1, i)) %>%
    # TP 변환평균(소수점 3자리로 표시)
    summarise(ave_TP = round2(exp(mean(TP_ln, na.rm = TRUE) + var(TP_ln, na.rm = TRUE) / 2), 3)) %>% 
    # 변수명 변환
    rename_with(~ str_c(i - 2002, "~", i - 2000), ave_TP)

  TP_assessment <- TP_assessment %>%
    left_join(temp, by = "총량지점명")
    # arrange(총량지점명)

  ## TOC _______________________________________________________________________
  temp <- obs_assessment %>%
    # 지점별 분리
    group_by(총량지점명) %>% 
    # 해당연도만 추출
    filter(연도 %in% c(i - 2, i - 1, i)) %>%
    # TOC 변환평균(소수점 3자리로 표시)
    summarise(ave_TOC = round2(exp(mean(TOC_ln, na.rm = TRUE) + var(TOC_ln, na.rm = TRUE) / 2), 1)) %>% 
    # 변수명 변환
    rename_with(~ str_c(i - 2002, "~", i - 2000), ave_TOC)
  
  TOC_assessment <- TOC_assessment %>%
    left_join(temp, by = "총량지점명") 
    # arrange(총량지점명)
}



##########  전체 자료 합치기  ##################################################
## BOD
BOD_total <- BOD_ymean %>% 
  left_join(
    bind_rows(BOD_ach.rate, BOD_assessment),
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

## T-P
TP_total <- TP_ymean %>% 
  left_join(
    bind_rows(TP_ach.rate, TP_assessment),
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

## TOC
TOC_total <- TOC_ymean %>% 
  left_join(
    bind_rows(TOC_ach.rate, TOC_assessment),
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


##########  파일 내보내기  #####################################################

## 강원도 자료 내보내기
write_xlsx(list("BOD" = BOD_total %>% filter(강원도 == "강원도"), 
                "TP" = TP_total %>% filter(강원도 == "강원도"), 
                "TOC" = TOC_total %>% filter(강원도 == "강원도")), 
           path = "수질 분석/Output/강원도 수질현황.xlsx")

## 한강수계 전체 자료 내보내기
write_xlsx(list("BOD" = BOD_total, "TP" = TP_total, "TOC" = TOC_total), 
           path = "수질 분석/Output/한강수계 전체 수질현황.xlsx")


