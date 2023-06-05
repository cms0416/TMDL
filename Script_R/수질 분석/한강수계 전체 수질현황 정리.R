################################################################################
# 라이브러리 로드
library(tidyverse)
library(data.table)
library(readxl)
library(writexl)

################################################################################


### 반올림 사용자 정의 함수 로드
source("Script/Function/round2func.R")

### Excel 파일 불러오기_readxl
obs <- read_excel("Data/총량측정망0722.xlsx")

target <- read_excel("Data/목표수질.xlsx")



###################  달성률 산정  ####################################################################################

## obs_측정값 정리
obs_achievement <- obs %>%
  left_join(target, by = "총량지점명") %>%
  filter(총량지점명 %in% c(
    "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A",
    "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
    "홍천A", "한탄A", "제천A", "한강B", "한강C", "달천A", "달천B",
    "한강D"
  )) %>%
  filter(연도 >= 2015) %>%
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
BOD <- obs_achievement %>%
  mutate(달성여부 = ifelse(BOD <= BOD_목표수질, 1, 0)) %>%
  select(-TP, -TP_목표수질, -TOC, -TOC_목표수질)

TP <- obs_achievement %>%
  mutate(달성여부 = ifelse(TP <= TP_목표수질, 1, 0)) %>%
  select(-BOD, -BOD_목표수질, -TOC, -TOC_목표수질)

TOC <- obs_achievement %>%
  mutate(달성여부 = ifelse(TOC <= TOC_목표수질, 1, 0)) %>%
  select(-BOD, -BOD_목표수질, -TP, -TP_목표수질)

## 지점별 연평균 계산_dplyr, tidyr
BOD_ymean <- obs_achievement %>%
  group_by(강원도, 권역, 총량지점명, BOD_목표수질, 연도) %>% # 지점별, 연도별 분리
  summarise(BOD = round2(mean(BOD, na.rm = TRUE), 1)) %>% # BOD 연평균(소수점 1자리로 표시)
  pivot_wider(names_from = 연도, values_from = BOD) # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)

TP_ymean <- obs_achievement %>%
  group_by(강원도, 권역, 총량지점명, TP_목표수질, 연도) %>% # 지점별, 연도별 분리
  summarise(TP = round2(mean(TP, na.rm = TRUE), 3)) %>% # TP 연평균(소수점 3자리로 표시)
  pivot_wider(names_from = 연도, values_from = TP) # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)

TOC_ymean <- obs_achievement %>%
  group_by(강원도, 권역, 총량지점명, TOC_목표수질, 연도) %>% # 지점별, 연도별 분리
  summarise(TOC = round2(mean(TOC, na.rm = TRUE), 1)) %>% # TOC 연평균(소수점 1자리로 표시)
  pivot_wider(names_from = 연도, values_from = TOC) # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)


## ------------------------------------------------------------------------------------------------------------------------------
BOD_group0 <- data.frame()
TP_group0 <- data.frame()
TOC_group0 <- data.frame()

for (i in 2017:2022) {
  ## BOD -----------------------------------------------------------------------------------------------------------------------
  temp <- BOD %>%
    group_by(총량지점명) %>% # 지점별 그룹지정
    filter(연도 %in% c(i - 2, i - 1, i)) %>% # 해당연도만 추출
    mutate(BOD_RANK = rank(BOD, ties.method = c("first"))) %>% # 데이터 순위 매기기(오름차순/동일값은 앞에 있는 값이 우선순위)
    mutate(Total = n()) %>% # 전체 데이터 갯수 세기
    mutate(달성률만족횟수 = ceiling(Total * 0.625)) %>% # 달성률 62.5% 만족하는 데이터 순위 확인
    mutate(달성기준수질 = ifelse(BOD_RANK == 달성률만족횟수, BOD, 0)) %>% # 달성률 62.5% 만족하는 데이터 입력(데이터가 없는 경우 0)
    mutate(목표달성횟수 = sum(달성여부)) %>%
    mutate(달성률 = round2((목표달성횟수 / Total), 3)) %>%
    mutate(평가기간 = ifelse(달성기준수질 != 0, paste0(i - 2002, "~", i - 2000), 0)) # 데이터가 있는 행에만 평가기간 입력

  BOD_group0 <- rbind(BOD_group0, temp)

  ## TP -----------------------------------------------------------------------------------------------------------------------
  temp <- TP %>%
    group_by(총량지점명) %>% # 지점별 그룹지정
    filter(연도 %in% c(i - 2, i - 1, i)) %>% # 해당연도만 추출
    mutate(TP_RANK = rank(TP, ties.method = c("first"))) %>% # 데이터 순위 매기기(오름차순/동일값은 앞에 있는 값이 우선순위)
    mutate(Total = n()) %>% # 전체 데이터 갯수 세기
    mutate(달성률만족횟수 = ceiling(Total * 0.625)) %>% # 달성률 62.5% 만족하는 데이터 순위 확인
    mutate(달성기준수질 = ifelse(TP_RANK == 달성률만족횟수, TP, 0)) %>% # 달성률 62.5% 만족하는 데이터 입력(데이터가 없는 경우 0)
    mutate(목표달성횟수 = sum(달성여부)) %>%
    mutate(달성률 = round2((목표달성횟수 / Total), 3)) %>%
    mutate(평가기간 = ifelse(달성기준수질 != 0, paste0(i - 2002, "~", i - 2000), 0)) # 데이터가 있는 행에만 평가기간 입력

  TP_group0 <- rbind(TP_group0, temp)

  ## TOC -----------------------------------------------------------------------------------------------------------------------
  temp <- TOC %>%
    group_by(총량지점명) %>% # 지점별 그룹지정
    filter(연도 %in% c(i - 2, i - 1, i)) %>% # 해당연도만 추출
    mutate(TOC_RANK = rank(TOC, ties.method = c("first"))) %>% # 데이터 순위 매기기(오름차순/동일값은 앞에 있는 값이 우선순위)
    mutate(Total = n()) %>% # 전체 데이터 갯수 세기
    mutate(달성률만족횟수 = ceiling(Total * 0.625)) %>% # 달성률 62.5% 만족하는 데이터 순위 확인
    mutate(달성기준수질 = ifelse(TOC_RANK == 달성률만족횟수, TOC, 0)) %>% # 달성률 62.5% 만족하는 데이터 입력(데이터가 없는 경우 0)
    mutate(목표달성횟수 = sum(달성여부)) %>%
    mutate(달성률 = round2((목표달성횟수 / Total), 3)) %>%
    mutate(평가기간 = ifelse(달성기준수질 != 0, paste0(i - 2002, "~", i - 2000), 0)) # 데이터가 있는 행에만 평가기간 입력

  TOC_group0 <- rbind(TOC_group0, temp)
}
## ------------------------------------------------------------------------------------------------------------------------------


## 자료 정리
BOD_group <- BOD_group0 %>%
  filter(평가기간 != 0) %>%
  select(권역, 총량지점명, 평가기간, BOD_목표수질, 달성률, 달성기준수질)

TP_group <- TP_group0 %>%
  filter(평가기간 != 0) %>%
  select(권역, 총량지점명, 평가기간, TP_목표수질, 달성률, 달성기준수질)

TOC_group <- TOC_group0 %>%
  filter(평가기간 != 0) %>%
  select(권역, 총량지점명, 평가기간, TOC_목표수질, 달성률, 달성기준수질)

# 달성률 '%' 추가
# BOD_group$달성률 <- paste0(sprintf("%.1f", BOD_group$달성률), "%")
# TP_group$달성률 <- paste0(sprintf("%.1f", TP_group$달성률), "%")
# TOC_group$달성률 <- paste0(sprintf("%.1f", TOC_group$달성률), "%")

## 자료형태 변환
BOD_ach.rate <- BOD_group %>%
  select(-달성기준수질, -권역, -BOD_목표수질) %>%
  pivot_wider(names_from = 평가기간, values_from = 달성률)

TP_ach.rate <- TP_group %>%
  select(-달성기준수질, -권역, -TP_목표수질) %>%
  pivot_wider(names_from = 평가기간, values_from = 달성률)

TOC_ach.rate <- TOC_group %>%
  select(-달성기준수질, -권역, -TOC_목표수질) %>%
  pivot_wider(names_from = 평가기간, values_from = 달성률)

BOD_achievement <- BOD_ymean %>%
  left_join(BOD_ach.rate, by = "총량지점명") %>%
  mutate(총량지점명 = factor(총량지점명, levels = c(
    "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A",
    "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
    "홍천A", "한탄A", "제천A", "한강B", "한강C", "달천A", "달천B",
    "한강D"
  ))) %>%
  arrange(총량지점명)

TP_achievement <- TP_ymean %>%
  left_join(TP_ach.rate, by = "총량지점명") %>%
  mutate(총량지점명 = factor(총량지점명, levels = c(
    "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A",
    "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
    "홍천A", "한탄A", "제천A", "한강B", "한강C", "달천A", "달천B",
    "한강D"
  ))) %>%
  arrange(총량지점명)

TOC_achievement <- TOC_ymean %>%
  left_join(TOC_ach.rate, by = "총량지점명") %>%
  mutate(총량지점명 = factor(총량지점명, levels = c(
    "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A",
    "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
    "홍천A", "한탄A", "제천A", "한강B", "한강C", "달천A", "달천B",
    "한강D"
  ))) %>%
  arrange(총량지점명)



###################  평가수질 산정  ####################################################################################

## BOD, TP, TOC 자연로그(ln)값 추가_dplyr
obs_assessment <- obs %>%
  left_join(target, by = "총량지점명") %>%
  mutate(
    BOD_ln = log(obs$BOD),
    TP_ln = log(obs$TP),
    TOC_ln = log(obs$TOC)
  ) %>%
  # filter(총량지점명 %in% c(
  #   "청미A", "양화A", "복하A", "한강E", "흑천A", "가평A",
  #   "북한D", "조종A", "경안A", "경안B", "한강F", "왕숙A", "한강G",
  #   "탄천A", "중랑A", "안양A", "공릉A", "임진A", "영평A", "신천A",
  #   "한탄B", "문산A", "임진B", "한강H", "한강I", "굴포A", "낙본A"
  # )) %>%
  filter(연도 >= 2015) %>%
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

## 지점별 연평균 계산_dplyr, tidyr
BOD_assessment <- obs_assessment %>%
  group_by(강원도, 권역, 총량지점명, BOD_목표수질, 연도) %>% # 지점별, 연도별 분리
  summarise(BOD = round2(mean(BOD, na.rm = TRUE), 1), .groups = "drop") %>% # BOD 연평균(소수점 1자리로 표시)
  pivot_wider(names_from = 연도, values_from = BOD) # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)

TP_assessment <- obs_assessment %>%
  group_by(강원도, 권역, 총량지점명, TP_목표수질, 연도) %>% # 지점별, 연도별 분리
  summarise(TP = round2(mean(TP, na.rm = TRUE), 3), .groups = "drop") %>% # TP 연평균(소수점 3자리로 표시)
  pivot_wider(names_from = 연도, values_from = TP) # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)

TOC_assessment <- obs_assessment %>%
  group_by(강원도, 권역, 총량지점명, TOC_목표수질, 연도) %>% # 지점별, 연도별 분리
  summarise(TOC = round2(mean(TOC, na.rm = TRUE), 1), .groups = "drop") %>% # TOC 연평균(소수점 1자리로 표시)
  pivot_wider(names_from = 연도, values_from = TOC) # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)


## 지점별 변환평균 계산 및 정리
temp <- data.frame()

for (i in 2015:2020) {
  ## BOD -----------------------------------------------------------------------------------------------------------------------
  temp <- obs_assessment %>%
    group_by(총량지점명) %>% # 지점별 분리
    filter(연도 %in% c(i, i + 1, i + 2)) %>% # 해당연도만 추출
    summarise(ave_BOD = round2(exp(mean(BOD_ln, na.rm = TRUE) + var(BOD_ln, na.rm = TRUE) / 2), 1)) # BOD변환평균(소수점 1자리로 표시)

  setnames(temp, "ave_BOD", str_c(i - 2000, "~", i - 1998)) # 변수명 변환_data.table

  BOD_assessment <- BOD_assessment %>%
    left_join(temp, by = "총량지점명") %>% # 연평균 및 변환평균 합치기
    arrange(총량지점명)

  ## T-P -----------------------------------------------------------------------------------------------------------------------
  temp <- obs_assessment %>%
    group_by(총량지점명) %>% # 지점별 분리
    filter(연도 %in% c(i, i + 1, i + 2)) %>% # 해당연도만 추출
    summarise(ave_TP = round2(exp(mean(TP_ln, na.rm = TRUE) + var(TP_ln, na.rm = TRUE) / 2), 3)) # TP 변환평균(소수점 3자리로 표시)

  setnames(temp, "ave_TP", str_c(i - 2000, "~", i - 1998)) # 변수명 변환_data.table

  TP_assessment <- TP_assessment %>%
    left_join(temp, by = "총량지점명") %>% # 연평균 및 변환평균 합치기
    arrange(총량지점명)

  ## TOC -----------------------------------------------------------------------------------------------------------------------
  temp <- obs_assessment %>%
    group_by(총량지점명) %>% # 지점별 분리
    filter(연도 %in% c(i, i + 1, i + 2)) %>% # 해당연도만 추출
    summarise(ave_TOC = round2(exp(mean(TOC_ln, na.rm = TRUE) + var(TOC_ln, na.rm = TRUE) / 2), 1)) # TOC 변환평균(소수점 3자리로 표시)

  setnames(temp, "ave_TOC", str_c(i - 2000, "~", i - 1998)) # 변수명 변환_data.table

  TOC_assessment <- TOC_assessment %>%
    left_join(temp, by = "총량지점명") %>% # 연평균 및 변환평균 합치기
    arrange(총량지점명)
}

BOD_total <- rbind(
  BOD_achievement,
  BOD_assessment %>% filter(총량지점명 %in% c(
    "청미A", "양화A", "복하A", "한강E", "흑천A", "가평A",
    "북한D", "조종A", "경안A", "경안B", "한강F", "왕숙A", "한강G",
    "탄천A", "중랑A", "안양A", "공릉A", "임진A", "영평A", "신천A",
    "한탄B", "문산A", "임진B", "한강H", "한강I", "굴포A", "낙본A"
  ))
) %>%
  left_join(BOD_assessment %>% filter(총량지점명 %in% c(
    "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A",
    "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
    "홍천A", "한탄A", "제천A", "한강B", "한강C", "달천A", "달천B",
    "한강D"
  )) %>%
    select(3, 13:18), by = "총량지점명")

TP_total <- rbind(
  TP_achievement,
  TP_assessment %>% filter(총량지점명 %in% c(
    "청미A", "양화A", "복하A", "한강E", "흑천A", "가평A",
    "북한D", "조종A", "경안A", "경안B", "한강F", "왕숙A", "한강G",
    "탄천A", "중랑A", "안양A", "공릉A", "임진A", "영평A", "신천A",
    "한탄B", "문산A", "임진B", "한강H", "한강I", "굴포A", "낙본A"
  ))
) %>%
  left_join(TP_assessment %>% filter(총량지점명 %in% c(
    "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A",
    "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
    "홍천A", "한탄A", "제천A", "한강B", "한강C", "달천A", "달천B",
    "한강D"
  )) %>%
    select(3, 13:18), by = "총량지점명")

TOC_total <- rbind(
  TOC_achievement,
  TOC_assessment %>% filter(총량지점명 %in% c(
    "청미A", "양화A", "복하A", "한강E", "흑천A", "가평A",
    "북한D", "조종A", "경안A", "경안B", "한강F", "왕숙A", "한강G",
    "탄천A", "중랑A", "안양A", "공릉A", "임진A", "영평A", "신천A",
    "한탄B", "문산A", "임진B", "한강H", "한강I", "굴포A", "낙본A"
  ))
) %>%
  left_join(TOC_assessment %>% filter(총량지점명 %in% c(
    "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A",
    "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
    "홍천A", "한탄A", "제천A", "한강B", "한강C", "달천A", "달천B",
    "한강D"
  )) %>%
    select(3, 13:18), by = "총량지점명")

#################################################################################
## 엑셀 파일 내보내기_writexl
write_xlsx(list("BOD" = BOD_total, "TP" = TP_total, "TOC" = TOC_total), path = "Output/Data/한강수계 전체 수질현황.xlsx")
