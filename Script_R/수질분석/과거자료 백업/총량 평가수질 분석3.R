################################################################################
# 라이브러리 로드
library(tidyverse)
library(data.table)
library(readxl)
library(lubridate)
library(writexl)

################################################################################

# 반올림 사용자 정의 함수 로드
source("Script/Function/round2func.R")

# Excel 파일 불러오기_readxl
obs <- read_excel("Data/총량측정망0722.xlsx")

# BOD, TP, TOC 자연로그(ln)값 추가_dplyr
obs <- obs %>%
  mutate(
    BOD_ln = log(obs$BOD),
    TP_ln = log(obs$TP),
    TOC_ln = log(obs$TOC)
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

# 지점별 연평균 계산_dplyr, tidyr
BOD <- obs %>%
  group_by(총량지점명, 연도) %>% # 지점별, 연도별 분리
  filter(연도 >= 2014) %>%
  summarise(BOD = round2(mean(BOD, na.rm = TRUE), 1)) %>% # BOD 연평균(소수점 1자리로 표시)
  spread(연도, BOD) # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)_tidyr

TP <- obs %>%
  group_by(총량지점명, 연도) %>% # 지점별, 연도별 분리
  filter(연도 >= 2014) %>%
  summarise(TP = round2(mean(TP, na.rm = TRUE), 3)) %>% # TP 연평균(소수점 3자리로 표시)
  spread(연도, TP) # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)_tidyr

TOC <- obs %>%
  group_by(총량지점명, 연도) %>% # 지점별, 연도별 분리
  filter(연도 >= 2014) %>%
  summarise(TOC = round2(mean(TOC, na.rm = TRUE), 1)) %>% # TP 연평균(소수점 3자리로 표시)
  spread(연도, TOC) # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)_tidyr

# 지점별 변환평균 계산 및 정리
for (i in 2014:2019) {
  ## BOD -----------------------------------------------------------------------------------------------------------------------
  assign(paste0(i - 2000, "~", i - 1998, " BOD"), obs %>%
    group_by(총량지점명) %>% # 지점별 분리
    filter(연도 %in% c(i, i + 1, i + 2)) %>% # 해당연도만 추출
    summarise(ave_BOD = round2(exp(mean(BOD_ln, na.rm = TRUE) + var(BOD_ln, na.rm = TRUE) / 2), 1))) # BOD변환평균(소수점 1자리로 표시)

  setnames(get(paste0(i - 2000, "~", i - 1998, " BOD")), "ave_BOD", paste0(i - 2000, "~", i - 1998)) # 변수명 변환_data.table

  BOD <- BOD %>% left_join(get(paste0(i - 2000, "~", i - 1998, " BOD")), by = "총량지점명") # 연평균 및 변환평균 합치기

  ## T-P -----------------------------------------------------------------------------------------------------------------------
  assign(paste0(i - 2000, "~", i - 1998, " TP"), obs %>%
    group_by(총량지점명) %>% # 지점별 분리
    filter(연도 %in% c(i, i + 1, i + 2)) %>% # 해당연도만 추출
    summarise(ave_TP = round2(exp(mean(TP_ln, na.rm = TRUE) + var(TP_ln, na.rm = TRUE) / 2), 3))) # TP 변환평균(소수점 3자리로 표시)

  setnames(get(paste0(i - 2000, "~", i - 1998, " TP")), "ave_TP", paste0(i - 2000, "~", i - 1998)) # 변수명 변환_data.table

  TP <- TP %>% left_join(get(paste0(i - 2000, "~", i - 1998, " TP")), by = "총량지점명") # 연평균 및 변환평균 합치기

  ## TOC -----------------------------------------------------------------------------------------------------------------------
  assign(paste0(i - 2000, "~", i - 1998, " TOC"), obs %>%
    group_by(총량지점명) %>% # 지점별 분리
    filter(연도 %in% c(i, i + 1, i + 2)) %>% # 해당연도만 추출
    summarise(ave_TOC = round2(exp(mean(TOC_ln, na.rm = TRUE) + var(TOC_ln, na.rm = TRUE) / 2), 1))) # TOC 변환평균(소수점 3자리로 표시)

  setnames(get(paste0(i - 2000, "~", i - 1998, " TOC")), "ave_TOC", paste0(i - 2000, "~", i - 1998)) # 변수명 변환_data.table

  TOC <- TOC %>% left_join(get(paste0(i - 2000, "~", i - 1998, " TOC")), by = "총량지점명") # 연평균 및 변환평균 합치기
}



# 엑셀 파일 내보내기
write_xlsx(BOD, path = "Output/Data/평가수질_BOD.xlsx")
write_xlsx(TP, path = "Output/Data/평가수질_TP.xlsx")
write_xlsx(TOC, path = "Output/Data/평가수질_TOC.xlsx")




# ------- 기존 유역순서 --------------------------------------------------------
# "골지A","오대A","주천A","평창A","옥동A","한강A","한강B","제천A","한강C","달천A",
# "달천B","한강D","섬강A","섬강B","청미A","양화A","복하A","한강E","흑천A","북한A",
# "북한B","소양A","인북A","소양B","북한C","가평A","홍천A","북한D","조종A","경안A",
# "경안B","한강F","왕숙A","한강G","탄천A","중랑A","한강H","안양A","한강I","굴포A",
# "공릉A","임진A","한탄A","영평A","신천A","한탄B","문산A","임진B", "낙본A"
# ------------------------------------------------------------------------------


# # 수계 순서에 맞춰 데이터 순서 조정(지점 추가 및 변경 시 수정)
# observatory <- c(
#   "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A",
#   "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
#   "홍천A", "한탄A", "제천A", "한강B", "한강C", "달천A", "달천B",
#   "한강D", "청미A", "양화A", "복하A", "한강E", "흑천A", "가평A",
#   "북한D", "조종A", "경안A", "경안B", "한강F", "왕숙A", "한강G",
#   "탄천A", "중랑A", "안양A", "공릉A", "임진A", "영평A", "신천A",
#   "한탄B", "문산A", "임진B", "한강H", "한강I", "굴포A", "낙본A"
# )
# 
# positions <- rank(observatory)
# BOD <- BOD[positions, ]
# TP <- TP[positions, ]
# TOC <- TOC[positions, ]