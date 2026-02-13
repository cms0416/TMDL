# 관련 패키지 로드
library(readxl)
library(lubridate)
library(tidyverse)
library(data.table)
# 반올림 사용자 정의 함수 로드
source("round2func.R")

# Excel 파일 불러오기_readxl
obs <- read_excel("D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/총량측정망 수질/전체 총량측정망/총량측정망0519.xlsx")
flow <- read_excel("D:/1. 수질총량/2. 기준유량/기준유량 구간최대값.xlsx")

# 저수기 구간 수질만 필터
obs <- obs %>% left_join(flow, by = "총량지점명") %>% 
  filter(유량 > 갈수기 & 유량 <= 저수기)

# BOD, TP, TOC 자연로그(ln)값 추가_dplyr
obs <- obs %>% mutate(BOD_ln = log(obs$BOD), 
                      TP_ln = log(obs$TP))

# 지점별 연평균 계산_dplyr, tidyr
BOD <- obs %>%
  group_by(총량지점명, 연도) %>%                       # 지점별, 연도별 분리
  filter(연도 > 2006) %>% 
  summarise(BOD = round(mean(BOD, na.rm=TRUE),1)) %>%  # BOD 연평균(소수점 1자리로 표시)
  spread(연도, BOD)                                    # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)_tidyr

TP <- obs %>%
  group_by(총량지점명, 연도) %>%                      # 지점별, 연도별 분리
  filter(연도 > 2006) %>% 
  summarise(TP = round2(mean(TP, na.rm=TRUE),3)) %>%   # TP 연평균(소수점 3자리로 표시)
  spread(연도, TP)                                    # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)_tidyr

# 지점별 변환평균 계산 및 정리
for(i in 2008:2017) {
  assign(paste0(i-2000, "~", i-1998, " BOD"), obs %>%
    group_by(총량지점명) %>%                                                                      # 지점별 분리
    filter(연도 %in% c(i, i+1, i+2)) %>%                                                          # 해당연도만 추출
    summarise(ave_BOD = round2(exp(mean(BOD_ln, na.rm=TRUE)+var(BOD_ln, na.rm=TRUE)/2),1)))  # BOD변환평균(소수점 1자리로 표시)
    
  setnames(get(paste0(i-2000, "~", i-1998, " BOD")), "ave_BOD", paste0(i-2000, "~", i-1998))    # 변수명 변환_data.table
    
  BOD <- BOD %>% left_join(get(paste0(i-2000, "~", i-1998, " BOD")), by = "총량지점명")         # 연평균 및 변환평균 합치기
    
  assign(paste0(i-2000, "~", i-1998, " TP"), obs %>%
    group_by(총량지점명) %>%                                                                      # 지점별 분리
    filter(연도 %in% c(i, i+1, i+2)) %>%                                                          # 해당연도만 추출
    summarise(ave_TP = round2(exp(mean(TP_ln, na.rm=TRUE)+var(TP_ln, na.rm=TRUE)/2),3)))     # TP 변환평균(소수점 3자리로 표시)  
    
  setnames(get(paste0(i-2000, "~", i-1998, " TP")), "ave_TP", paste0(i-2000, "~", i-1998))      # 변수명 변환_data.table
    
  TP <- TP %>% left_join(get(paste0(i-2000, "~", i-1998, " TP")), by = "총량지점명")            # 연평균 및 변환평균 합치기
}

# 강원도 단위유역만 필터
BOD <- BOD %>% filter(총량지점명 %in% c("골지A","오대A","주천A","평창A","옥동A","한강A","한강D",
                                        "섬강A","섬강B","북한A","북한B","소양A","인북A","소양B",
                                        "북한C","홍천A","한탄A","북한D","제천A","한강B","한탄B","임진A"))

TP <- TP %>% filter(총량지점명 %in% c("골지A","오대A","주천A","평창A","옥동A","한강A","한강D",
                                      "섬강A","섬강B","북한A","북한B","소양A","인북A","소양B",
                                      "북한C","홍천A","한탄A","북한D","제천A","한강B","한탄B","임진A"))

# 수계 순서에 맞춰 데이터 순서 조정(지점 추가 및 변경 시 수정)
총량지점명 <- c("골지A","오대A","주천A","평창A","옥동A","한강A","한강D",
                "섬강A","섬강B","북한A","북한B","소양A","인북A","소양B",
                "북한C","홍천A","한탄A","북한D","제천A","한강B","한탄B","임진A") 
ID <- c(1:22)
OBSRANK <- data.frame(총량지점명, ID)
BOD <- BOD %>% left_join(OBSRANK, by = "총량지점명") %>% arrange(ID) %>% select(-ID)
TP <- TP %>% left_join(OBSRANK, by = "총량지점명") %>% arrange(ID) %>% select(-ID)

# 데이터 csv로 내보내기
write.csv(BOD, file = "D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/평가수질(저수기)_BOD 내보내기.csv")
write.csv(TP, file = "D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/평가수질(저수기)_TP 내보내기.csv")