# 관련 패키지 로드
library(readxl)
library(lubridate)
library(tidyverse)
library(data.table)

# Excel 파일 불러오기_readxl
obs <- read_excel("D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/총량측정망 수질/1.한강수계 총량측정망(05-18)_0제거.xls",
                  skip = 2)

# 년 추가_lubridate, dplyr
obs <- obs %>% mutate(년 = year(obs$일자))
obs$년 <- as.integer(obs$년)

# BOD, TP, TOC 자연로그(ln)값 추가_dplyr
obs <- obs %>% mutate("BOD(ln)"=log(obs$`BOD(㎎/L)`), 
                      "TP(ln)"=log(obs$`총인(T-P)(㎎/L)`), 
                      "TOC(ln)"=log(obs$`총유기탄소(TOC)(㎎/L)`))

# 지점별 연평균 계산_dplyr, tidyr
BOD <- obs %>%
  group_by(총량지점명, 년) %>%                                 # 지점별, 연도별 분리
  summarise(BOD = round(mean(`BOD(㎎/L)`, na.rm=TRUE),3)) %>%  # BOD 연평균(소수점 3자리로 표시)
  spread(년, BOD)                                              # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)_tidyr
  
TP <- obs %>%
  group_by(총량지점명, 년) %>%                                       # 지점별, 연도별 분리
  summarise(TP = round(mean(`총인(T-P)(㎎/L)`, na.rm=TRUE),3)) %>%   # TP 연평균(소수점 3자리로 표시)
  spread(년, TP)                                                     # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)_tidyr

# 지점별 변환평균 계산 및 정리
for(i in 2012:2017) {
  assign(paste0(i-2000, "~", i-1998, " BOD"), obs %>%
  group_by(총량지점명) %>%                                                                      # 지점별 분리
  filter(년 %in% c(i, i+1, i+2)) %>%                                                            # 해당연도만 추출
  summarise(ave_BOD = round(exp(mean(`BOD(ln)`, na.rm=TRUE)+var(`BOD(ln)`, na.rm=TRUE)/2),3)))  # BOD 변환평균(소수점 3자리로 표시)

  setnames(get(paste0(i-2000, "~", i-1998, " BOD")), "ave_BOD", paste0(i-2000, "~", i-1998))    # 변수명 변환_data.table
  
  BOD <- BOD %>% left_join(get(paste0(i-2000, "~", i-1998, " BOD")), by = "총량지점명")         # 연평균 및 변환평균 합치기

  assign(paste0(i-2000, "~", i-1998, " TP"), obs %>%
  group_by(총량지점명) %>%                                                                      # 지점별 분리
  filter(년 %in% c(i, i+1, i+2)) %>%                                                            # 해당연도만 추출
  summarise(ave_TP = round(exp(mean(`TP(ln)`, na.rm=TRUE)+var(`TP(ln)`, na.rm=TRUE)/2),3)))     # TP 변환평균(소수점 3자리로 표시)  
  
  setnames(get(paste0(i-2000, "~", i-1998, " TP")), "ave_TP", paste0(i-2000, "~", i-1998))      # 변수명 변환_data.table
  
  TP <- TP %>% left_join(get(paste0(i-2000, "~", i-1998, " TP")), by = "총량지점명")            # 연평균 및 변환평균 합치기
}

# 수계 순서에 맞춰 데이터 순서 조정(지점 추가 및 변경 시 수정)
observatory <- c("골지A","오대A","주천A","평창A","옥동A","한강A","한강B","제천A","한강C","달천A",
                 "달천B","한강D","섬강A","섬강B","청미A","양화A","복하A","한강E","흑천A","북한A",
                 "북한B","소양A","인북A","소양B","북한C","가평A","홍천A","북한D","조종A","경안A",
                 "경안B","한강F","왕숙A","한강G","탄천A","중랑A","한강H","안양A","한강I","굴포A",
                 "공릉A","임진A","한탄A","영평A","신천A","한탄B","문산A","임진B")                # 낙본A 추가 필요
positions <- rank(observatory)
BOD <- BOD[positions,]
TP <- TP[positions,]

# 데이터 csv로 내보내기
write.csv(BOD, file = "D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/평가수질_BOD 내보내기.csv")
write.csv(TP, file = "D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/평가수질_TP 내보내기.csv")
