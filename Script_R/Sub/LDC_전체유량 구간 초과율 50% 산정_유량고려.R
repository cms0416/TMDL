# 관련 패키지 로드
library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)

# Excel 파일 불러오기_readxl
OBS <- read_excel("D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/총량측정망 수질/전체 총량측정망/총량측정망0520.xlsx")

# 결측치 제거
OBS <- na.omit(OBS)

# 연도별 초과율에 따른 수질 구하기 
for(i in 2014:2018) {
# BOD -----------------------------------------------------------------------------------------------------------------------
  assign(paste0("BOD_", i, "_", i+2), OBS %>%
           group_by(총량지점명) %>%                                              # 지점별 분리
           filter(연도 %in% c(i, i+1, i+2)) %>%                                  # 해당연도만 추출
           select(총량지점명, 연도, BOD, TP) %>% 
           mutate(BOD_RANK = rank(BOD, ties.method = c("first"))) %>%           # 데이터 순위 매기기 
           mutate(Total = n()) %>%                                               # 전체 데이터 갯수 세기
           mutate(a.50 = ceiling(Total * 0.50)) %>%                                 # 각 초과율에 해당하는 데이터 순위 확인  
           mutate(b.50 = ifelse(BOD_RANK == a.50, BOD, 0)) %>%                   # 각 초과율에 해당하는 순위를 만족하는 데이터 입력(데이터가 없는 경우 0 입력)
           select(총량지점명, b.50) %>% 
           mutate(평가기간 = ifelse(b.50 != 0, paste0(i, "~", i+2),0)) %>%       # 데이터가 있는 행에만 평가기간 입력 
           filter(평가기간 != 0)                                                 # 평가기간으로 데이터가 있는 행만 추출 
  )
# TP ----------------------------------------------------------------------------------------------------------------------- 
  assign(paste0("TP_", i, "_", i+2), OBS %>%
           group_by(총량지점명) %>%                                              # 지점별 분리
           filter(연도 %in% c(i, i+1, i+2)) %>%                                  # 해당연도만 추출
           select(총량지점명, 연도, BOD, TP) %>% 
           mutate(TP_RANK = rank(TP, ties.method = c("first"))) %>%             # 데이터 순위 매기기 
           mutate(Total = n()) %>%                                               # 전체 데이터 갯수 세기
           mutate(a.50 = ceiling(Total * 0.50)) %>%                                 # 각 초과율에 해당하는 데이터 순위 확인  
           mutate(b.50 = ifelse(TP_RANK == a.50, TP, 0)) %>%                     # 각 초과율에 해당하는 순위를 만족하는 데이터 입력(데이터가 없는 경우 0 입력)
           select(총량지점명, b.50) %>% 
           mutate(평가기간 = ifelse(b.50 != 0, paste0(i, "~", i+2),0)) %>%       # 데이터가 있는 행에만 평가기간 입력 
           filter(평가기간 != 0)                                                 # 평가기간으로 데이터가 있는 행만 추출 
  )
}

# 연도별 데이터 하나로 합치기
BOD_group <- bind_rows(BOD_2014_2016, BOD_2015_2017, BOD_2016_2018, BOD_2017_2019, BOD_2018_2020)

# BOD_2008_2010, BOD_2009_2011, BOD_2005_2007, BOD_2006_2008, BOD_2007_2009, 
# BOD_2010_2012, BOD_2011_2013, BOD_2012_2014, BOD_2013_2015, 

TP_group <- bind_rows(TP_2014_2016, TP_2015_2017, TP_2016_2018, TP_2017_2019, TP_2018_2020)

# TP_2008_2010, TP_2009_2011, TP_2005_2007, TP_2006_2008, TP_2007_2009,
# TP_2010_2012, TP_2011_2013, TP_2012_2014, TP_2013_2015, 

# 열 순서 변경(평가기간을 두번째로)
BOD_group <- BOD_group[c(1, 3, 2)]
TP_group <- TP_group[c(1, 3, 2)]

# 데이터가 없는(0으로 입력) 셀을 합계로 제거 
BOD_group <- BOD_group %>% group_by(총량지점명, 평가기간) %>% 
             summarise(c.50 = sum(b.50))
TP_group <- TP_group %>% group_by(총량지점명, 평가기간) %>% 
            summarise(c.50 = sum(b.50))

# 변수명 변경 
setnames(TP_group, "c.50", "50%")
setnames(BOD_group, "c.50", "50%")

BOD_group <- BOD_group %>% filter(총량지점명 %in% c("골지A","오대A","주천A","평창A","옥동A","한강A","한강D",
                                                    "섬강A","섬강B","북한A","북한B","소양A","인북A","소양B",
                                                    "북한C","홍천A","한탄A","북한D","제천A","한강B","한탄B","임진A"))

TP_group <- TP_group %>% filter(총량지점명 %in% c("골지A","오대A","주천A","평창A","옥동A","한강A","한강D",
                                                  "섬강A","섬강B","북한A","북한B","소양A","인북A","소양B",
                                                  "북한C","홍천A","한탄A","북한D","제천A","한강B","한탄B","임진A"))

# 자료형태 변환
BOD_group <- BOD_group %>% spread(평가기간, "50%")
TP_group <- TP_group %>% spread(평가기간, "50%")

# 수계 순서에 맞춰 데이터 순서 조정(지점 추가 및 변경 시 수정)
총량지점명 <- c("골지A","오대A","주천A","평창A","옥동A","한강A","한강D",
                "섬강A","섬강B","북한A","북한B","소양A","인북A","소양B",
                "북한C","홍천A","한탄A","북한D","제천A","한강B","한탄B","임진A") 
ID <- c(1:22)
OBSRANK <- data.frame(총량지점명, ID)
BOD_group <- BOD_group %>% left_join(OBSRANK, by = "총량지점명") %>% arrange(ID) %>% select(-ID)
TP_group <- TP_group %>% left_join(OBSRANK, by = "총량지점명") %>% arrange(ID) %>% select(-ID)

# 데이터 csv로 내보내기
write.csv(TP_group, file = "D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/TP_LDC_all_50.csv")
write.csv(BOD_group, file = "D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/BOD_LDC_all_50.csv")

