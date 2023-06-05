# 관련 패키지 로드
library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)

# Excel 파일 불러오기_readxl
OBS <- read_excel("D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/총량측정망 수질/전체 총량측정망/총량측정망0520.xlsx")
target <- read_excel("D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/총량측정망 수질/목표수질.xlsx") %>% 
  select(-TOC_목표수질)

# 강원도포함 수계만 필터
OBS <- OBS %>% filter(총량지점명 %in% c("골지A","오대A","주천A","평창A","옥동A","한강A","섬강A","섬강B",
                                        "북한A","북한B","소양A","인북A","소양B","북한C","홍천A","한탄A",
                                        "낙본A","한강D","북한D","제천A","한강B","한탄B","임진A"))

# 유량제외 및 항목별 분리
BOD <- OBS %>% select(총량지점명, BOD, 연도)
TP <- OBS %>% select(총량지점명, TP, 연도)

# 결측치 제거
BOD <- na.omit(BOD)
TP <- na.omit(TP)

# 0값 제거
BOD <- BOD[!(BOD$BOD == 0), ]
TP <- TP[!(TP$TP == 0), ]

# 목표수질 추가 및 달성여부 확인
BOD <- BOD %>% left_join(target, by = "총량지점명") %>% 
  mutate(달성여부 = ifelse(BOD <= BOD_목표수질, 1, 0)) %>% 
  select(-TP_목표수질)

TP <- TP %>% left_join(target, by = "총량지점명") %>% 
  mutate(달성여부 = ifelse(TP <= TP_목표수질, 1, 0)) %>% 
  select(-BOD_목표수질)

for(i in 2014:2018) {
  # BOD -----------------------------------------------------------------------------------------------------------------------
  assign(paste0("BOD_", i, "_", i+2), BOD %>%
           group_by(총량지점명) %>%                                            # 지점별 그룹지정
           filter(연도 %in% c(i, i+1, i+2)) %>%                                # 해당연도만 추출
           mutate(BOD_RANK = rank(BOD, ties.method = c("first"))) %>%          # 데이터 순위 매기기(오름차순/동일값은 앞에 있는 값이 우선순위)
           mutate(Total = n()) %>%                                             # 전체 데이터 갯수 세기
           mutate(목표달성횟수 = ceiling(Total * 0.625)) %>%                   # 각 초과율에 해당하는 데이터 순위 확인  
           mutate(달성기준수질 = ifelse(BOD_RANK == 목표달성횟수, BOD, 0)) %>% # 각 초과율에 해당하는 순위를 만족하는 데이터 입력(데이터가 없는 경우 0)
           mutate(달성수 = sum(달성여부)) %>% 
           mutate(달성률 = round((달성수/Total)*100, 2)) %>% 
           select(총량지점명, 달성률, 달성기준수질) %>% 
           mutate(평가기간 = ifelse(달성기준수질 != 0, paste0(i, "~", i+2),0)) %>% # 데이터가 있는 행에만 평가기간 입력 
           filter(평가기간 != 0)                                                   # 평가기간으로 데이터가 있는 행만 추출 
  )
  # TP ----------------------------------------------------------------------------------------------------------------------- 
  assign(paste0("TP_", i, "_", i+2), TP %>%
           group_by(총량지점명) %>%                                           # 지점별 그룹지정
           filter(연도 %in% c(i, i+1, i+2)) %>%                               # 해당연도만 추출
           mutate(TP_RANK = rank(TP, ties.method = c("first"))) %>%           # 데이터 순위 매기기(오름차순/동일값은 앞에 있는 값이 우선순위)
           mutate(Total = n()) %>%                                            # 전체 데이터 갯수 세기
           mutate(목표달성횟수 = ceiling(Total * 0.625)) %>%                  # 각 초과율에 해당하는 데이터 순위 확인  
           mutate(달성기준수질 = ifelse(TP_RANK == 목표달성횟수, TP, 0)) %>%  # 각 초과율에 해당하는 순위를 만족하는 데이터 입력(데이터가 없는 경우 0)
           mutate(달성수 = sum(달성여부)) %>% 
           mutate(달성률 = round((달성수/Total)*100, 2)) %>% 
           select(총량지점명, 달성률, 달성기준수질) %>% 
           mutate(평가기간 = ifelse(달성기준수질 != 0, paste0(i, "~", i+2),0)) %>% # 데이터가 있는 행에만 평가기간 입력 
           filter(평가기간 != 0)                                                   # 평가기간으로 데이터가 있는 행만 추출 
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
BOD_group <- BOD_group[c(1, 4, 2, 3)]
TP_group <- TP_group[c(1, 4, 2, 3)]


# 자료형태 변환
BOD_achconc <- BOD_group %>% select(-달성률) %>% spread(평가기간, 달성기준수질)
TP_achconc <- TP_group %>% select(-달성률) %>% spread(평가기간, 달성기준수질)

BOD_achrate <- BOD_group %>% select(-달성기준수질) %>% spread(평가기간, 달성률)
TP_achrate <- TP_group %>% select(-달성기준수질) %>% spread(평가기간, 달성률)

# 수계 순서에 맞춰 데이터 순서 조정(지점 추가 및 변경 시 수정)
총량지점명 <- c("골지A","오대A","주천A","평창A","옥동A","한강A","섬강A","섬강B",
                "북한A","북한B","소양A","인북A","소양B","북한C","홍천A","한탄A",
                "낙본A","한강D","북한D","제천A","한강B","한탄B","임진A") 
ID <- c(1:23)
OBSRANK <- data.frame(총량지점명, ID)
BOD_achconc <- BOD_achconc %>% left_join(OBSRANK, by = "총량지점명") %>% arrange(ID) %>% select(-ID)
TP_achconc <- TP_achconc %>% left_join(OBSRANK, by = "총량지점명") %>% arrange(ID) %>% select(-ID)
BOD_achrate <- BOD_achrate %>% left_join(OBSRANK, by = "총량지점명") %>% arrange(ID) %>% select(-ID)
TP_achrate <- TP_achrate %>% left_join(OBSRANK, by = "총량지점명") %>% arrange(ID) %>% select(-ID)

# 데이터 csv로 내보내기
write.csv(TP_achconc, file = "D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/TP_달성률625_달성기준수질.csv")
write.csv(BOD_achconc, file = "D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/BOD_달성률625_달성기준수질.csv")
write.csv(TP_achrate, file = "D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/TP_달성률625_달성률.csv")
write.csv(BOD_achrate, file = "D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/BOD_달성률625_달성률.csv")

