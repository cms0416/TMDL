# 관련 패키지 로드
library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
# 반올림 사용자 정의 함수 로드
source("round2func.R")

# Excel 파일 불러오기_readxl
OBS <- read_excel("D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/총량측정망 수질/전체 총량측정망/총량측정망0519.xlsx")
FLOW <- read_excel("D:/1. 수질총량/2. 기준유량/기준유량 구간최대값.xlsx")

# '총량지점명' 문자형(character)에서 요인형(factor)으로 변환
OBS$총량지점명 <- as.factor(OBS$총량지점명)
FLOW$총량지점명 <- as.factor(FLOW$총량지점명)

# 결측치 제거
OBS <- na.omit(OBS)

# 연도 추가_lubridate, dplyr
OBS <- OBS %>% mutate(연도 = year(OBS$일자))
OBS$연도 <- as.integer(OBS$연도)

# 유량구간 및 목표수질 초과여부 확인
OBS1 <- OBS %>% left_join(FLOW, by = "총량지점명") %>% 
  mutate(유량구간 = ifelse(유량 <= 갈수기, "갈수기",
                       ifelse(유량 <= 저수기, "저수기",
                          ifelse(유량 <= 평수기, "평수기",
                             ifelse(유량 <= 풍수기, "풍수기", "홍수기")))))

# 연도별, 유황구간별 기준수질(평균) 계산
BOD_ave <- OBS1 %>% filter(연도 %in% c(2015)) %>% 
  group_by(총량지점명, 연도) %>% 
  summarise(연평균 = round2(mean(BOD), 1))

BOD_std <- OBS1 %>% filter(연도 %in% c(2015)) %>% 
  group_by(총량지점명, 연도, 유량구간) %>% 
  summarise(기준수질 = round2(mean(BOD), 1)) %>% 
  spread(유량구간, 기준수질) %>%  # 데이터 형태 변환(long -> wide)_tidyr
  left_join(BOD_ave, by = c("총량지점명", "연도")) %>% 
  select(총량지점명, 연도, 연평균, 홍수기, 풍수기, 평수기, 저수기, 갈수기) # 열 순서 변경
  

TP_ave <- OBS1 %>% filter(연도 %in% c(2015)) %>% 
  group_by(총량지점명, 연도) %>% 
  summarise(연평균 = round2(mean(TP), 3))

TP_std <- OBS1 %>% filter(연도 %in% c(2015)) %>% 
  group_by(총량지점명, 연도, 유량구간) %>% 
  summarise(기준수질 = round2(mean(TP), 3)) %>% 
  spread(유량구간, 기준수질) %>% 
  left_join(TP_ave, by = c("총량지점명", "연도")) %>% 
  select(총량지점명, 연도, 연평균, 홍수기, 풍수기, 평수기, 저수기, 갈수기)

# 수계 순서에 맞춰 데이터 순서 조정(지점 추가 및 변경 시 수정)
총량지점명 <- c("골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "한강B",
                "제천A", "한강C", "달천A", "달천B", "한강D", "섬강A", "섬강B",
                "청미A", "양화A", "복하A", "한강E", "흑천A", "북한A", "북한B",
                "소양A", "인북A", "소양B", "북한C", "가평A", "홍천A", "북한D",
                "조종A", "경안A", "경안B", "한강F", "왕숙A", "한강G", "탄천A",
                "중랑A", "한강H", "안양A", "한강I", "굴포A", "공릉A", "임진A",
                "한탄A", "영평A", "신천A", "한탄B", "문산A", "임진B") 
총량지점명 <- as.factor(총량지점명)
ID <- c(1:48)
OBSRANK <- data.frame(총량지점명, ID)

BOD_std <- BOD_std %>% left_join(OBSRANK, by = "총량지점명") %>% 
  arrange(ID) %>% select(-ID)

TP_std <- TP_std %>% left_join(OBSRANK, by = "총량지점명") %>% 
  arrange(ID) %>% select(-ID)

# 데이터 csv로 내보내기
write.csv(BOD_std, file = "D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/BOD_기준수질2015.csv")
write.csv(TP_std, file = "D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/TP_기준수질2015.csv")

