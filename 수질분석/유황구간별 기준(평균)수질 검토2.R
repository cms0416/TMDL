# 관련 패키지 로드
library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)

# 반올림 사용자 정의 함수 로드
source("Script/Function/round2func.R")

# Excel 파일 불러오기_readxl
OBS <- read_excel("Data/총량측정망0520.xlsx")
FLOW <- read_excel("Data/기준유량 구간최대값.xlsx")

# 결측치 제거
OBS <- na.omit(OBS)

# 낙본A 제거
OBS <- OBS %>% filter(총량지점명 != "낙본A")

# 연도 추가_lubridate, dplyr
OBS <- OBS %>% mutate(연도 = year(OBS$일자))
OBS$연도 <- as.integer(OBS$연도)

# 유량구간 및 목표수질 초과여부 확인
OBS1 <- OBS %>% left_join(FLOW, by = "총량지점명") %>% 
  mutate(유량구간 = ifelse(유량 <= 갈수기, "갈수기",
                       ifelse(유량 <= 저수기, "저수기",
                          ifelse(유량 <= 평수기, "평수기",
                             ifelse(유량 <= 풍수기, "풍수기", "홍수기")))))

# 유황구간별 자료수
count_all <- OBS1 %>% filter(연도 %in% c(2010:2020)) %>% 
  group_by(총량지점명, 연도) %>% 
  summarise(수치 = n()) %>% mutate(유량구간 = "연평균") %>% 
  select(총량지점명, 연도, 유량구간, 수치)

count_flow <- OBS1 %>% filter(연도 %in% c(2010:2020)) %>% 
  group_by(총량지점명, 연도, 유량구간) %>% 
  summarise(수치 = n())  

count_bind <- bind_rows(count_all, count_flow) %>% arrange(총량지점명) %>% 
  spread(유량구간, 수치) %>%  # 데이터 형태 변환(long -> wide -> long)_tidyr
  gather(유량구간, 수치, c("연평균", "홍수기", "풍수기", "평수기", "저수기", "갈수기")) %>% 
  mutate(구분 = "자료수")


# 연도별, 유황구간별 기준수질(평균) 계산
# BOD
BOD_ave <- OBS1 %>% filter(연도 %in% c(2010:2020)) %>% 
  group_by(총량지점명, 연도) %>% 
  summarise(수치 = round2(mean(BOD), 1)) %>% mutate(유량구간 = "연평균") %>% 
  select(총량지점명, 연도, 유량구간, 수치)

BOD_std <- OBS1 %>% filter(연도 %in% c(2010:2020)) %>% 
  group_by(총량지점명, 연도, 유량구간) %>% 
  summarise(수치 = round2(mean(BOD), 1)) 

BOD_bind <- bind_rows(BOD_ave, BOD_std) %>% arrange(총량지점명) %>% 
  spread(유량구간, 수치) %>%  # 데이터 형태 변환(long -> wide -> long)_tidyr
  gather(유량구간, 수치, c("연평균", "홍수기", "풍수기", "평수기", "저수기", "갈수기")) %>% 
  mutate(구분 = "BOD")

# T-P
TP_ave <- OBS1 %>% filter(연도 %in% c(2010:2020)) %>% 
  group_by(총량지점명, 연도) %>% 
  summarise(수치 = round2(mean(TP), 3)) %>% mutate(유량구간 = "연평균") %>% 
  select(총량지점명, 연도, 유량구간, 수치)

TP_std <- OBS1 %>% filter(연도 %in% c(2010:2020)) %>% 
  group_by(총량지점명, 연도, 유량구간) %>% 
  summarise(수치 = round2(mean(TP), 3))

TP_bind <- bind_rows(TP_ave, TP_std) %>% arrange(총량지점명) %>% 
  spread(유량구간, 수치) %>%  # 데이터 형태 변환(long -> wide -> long)_tidyr
  gather(유량구간, 수치, c("연평균", "홍수기", "풍수기", "평수기", "저수기", "갈수기")) %>% 
  mutate(구분 = "TP")

# 도경계 -----------------------------------------------------------------------
# 전체 합치기
all_bind <- bind_rows(BOD_bind, TP_bind, count_bind)
all_bind <- all_bind %>%
  filter(총량지점명 %in% c("한강A", "한강D", "섬강B", "북한C", "홍천A", "한탄A")) %>% 
  spread(구분, 수치)

all_bind$자료수[is.na(all_bind$자료수)] <- 0   # 자료수 결측치 0으로 변경

# '총량지점명', '유량구간' 요인형(factor)으로 변환 후 요인 순서 변경
all_bind$유량구간 <- as.factor(all_bind$유량구간)
all_bind$총량지점명 <- as.factor(all_bind$총량지점명)
all_bind <- all_bind %>% mutate(유량구간 = factor(유량구간, levels = c("연평균", "홍수기", "풍수기", "평수기", "저수기", "갈수기")), 총량지점명 = factor(총량지점명, levels = c("한강A", "한강D", "섬강B", "북한C", "홍천A", "한탄A")))
  
# 순서 정렬
all_bind <- all_bind[c(order(all_bind$총량지점명, all_bind$연도, all_bind$유량구간)),]

# 데이터 csv로 내보내기
write.csv(all_bind, file = "Output/Data/기준수질_도경계_1320.csv")
write.csv(OBS1, file = "Output/Data/유황별수질.csv")

# 한강수계 전체 ----------------------------------------------------------------
# 전체 합치기
all_bind2 <- bind_rows(BOD_bind, TP_bind, count_bind)
all_bind2 <- all_bind2 %>%
  spread(구분, 수치)

all_bind2$자료수[is.na(all_bind2$자료수)] <- 0   # 자료수 결측치 0으로 변경

# '총량지점명', '유량구간' 요인형(factor)으로 변환 후 요인 순서 변경
all_bind2$유량구간 <- as.factor(all_bind2$유량구간)
all_bind2$총량지점명 <- as.factor(all_bind2$총량지점명)
all_bind2 <- all_bind2 %>%
  mutate(유량구간 = factor(유량구간, levels = c("연평균", "홍수기", "풍수기", "평수기", "저수기", "갈수기")), 
             총량지점명 = factor(총량지점명, levels = c("골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "한강B", "제천A", "한강C", "달천A", "달천B", "한강D", "섬강A", "섬강B", "청미A", "양화A", "복하A", "한강E", "흑천A", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C", "가평A", "홍천A", "북한D", "조종A", "경안A", "경안B", "한강F", "왕숙A", "한강G", "탄천A", "중랑A", "한강H", "안양A", "한강I", "굴포A", "공릉A", "임진A", "한탄A", "영평A", "신천A", "한탄B", "문산A", "임진B")))

# 순서 정렬
all_bind2 <- all_bind2[c(order(all_bind2$총량지점명, all_bind2$연도, all_bind2$유량구간)),]

# 데이터 csv로 내보내기
write.csv(all_bind2, file = "Output/Data/기준수질_전체_1020.csv")



# 3년 단위 평균 계산 -----------------------------------------------------------
BOD_std2 <- OBS1 %>% filter(연도 %in% c(2015:2017)) %>% 
  group_by(총량지점명, 유량구간) %>%
  filter(총량지점명 %in% c("한강A", "한강D", "섬강B", "북한C", "홍천A", "한탄A")) %>%
  filter(유량구간 %in% c("5저수기")) %>%
  summarise(수치 = round2(mean(BOD), 1)) 

TP_std2 <- OBS1 %>% filter(연도 %in% c(2015:2017)) %>% 
  group_by(총량지점명, 유량구간) %>% 
  filter(총량지점명 %in% c("한강A", "한강D", "섬강B", "북한C", "홍천A", "한탄A")) %>%
  filter(유량구간 %in% c("4평수기", "5저수기")) %>%
  summarise(수치 = round2(mean(TP), 3))

