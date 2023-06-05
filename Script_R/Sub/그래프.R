# 관련 패키지 로드
library(readxl)
library(lubridate)
library(tidyverse)
library(data.table)
library(ggthemes)

# Excel 파일 불러오기_readxl
obs <- read_excel("D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/총량측정망 수질/전체 총량측정망/총량측정망0519.xlsx")
flow <- read_excel("D:/1. 수질총량/2. 기준유량/기준유량 구간최대값.xlsx")
target <- read_excel("D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/총량측정망 수질/목표수질.xlsx") %>% 
  select(-TOC_목표수질)

# 실측데이터 결측치 제거
obs <- na.omit(obs)

# obs 연도 추가_lubridate, dplyr
obs <- obs %>% mutate(연도 = year(obs$일자)) %>% 
  mutate(월 = month(obs$일자)) %>% 
  left_join(target, by = "총량지점명")

# 유량구간 및 목표수질 초과여부 확인
obs1 <- obs %>% left_join(flow, by = "총량지점명") %>% 
  mutate(유량구간 = ifelse(유량 <= 갈수기, "갈수기",
                       ifelse(유량 <= 저수기, "저수기",
                              ifelse(유량 <= 평수기, "평수기",
                                     ifelse(유량 <= 풍수기, "풍수기", "홍수기"))))) %>% 
  mutate(BOD초과 = ifelse(BOD>BOD_목표수질, 1, 0)) %>% 
  mutate(TP초과 = ifelse(TP>TP_목표수질, 1, 0))

# '연도'를 요인형(factor)으로 변환
obs1$연도 <- as.factor(obs1$연도)

# T-P 박스플롯 (연도 별)
obs1 %>% 
  filter(연도 %in% c(2010:2019)) %>% 
  filter(총량지점명 == "한강A") %>% 
  ggplot(aes(x = 연도, y = TP)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) + 
  geom_jitter(aes(col = 유량구간), width = 0.07, size = 2, alpha = 0.4) +
  scale_y_log10() +
  stat_summary(fun.y = "mean", geom = "point", shape = 22, size = 3, fill = "blue") +
  ggtitle("한강A T-P") + xlab("연도")+ ylab("T-P (mg/L)") + theme_bw() +
  theme(plot.title = element_text(size = 17),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(size = 12))

# T-P 박스플롯 (도경계 지점 별)
obs1 %>% 
  filter(연도 %in% c(2016:2018)) %>% 
  filter(총량지점명 %in% c("한강A", "섬강A", "홍천A", "북한C", "한탄A")) %>% 
  ggplot(aes(x = 총량지점명, y = TP)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) + 
  geom_jitter(aes(col = 유량구간), width = 0.07, size = 2, alpha = 0.4) +
  scale_y_log10() +
  stat_summary(fun.y = "mean", geom = "point", shape = 22, size = 3, fill = "blue") +
  ggtitle("T-P") + xlab("단위유역")+ ylab("T-P (mg/L)") + theme_bw() +
  theme(plot.title = element_text(size = 17),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(size = 12))
  
# BOD 박스플롯
obs1 %>% 
  filter(연도 %in% 2018) %>% 
  filter(총량지점명 %in% c("한강A", "섬강A", "홍천A", "북한C", "한탄A")) %>% 
  ggplot(aes(x = 총량지점명, y = BOD)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) + 
  geom_jitter(aes(col = 유량구간), width = 0.07) +
  stat_summary(fun.y="mean", geom="point", shape=22, size=3, fill="blue") +
  ggtitle("BOD") + xlab("단위유역")+ ylab("BOD (mg/L)") + theme_bw() +
  theme(plot.title = element_text(size = 17),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(size = 12))

