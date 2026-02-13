# 관련 패키지 로드
library(readxl)
library(lubridate)
library(tidyverse)
library(data.table)
library(ggthemes)

# Excel 파일 불러오기_readxl
obs <- read_excel("D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/총량측정망 수질/1.한강수계 총량측정망(05-18).xls",
                  skip = 2) %>% select(총량지점명, 일자, "BOD(㎎/L)", "총인(T-P)(㎎/L)", "유량(㎥/s)")
fdc <- read_excel("D:/1. 수질총량/3. 목표수질/1. 한강수계/검토자료/LDC/유황구간(이티 제공자료)/fdc.xlsx")
flow <- read_excel("D:/1. 수질총량/2. 기준유량/기준유량 구간최대값.xlsx")
target <- read_excel("D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/총량측정망 수질/목표수질.xlsx") %>% 
  select(-TOC_목표수질)

# fdc 데이터 형태 변환(wide -> long)_tidyr
fdc1 <- fdc %>% gather(총량지점명, 유량, 골지A:한강J) %>% 
  select(총량지점명, exceedance, 유량)

# '총량지점명' 문자형(character)에서 요인형(factor)으로 변환
obs$총량지점명 <- as.factor(obs$총량지점명)
flow$총량지점명 <- as.factor(flow$총량지점명)
target$총량지점명 <- as.factor(target$총량지점명)
fdc1$총량지점명 <- as.factor(fdc$총량지점명)

# 기준 부하량 계산
load1 <- fdc1 %>% left_join(target, by = "총량지점명") %>% 
  mutate(BOD_LOAD = 유량 * BOD_목표수질 * 86.4) %>% 
  mutate(TP_LOAD = 유량 * TP_목표수질 * 86.4) 
load1$총량지점명 <- as.factor(load1$총량지점명)

# 실측데이터 결측치 제거
obs <- na.omit(obs)

# obs 변수명 변경(R 변수명 규칙에 맞게 조정)_data.table
setnames(obs, c("BOD(㎎/L)", "총인(T-P)(㎎/L)", "유량(㎥/s)"), c("BOD", "TP", "유량"))

# obs 연도 추가_lubridate, dplyr
obs <- obs %>% mutate(연도 = year(obs$일자)) %>% 
  mutate(월 = month(obs$일자)) %>% 
  left_join(target, by = "총량지점명") %>% 
  mutate(BOD_측정부하량 = 유량 * BOD * 86.4) %>% 
  mutate(TP_측정부하량 = 유량 * TP * 86.4)

obs0 <- obs %>% filter(총량지점명 %in% "한강A")

for (i in 1:length(obs0$유량)){
  gap_abs <- abs(fdc$한강A - obs0$유량[i])
  min_idx <- which.min(gap_abs)
  obs0$exceedance[i] <- fdc$exceedance[min_idx]
}


obs$연도 <- as.integer(obs$연도)

# 유량구간 및 목표수질 초과여부 확인
obs1 <- obs %>% left_join(flow, by = "총량지점명") %>% 
  mutate(유량구간 = ifelse(유량 <= 갈수기, "갈수기",
                       ifelse(유량 <= 저수기, "저수기",
                              ifelse(유량 <= 평수기, "평수기",
                                     ifelse(유량 <= 풍수기, "풍수기", "홍수기"))))) %>% 
  mutate(BOD초과 = ifelse(BOD>BOD_목표수질, 1, 0)) %>% 
  mutate(TP초과 = ifelse(TP>TP_목표수질, 1, 0)) %>% 
  mutate(TOC초과 = ifelse(TOC>TOC_목표수질, 1, 0))
                                           
# T-P 박스플롯
obs1 %>% 
  filter(연도 %in% c(2016:2018)) %>% 
  filter(총량지점명 %in% c("한강A", "섬강A", "홍천A", "북한C", "한탄A")) %>% 
  ggplot(aes(x = 총량지점명, y = TP)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) + 
  geom_jitter(aes(col = 유량구간), width = 0.07) +
  scale_y_log10() +
  stat_summary(fun.y="mean", geom="point", shape=22, size=3, fill="blue") +
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

