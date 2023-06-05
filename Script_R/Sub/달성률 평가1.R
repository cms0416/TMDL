### 관련 패키지 로드
library(readxl)
library(lubridate)
library(tidyverse)
library(data.table)
library(ggthemes)
library(extrafont)

### Excel 파일 불러오기_readxl
obs <- read_excel("D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/총량측정망 수질/전체 총량측정망/총량측정망0520.xlsx") %>% 
  select(총량지점명, 일자, BOD, TP, 유량, 연도)
fdc <- read_excel("D:/1. 수질총량/3. 목표수질/1. 한강수계/검토자료/LDC/유황구간(이티 제공자료)/fdc.xlsx")
flow <- read_excel("D:/1. 수질총량/2. 기준유량/기준유량 구간최대값.xlsx")
target <- read_excel("D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/총량측정망 수질/목표수질.xlsx") %>% 
  select(-TOC_목표수질)

# 강원도포함 / 한강수계만 필터
obs <- obs %>% filter(총량지점명 %in% c("골지A","오대A","주천A","평창A","옥동A","한강A","섬강A","섬강B",
                                        "북한A","북한B","소양A","인북A","소양B","북한C","홍천A","한탄A",
                                        "한강D","북한D","제천A","한강B","한탄B","임진A"))

### 초과유량 백분율 퍼센트로 계산
fdc <- fdc %>% mutate(퍼센트 = exceedance * 100) %>% 
  select(ID, 퍼센트, everything())  # 열 순서 조정

### fdc 데이터 형태 변환(wide -> long)_tidyr
## gather(key, value, key에 포함시킬 열 or 제외 시킬 열)
## key : 각 열의 header가 값으로 지정됨, value : 각 열의 header 아래 값들이 값으로 지정됨
fdc1 <- fdc %>% gather(총량지점명, 유량, -ID, -퍼센트, -exceedance) %>%   
  select(총량지점명, 퍼센트, 유량)

# '총량지점명' 문자형(character)에서 요인형(factor)으로 변환
# obs$총량지점명 <- as.factor(obs$총량지점명)
# flow$총량지점명 <- as.factor(flow$총량지점명)
# target$총량지점명 <- as.factor(target$총량지점명)
# fdc1$총량지점명 <- as.factor(fdc1$총량지점명)

### 기준 부하량 계산
load1 <- fdc1 %>% left_join(target, by = "총량지점명") %>%   # 목표수질 추가
  mutate(BOD_LOAD = 유량 * BOD_목표수질 * 86.4) %>%          # 목표부하량 계산
  mutate(TP_LOAD = 유량 * TP_목표수질 * 86.4) 
load1$총량지점명 <- as.factor(load1$총량지점명)

### 실측데이터(수질) 결측치 제거
obs <- obs %>% filter(!is.na(BOD)) #BOD열에서 결측치 제거(결측치가 아닌 값들만 필터)

### obs_측정값 정리_lubridate, dplyr
obs <- obs %>% mutate(월 = month(obs$일자)) %>%      # 월 추가
  mutate(계절 = ifelse(월 >= 3 & 월 <= 5, "봄",      # 계절 추가
                      ifelse(월 >= 6 & 월 <= 8, "여름",
                              ifelse(월 >= 9 & 월 <= 11, "가을", "겨울"))))  %>%
  mutate(BOD_측정부하량 = round(유량 * BOD * 86.4, 2)) %>%     # 측정부하량 계산 
  mutate(TP_측정부하량 = round(유량 * TP * 86.4, 3))

# '계절' 요인형(factor)으로 변환 후 계절 순서로 요인 순서 변경
obs$월 <- as.factor(obs$월)
obs$계절 <- as.factor(obs$계절)
obs <- obs %>% mutate(계절 = factor(계절, levels = c("봄", "여름", "가을", "겨울")))

#단위유역 및 연도 필터
#obs0 <- obs %>% filter(총량지점명 %in% "한강A") %>% 
#  filter(연도 %in% c(2017:2019))

#load2 <- load1 %>% filter(총량지점명 %in% "한강A")

# 초과확률 불러오기
#for (i in 1:length(obs0$유량)){
#  gap_abs <- abs(fdc$한강A - obs0$유량[i])
#  min_idx <- which.min(gap_abs)
#  obs0$퍼센트[i] <- fdc$퍼센트[min_idx]
#}


# 유량구간 및 목표수질 초과여부 확인
obs1 <- obs %>% left_join(target, by = "총량지점명") %>% 
  left_join(flow, by = "총량지점명") %>% 
  mutate(유량구간 = ifelse(유량 <= 갈수기, "갈수기",
                         ifelse(유량 <= 저수기, "저수기",
                                  ifelse(유량 <= 평수기, "평수기",
                                           ifelse(유량 <= 풍수기, "풍수기", "홍수기"))))) %>% 
  mutate(BOD달성 = ifelse(BOD <= BOD_목표수질, 1, 0)) %>% 
  mutate(TP달성 = ifelse(TP <= TP_목표수질, 1, 0)) %>% 
  mutate(자료수 = 1) %>% 
  select(-홍수기, -풍수기, -평수기, -저수기, -갈수기) %>% 
  filter(연도 >= 2014)

#--------------------------------------------------------------------------------
# 전체 자료 초과율
total <- obs1 %>% group_by(총량지점명, 연도, 월, 계절) %>% 
  summarise(전체_자료수 = sum(자료수),
                  BOD_달성수 = sum(BOD달성), 
                  TP_달성수 = sum(TP달성))

total <- total %>% mutate(BOD달성률 = round(BOD_달성수/전체_자료수*100, 2),
                          TP달성률 = round(TP_달성수/전체_자료수*100, 2))

# 계절별 수질 현황
seasonal <- obs1 %>% group_by(총량지점명, 연도, 계절) %>% 
  summarise(전체_자료수 = sum(자료수),
                  BOD_달성수 = sum(BOD달성), 
                  TP_달성수 = sum(TP달성))


seasonal <- seasonal %>% mutate(BOD달성률 = round(BOD_달성수/전체_자료수*100, 2),
                                TP달성률 = round(TP_달성수/전체_자료수*100, 2))
                                #BOD계절별비율 = round(prop.table(BOD_달성수)*100, 2),
                                #TP계절별비율 = round(prop.table(TP_달성수)*100, 2))
                              

# 월별 수질 현황
monthly <- obs1 %>% group_by(총량지점명, 연도, 월) %>% 
  summarise(전체_자료수 = sum(자료수),
                  BOD_달성수 = sum(BOD달성), 
                  TP_달성수 = sum(TP달성))

monthly <- monthly %>% mutate(BOD달성률 = round(BOD_달성수/전체_자료수*100, 2),
                              TP달성률 = round(TP_달성수/전체_자료수*100, 2))
                              #BOD계절별비율 = round(prop.table(BOD_달성수)*100, 2),
                              #TP계절별비율 = round(prop.table(TP_달성수)*100, 2))

# 유황별 수질 현황
flowduration <- obs1 %>% group_by(총량지점명, 연도, 유량구간) %>% 
  summarise(전체_자료수 = sum(자료수),
                  BOD_달성수 = sum(BOD달성), 
                  TP_달성수 = sum(TP달성))

flowduration <- flowduration %>% mutate(BOD달성률 = round(BOD_달성수/전체_자료수*100, 2),
                                        TP달성률 = round(TP_달성수/전체_자료수*100, 2))
                                        #BOD계절별비율 = round(prop.table(BOD_달성수)*100, 2),
                                        #TP계절별비율 = round(prop.table(TP_달성수)*100, 2))

# 수질자료 내보내기
write.csv(obs1, file = "D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/월별계절별수질_TP.csv")


ggplot() + 
  geom_jitter(data = obs1, aes(x = 총량지점명, y = TP))


##################################
###
###  그래프(LDC) 작성
###
##################################

#### T-P LDC 초과 자료 표시 ####
#png(width = 1800, height = 1200, filename = "pngaa.jpg", type = "cairo", antialias = "subpixel", res = 220) 
ggplot() +
  geom_vline(xintercept = c(0, 10, 40, 60, 90, 100), linetype = 'dashed', size = 0.5) +    # 유황구간 구분 수직선 추가
  geom_point(data = obs1, aes(x = 퍼센트, y = TP_측정부하량, color = "black"),        # 실측 부하량 포인트
             shape = 21, size = 3, stroke = 1) + 
  geom_point(data = obs1[obs1$TP초과 == "1",], aes(x = 퍼센트, y = TP_측정부하량),        # 실측 부하량 포인트
             color = "black", fill = "pink", shape = 21, alpha = 0.6, size = 3) + 
  geom_line(data = load2, aes(x = 퍼센트, y = TP_LOAD, color = "red"), group = 1,     # 목표 부하량 지속 곡선(LDC)
            linetype = 2, size = 1.1) + 
  scale_y_log10() +                                                                   # y축 log 스케일 변환
  scale_x_continuous(breaks = seq(0, 100, 10),                    # x축 표시 형식 설정
                     labels = paste0(c(0,10,20,30,40,50,60,70,80,90,100))) +
  expand_limits(x = 0, y = 1) +                                                       # x축, y축 각각 최소값 설정
  geom_text(aes(x = 5, y = 0.1, label = "홍수기"), family = "NanumBarunGothic", size = 4, fontface = "bold") + # 유황구간 주석 
  geom_text(aes(x = 25, y = 0.1, label = "풍수기"), family = "NanumBarunGothic", size = 4, fontface = "bold") +
  geom_text(aes(x = 50, y = 0.1, label = "평수기"), family = "NanumBarunGothic", size = 4, fontface = "bold") +
  geom_text(aes(x = 75, y = 0.1, label = "저수기"), family = "NanumBarunGothic", size = 4, fontface = "bold") +
  geom_text(aes(x = 95, y = 0.1, label = "갈수기"), family = "NanumBarunGothic", size = 4, fontface = "bold") +
  ggtitle("한강A T-P LDC") + xlab("초과 유량 백분율(%)") + ylab("T-P 부하량 (kg/d)") +                   # 그래프 및 축 제목
  theme_test() + 
  scale_color_manual(name = NULL, values = c("black", "red"),
                     labels = c("실측부하량", "LDC")) +
  guides(color = guide_legend(override.aes = list(shape = c(21, NA), linetype = c(NA, 2)))) +
  theme(plot.title = element_text(family = "NanumBarunGothic", size = 17, face = "bold", hjust = 0.5),   # 그래프 제목 설정
        axis.title = element_text(family = "NanumBarunGothic", size = 13, face = "bold"),  # 축 제목 설정
        axis.text.x = element_text(size = 12),                                        # x축 수치 사이즈
        axis.text.y = element_text(size = 12),                                        ## y축 수치 사이즈
        legend.title = element_text(family = "NanumBarunGothic", size = 10),
        legend.text = element_text(family = "NanumBarunGothic", size = 10),
        legend.position = c(0.73, 0.90),                                              # 범례 위치
        legend.direction = "horizontal",                                              # 범례 방향
        legend.box = "horizontal",                                                    # 범례 배치 방향
        legend.background = element_rect(fill = "white", color = "black"),            # 범례 배경 및 테두리 색
        legend.margin = margin(5, 5, 5, 5))
#dev.off()

ggsave(filename = "LDC graph_TP.jpg", scale = 1.15, width = 18, height = 11, dpi = 320, units = "cm")

#### T-P LDC + 실측 부하량(계절) ####
# png(width = 1800, height = 1200, filename = "pngaa2.jpg", type = "cairo", antialias = "subpixel", res = 220) 
tp_season <- ggplot() +
  geom_vline(xintercept = c(0, 10, 40, 60, 90, 100), linetype = 'dashed', size = 0.5) +    # 유황구간 구분 수직선 추가
  geom_point(data = obs0, aes(x = 퍼센트, y = TP_측정부하량, fill = 계절,        # 실측 부하량 포인트
                              color = "black"), shape = 21, alpha = 0.6, size = 3) + 
  geom_line(data = load2, aes(x = 퍼센트, y = TP_LOAD, color = "red"), group = 1,     # 목표 부하량 지속 곡선(LDC)
            linetype = 2, size = 1.1) + 
  scale_y_log10() +                                                                   # y축 log 스케일 변환
  scale_x_continuous(breaks = seq(0, 100, 10),                    # x축 표시 형식 설정
                     labels = paste0(c(0,10,20,30,40,50,60,70,80,90,100))) +
  expand_limits(x = 0, y = 1) +                                                       # x축, y축 각각 최소값 설정
  geom_text(aes(x = 5, y = 0.1, label = "홍수기"), family = "NanumBarunGothic", size = 4, fontface = "bold") + # 유황구간 주석 
  geom_text(aes(x = 25, y = 0.1, label = "풍수기"), family = "NanumBarunGothic", size = 4, fontface = "bold") +
  geom_text(aes(x = 50, y = 0.1, label = "평수기"), family = "NanumBarunGothic", size = 4, fontface = "bold") +
  geom_text(aes(x = 75, y = 0.1, label = "저수기"), family = "NanumBarunGothic", size = 4, fontface = "bold") +
  geom_text(aes(x = 95, y = 0.1, label = "갈수기"), family = "NanumBarunGothic", size = 4, fontface = "bold") + 
  ggtitle("한강A T-P LDC (계절별)") + xlab("초과 유량 백분율(%)") + ylab("T-P 부하량 (kg/d)") +                   # 그래프 및 축 제목
  theme_test() + 
  scale_fill_discrete(name = "계절") +             # 범례 제목
  scale_color_manual(name = NULL, values = c("black", "red"),
                     labels = c("실측부하량", "LDC")) +
  guides(color = guide_legend(override.aes = list(shape = c(21, NA), linetype = c(NA, 2)))) +
  theme(plot.title = element_text(family = "NanumBarunGothic", size = 17, face = "bold", hjust = 0.5),   # 그래프 제목 설정
        axis.title = element_text(family = "NanumBarunGothic", size = 13, face = "bold"),  # 축 제목 설정
        axis.text.x = element_text(size = 12),                                        # x축 수치 사이즈
        axis.text.y = element_text(size = 12),                                        ## y축 수치 사이즈
        legend.title = element_text(family = "NanumBarunGothic", size = 10),
        legend.text = element_text(family = "NanumBarunGothic", size = 10),
        legend.position = c(0.65, 0.90),                                              # 범례 위치
        legend.direction = "horizontal",                                              # 범례 방향
        legend.box = "horizontal",                                                    # 범례 배치 방향
        legend.background = element_rect(fill = "white", color = "black"),            # 범례 배경 및 테두리 색
        legend.margin = margin(5, 5, 5, 5))                                           # 범례 여백
#dev.off()
ggsave(filename = "LDC graph_TP(계절별).jpg", scale = 1.15, width = 18, height = 11, dpi = 320, units = "cm")

#### T-P LDC + 실측 부하량(월) ####
tp_month <- ggplot() +
  geom_vline(xintercept = c(0, 10, 40, 60, 90, 100), linetype = 'dashed', size = 0.5) +    # 유황구간 구분 수직선 추가
  geom_point(data = obs0, aes(x = 퍼센트, y = TP_측정부하량, fill = 월,              # 실측 부하량 포인트
                              color = "black"), shape = 21, alpha = 0.6, size = 3) +
  geom_line(data = load2, aes(x = 퍼센트, y = TP_LOAD, color = "red"), group = 1,    # 목표 부하량 지속 곡선(LDC)
            linetype = 2, size = 1.1) +
  scale_y_log10() +                                                                   # y축 log 스케일 변환
  scale_x_continuous(breaks = seq(0, 100, 10),                                        # x축 표시 형식 설정
                     labels = paste0(c(0,10,20,30,40,50,60,70,80,90,100))) +
  expand_limits(x = 0, y = 1) +                                                       # x축, y축 각각 최소값 설정
  geom_text(aes(x = 5, y = 0.1, label = "홍수기"), family = "NanumBarunGothic", size = 5, fontface = "bold") + # 유황구간 주석 
  geom_text(aes(x = 25, y = 0.1, label = "풍수기"), family = "NanumBarunGothic", size = 5, fontface = "bold") +
  geom_text(aes(x = 50, y = 0.1, label = "평수기"), family = "NanumBarunGothic", size = 5, fontface = "bold") +
  geom_text(aes(x = 75, y = 0.1, label = "저수기"), family = "NanumBarunGothic", size = 5, fontface = "bold") +
  geom_text(aes(x = 95, y = 0.1, label = "갈수기"), family = "NanumBarunGothic", size = 5, fontface = "bold") + 
  ggtitle("한강A T-P LDC (월별)") + xlab("초과 유량 백분율(%)") + ylab("T-P 부하량 (kg/d)") + # 그래프 및 축 제목
  theme_test() + 
  theme(plot.title = element_text(family = "NanumBarunGothic", size = 17, face = "bold", hjust = 0.5), ## 그래프 제목 설정
        axis.title = element_text(family = "NanumBarunGothic", size = 13, face = "bold"), # 축 제목 설정
        axis.text.x = element_text(size = 12),                                        # x축 수치 사이즈
        axis.text.y = element_text(size = 12),                                        # y축 수치 사이즈
        legend.title = element_text(family = "NanumBarunGothic", size = 10),
        legend.text = element_text(family = "NanumBarunGothic", size = 10), 
        legend.position = c(0.60, 0.86),                                            # 범례 위치
        legend.direction = "horizontal",                                            # 범례 방향
        legend.background = element_rect(fill = "white", color = "black"),          # 범례 배경 및 테두리 색
        legend.margin = margin(5, 5, 5, 5)) +                                       # 범례 여백
  scale_fill_discrete(name = "월") +                                               # 범례 제목
  scale_color_manual(name = NULL, values = c("black", "red"),
                     labels = c("실측부하량", "LDC")) +
  guides(color = guide_legend(override.aes = list(shape = c(21, NA), linetype = c(NA, 2)))) +
  guides(fill = guide_legend(nrow = 1))                                              # 범례 한줄로

ggsave(filename = "LDC graph_TP(월별).jpg", scale = 1.15, width = 18, height = 11, dpi = 320, units = "cm")

# 계절별/월별 그래프 합치기
library(gridExtra)
grid.arrange(tp_season, tp_month, nrow=2)

#### BOD LDC + 실측 부하량(계절) ####
ggplot() +
  geom_vline(xintercept = c(0, 10, 40, 60, 90, 100), linetype = 'dashed', size = 0.5) +    # 유황구간 구분 수직선 추가
  geom_point(data = obs0, aes(x = 퍼센트, y = BOD_측정부하량, fill = 계절,        # 실측 부하량 포인트
                              color = "black"), shape = 21, alpha = 0.6, size = 3) + 
  geom_line(data = load2, aes(x = 퍼센트, y = BOD_LOAD, color = "red"), group = 1, # 목표 부하량 지속 곡선(LDC)
            linetype = 2, size = 1.1) + 
  scale_y_log10() +                                                                   # y축 log 스케일 변환
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100),                    # x축 표시 형식 설정
                     labels = paste0(c(0,10,20,30,40,50,60,70,80,90,100))) +
  expand_limits(x = 0, y = 1) +                                                       # x축, y축 각각 최소값 설정
  geom_text(aes(x = 5, y = 0.1, label = "홍수기"), family = "NanumBarunGothic", size = 5, fontface = "bold") + # 유황구간 주석 
  geom_text(aes(x = 25, y = 0.1, label = "풍수기"), family = "NanumBarunGothic", size = 5, fontface = "bold") +
  geom_text(aes(x = 50, y = 0.1, label = "평수기"), family = "NanumBarunGothic", size = 5, fontface = "bold") +
  geom_text(aes(x = 75, y = 0.1, label = "저수기"), family = "NanumBarunGothic", size = 5, fontface = "bold") +
  geom_text(aes(x = 95, y = 0.1, label = "갈수기"), family = "NanumBarunGothic", size = 5, fontface = "bold") +
  ggtitle("한강A BOD LDC (계절별)") + xlab("초과 유량 백분율(%)") + ylab("BOD 부하량 (kg/d)") +   # 그래프 및 축 제목
  theme_test() + 
  theme(plot.title = element_text(family = "NanumBarunGothic", size = 17, face = "bold", hjust = 0.5), # 그래프 제목 설정
        axis.title = element_text(family = "NanumBarunGothic", size = 13, face = "bold"),  # 축 제목 설정
        axis.text.x = element_text(size = 12),                                        # x축 수치 사이즈
        axis.text.y = element_text(size = 12),                                        # y축 수치 사이즈
        legend.title = element_text(family = "NanumBarunGothic", size = 10),
        legend.text = element_text(family = "NanumBarunGothic", size = 10), 
        legend.position = c(0.65, 0.90),                                            # 범례 위치
        legend.direction = "horizontal",                                            # 범례 방향
        legend.box = "horizontal",                                                  # 범례 배치 방향
        legend.background = element_rect(fill = "white", color = "black"),          # 범례 배경 및 테두리 색
        legend.margin = margin(5, 5, 5, 5)) +                                       # 범례 여백
  scale_fill_discrete(name = "계절") +                                               # 범례 제목
  scale_color_manual(name = NULL, values = c("black", "red"),
                     labels = c("실측부하량", "LDC")) +
  guides(color = guide_legend(override.aes = list(shape = c(21, NA), linetype = c(NA, 2))))


#### BOD LDC + 실측 부하량(월) ####
ggplot() +
  geom_vline(xintercept = c(0, 10, 40, 60, 90, 100), linetype = 'dashed', size = 0.5) +    # 유황구간 구분 수직선 추가
  geom_point(data = obs0, aes(x = 퍼센트, y = BOD_측정부하량, fill = 월,              # 실측 부하량 포인트
                              color = "black"), shape = 21, alpha = 0.6, size = 3) +
  geom_line(data = load2, aes(x = 퍼센트, y = BOD_LOAD, color = "red"), group = 1,     # 목표 부하량 지속 곡선(LDC)
            linetype = 2, size = 1.1) + 
  scale_y_log10() +                                                                   # y축 log 스케일 변환
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100),                    # x축 표시 형식 설정
                     labels = paste0(c(0,10,20,30,40,50,60,70,80,90,100))) +
  expand_limits(x = 0, y = 1) +                                                       # x축, y축 각각 최소값 설정
  geom_text(aes(x = 5, y = 0.1, label = "홍수기"), family = "NanumBarunGothic", size = 5, fontface = "bold") + # 유황구간 주석 
  geom_text(aes(x = 25, y = 0.1, label = "풍수기"), family = "NanumBarunGothic", size = 5, fontface = "bold") +
  geom_text(aes(x = 50, y = 0.1, label = "평수기"), family = "NanumBarunGothic", size = 5, fontface = "bold") +
  geom_text(aes(x = 75, y = 0.1, label = "저수기"), family = "NanumBarunGothic", size = 5, fontface = "bold") +
  geom_text(aes(x = 95, y = 0.1, label = "갈수기"), family = "NanumBarunGothic", size = 5, fontface = "bold") +
  ggtitle("한강A BOD LDC (월별)") + xlab("초과 유량 백분율(%)") + ylab("BOD 부하량 (kg/d)") +     # 그래프 및 축 제목
  theme_test() + 
  theme(plot.title = element_text(family = "NanumBarunGothic", size = 17, face = "bold", hjust = 0.5), # 그래프 제목 설정
        axis.title = element_text(family = "NanumBarunGothic", size = 13, face = "bold"),  # 축 제목 설정
        axis.text.x = element_text(size = 12),                                        # x축 수치 사이즈
        axis.text.y = element_text(size = 12),                                        # y축 수치 사이즈
        legend.title = element_text(family = "NanumBarunGothic", size = 10), 
        legend.text = element_text(family = "NanumBarunGothic", size = 10),
        legend.position = c(0.60, 0.86),                                            # 범례 위치
        legend.direction = "horizontal",                                            # 범례 방향
        legend.background = element_rect(fill = "white", color = "black"),          # 범례 배경 및 테두리 색
        legend.margin = margin(5, 5, 5, 5)) +                                       # 범례 여백
  scale_fill_discrete(name = "월") +                                                # 범례 제목
  scale_color_manual(name = NULL, values = c("black", "red"),
                     labels = c("실측부하량", "LDC")) +
  guides(color = guide_legend(override.aes = list(shape = c(21, NA), linetype = c(NA, 2)))) +
  guides(fill = guide_legend(nrow = 1))                                               # 범례 한줄로

