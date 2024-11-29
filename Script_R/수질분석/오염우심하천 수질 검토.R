#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(janitor)
library(readxl)
library(writexl)
library(scales)
library(ggthemes)
library(showtext)

## 그래프용 Noto Sans KR 폰트 추가
font_add_google('Noto Sans KR', 'notosanskr')
showtext_auto()


#####  함수 정의  ##############################################################
## 월별 계절 정의 함수 로드
source("Script_R/Function/func_season.R")


#####  파일 불러오기  ##########################################################
obs_우심 <- read_excel("수질분석/오염우심하천_결과정리.xlsx") %>% 
  mutate(유역 = str_sub(지점명, start = 1L, end = 2L), .before = 1) %>% 
  mutate(지점번호 = str_extract_all(지점명, "[0-9]{1,}"), .after = 지점명) %>% 
  arrange(유역, 일자, 지점번호) %>% 
  select(-c(회차, 날씨))


#####  데이터 정리  ############################################################

## obs_측정값 정리
obs_우심1 <- obs_우심 %>%
  # BOD열에서 결측치 제거(결측치가 아닌 값들만 필터)
  filter(!is.na(BOD)) %>%
  mutate(월 = month(일자)) %>%
  # 월별로 계절 정의
  season() %>% 
  # 지점 번호 요인형(factor)으로 변환(X축으로 지정 하기 위해서)
  mutate(지점번호 = factor(지점번호, levels = c(1:20)))


###################  그래프 작성  ##############################################

### 오대천 그래프 ---------------------------------------------------------------------------------------------
pdf("E:/Coding/TMDL/수질 분석/Output/Plot/오염우심하천/오대천.pdf", 
    width = 8.1, height = 5.9)

# 오대 BOD 그래프
obs_우심1 %>% filter(유역 == "오대") %>% 
  ggplot(aes(x = 지점번호, y = BOD)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "BOD (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 오대 T-P 그래프
obs_우심1 %>% filter(유역 == "오대") %>% 
  ggplot(aes(x = 지점번호, y = TP)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "T-P (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 오대 SS 그래프
obs_우심1 %>% filter(유역 == "오대") %>% 
  ggplot(aes(x = 지점번호, y = SS)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "SS (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 오대 TOC 그래프
obs_우심1 %>% filter(유역 == "오대") %>% 
  ggplot(aes(x = 지점번호, y = TOC)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "TOC (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 오대 T-N 그래프
obs_우심1 %>% filter(유역 == "오대") %>% 
  ggplot(aes(x = 지점번호, y = TN)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "T-N (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 오대 EC 그래프
obs_우심1 %>% filter(유역 == "오대") %>% 
  ggplot(aes(x = 지점번호, y = EC)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "EC (µS/cm)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

dev.off()


### 섬강 그래프 -----------------------------------------------------------------------------------------------
pdf("E:/Coding/TMDL/수질 분석/Output/Plot/오염우심하천/섬강.pdf", 
    width = 8.1, height = 5.9)

# 섬강 BOD 그래프
obs_우심1 %>% filter(유역 == "섬강") %>% 
  ggplot(aes(x = 지점번호, y = BOD)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "BOD (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

# pdf("E:/Coding/TMDL/수질 분석/Output/Plot/오염우심하천_섬강A.pdf",
#     width = 8.4, height = 6)

## 섬강 T-P 그래프
obs_우심1 %>% filter(유역 == "섬강") %>%   # 섬강A : 지점번호 %in% c("1":"9") 추가
  ggplot(aes(x = 지점번호, y = TP)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절),
    shape = 21, alpha = 0.6, size = 1.8,
    position = position_jitter(0.2)
  ) +
  scale_y_log10() +
  labs(y = "T-P (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

# dev.off()

## 섬강 SS 그래프
obs_우심1 %>% filter(유역 == "섬강") %>% 
  ggplot(aes(x = 지점번호, y = SS)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "SS (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 섬강 TOC 그래프
obs_우심1 %>% filter(유역 == "섬강") %>% 
  ggplot(aes(x = 지점번호, y = TOC)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  # scale_y_log10() +
  labs(y = "TOC (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 섬강 T-N 그래프
obs_우심1 %>% filter(유역 == "섬강") %>% 
  ggplot(aes(x = 지점번호, y = TN)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  # scale_y_log10() +
  labs(y = "T-N (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 섬강 EC 그래프
obs_우심1 %>% filter(유역 == "섬강") %>% 
  ggplot(aes(x = 지점번호, y = EC)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  # scale_y_log10() +
  labs(y = "EC (µS/cm)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

dev.off()


### 한탄강 그래프 ---------------------------------------------------------------------------------------------
pdf("E:/Coding/TMDL/수질 분석/Output/Plot/오염우심하천/한탄강.pdf", 
    width = 8.1, height = 5.9)

# 한탄 BOD 그래프
obs_우심1 %>% filter(유역 == "한탄") %>% 
  ggplot(aes(x = 지점번호, y = BOD)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "BOD (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 한탄 T-P 그래프
obs_우심1 %>% filter(유역 == "한탄") %>% 
  ggplot(aes(x = 지점번호, y = TP)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "T-P (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 한탄 SS 그래프
obs_우심1 %>% filter(유역 == "한탄") %>% 
  ggplot(aes(x = 지점번호, y = SS)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "SS (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 한탄 TOC 그래프
obs_우심1 %>% filter(유역 == "한탄") %>% 
  ggplot(aes(x = 지점번호, y = TOC)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "TOC (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 한탄 T-N 그래프
obs_우심1 %>% filter(유역 == "한탄") %>% 
  ggplot(aes(x = 지점번호, y = TN)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  # scale_y_log10() +
  labs(y = "T-N (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 한탄 EC 그래프
obs_우심1 %>% filter(유역 == "한탄") %>% 
  ggplot(aes(x = 지점번호, y = EC)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  # scale_y_log10() +
  labs(y = "EC (µS/cm)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

dev.off()


### 인북천 그래프 ---------------------------------------------------------------------------------------------
pdf("E:/Coding/TMDL/수질 분석/Output/Plot/오염우심하천/인북천.pdf", 
    width = 8.1, height = 5.9)

# 인북 BOD 그래프
obs_우심1 %>% filter(유역 == "인북") %>% 
  ggplot(aes(x = 지점번호, y = BOD)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "BOD (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 인북 T-P 그래프
obs_우심1 %>% filter(유역 == "인북") %>% 
  ggplot(aes(x = 지점번호, y = TP)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "T-P (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 인북 SS 그래프
obs_우심1 %>% filter(유역 == "인북") %>% 
  ggplot(aes(x = 지점번호, y = SS)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "SS (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 인북 TOC 그래프
obs_우심1 %>% filter(유역 == "인북") %>% 
  ggplot(aes(x = 지점번호, y = TOC)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "TOC (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 인북 T-N 그래프
obs_우심1 %>% filter(유역 == "인북") %>% 
  ggplot(aes(x = 지점번호, y = TN)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "T-N (mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

## 인북 EC 그래프
obs_우심1 %>% filter(유역 == "인북") %>% 
  ggplot(aes(x = 지점번호, y = EC)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절), shape = 21, alpha = 0.6, size = 1.8,
              position = position_jitter(0.2)) +
  scale_y_log10() +
  labs(y = "EC (µS/cm)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

dev.off()

