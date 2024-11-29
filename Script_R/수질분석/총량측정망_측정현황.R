##########  라이브러리 로드  ###################################################
library(tidyverse)
library(readxl)
library(writexl)
library(janitor)
library(ggthemes)
library(showtext)

## 그래프용 Noto Sans KR 폰트 추가
font_add_google('Noto Sans KR', 'notosanskr')
showtext_auto()


#####  함수 정의  ##############################################################
## 날짜 형식 변경 및 연도, 월 추가
source("Script_R/Function/func_date_format.R")
## 월별 계절 정의 함수 로드
source("Script_R/Function/func_season.R")


##########  파일 불러오기  #####################################################
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "수질분석/4대강수계",
  pattern = "*.xls", full.names = T
)

# 데이터 불러오기 및 합치기
obs <- files %>%
  map_dfr(~ {
    data <- read_excel(.x, skip = 1, col_names = T)
    watershed <- str_extract(basename(.x), "[가-힣]+")
    data <- data %>% mutate(수계 = watershed, .before = 1)
  }) %>% 
  # 필요없는 열 삭제
  select(-c(4:7, 9:11, 13))


##########  데이터 정리  #######################################################
obs_1 <- obs %>% 
  set_names(c(
    "수계", "총량지점명", "일자", "BOD", "TP", "유량"
  )) %>% 
  # 날짜 형식 변경 및 연도추가
  date_format(일자) %>%
  # 월별로 계절 정의
  season() %>% 
  mutate(횟수 = 1) %>% 
  filter(!is.na(TP))


계절별_평균 <- obs_1 %>% 
  group_by(수계, 총량지점명, 연도, 계절) %>% 
  summarise(across(횟수, ~ sum(.)), .groups = "drop") %>% 
  group_by(수계, 계절) %>% 
  summarise(across(횟수, ~ round(mean(.), 2)), .groups = "drop")

계절별_평균_wide <- 계절별_평균 %>% 
    pivot_wider(
    names_from = 수계,
    values_from = 횟수
  )

월별_평균 <- obs_1 %>% 
  group_by(수계, 총량지점명, 연도, 월) %>% 
  summarise(across(횟수, ~ sum(.)), .groups = "drop") %>% 
  group_by(수계, 월) %>% 
  summarise(across(횟수, ~ round(mean(.), 2)), .groups = "drop")

월별_평균_wide <- 월별_평균 %>% 
  pivot_wider(
    names_from = 수계,
    values_from = 횟수
  )

월별_횟수 <- obs_1 %>% 
  group_by(수계, 월) %>% 
  summarise(across(횟수, ~ sum(.)), .groups = "drop") %>% 
  pivot_wider(
    names_from = 수계,
    values_from = 횟수
  ) %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns() 


##########  한강수계 정리  #####################################################
한강 <- read_excel("수질분석/총량측정망_2007_2024.xlsx") %>% 
  select(총량지점명, 일자, BOD, TP, 유량, 연도, 월) %>% 
  filter(연도 != 2024) %>% 
  # 월별로 계절 정의
  season() %>% 
  mutate(횟수 = 1) %>% 
  filter(!is.na(TP)) %>% 
  group_by(총량지점명, 연도, 계절) %>% 
  summarise(across(횟수, ~ sum(.)), .groups = "drop") %>% 
  group_by(연도, 계절) %>% 
  summarise(across(횟수, ~ round(mean(.), 2)), .groups = "drop") 



##****************************************************************************##
###################################  그래프  ###################################
##****************************************************************************##

pdf("E:/Coding/TMDL/수질분석/4대강수계/총량측정망현황_그래프(8.2x3.3).pdf",
    width = 8.2, height = 3.3)

## 계절별 측정 횟수 평균 -----
계절별_평균 %>% 
  ggplot(aes(x = 계절, y = 횟수, fill = 수계)) +
  geom_bar(position = position_dodge(0.8), stat = 'identity', 
           size = 0.8, width = 0.7) +
  geom_text(aes(label = 횟수), vjust = 1.3,
            position = position_dodge(0.8), size = 3.0) +
  labs(x = "계절", y = "계절별 측정 횟수") +
  theme_classic(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(size = 13, color = "black"),
    axis.text.y = element_text(size = 13, color = "black"),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5),
    legend.title = element_text(face = "bold", size = 11, color = "black"),
    legend.text = element_text(size = 11, color = "black")
    )

## 월별 측정 횟수 평균 -----
월별_평균 %>% 
  ggplot(aes(x = 월, y = 횟수, fill = 수계)) +
  geom_bar(position = position_dodge(0.8), stat='identity', 
           size=0.8, width=0.7) +
  scale_x_continuous(name = "월", breaks = seq(1, 12, by = 1)) +
  labs(y = "월별 측정 횟수") +
  theme_classic(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(size = 13, color = "black"),
    axis.text.y = element_text(size = 13, color = "black"),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5),
    legend.title = element_text(face = "bold", size = 11, color = "black"),
    legend.text = element_text(size = 11, color = "black")
  )

## 한강 수계 연도별 겨울 측정 횟수 -----
한강 %>%
  # 연도 줄여서 표기
  mutate(연도 = str_sub(연도, 3, 4) %>% str_c("'", .)) %>% 
  filter(계절 == "겨울") %>% 
  ggplot(aes(x = 연도, y = 횟수)) +
  geom_bar(stat = 'identity', fill = "deepskyblue3", width = 0.7) +
  geom_text(aes(label = 횟수), vjust = 1.3,
            position = position_dodge(0.8), size = 3.3) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  labs(x = "연도", y = "동절기 측정 횟수") +
  theme_classic(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(size = 13, color = "black"),
    axis.text.y = element_text(size = 13, color = "black")
  )

dev.off()


