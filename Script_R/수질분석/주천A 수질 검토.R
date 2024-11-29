#####  라이브러리 로드  ########################################################
# 데이터 정리
library(tidyverse)
library(magrittr)
library(janitor)
library(readxl)
library(writexl)
library(zoo)

# 데이터 분석
library(psych) # 상관분석
library(broom)
library(ggcorrplot)
library(GGally)
# library(lmtest)

# 그래프 작성
library(scales)
library(ggthemes)
library(patchwork)
library(showtext)

## 그래프용 Noto Sans KR 폰트 추가
font_add_google("Noto Sans KR", "notosanskr")
showtext_auto()


#####  함수 정의  ##############################################################
## 월별 계절 정의 함수 로드
source("Script_R/Function/func_season.R")


#####  데이터 정리  ############################################################

## 검토 조건 설정
단위유역 <- "주천A"
시작연도 <- 2014
종료연도 <- 2023
목표수질_TP <- 0.027


## 기상자료
기상자료 <- read_excel("수질분석/기상자료/안흥기상대_강수량_기온_2014_2023.xlsx") %>%
  select(-c(지점, 지점명)) %>%
  set_names(c("일자", "평균기온", "일강수량")) %>%
  # 3일 누적 강수량 산정(zoo::rollapply)
  mutate(
    누적강수_3일 = rollapply(일강수량,
      width = 3, FUN = sum, align = "right",
      fill = NA, na.rm = TRUE
    ),
    누적강수_4일 = rollapply(일강수량,
      width = 4, FUN = sum, align = "right",
      fill = NA, na.rm = TRUE
    ),
    누적강수_5일 = rollapply(일강수량,
      width = 5, FUN = sum, align = "right",
      fill = NA, na.rm = TRUE
    )
  )


## 수질측정 자료 정리
수질측정망 <- read_excel("수질분석/총량측정망_전체_2007_2024.xlsx") %>%
  filter(
    # T-P열에서 결측치 제거
    !is.na(TP),
    # 해당 단위유역만 선택
    str_detect(총량지점명, 단위유역) == TRUE,
    # 시작 및 종료 연도 설정
    연도 >= 시작연도, 연도 <= 종료연도
  ) %>%
  season() %>%
  mutate(달성여부 = ifelse(TP > 목표수질_TP, "초과", "달성"))


## 총량측정망 자료 정리
총량측정망 <- 수질측정망 %>%
  filter(총량지점명 == 단위유역) %>%
  # T-P열에서 결측치 제거(결측치가 아닌 값들만 필터)
  filter(!is.na(TP)) %>%
  mutate(
    측정부하량_TP = 유량 * TP * 86.4,
    목표부하량_TP = 유량 * 목표수질_TP * 86.4
  ) %>%
  left_join(기상자료, by = "일자") %>%
  arrange(desc(유량)) %>%
  rowid_to_column(var = "유량크기순서") %>%
  mutate(
    유량백분율 = 유량크기순서 / nrow(.) * 100,
    유황구간 = case_when(
      유량백분율 <= 10 ~ "홍수기",
      유량백분율 > 10 & 유량백분율 <= 40 ~ "풍수기",
      유량백분율 > 40 & 유량백분율 <= 60 ~ "평수기",
      유량백분율 > 60 & 유량백분율 <= 90 ~ "저수기",
      유량백분율 > 90 & 유량백분율 <= 100 ~ "갈수기"
    )
  )


## 수질 자료와 기상자료 합치기
총량측정망_기상 <- 기상자료 %>%
  left_join(총량측정망 %>% select(-c(평균기온:누적강수_5일)), by = "일자") %>%
  mutate(연도 = year(일자))



#####  데이터 분석  ############################################################

## 월별 강수량 합계
강수량_월별 <- 기상자료 %>%
  select(일자, 일강수량) %>%
  mutate(
    연도 = year(일자),
    월 = month(일자)
  ) %>%
  # 연도별 월강수량 합계
  group_by(연도, 월) %>%
  summarise(월강수량 = sum(일강수량, na.rm = TRUE), .groups = "drop") %>%
  # 월별 강수량 평균
  group_by(월) %>%
  summarise(월강수량 = mean(월강수량, na.rm = TRUE), .groups = "drop")

## 월별 달성률
달성률_월별 <- 총량측정망 %>%
  tabyl(월, 달성여부) %>%
  mutate(총계 = 달성 + 초과, .after = 1) %>%
  mutate(달성률 = round(달성 / 총계 * 100, 2)) %>%
  left_join(강수량_월별, by = "월")

## 계절별 달성률
달성률_계절별 <- 총량측정망 %>%
  tabyl(계절, 달성여부) %>%
  mutate(총계 = 달성 + 초과, .after = 1) %>%
  mutate(달성률 = round(달성 / 총계 * 100, 2))

## 유황구간별 달성률
달성률_유황구간별 <- 총량측정망 %>%
  tabyl(유황구간, 달성여부) %>%
  mutate(총계 = 달성 + 초과, .after = 1) %>%
  mutate(달성률 = round(달성 / 총계 * 100, 2))


## 유역내 측정지점 별 평균
수질측정망_평균 <- 수질측정망 %>%
  select(-c(일자, 월, 계절)) %>%
  filter(연도 > 2020) %>%
  group_by(총량지점명, 연도) %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)), .groups = "drop")


## 유역내 측정지점 별 달성률
수질측정망_달성률 <- 수질측정망 %>%
  tabyl(총량지점명, 달성여부) %>%
  mutate(총계 = 달성 + 초과, .after = 1) %>%
  mutate(달성률 = round(달성 / 총계 * 100, 2))


## 데이터 분석결과 내보내기
write_xlsx(
  list(
    "수질현황" = 총량측정망,
    "달성률_월별" = 달성률_월별,
    "달성률_계절별" = 달성률_계절별,
    "달성률_유황구간별" = 달성률_유황구간별
  ),
  path = str_c("수질분석/Output/수질검토_", 단위유역, ".xlsx")
)


######  그래프 작성  ###########################################################

## 유량 / 수질(T-P) 그래프  ----------------------------------------------------
original_plot <- 총량측정망 %>%
  ggplot() +
  geom_bar(aes(x = 유량백분율, y = 유량 / 1000, color = "steelblue2"), 
           stat = "identity", width = 0.3, fill = "steelblue2") +
  geom_point(aes(x = 유량백분율, y = TP, color = "black", fill = 계절), 
             shape = 21, alpha = 0.6, size = 3) +
  geom_vline(xintercept = c(0, 10, 40, 60, 90, 100), 
             linetype = "dashed", linewidth = 0.5) +
  geom_hline(aes(yintercept = 목표수질_TP, color = "red"), 
             linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(
    name = "T-P(mg/L)",
    breaks = seq(0, 1.0, 0.15),
    sec.axis = sec_axis(~ . * 1000, name = "유량(㎥/s)", labels = scales::comma)
  ) +
  scale_x_continuous(
    name = "유량 백분율(%)",
    breaks = seq(0, 100, 10), # x축 표시 형식 설정
    labels = paste0(c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))
  ) +
  annotate(geom = "text", x = 5, y = 0.65, label = "홍수기") +
  annotate(geom = "text", x = 25, y = 0.65, label = "풍수기") +
  annotate(geom = "text", x = 50, y = 0.65, label = "평수기") +
  annotate(geom = "text", x = 75, y = 0.65, label = "저수기") +
  annotate(geom = "text", x = 95, y = 0.65, label = "갈수기") +
  theme_test(base_family = "notosanskr", base_size = 14) +
  scale_fill_discrete(name = "계절") + # 범례 제목
  scale_color_manual(
    name = NULL, values = c("black", "red", "steelblue2"),
    labels = c("수질", "목표수질", "유량")
  ) +
  theme(
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

# 상단 그래프
p1 <- 총량측정망 %>%
  ggplot() +
  geom_bar(aes(x = 유량백분율, y = 유량 / 1000, color = "steelblue2"), 
           stat = "identity", width = 0.3, fill = "steelblue2") +
  geom_point(aes(x = 유량백분율, y = TP, color = "black", fill = 계절), 
             shape = 21, alpha = 0.6, size = 3) +
  geom_vline(xintercept = c(0, 10, 40, 60, 90, 100), 
             linetype = "dashed", linewidth = 0.5) +
  geom_hline(aes(yintercept = 목표수질_TP, color = "red"), 
             linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(
    name = "",
    breaks = seq(0, 1.0, 0.15),
    sec.axis = sec_axis(~ . * 1000, name = "", labels = scales::comma)
  ) +
  scale_x_continuous(
    name = NULL,
    breaks = seq(0, 100, 10), # x축 표시 형식 설정
    labels = NULL
  ) +
  coord_cartesian(ylim = c(0.23, 0.8)) +  # 데이터 자르지 않고 보이는 영역만 조정
  annotate(geom = "text", x = 5, y = 0.65, label = "홍수기") +
  annotate(geom = "text", x = 25, y = 0.65, label = "풍수기") +
  annotate(geom = "text", x = 50, y = 0.65, label = "평수기") +
  annotate(geom = "text", x = 75, y = 0.65, label = "저수기") +
  annotate(geom = "text", x = 95, y = 0.65, label = "갈수기") +
  theme_test(base_family = "notosanskr", base_size = 14) +
  scale_fill_discrete(name = "계절") + # 범례 제목
  scale_color_manual(
    name = NULL, values = c("black", "red", "steelblue2"),
    labels = c("수질", "목표수질", "유량")
  ) +
  theme(
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5),
    axis.ticks.x = element_blank() # x축 눈금 제거
  )

# 하단 그래프
p2 <- 총량측정망 %>%
  ggplot() +
  geom_bar(aes(x = 유량백분율, y = 유량 / 1000, color = "steelblue2"), 
           stat = "identity", width = 0.3, fill = "steelblue2") +
  geom_point(aes(x = 유량백분율, y = TP, color = "black", fill = 계절), 
             shape = 21, alpha = 0.6, size = 3) +
  geom_vline(xintercept = c(0, 10, 40, 60, 90, 100), 
             linetype = "dashed", linewidth = 0.5) +
  geom_hline(aes(yintercept = 목표수질_TP, color = "red"), 
             linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(
    name = "T-P(mg/L)",
    sec.axis = sec_axis(~ . * 1000, name = "유량(㎥/s)", labels = scales::comma)
  ) +
  scale_x_continuous(
    name = "유량 백분율(%)",
    breaks = seq(0, 100, 10), # x축 표시 형식 설정
    labels = paste0(c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))
  ) +
  coord_cartesian(ylim = c(0, 0.17)) +  # 데이터 자르지 않고 보이는 영역만 조정
  theme_test(base_family = "notosanskr", base_size = 14) +
  scale_color_manual(
    name = NULL, values = c("black", "red", "steelblue2"),
    labels = c("수질", "목표수질", "유량")
  ) +
  theme(
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "none" # 범례 제거
  )

# 두 그래프를 위아래로 배치
final_plot <- p1 / p2 + 
  plot_layout(heights = c(1, 1.4))  # 상하 비율 조정

final_plot2 <- plot_grid(
  p1, p2,
  ncol = 1, # 1열로 배치
  rel_heights = c(1, 1.4) # 메인 그래프와 확대 그래프의 높이 비율
)


## 그래프 pdf 출력
pdf("E:/Coding/TMDL/수질분석/Output/Plot/그래프_주천A_TP(8.4x5).pdf",
  width = 8.4, height = 5
)

original_plot
final_plot

dev.off()





## 간이 LDC 그래프 -------------------------------------------------------------
총량측정망 %>%
  ggplot() +
  geom_vline(xintercept = c(0, 10, 40, 60, 90, 100), linetype = "dashed", size = 0.5) +
  geom_point(aes(
    x = 유량백분율, y = 측정부하량_TP, fill = 계절, # 실측 부하량 포인트
    color = "black"
  ), shape = 21, alpha = 0.6, size = 3) +
  geom_line(aes(x = 유량백분율, y = 목표부하량_TP, color = "red"),
    group = 1, # 목표 부하량 지속 곡선(LDC)
    linetype = 2, size = 1.1
  ) +
  scale_y_log10(name = "T-P 부하량 (kg/d)") + # y축 log 스케일 변환
  scale_x_continuous(
    name = "유량 백분율(%)",
    breaks = seq(0, 100, 10), # x축 표시 형식 설정
    labels = paste0(c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))
  ) +
  annotate(geom = "text", x = 5, y = 0.1, label = "홍수기") +
  annotate(geom = "text", x = 25, y = 0.1, label = "풍수기") +
  annotate(geom = "text", x = 50, y = 0.1, label = "평수기") +
  annotate(geom = "text", x = 75, y = 0.1, label = "저수기") +
  annotate(geom = "text", x = 95, y = 0.1, label = "갈수기") +
  theme_test(base_family = "notosanskr", base_size = 14) +
  scale_fill_discrete(name = "계절") + # 범례 제목
  scale_color_manual(
    name = NULL, values = c("black", "red"),
    labels = c("실측부하량", "LDC")
  ) +
  guides(color = guide_legend(override.aes = list(shape = c(21, NA), linetype = c(NA, 2)))) +
  theme(
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )


## 주천A 유역 내 측정 지점 별 박스플롯  ----------------------------------------
수질측정망 %>%
  ggplot(aes(x = 총량지점명, y = TP)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절),
    shape = 21, alpha = 0.6, size = 2.3,
    position = position_jitter(0.2)
  ) +
  scale_x_discrete(limits = c("주천A1", "주천A2", "주천A")) +
  scale_y_log10(name = "T-P(mg/L)") +
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

## 강수 / 수질(T-P) 그래프  ----------------------------------------------------
총량측정망_기상 %>%
  filter(연도 > 2020, 연도 < 2024) %>%
  ggplot() +
  geom_bar(aes(x = 일자, y = 일강수량 / 300, color = "steelblue2"),
    stat = "identity", width = 0.3, fill = "steelblue2"
  ) +
  geom_point(aes(x = 일자, y = TP, color = "black", fill = 계절),
    shape = 21, alpha = 0.6, size = 2.5
  ) +
  geom_hline(aes(yintercept = 목표수질_TP, color = "red"),
    linetype = "dashed", linewidth = 0.7
  ) +
  scale_y_continuous(
    name = "T-P(mg/L)",
    sec.axis = sec_axis(~ . * 300, name = "강수량(mm)", labels = scales::comma)
  ) +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  scale_fill_discrete(name = "계절", na.translate = F) + # 범례 제목(NA값 제외)
  scale_color_manual(
    name = NULL, values = c("black", "red", "steelblue2"),
    labels = c("수질", "목표수질", "강수량")
  ) +
  theme(
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )
