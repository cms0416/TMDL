#####  □ 라이브러리 로드  ######################################################
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


#####  □ 함수 정의  ############################################################
## 월별 계절 정의 함수 로드
source("Script_R/Function/func_season.R")


#####  □ 데이터 정리  ##########################################################

## 목표수질 파일 불러오기
목표수질 <- read_excel("수질분석/목표수질.xlsx")

## 검토 조건 설정
단위유역 <- "주천A"
기상대 <- "안흥"
시작연도 <- 2015
종료연도 <- 2024
목표수질_TP <- 목표수질 %>%
  filter(총량지점명 == 단위유역) %>%
  pull(TP_목표수질)


## 기상자료(일단위)
기상자료 <- read_excel("수질분석/기상자료/기상자료_강수량_기온.xlsx") %>%
  filter(str_detect(지점명, 기상대) == TRUE) %>%
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

## 기상자료(시단위)
기상자료_시단위 <- read_excel("수질분석/기상자료/기상자료_강수량_기온_시단위.xlsx") %>%
  filter(str_detect(지점명, 기상대) == TRUE) %>%
  select(-c(지점, 지점명)) %>%
  set_names(c("일시", "기온", "강수량")) %>%
  mutate(
    날짜 = as.Date(일시),
    연도 = year(일시),
    월 = month(일시),
    # 고강도 강우(강한비) 확인 : 시간당 강수량 20mm이상(한강홍수통제소 기준)
    고강도강우 = 강수량 >= 20,
    매우고강도강우 = 강수량 >= 30
  )

## 수질측정망 자료 정리(단위유역 말단 측정망 외에 소유역까지 포함해서 정리)
수질측정망 <- read_excel("수질분석/총량측정망_소유역포함_2007_2024.xlsx") %>%
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
  mutate(
    측정부하량_TP = 유량 * TP * 86.4,
    목표부하량_TP = 유량 * 목표수질_TP * 86.4
  ) %>%
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
  ) %>%
  left_join(기상자료, by = "일자")


## 수질 자료와 기상자료 합치기
총량측정망_기상 <- 기상자료 %>%
  left_join(수질측정망 %>%
              filter(총량지점명 == 단위유역),
            by = "일자") %>%
  mutate(
    연도 = year(일자),
    월 = month(일자)
  ) %>%
  season()



#####  □ 데이터 분석  ##########################################################

### 강수량 데이터 정리 ---------------------------------------------------------
## 연월별 강수량 합계
강수량_연월별_합계 <- 기상자료 %>%
  mutate(
    연도 = year(일자),
    월 = month(일자)) %>%
  # 연도별 월강수량 합계
  group_by(연도, 월) %>%
  summarise(월강수량 = sum(일강수량, na.rm=TRUE), .groups = "drop") %>%
  group_by(연도) %>%
  group_modify(~ .x %>% adorn_totals(where = "row", name = "소계"))

## 월별 강수량 평균
강수량_월별_평균 <- 강수량_연월별_합계 %>%
  filter(월 != "소계") %>%
  group_by(월) %>%
  summarise(월강수량 = round(mean(월강수량, na.rm = TRUE), 1), .groups = "drop") %>%
  mutate(월 = as.numeric(월)) %>%
  arrange(월)

# ## 연도별 강수량 합계
# 강수량_연도별_합계 <- 기상자료 %>%
#   select(일자, 일강수량) %>%
#   mutate(
#     연도 = year(일자)) %>%
#   # 연도별 월강수량 합계
#   group_by(연도) %>%
#   summarise(연강수량 = sum(일강수량, na.rm=TRUE), .groups = "drop")

# ## 연도 및 계절별 강수량 합계
# 강수량_연도_계절별_합계 <- 기상자료 %>%
#   select(일자, 일강수량) %>%
#   mutate(
#     연도 = year(일자),
#     월 = month(일자),
#     계절 = ifelse(월 >= 3 & 월 <= 5, "봄",
#                 ifelse(월 >= 6 & 월 <= 8, "여름",
#                        ifelse(월 >= 9 & 월 <= 11, "가을", "겨울")
#                 )
#     ),
#     # '계절' 요인형(factor)으로 변환
#     계절 = factor(계절, levels = c("봄", "여름", "가을", "겨울"))) %>%
#   # 연도별 월강수량 합계
#   group_by(연도, 계절) %>%
#   summarise(월강수량 = sum(일강수량, na.rm=TRUE), .groups = "drop") %>%
#   group_by(연도) %>%
#   group_modify(~ .x %>% adorn_totals(where = "row", name = "소계"))



### 강우강도 관련 검토 ---------------------------------------------------------
## 일별 최대 시간강우량 및 고강도 여부
기상지표_일별 <- 기상자료_시단위 %>%
  filter(!all(is.na(강수량))) %>%
  group_by(날짜) %>%
  summarise(
    일강수량 = sum(강수량, na.rm = TRUE),
    최대시간강우량 = max(강수량, na.rm = TRUE),
    고강도강우_발생여부 = any(고강도강우, na.rm = TRUE),
    매우고강도강우_발생여부 = any(매우고강도강우, na.rm = TRUE),
    평균기온 = round(mean(기온, na.rm = TRUE), 1),
    연도 = first(연도),
    강우일 = sum(강수량, na.rm = TRUE) > 0,
    .groups = "drop"
  )

##연도별 기상 지표 요약
기상지표_연도별 <- 기상지표_일별 %>%
  group_by(연도) %>%
  summarise(
    강수량_합계 = sum(일강수량, na.rm = TRUE),
    강수량_일평균 = round(mean(일강수량, na.rm = TRUE), 1),
    강우일수 = sum(강우일, na.rm = TRUE),
    고강도일수 = sum(고강도강우_발생여부, na.rm = TRUE),
    # 매우고강도일수 = sum(매우고강도강우_발생여부, na.rm = TRUE),
    평균기온 = round(mean(평균기온, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  mutate(
    고강도비율 = round(고강도일수 / 강우일수 * 100, 1)
    # 매우고강도비율 = round(매우고강도일수 / 강우일수 * 100, 1)
  ) %>% 
  relocate(평균기온, .after = last_col())



### 달성률 데이터 정리 ---------------------------------------------------------
## 유황구간별 달성률
달성률_유황구간별 <- 총량측정망 %>%
  tabyl(유황구간, 달성여부) %>%
  mutate(총계 = 달성 + 초과, .after = 1) %>%
  mutate(달성률 = round(달성 / 총계 * 100, 1))

## 계절별 달성률
달성률_계절별 <- 총량측정망 %>%
  tabyl(계절, 달성여부) %>%
  mutate(총계 = 달성 + 초과, .after = 1) %>%
  mutate(달성률 = round(달성 / 총계 * 100, 1))

## 월별 달성률
달성률_월별 <- 총량측정망 %>%
  tabyl(월, 달성여부) %>%
  mutate(총계 = 달성 + 초과, .after = 1) %>%
  mutate(달성률 = round(달성 / 총계 * 100, 1)) %>%
  left_join(강수량_월별_평균, by = "월")

## 연도 별 달성률 및 강우강도 지표
달성률_강우강도_연도별 <- 총량측정망 %>%
  group_by(연도) %>%
  summarise(
    TP_평균 = round(mean(TP, na.rm = TRUE), 3),
    유량_합계 = sum(유량, na.rm = TRUE),
    초과 = sum(달성여부 == "초과"),
    달성 = sum(달성여부 == "달성"),
    .groups = "drop"
  ) %>%
  mutate(
    총계 = 달성 + 초과,
    달성률 = round(달성 / 총계 * 100, 1)
  ) %>%
  relocate(c(TP_평균, 유량_합계), .after = 달성률) %>%
  left_join(기상지표_연도별, by = "연도")

# ## 연도 및 계절별 달성률
# 달성률_연도_계절별 <- 총량측정망 %>%
#   group_by(연도, 계절) %>%
#   summarise(
#     초과 = sum(달성여부 == "초과"),
#     달성 = sum(달성여부 == "달성"),
#     .groups = "drop"
#   ) %>%
#   group_by(연도) %>%
#   group_modify(~ .x %>% adorn_totals(where = "row", name = "소계")) %>%
#   mutate(
#     총계 = 달성 + 초과,
#     달성률 = round(달성 / 총계 * 100, 1)
#   ) %>%
#   left_join(강수량_연도_계절별_합계, by = c("연도", "계절"))


### 유역내 측정지점 별 정리(소유역) --------------------------------------------
## 유역내 측정지점 별 연평균
수질측정망_평균 <- 수질측정망 %>%
  select(-c(일자, 월, 계절)) %>%
  filter(연도 > 2020) %>%
  group_by(총량지점명, 연도) %>%
  summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)), .groups = "drop")

## 유역내 측정지점 별 달성률
수질측정망_달성률 <- 수질측정망 %>%
  tabyl(총량지점명, 달성여부) %>%
  mutate(총계 = 달성 + 초과, .after = 1) %>%
  mutate(달성률 = round(달성 / 총계 * 100, 1))


### 데이터 분석결과 내보내기 ---------------------------------------------------
write_xlsx(
  list(
    "수질현황" = 총량측정망,
    "달성률_유황구간별" = 달성률_유황구간별,
    "달성률_계절별" = 달성률_계절별,
    "달성률_월별" = 달성률_월별,
    "달성률_강우강도_연도별" = 달성률_강우강도_연도별
  ),
  path = str_c("수질분석/Output/수질분석_", 단위유역, ".xlsx")
)



##### □ 그래프 작성  ###########################################################

## 강수 / 수질(T-P) 그래프(날짜기준)  ------------------------------------------
그래프_강수수질_기본 <- 총량측정망_기상 %>%
  mutate(일자 = as.Date(일자)) %>% 
  filter(연도 >= 2020) %>%
  ggplot() +
  geom_bar(aes(x = 일자, y = 일강수량 / 80, color = "steelblue2"),
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
    breaks = seq(0, 1.6, 0.4),
    sec.axis = sec_axis(~ . * 80, name = "강수량(mm)", labels = scales::comma)
  ) +
  scale_x_date(
    name = "연도",
    date_breaks = "1 year", 
    date_labels = "%Y"
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

그래프_강수수질_기본

## 강수 / 수질(T-P) 그래프 분할  -----------------------------------------------
###### ㄴ상단 그래프 #####
그래프_강수수질_상 <- 총량측정망_기상 %>%
  mutate(일자 = as.Date(일자)) %>% 
  filter(연도 >= 2020) %>%
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
    name = NULL,
    breaks = seq(0, 1.6, 0.4),
    sec.axis = sec_axis(~ . * 300, name = NULL, 
                        breaks = seq(0, 100, 20),
                        labels = scales::comma)
  ) +
  coord_cartesian(ylim = c(0.6, 1.65)) + # 데이터 자르지 않고 보이는 영역만 조정
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
    legend.margin = margin(5, 5, 5, 5),
    axis.ticks.x = element_blank(), # x축 눈금 제거
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(10, 10, 0, 10)  # 위, 오른쪽, 아래, 왼쪽 여백
  )

###### ㄴ하단 그래프 #####
그래프_강수수질_하 <- 총량측정망_기상 %>%
  mutate(일자 = as.Date(일자)) %>% 
  filter(연도 >= 2020) %>%
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
    breaks = seq(0, 1.6, 0.1),
    sec.axis = sec_axis(~ . * 300, name = "강수량(mm)", labels = scales::comma)
  ) +
  scale_x_date(
    name = "연도",
    date_breaks = "1 year", 
    date_labels = "%Y"
  ) +
  coord_cartesian(ylim = c(0, 0.4)) + # 데이터 자르지 않고 보이는 영역만 조정
  theme_bw(base_family = "notosanskr", base_size = 14) +
  scale_fill_discrete(name = "계절", na.translate = F) + # 범례 제목(NA값 제외)
  scale_color_manual(
    name = NULL, values = c("black", "red", "steelblue2"),
    labels = c("수질", "목표수질", "강수량")
  ) +
  theme(
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "none",  # 범례 제거
    plot.margin = margin(3, 10, 10, 10)   # 위, 오른쪽, 아래, 왼쪽 여백
  )

# 두 그래프 위아래로 붙이기
그래프_강수수질 <- 그래프_강수수질_상 / 그래프_강수수질_하 +
  plot_layout(heights = c(1, 5))  # 위:아래 비율

그래프_강수수질


## 유량 / 수질(T-P) 그래프  ----------------------------------------------------
그래프_유량수질_기본 <- 총량측정망 %>%
  ggplot() +
  geom_bar(aes(x = 유량백분율, y = 유량 / 250, color = "steelblue2"),
           stat = "identity", width = 0.3, fill = "steelblue2"
  ) +
  geom_vline(
    xintercept = c(0, 10, 40, 60, 90, 100),
    linetype = "dashed", linewidth = 0.5
  ) +
  geom_point(aes(x = 유량백분율, y = TP, color = "black", fill = 계절),
             shape = 21, alpha = 0.6, size = 2.5
  ) +
  geom_hline(aes(yintercept = 목표수질_TP, color = "red"),
             linetype = "dashed", linewidth = 0.7
  ) +
  scale_y_continuous(
    name = "T-P(mg/L)",
    breaks = seq(0, 0.8, 0.2),
    sec.axis = sec_axis(~ . * 250, name = "유량(㎥/s)", labels = scales::comma)
  ) +
  scale_x_continuous(
    name = "유량 백분율(%)",
    breaks = seq(0, 100, 10),  # x축 표시 형식 설정
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
    legend.position = "top",  # 범례 위치
    legend.direction = "horizontal",  # 범례 방향
    legend.box = "horizontal",  # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"),  # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

그래프_유량수질_기본

## 유량 / 수질(T-P) 그래프 분할  -----------------------------------------------
###### ㄴ상단 그래프 #####
그래프_유량수질_상 <- 총량측정망 %>%
  ggplot() +
  geom_bar(aes(x = 유량백분율, y = 유량 / 300, color = "steelblue2"),
           stat = "identity", width = 0.3, fill = "steelblue2"
  ) +
  geom_vline(
    xintercept = c(0, 10, 40, 60, 90, 100),
    linetype = "dashed", linewidth = 0.5
  ) +
  geom_point(aes(x = 유량백분율, y = TP, color = "black", fill = 계절),
             shape = 21, alpha = 0.6, size = 3
  ) +
  geom_hline(aes(yintercept = 목표수질_TP, color = "red"),
             linetype = "dashed", linewidth = 0.7
  ) +
  scale_y_continuous(
    name = NULL,
    breaks = seq(0, 1.6, 0.4),
    sec.axis = sec_axis(~ . * 300, name = NULL, 
                        breaks = seq(0, 400, 100),
                        labels = scales::comma)
  ) +
  coord_cartesian(ylim = c(0.65, 1.6)) +  # 데이터 자르지 않고 보이는 영역만 조정
  annotate(geom = "text", x = 5,  y = 1.2, label = "홍수기") +
  annotate(geom = "text", x = 25, y = 1.2, label = "풍수기") +
  annotate(geom = "text", x = 50, y = 1.2, label = "평수기") +
  annotate(geom = "text", x = 75, y = 1.2, label = "저수기") +
  annotate(geom = "text", x = 95, y = 1.2, label = "갈수기") +
  theme_test(base_family = "notosanskr", base_size = 14) +
  scale_fill_discrete(name = "계절") +  # 범례 제목
  scale_color_manual(
    name = NULL, values = c("black", "red", "steelblue2"),
    labels = c("수질", "목표수질", "유량")
  ) +
  theme(
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top",  # 범례 위치
    legend.direction = "horizontal",  # 범례 방향
    legend.box = "horizontal",  # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5),
    axis.ticks.x = element_blank(),  # x축 눈금 제거
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(10, 10, 0, 10)  # 위, 오른쪽, 아래, 왼쪽 여백
  )

###### ㄴ하단 그래프 #####
그래프_유량수질_하 <- 총량측정망 %>%
  ggplot() +
  geom_bar(aes(x = 유량백분율, y = 유량 / 250, color = "steelblue2"),
           stat = "identity", width = 0.3, fill = "steelblue2"
  ) +
  geom_vline(
    xintercept = c(0, 10, 40, 60, 90, 100),
    linetype = "dashed", linewidth = 0.5
  ) +
  geom_point(aes(x = 유량백분율, y = TP, color = "black", fill = 계절),
             shape = 21, alpha = 0.6, size = 3
  ) +
  geom_hline(aes(yintercept = 목표수질_TP, color = "red"),
             linetype = "dashed", linewidth = 0.7
  ) +
  scale_y_continuous(
    name = "T-P(mg/L)",
    sec.axis = sec_axis(~ . * 250, name = "유량(㎥/s)", labels = scales::comma)
  ) +
  scale_x_continuous(
    name = "유량 백분율(%)",
    breaks = seq(0, 100, 10), # x축 표시 형식 설정
    labels = paste0(c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))
  ) +
  coord_cartesian(ylim = c(0, 0.4)) +  # 데이터 자르지 않고 보이는 영역만 조정
  theme_test(base_family = "notosanskr", base_size = 14) +
  scale_color_manual(
    name = NULL, values = c("black", "red", "steelblue2"),
    labels = c("수질", "목표수질", "유량")
  ) +
  theme(
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "none",  # 범례 제거
    plot.margin = margin(3, 10, 10, 10)   # 위, 오른쪽, 아래, 왼쪽 여백
  )

# 두 그래프를 위아래로 배치
그래프_유량수질 <- 그래프_유량수질_상 / 그래프_유량수질_하 +
  plot_layout(heights = c(1, 5))  # 위:아래 비율

그래프_유량수질




## 연도 별 박스플롯  ----------------------------------------------
그래프_연도별박스플롯 <- 총량측정망 %>%
  ggplot(aes(x = factor(연도), y = TP)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(color = "black", fill = 계절),
              shape = 21, alpha = 0.6, size = 2.3,
              position = position_jitter(0.2)
  ) +
  geom_hline(aes(yintercept = 목표수질_TP, color = "red"),
             linetype = "dashed", linewidth = 0.7
  ) +
  scale_x_discrete(name = "연도") +
  scale_y_log10(name = "T-P(mg/L)") +
  scale_color_manual(
    name = NULL, values = c("black", "red"),
    labels = c("수질", "목표수질")
  ) +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

그래프_연도별박스플롯


## ★ 그래프 pdf 출력  ----------------------------------------------------------
pdf("E:/Coding/TMDL/수질분석/Output/Plot/그래프_주천A_TP(8.4x5).pdf",
    width = 8.4, height = 5
)

그래프_유량수질
그래프_강수수질
그래프_연도별박스플롯

dev.off()


#####________________________________######

## 강수 / 수질(T-P) 그래프(강우크기순서)  --------------------------------------
총량측정망_기상 %>%
  arrange(desc(일강수량)) %>%
  rowid_to_column(var = "강우크기순서") %>%
  filter(연도 >= 2015, 연도 <= 2024) %>%
  ggplot() +
  geom_bar(aes(x = 강우크기순서, y = 일강수량 / 80, color = "steelblue2"),
           stat = "identity", width = 0.3, fill = "steelblue2"
  ) +
  geom_point(aes(x = 강우크기순서, y = TP, color = "black", fill = 계절),
             shape = 21, alpha = 0.6, size = 2.5
  ) +
  geom_hline(aes(yintercept = 목표수질_TP, color = "red"),
             linetype = "dashed", linewidth = 0.7
  ) +
  scale_y_continuous(
    name = "T-P(mg/L)",
    breaks = seq(0,1.6, 0.4),
    sec.axis = sec_axis(~ . * 80, name = "강수량(mm)", labels = scales::comma)
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

## 유량 / 강수량 그래프  ----------------------------------------------------
총량측정망_기상 %>%
  filter(연도 >= 2020) %>%
  ggplot() +
  geom_bar(aes(x = 일자, y = 일강수량 * 3.3, color = "steelblue2"),
           stat = "identity", width = 0.3, fill = "steelblue2"
  ) +
  geom_point(aes(x = 일자, y = 유량, color = "black", fill = 계절),
             shape = 21, alpha = 0.6, size = 3
  ) +
  scale_y_continuous(
    name = "유량(㎥/s)",
    breaks = seq(0, 400, 50),
    sec.axis = sec_axis(~ . / 3.3, name = "강수량(mm)", labels = scales::comma)
  ) +
  scale_x_continuous(name = "일자") +
  theme_test(base_family = "notosanskr", base_size = 14) +
  scale_fill_discrete(name = "계절") + # 범례 제목
  scale_color_manual(
    name = NULL, values = c("black", "steelblue2"),
    labels = c("유량", "강수량")
  ) +
  theme(
    panel.border = element_rect(linewidth = 0.5, fill = NA),
    legend.position = "top", # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )


## 유역 내 측정 지점 별 박스플롯  ----------------------------------------------
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
