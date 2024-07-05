#####  라이브러리 로드  ########################################################
# 데이터 정리
library(tidyverse)
library(magrittr)
library(janitor)
library(readxl)
library(writexl)
library(zoo)

# 데이터 분석
library(lmtest)
library(car)

# 그래프 작성
library(scales)
library(ggthemes)
library(showtext)

## 그래프용 Noto Sans KR 폰트 추가
font_add_google("Noto Sans KR", "notosanskr")
showtext_auto()

#####  데이터 정리  ############################################################

## 기상자료
기상자료 <- read_excel("수질 분석/섬강A/횡성기상대_강수량_기온_2014_2023.xlsx") %>%
  select(일자:일강수량) %>% 
  # 3일 누적 강수량 산정
  mutate(
    누적강수_3일 = rollapply(일강수량, width = 3, FUN = sum, align = "right", 
                        fill = NA, na.rm = TRUE),
    누적강수_4일 = rollapply(일강수량, width = 4, FUN = sum, align = "right", 
                        fill = NA, na.rm = TRUE),
    누적강수_5일 = rollapply(일강수량, width = 5, FUN = sum, align = "right", 
                        fill = NA, na.rm = TRUE)
  )

## 수질측정망 자료 정리
obs <- read_excel("수질 분석/섬강A/섬강A_수질측정망_2014_2023.xlsx") %>%
  # BOD열에서 결측치 제거(결측치가 아닌 값들만 필터)
  filter(!is.na(BOD)) %>%
  # filter(!(TP == 2.108)) %>%
  mutate(
    연도 = year(일자),
    월 = month(일자),
    계절 = ifelse(월 >= 3 & 월 <= 5, "봄",
      ifelse(월 >= 6 & 월 <= 8, "여름",
        ifelse(월 >= 9 & 월 <= 11, "가을", "겨울")
      )
    )
  ) %>%
  # '계절' 요인형(factor)으로 변환 후 계절 순서로 요인 순서 변경
  mutate(계절 = factor(계절, levels = c("봄", "여름", "가을", "겨울")))
# filter(계절 %in% c("여름", "가을"), !측정소명 %in% c("계천2", "금계천2"))


## 총량측정망(섬강A) 자료 정리
섬강A <- read_excel("수질 분석/총량측정망_강원_2007_2024.xlsx") %>% 
  filter(총량지점명 == "섬강A", 연도 >= 2014) %>% 
  # BOD열에서 결측치 제거(결측치가 아닌 값들만 필터)
  filter(!is.na(BOD)) %>%
  # filter(!(TP == 2.108)) %>%
  mutate(
    연도 = year(일자),
    월 = month(일자),
    계절 = ifelse(월 >= 3 & 월 <= 5, "봄",
                ifelse(월 >= 6 & 월 <= 8, "여름",
                       ifelse(월 >= 9 & 월 <= 11, "가을", "겨울")
                )
    )
  ) %>%
  # '계절' 요인형(factor)으로 변환 후 계절 순서로 요인 순서 변경
  mutate(계절 = factor(계절, levels = c("봄", "여름", "가을", "겨울"))) %>% 
  mutate(
    목표수질_TP = 0.035,
    초과여부 = ifelse(TP > 목표수질_TP, "초과", "달성")
  ) %>%
  left_join(기상자료, by = "일자") %>%
  arrange(desc(유량)) %>%
  rowid_to_column(var = "유량크기순서")


## 섬강A 자료와 기상자료 합치기
섬강A_기상 <- 기상자료 %>%
  left_join(섬강A %>% select(-c(평균기온:누적강수_5일)), by = "일자")


write_xlsx(섬강A, path = "수질 분석/Output/섬강A.xlsx")


#####  데이터 분석  ############################################################

## 계절별 초과율
초과율_계절별 <- 섬강A %>% 
  select(계절, 초과여부) %>% 
  group_by(계절) %>% 
  tally(name = "총측정횟수") %>% 
  left_join(
    섬강A %>% 
      select(계절, 초과여부) %>% 
      filter(초과여부 == "초과") %>% 
      group_by(계절) %>% 
      tally(name = "초과횟수"),
    by = "계절"
  ) %>% 
  mutate(초과율 = 초과횟수 / 총측정횟수) %>% 
  adorn_pct_formatting("초과율", digits = 2, rounding = "half up", affix_sign = TRUE)

## 월별 초과율
초과율_월별 <- 섬강A %>% 
  select(월, 초과여부) %>% 
  group_by(월) %>% 
  tally(name = "총측정횟수") %>% 
  left_join(
    섬강A %>% 
      select(월, 초과여부) %>% 
      filter(초과여부 == "초과") %>% 
      group_by(월) %>% 
      tally(name = "초과횟수"),
    by = "월"
  ) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(초과율 = 초과횟수 / 총측정횟수) %>% 
  adorn_pct_formatting("초과율", digits = 2, rounding = "half up", affix_sign = TRUE)  



#####  통계 분석  ##############################################################

## 데이터 정리
통계분석 <- 섬강A %>%
  select(-c(유량크기순서:일자, 연도:초과여부)) %>% 
  relocate(TP, .before = 1)

# 변수 간 상관관계 분석
cor_matrix <- 통계분석 %>%
  cor(use = "complete.obs")

# 상관관계 시각화
ggcorr(cor_matrix, label = TRUE, label_round = 2, label_alpha = TRUE)

# 회귀분석
model <- lm(TP ~ BOD + 유량 + TOC + 수온 + pH + EC + DO + COD + SS + TN + 평균기온 + 일강수량 + 누적강수_3일 + 누적강수_4일 + 누적강수_5일, data = 통계분석)

# 회귀분석 결과 요약
summary(model)

# 회귀분석 결과를 정리
tidy(model)

# 잔차 분석
par(mfrow = c(2, 2))
plot(model)


#_______________________________________________________________________________

## 단조 관계 가정
pairs(상관분석)

## 스피어만 상관 분석
library(psych)

# r-value
상관분석결과_r <- corr.test(상관분석,
  use = "complete",
  method = "spearman",
  adjust = "none"
)$r 

# p-value
상관분석결과_p <- corr.test(상관분석,
                      use = "complete",
                      method = "spearman",
                      adjust = "none"
)$p

######  그래프 작성  ###########################################################

pdf("E:/Coding/TMDL/수질 분석/Output/Plot/섬강A.pdf",
  width = 8.1, height = 5.9
)

## 유량 / 수질(T-P) 그래프
섬강A %>%
  ggplot() +
  geom_bar(aes(x = 유량크기순서, y = 유량 / 1000, color = "steelblue2"), stat = "identity", width = 0.3, fill = "steelblue2") +
  geom_point(aes(x = 유량크기순서, y = TP, color = "black", fill = 계절),
    shape = 21, alpha = 0.6, size = 3
  ) +
  geom_hline(aes(yintercept = 0.035, color = "red"), linetype = "dashed", size = 0.7) +
  scale_y_continuous(
    name = "T-P(mg/L)",
    sec.axis = sec_axis(~ . * 1000, name = "유량", labels = scales::comma)
  ) +
  theme_bw(base_family = "notosanskr", base_size = 14) +
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

## 섬강A 유역 내 측정 지점 별 박스플롯
obs %>%
  filter(!측정소명 %in% c("계천2", "금계천2")) %>%
  ggplot(aes(x = 측정소명, y = TP)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절),
    shape = 21, alpha = 0.6, size = 2.3,
    position = position_jitter(0.2)
  ) +
  scale_x_discrete(limits = c("전천", "섬강1", "섬강2", "섬강3")) +
  scale_y_log10(name = "T-P(mg/L)") +
  # labs(y = "T-P(mg/L)") +
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

## 강수 / 수질(T-P) 그래프
섬강A_기상 %>% 
  ggplot() +
  geom_bar(aes(x = 일자, y = 일강수량 / 1000, color = "steelblue2"), stat = "identity", width = 0.3, fill = "steelblue2") +
  geom_point(aes(x = 일자, y = TP, color = "black", fill = 계절),
    shape = 21, alpha = 0.6, size = 2.5
  ) +
  geom_hline(aes(yintercept = 0.035, color = "red"), linetype = "dashed", size = 0.7) +
  scale_y_continuous(
    name = "T-P(mg/L)",
    sec.axis = sec_axis(~ . * 1000, name = "강수량(mm)", labels = scales::comma)
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

dev.off()



섬강A_기상 %>%
ggplot(aes(x = 유량, y = TP)) +
  geom_point(color = "sienna") +
  labs(x = "일강수량(mm)", y = "T-P (mg/L)")



# 섬강A %>%
#   filter(측정소명 == "섬강3") %>%
#   ggplot() +
#   geom_point(aes(x = 유량크기순서, y = TP, color = "black", fill = 계절),
#     shape = 21, alpha = 0.6, size = 3
#   ) +
#   geom_hline(aes(yintercept = 0.035, color = "red"), linetype = "dashed", size = 0.7) +
#   scale_y_log10(name = "T-P(mg/L)") +
#   theme_bw(base_family = "notosanskr", base_size = 14) +
#   scale_fill_discrete(name = "계절") + # 범례 제목
#   scale_color_manual(
#     name = NULL, values = c("black", "red"),
#     labels = c("수질", "목표수질")
#   ) +
#   theme(
#     panel.border = element_rect(linewidth = 0.5, fill = NA),
#     legend.position = "top", # 범례 위치
#     legend.direction = "horizontal", # 범례 방향
#     legend.box = "horizontal", # 범례 배치 방향
#     legend.background = element_rect(linewidth = 0.3, fill = "white", color = "black"), # 범례 배경 및 테두리 색
#     legend.margin = margin(5, 5, 5, 5)
#   )

