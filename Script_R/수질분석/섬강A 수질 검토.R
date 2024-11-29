#####  라이브러리 로드  ########################################################
# 데이터 정리
library(tidyverse)
library(magrittr)
library(janitor)
library(readxl)
library(writexl)
library(zoo)

# 데이터 분석
library(psych)  # 상관분석
library(broom)
library(ggcorrplot)
# library(GGally)
# library(lmtest)

# 그래프 작성
library(scales)
library(ggthemes)
library(showtext)

## 그래프용 Noto Sans KR 폰트 추가
font_add_google("Noto Sans KR", "notosanskr")
showtext_auto()

#####  데이터 정리  ############################################################

## 기상자료
기상자료 <- read_excel("수질분석/기상자료/횡성기상대_강수량_기온_2014_2023.xlsx") %>%
  select(-c(지점, 지점명)) %>% 
  set_names(c("일자", "평균기온", "일강수량")) %>%
  # 3일 누적 강수량 산정(zoo::rollapply)
  mutate(
    누적강수_3일 = rollapply(일강수량, width = 3, FUN = sum, align = "right", 
                        fill = NA, na.rm = TRUE),
    누적강수_4일 = rollapply(일강수량, width = 4, FUN = sum, align = "right", 
                        fill = NA, na.rm = TRUE),
    누적강수_5일 = rollapply(일강수량, width = 5, FUN = sum, align = "right", 
                        fill = NA, na.rm = TRUE)
  )

## 수질측정망 자료 정리
obs <- read_excel("수질분석/섬강A/섬강A_수질측정망_2014_2023.xlsx") %>%
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
    ),
    초과여부 = ifelse(TP > 0.035, "초과", "달성")
  ) %>%
  # '계절' 요인형(factor)으로 변환 후 계절 순서로 요인 순서 변경
  mutate(계절 = factor(계절, levels = c("봄", "여름", "가을", "겨울")))
# filter(계절 %in% c("여름", "가을"), !측정소명 %in% c("계천2", "금계천2"))


## 총량측정망(섬강A) 자료 정리
섬강A <- read_excel("수질분석/총량측정망_강원_2007_2024.xlsx") %>% 
  filter(총량지점명 == "섬강A", 연도 >= 2014, 연도 <= 2023) %>% 
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
    초과여부 = ifelse(TP > 목표수질_TP, "초과", "달성"),
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
    ))


## 섬강A 자료와 기상자료 합치기
섬강A_기상 <- 기상자료 %>%
  left_join(섬강A %>% select(-c(평균기온:누적강수_5일)), by = "일자")


write_xlsx(섬강A, path = "수질분석/Output/수질검토_섬강A.xlsx")


#####  데이터 분석  ############################################################

## 월별 강수량 합계
강수량_월별 <- 기상자료 %>% 
  select(일자, 일강수량) %>% 
  mutate(
    연도 = year(일자),
    월 = month(일자)) %>% 
  # 연도별 월강수량 합계
  group_by(연도, 월) %>%
  summarise(월강수량 = sum(일강수량, na.rm=TRUE), .groups = "drop") %>%
  # 월별 강수량 평균
  group_by(월) %>%
  summarise(월강수량 = mean(월강수량, na.rm=TRUE), .groups = "drop")

## 월별 초과율
초과율_월별 <- 섬강A %>%
  tabyl(월, 초과여부) %>% 
  mutate(총계 = 달성 + 초과, .after = 1) %>% 
  mutate(초과율 = round(초과 / 총계 * 100, 2)) %>% 
  left_join(강수량_월별, by = "월")

## 계절별 초과율
초과율_계절별 <- 섬강A %>%
  tabyl(계절, 초과여부) %>% 
  mutate(총계 = 달성 + 초과, .after = 1) %>% 
  mutate(초과율 = round(초과 / 총계 * 100, 2))

## 유황구간별 초과율
초과율_유황구간별 <- 섬강A %>%
  tabyl(유황구간, 초과여부) %>% 
  mutate(총계 = 달성 + 초과, .after = 1) %>% 
  mutate(초과율 = round(초과 / 총계 * 100, 2))


## 유역내 측정지점 별 평균
obs_평균 <- obs %>% 
  select(-c(일자, 연도, 월, 계절)) %>% 
  group_by(측정소명) %>% 
  summarise(across(where(is.numeric), mean))
    

## 유역내 측정지점 별 달성률
obs_달성률 <- obs %>%
  tabyl(측정소명, 초과여부) %>% 
  mutate(총계 = 달성 + 초과, .after = 1) %>% 
  mutate(달성률 = round(달성 / 총계 * 100, 2))

  

#####  통계 분석  ##############################################################

## 데이터 정리
통계분석 <- 섬강A %>%
  select(-c(유량크기순서:일자, 연도:초과여부)) %>% 
  relocate(TP, .before = 1) %>% 
  # TP와 상관관계가 높은 순으로 데이터 순서 변경(상관관계 시각화시 순서대로 표현되도록)
  select(TP, SS, 유량, 누적강수_3일, 누적강수_4일, 누적강수_5일, 평균기온, 
         일강수량, COD, 수온, TOC, TN, BOD, pH, EC, DO)


## 단조 관계 가정
ggpairs(통계분석)

## 스피어만 상관 분석
# r-value(상관계수) 행렬
cor_matrix <- corr.test(통계분석,
                      use = "complete",
                      method = "pearson",
                      adjust = "none"
)$r 

# p-value
p_matrix <- corr.test(통계분석,
                      use = "complete",
                      method = "pearson",
                      adjust = "none"
)$p

# 상관계수 및 p-value 데이터프레임으로 변환
cor_df <- cor_matrix %>% as.table() %>% as.data.frame()
p_df <- p_matrix %>% as.table() %>% as.data.frame()

cor_p_df <- cor_df %>% 
  left_join(p_df, by = c("Var1", "Var2")) %>% 
  set_names(c("수질항목1", "수질항목2", "상관계수", "p_value")) %>% 
  mutate(
    상관계수 = round(상관계수, 3),
    p_value = round(p_value, 3),
    유의수준 = ifelse(p_value > 0.05, "X", "O")
    )

cor_p_tp <- cor_p_df %>% 
  filter(수질항목2 == "TP") %>% 
  select(-수질항목2, -유의수준) %>% 
  rename(변수 = 수질항목1) %>% 
  arrange(desc(상관계수))

# 파일 내보내기
write_xlsx(cor_p_tp, path = "수질분석/Output/섬강A_TP_상관계수.xlsx")

# 상관계수 시각화
pdf("E:/Coding/TMDL/수질분석/Output/Plot/섬강A_상관계수.pdf",
    width = 8.1, height = 8.1)

ggcorrplot(cor_matrix, 
           # hc.order = TRUE, 
           type = 'lower',
           lab = TRUE,
           p.mat = p_matrix,
           insig='blank',
           lab_size = 3.5,
           legend.title = "상관계수",
           colors = c("#3B9AB2", "white", "red"),
           outline.color = 'grey', 
           digits = 3,
           ggtheme = theme_bw, tl.cex = 10)

dev.off()


######  그래프 작성  ###########################################################

pdf("E:/Coding/TMDL/수질분석/Output/Plot/그래프_섬강A_TP(8.4x4).pdf",
  width = 8.4, height = 4)

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

## 유량 / 수질(T-P) 그래프
섬강A %>%
  ggplot() +
  geom_bar(aes(x = 유량백분율, y = 유량 / 1000, color = "steelblue2"), stat = "identity", width = 0.3, fill = "steelblue2") +
  geom_point(aes(x = 유량백분율, y = TP, color = "black", fill = 계절),
             shape = 21, alpha = 0.6, size = 3
  ) +
  geom_vline(xintercept = c(0, 10, 40, 60, 90, 100), linetype = 'dashed', size = 0.5) +
  geom_hline(aes(yintercept = 0.035, color = "red"), linetype = "dashed", size = 0.7) +
  scale_y_continuous(
    name = "T-P(mg/L)",
    sec.axis = sec_axis(~ . * 1000, name = "유량(㎥/s)", labels = scales::comma)
  ) +
  scale_x_continuous(
    name = "유량 백분율(%)",
    breaks = seq(0, 100, 10),                    # x축 표시 형식 설정
    labels = paste0(c(0,10,20,30,40,50,60,70,80,90,100))
  ) +
  annotate(geom='text', x = 5, y = 0.28, label = "홍수기") +
  annotate(geom='text', x = 25, y = 0.28, label = "풍수기") +
  annotate(geom='text', x = 50, y = 0.28, label = "평수기") +
  annotate(geom='text', x = 75, y = 0.28, label = "저수기") +
  annotate(geom='text', x = 95, y = 0.28, label = "갈수기") +
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

## 간이 LDC 그래프
섬강A %>% 
  ggplot() +
  geom_vline(xintercept = c(0, 10, 40, 60, 90, 100), linetype = 'dashed', size = 0.5) +
  geom_point(aes(x = 유량백분율, y = 측정부하량_TP, fill = 계절,        # 실측 부하량 포인트
                              color = "black"), shape = 21, alpha = 0.6, size = 3) + 
  geom_line(aes(x = 유량백분율, y = 목표부하량_TP, color = "red"), group = 1,     # 목표 부하량 지속 곡선(LDC)
            linetype = 2, size = 1.1) + 
  scale_y_log10(name = "T-P 부하량 (kg/d)") +   # y축 log 스케일 변환
  scale_x_continuous(
    name = "유량 백분율(%)",
    breaks = seq(0, 100, 10),                    # x축 표시 형식 설정
    labels = paste0(c(0,10,20,30,40,50,60,70,80,90,100))
    ) +
  annotate(geom='text', x = 5, y = 0.1, label = "홍수기") +
  annotate(geom='text', x = 25, y = 0.1, label = "풍수기") +
  annotate(geom='text', x = 50, y = 0.1, label = "평수기") +
  annotate(geom='text', x = 75, y = 0.1, label = "저수기") +
  annotate(geom='text', x = 95, y = 0.1, label = "갈수기") +
  theme_test(base_family = "notosanskr", base_size = 14) + 
  scale_fill_discrete(name = "계절") +             # 범례 제목
  scale_color_manual(name = NULL, values = c("black", "red"),
                     labels = c("실측부하량", "LDC")) +
  guides(color = guide_legend(override.aes = list(shape = c(21, NA), linetype = c(NA, 2)))) +
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

