################################################################################
# 라이브러리 로드
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

################################################################################


### Excel 파일 불러오기_readxl
obs <- read_excel("수질 분석/섬강A유역_수질측정망_2019_2023.xlsx")


###################  데이터 정리  ##############################################

## obs_측정값 정리
obs1 <- obs %>%
  # BOD열에서 결측치 제거(결측치가 아닌 값들만 필터)
  filter(!is.na(BOD)) %>%
  filter(!(TP == 2.108)) %>% 
  mutate(
    월 = month(일자),
    계절 = ifelse(월 >= 3 & 월 <= 5, "봄",
                ifelse(월 >= 6 & 월 <= 8, "여름",
                       ifelse(월 >= 9 & 월 <= 11, "가을", "겨울")
                )
    )) %>%
  # '계절' 요인형(factor)으로 변환 후 계절 순서로 요인 순서 변경
  mutate(계절 = factor(계절, levels = c("봄", "여름", "가을", "겨울")))
  # filter(계절 %in% c("여름", "가을"), !측정소명 %in% c("계천2", "금계천2"))

섬강A <- obs1 %>% 
  filter(측정소명 == "섬강3") %>% 
  arrange(desc(유량)) %>% 
  rowid_to_column(var = "유량크기순서")
  



###################  그래프 작성  ##############################################

섬강A %>% filter(측정소명 == "섬강3") %>% 
  ggplot() + 
  geom_bar(aes(x = 유량크기순서, y = 유량/1000, color = "steelblue2"), stat='identity', width = 0.3, fill = "steelblue2") +
  geom_point(aes(x = 유량크기순서, y = TP, color = "black", fill = 계절), 
             shape = 21, alpha = 0.6, size = 3) +
  geom_hline(aes(yintercept = 0.035, color = "red"), linetype = "dashed", size = 0.7) +
  scale_y_continuous(name = "T-P(mg/L)", 
                     sec.axis = sec_axis(~.*1000, name = "유량", labels = scales::comma)) +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  scale_fill_discrete(name = "계절") +             # 범례 제목
  scale_color_manual(name = NULL, values = c("black", "red", "steelblue2"),
                     labels = c("수질", "목표수질", "유량")) +
  theme(
    legend.position = c(0.77, 0.90),                                              # 범례 위치
    legend.direction = "horizontal",                                              # 범례 방향
    legend.box = "vertical",                                                    # 범례 배치 방향
    legend.background = element_rect(fill = "white", color = "black"),            # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )



섬강A %>% filter(측정소명 == "섬강3") %>% 
  ggplot() + 
  geom_point(aes(x = 유량크기순서, y = TP, color = "black", fill = 계절), 
             shape = 21, alpha = 0.6, size = 3) +
  geom_hline(aes(yintercept = 0.035, color = "red"), linetype = "dashed", size = 0.7) +
  scale_y_log10(name = "T-P(mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  scale_fill_discrete(name = "계절") +             # 범례 제목
  scale_color_manual(name = NULL, values = c("black", "red"),
                     labels = c("수질", "목표수질")) +
  theme(
    legend.position = c(0.68, 0.93),                                              # 범례 위치
    legend.direction = "horizontal",                                              # 범례 방향
    legend.box = "horizontal",                                                    # 범례 배치 방향
    legend.background = element_rect(fill = "white", color = "black"),            # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )



obs1 %>% 
  filter(!측정소명 %in% c("계천2", "금계천2")) %>% 
  ggplot(aes(x = 측정소명, y = TP)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(fill = 계절), 
              shape = 21, alpha = 0.6, size = 2.3,
              position = position_jitter(0.3)) +
  scale_x_discrete(limits = c("전천", "섬강1", "섬강2", "섬강3")) +
  labs(y = "T-P(mg/L)") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    legend.position = c(0.2, 0.93),                                              # 범례 위치
    legend.direction = "horizontal",                                             # 범례 방향
    legend.background = element_rect(fill = "white", color = "black"),           # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )
  


obs1 %>% ggplot(aes(x = 일자, y = TP, color = 측정소명)) +
  geom_line() +
  scale_y_log10()
  
  
