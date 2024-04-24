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
obs_우심 <- read_excel("수질 분석/오염우심하천_결과정리3.xlsx") %>% 
  mutate(권역 = str_sub(지점명, start = 1L, end = 3L), .before = 1) %>% 
  mutate(지점번호 = str_extract_all(지점명, "[0-9]{1,}"), .after = 지점명) %>% 
  arrange(권역, 일자, 지점번호) %>% 
  select(-c(EC, 회차, 날씨))


###################  데이터 정리  ##############################################

## obs_측정값 정리
obs_우심1 <- obs_우심 %>%
  # BOD열에서 결측치 제거(결측치가 아닌 값들만 필터)
  filter(!is.na(BOD)) %>%
  mutate(
    월 = month(일자),
    계절 = ifelse(월 >= 3 & 월 <= 5, "봄",
                ifelse(월 >= 6 & 월 <= 8, "여름",
                       ifelse(월 >= 9 & 월 <= 11, "가을", "겨울")
                )
    )) %>%
  # '계절' 요인형(factor)으로 변환 후 계절 순서로 요인 순서 변경
  mutate(계절 = factor(계절, levels = c("봄", "여름", "가을", "겨울"))) 


###################  그래프 작성  ##############################################

## 섬강A 그래프
obs_우심1 %>% filter(권역 == "섬강A") %>% ggplot(aes(x = 지점명, y = TP)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = 계절), position=position_jitter(0.2)) +
  labs(y = "T-P(mg/L)") +
  theme_calc(base_family = "notosanskr", base_size = 14) +
  theme(
    line = element_line(linewidth = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal"
  )

## 섬강A 그래프
obs_우심1 %>% filter(권역 == "섬강B") %>% ggplot(aes(x = 지점명, y = TP)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = 계절), position=position_jitter(0.2)) +
  # scale_y_log10() +
  labs(y = "T-P(mg/L)") +
  theme_calc(base_family = "notosanskr", base_size = 14) +
  theme(
    line = element_line(linewidth = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal"
  )


obs_우심1 %>% ggplot(aes(x = 일자, y = TP, color = 측정소명)) +
  geom_line() +
  scale_y_log10()
