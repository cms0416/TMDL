#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(readxl)

## 그래프 관련
library(scales)
library(ggthemes)
library(showtext)

## 그래프용 Noto Sans KR 폰트 추가
font_add_google('Noto Sans KR', 'notosanskr')
showtext_auto()


#####  파일 불러오기기  ########################################################
df <- read_excel(
  "E:/Coding/TMDL/전국오염원조사/Output/전국오염원조사 자료 정리(강원도전체시군기준).xlsx",
  guess_max = Inf
)


#####  데이터 정리  ############################################################

# 기준 연도 설정
years <- as.character(2020:2024)
기준연도 <- list(2024)


생활계 <- df %>% 
  filter(분류 == "인구", 시군 == "강원도") %>% 
  pivot_longer(
    cols = all_of(years),
    names_to = "연도",
    values_to = "인구"
  ) %>% 
  mutate(연도 = as.numeric(연도))


축산계 <- df %>% 
  filter(오염원 == "축산계", 시군 == "강원도",
         분류 %in% c("소계", "한우", "젖소", "돼지", "가금")) %>% 
  pivot_longer(
    cols = all_of(years),
    names_to = "연도",
    values_to = "사육두수"
  ) %>% 
  mutate(연도 = as.numeric(연도))


산업계 <- df %>%
  filter(오염원 %in% c("산업계_업소수", "산업계_폐수발생량", "산업계_폐수방류량"), 
         시군 == "강원도") %>% 
  pivot_longer(
    cols = all_of(years),
    names_to = "연도"
  ) %>% 
  mutate(오염원 = str_remove(오염원, "산업계_")) %>%
  pivot_wider(
    names_from = 오염원,
    values_from = value
  ) %>% 
  mutate(연도 = as.numeric(연도))



##**************************************************************************** ##
###################################  그래프  ###################################
##**************************************************************************** ##


### 그래프 pdf파일 생성 경로 및 사이즈 설정
pdf("E:/Coding/TMDL/전국오염원조사/Output/Plot/유역별_오염원현황_그래프(8.2x4.3).pdf",
    width = 8.2, height = 4.3)


## 그래프_생활계_연도별인구변화_오대A -----
생활계 %>% 
  filter(단위유역 == "오대A") %>% 
  ggplot(aes(x = 연도, y = 인구)) +
  geom_bar(stat='identity', width = 0.5, fill = "wheat") +
  geom_line(stat='identity', linewidth = 0.8, color = "black") +
  geom_point(stat='identity', size = 2, color = "black") +
  geom_text(aes(label = comma(round(인구))), size = 4, vjust = -1, 
            color = "black", check_overlap = TRUE) +
  # scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "인구(명)", 
                     limits = c(0, 15000), labels = scales::comma) +
  theme_few(base_family = "notosanskr") +
  theme(
    line = element_line(linewidth = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.ticks.x.top = element_line(),
    axis.ticks.y.right = element_line(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal"
  )

## 그래프_생활계_연도별인구변화_주천A -----
생활계 %>% 
  filter(단위유역 == "주천A") %>%
  ggplot(aes(x = 연도, y = 인구)) +
  geom_bar(stat='identity', width = 0.5, fill = "wheat") +
  geom_line(stat='identity', linewidth = 0.8, color = "black") +
  geom_point(stat='identity', size = 2, color = "black") +
  geom_text(aes(label = comma(round(인구))), size = 4, vjust = -1, 
            color = "black", check_overlap = TRUE) +
  # scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "인구(명)", 
                     limits = c(0, 30000), labels = scales::comma) +
  theme_few(base_family = "notosanskr") +
  theme(
    line = element_line(linewidth = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.ticks.x.top = element_line(),
    axis.ticks.y.right = element_line(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal"
  )

## 그래프_생활계_연도별인구변화_섬강A -----
생활계 %>% 
  filter(단위유역 == "섬강A") %>%
  ggplot(aes(x = 연도, y = 인구)) +
  geom_bar(stat='identity', width = 0.5, fill = "wheat") +
  geom_line(stat='identity', linewidth = 0.8, color = "black") +
  geom_point(stat='identity', size = 2, color = "black") +
  geom_text(aes(label = comma(round(인구))), size = 4, vjust = -1, 
            color = "black", check_overlap = TRUE) +
  # scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "인구(명)", 
                     limits = c(0, 60000), labels = scales::comma) +
  theme_few(base_family = "notosanskr") +
  theme(
    line = element_line(linewidth = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.ticks.x.top = element_line(),
    axis.ticks.y.right = element_line(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal"
  )

## 그래프_생활계_연도별인구변화_홍천A -----
생활계 %>% 
  filter(단위유역 == "홍천A") %>%
  ggplot(aes(x = 연도, y = 인구)) +
  geom_bar(stat='identity', width = 0.5, fill = "wheat") +
  geom_line(stat='identity', linewidth = 0.8, color = "black") +
  geom_point(stat='identity', size = 2, color = "black") +
  geom_text(aes(label = comma(round(인구))), size = 4, vjust = -1, 
            color = "black", check_overlap = TRUE) +
  # scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "인구(명)", 
                     limits = c(0, 100000), labels = scales::comma) +
  theme_few(base_family = "notosanskr") +
  theme(
    line = element_line(linewidth = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.ticks.x.top = element_line(),
    axis.ticks.y.right = element_line(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal"
  )


#_______________________________________________________________________________

## 그래프_축산계_가축사육두수변화_오대A -----
축산계 %>% 
  filter(단위유역 == "오대A") %>%
  ggplot() +
  geom_bar(data = . %>% filter(분류 == "소계"), 
           aes(x = 연도, y = 사육두수 * 0.1, fill = "총 사육두수"), 
           stat='identity', width = 0.7) +
  geom_line(data = . %>% filter(분류 == "돼지"),
            aes(x = 연도, y = 사육두수 * 0.1, color = 분류), 
            stat='identity', linewidth = 0.8) +
  geom_point(data = . %>% filter(분류 == "돼지"), 
             aes(x = 연도, y = 사육두수 * 0.1, color = 분류), 
             stat='identity', size = 2) +
  geom_line(data = . %>% filter(분류 %in% c("한우", "젖소", "가금")),
            aes(x = 연도, y = 사육두수, color = 분류), 
            stat='identity', linewidth = 0.8) +
  geom_point(data = . %>% filter(분류 %in% c("한우", "젖소", "가금")), 
             aes(x = 연도, y = 사육두수, color = 분류), 
             stat='identity', size = 2) +
  geom_text(data = . %>% filter(분류 == "소계"),
            aes(x = 연도, y = 사육두수 * 0.1, 
                label = comma(round(사육두수))), 
            size = 3.5, vjust = -0.5, 
            color = "black", check_overlap = TRUE) +
  geom_text(data = . %>% filter(분류 == "돼지"),
            aes(x = 연도, y = 사육두수 * 0.1, 
                label = comma(round(사육두수))), 
            size = 3.5, vjust = 2, 
            color = "black", check_overlap = TRUE) +
  geom_text(data = . %>% filter(분류 %in% c("가금")),
            aes(x = 연도, y = 사육두수, 
                label = comma(round(사육두수))), 
            size = 3.5, vjust = 1.7, 
            color = "black", check_overlap = TRUE) +
  geom_text(data = . %>% filter(분류 %in% c("한우", "젖소")),
            aes(x = 연도, y = 사육두수, 
                label = comma(round(사육두수))), 
            size = 3.5, vjust = -0.7, 
            color = "black", check_overlap = TRUE) +
  scale_fill_manual(values = c("총 사육두수" = "wheat"), breaks = "총 사육두수") +
  scale_color_manual(values = c("한우" = "red", "젖소" = "mediumblue", 
                                "돼지" = "darkgreen", "가금" = "darkviolet"),
                     breaks = c("한우", "젖소", "돼지", "가금")) +
  scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "한우, 젖소, 가금(두)", 
                     limits = c(0, 2500),
                     labels = scales::comma,
                     sec.axis = sec_axis(~.*10, name = "총 사육두수, 돼지(두)", 
                                         labels = scales::comma)) +
  theme_few(base_family = "notosanskr") +
  theme(
    line = element_line(linewidth = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.ticks.x.top = element_line(),
    axis.ticks.y.right = element_line(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal"
  )

## 그래프_축산계_가축사육두수변화_주천A -----
축산계 %>% 
  filter(단위유역 == "주천A") %>%
  ggplot() +
  geom_bar(data = . %>% filter(분류 == "소계"), 
           aes(x = 연도, y = 사육두수 * 0.33, fill = "총 사육두수"), 
           stat='identity', width = 0.7) +
  geom_line(data = . %>% filter(분류 == "가금"),
            aes(x = 연도, y = 사육두수 * 0.33, color = 분류), 
            stat='identity', linewidth = 0.8) +
  geom_point(data = . %>% filter(분류 == "가금"), 
             aes(x = 연도, y = 사육두수 * 0.33, color = 분류), 
             stat='identity', size = 2) +
  geom_line(data = . %>% filter(분류 %in% c("한우", "젖소", "돼지")),
            aes(x = 연도, y = 사육두수, color = 분류), 
            stat='identity', linewidth = 0.8) +
  geom_point(data = . %>% filter(분류 %in% c("한우", "젖소", "돼지")), 
             aes(x = 연도, y = 사육두수, color = 분류), 
             stat='identity', size = 2) +
  geom_text(data = . %>% filter(분류 == "소계"),
            aes(x = 연도, y = 사육두수 * 0.33, 
                label = comma(round(사육두수))), 
            size = 3.5, vjust = -0.5, 
            color = "black", check_overlap = TRUE) +
  geom_text(data = . %>% filter(분류 == "가금"),
            aes(x = 연도, y = 사육두수 * 0.33, 
                label = comma(round(사육두수))), 
            size = 3.5, vjust = 1.7, 
            color = "black", check_overlap = TRUE) +
  geom_text(data = . %>% filter(분류 %in% c("한우", "돼지")),
            aes(x = 연도, y = 사육두수, 
                label = comma(round(사육두수))), 
            size = 3.5, vjust = -0.7, 
            color = "black", check_overlap = TRUE) +
  geom_text(data = . %>% filter(분류 %in% c("젖소")),
            aes(x = 연도, y = 사육두수, 
                label = comma(round(사육두수))), 
            size = 3.5, vjust = 1.7, 
            color = "black", check_overlap = TRUE) +
  scale_fill_manual(values = c("총 사육두수" = "wheat"), breaks = "총 사육두수") +
  scale_color_manual(values = c("한우" = "red", "젖소" = "mediumblue", 
                                "돼지" = "darkgreen", "가금" = "darkviolet"),
                     breaks = c("한우", "젖소", "돼지", "가금")) +
  scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "한우, 젖소, 돼지(두)", 
                     limits = c(0, 100000),
                     labels = scales::comma,
                     sec.axis = sec_axis(~./0.33, name = "총 사육두수, 가금(두)", 
                                         labels = scales::comma)) +
  theme_few(base_family = "notosanskr") +
  theme(
    line = element_line(linewidth = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.ticks.x.top = element_line(),
    axis.ticks.y.right = element_line(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal"
  )

## 그래프_축산계_가축사육두수변화_섬강A -----
축산계 %>% 
  filter(단위유역 == "섬강A") %>%
  ggplot() +
  geom_bar(data = . %>% filter(분류 == "소계"), 
           aes(x = 연도, y = 사육두수 * 0.08, fill = "총 사육두수"), 
           stat='identity', width = 0.7) +
  geom_line(data = . %>% filter(분류 == "가금"),
            aes(x = 연도, y = 사육두수 * 0.08, color = 분류), 
            stat='identity', linewidth = 0.8) +
  geom_point(data = . %>% filter(분류 == "가금"), 
             aes(x = 연도, y = 사육두수 * 0.08, color = 분류), 
             stat='identity', size = 2) +
  geom_line(data = . %>% filter(분류 %in% c("한우", "젖소", "돼지")),
            aes(x = 연도, y = 사육두수, color = 분류), 
            stat='identity', linewidth = 0.8) +
  geom_point(data = . %>% filter(분류 %in% c("한우", "젖소", "돼지")), 
             aes(x = 연도, y = 사육두수, color = 분류), 
             stat='identity', size = 2) +
  geom_text(data = . %>% filter(분류 == "소계"),
            aes(x = 연도, y = 사육두수 * 0.08, 
                label = comma(round(사육두수))), 
            size = 3.5, vjust = -0.5, 
            color = "black", check_overlap = TRUE) +
  geom_text(data = . %>% filter(분류 == "가금"),
            aes(x = 연도, y = 사육두수 * 0.08, 
                label = comma(round(사육두수))), 
            size = 3.5, vjust = 2, 
            color = "black", check_overlap = TRUE) +
  geom_text(data = . %>% filter(분류 %in% c("돼지")),
            aes(x = 연도, y = 사육두수, 
                label = comma(round(사육두수))), 
            size = 3.5, vjust = 1.7, 
            color = "black", check_overlap = TRUE) +
  geom_text(data = . %>% filter(분류 %in% c("한우", "젖소")),
            aes(x = 연도, y = 사육두수, 
                label = comma(round(사육두수))), 
            size = 3.5, vjust = -0.7, 
            color = "black", check_overlap = TRUE) +
  scale_fill_manual(values = c("총 사육두수" = "wheat"), breaks = "총 사육두수") +
  scale_color_manual(values = c("한우" = "red", "젖소" = "mediumblue", 
                                "돼지" = "darkgreen", "가금" = "darkviolet"),
                     breaks = c("한우", "젖소", "돼지", "가금")) +
  scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "한우, 젖소, 돼지(두)", 
                     limits = c(0, 180000),
                     labels = scales::comma,
                     sec.axis = sec_axis(~./0.08, name = "총 사육두수, 가금(두)", 
                                         labels = scales::comma)) +
  theme_few(base_family = "notosanskr") +
  theme(
    line = element_line(linewidth = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.ticks.x.top = element_line(),
    axis.ticks.y.right = element_line(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal"
  )

## 그래프_축산계_가축사육두수변화_홍천A -----
축산계 %>% 
  filter(단위유역 == "홍천A") %>%
  ggplot() +
  geom_bar(data = . %>% filter(분류 == "소계"), 
           aes(x = 연도, y = 사육두수 * 0.15, fill = "총 사육두수"), 
           stat='identity', width = 0.7) +
  geom_line(data = . %>% filter(분류 == "가금"),
            aes(x = 연도, y = 사육두수 * 0.15, color = 분류), 
            stat='identity', linewidth = 0.8) +
  geom_point(data = . %>% filter(분류 == "가금"), 
             aes(x = 연도, y = 사육두수 * 0.15, color = 분류), 
             stat='identity', size = 2) +
  geom_line(data = . %>% filter(분류 %in% c("한우", "젖소", "돼지")),
            aes(x = 연도, y = 사육두수, color = 분류), 
            stat='identity', linewidth = 0.8) +
  geom_point(data = . %>% filter(분류 %in% c("한우", "젖소", "돼지")), 
             aes(x = 연도, y = 사육두수, color = 분류), 
             stat='identity', size = 2) +
  geom_text(data = . %>% filter(분류 == "소계"),
            aes(x = 연도, y = 사육두수 * 0.15, 
                label = comma(round(사육두수))), 
            size = 3.5, vjust = -0.5, 
            color = "black", check_overlap = TRUE) +
  geom_text(data = . %>% filter(분류 == "가금"),
            aes(x = 연도, y = 사육두수 * 0.15, 
                label = comma(round(사육두수))), 
            size = 3.5, vjust = 1.7, 
            color = "black", check_overlap = TRUE) +
  geom_text(data = . %>% filter(분류 %in% c("한우")),
            aes(x = 연도, y = 사육두수, 
                label = comma(round(사육두수))), 
            size = 3.5, vjust = 1.7, 
            color = "black", check_overlap = TRUE) +
  geom_text(data = . %>% filter(분류 %in% c("젖소", "돼지")),
            aes(x = 연도, y = 사육두수, 
                label = comma(round(사육두수))), 
            size = 3.5, vjust = -0.7, 
            color = "black", check_overlap = TRUE) +
  scale_fill_manual(values = c("총 사육두수" = "wheat"), breaks = "총 사육두수") +
  scale_color_manual(values = c("한우" = "red", "젖소" = "mediumblue", 
                                "돼지" = "darkgreen", "가금" = "darkviolet"),
                     breaks = c("한우", "젖소", "돼지", "가금")) +
  scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "한우, 젖소, 돼지(두)", 
                     limits = c(0, 200000),
                     labels = scales::comma,
                     sec.axis = sec_axis(~./0.15, name = "총 사육두수, 가금(두)", 
                                         labels = scales::comma)) +
  theme_few(base_family = "notosanskr") +
  theme(
    line = element_line(linewidth = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.ticks.x.top = element_line(),
    axis.ticks.y.right = element_line(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal"
  )


#_______________________________________________________________________________

## 그래프_산업계_연도추이_오대A -----
산업계 %>%
  filter(단위유역 == "오대A") %>% 
  ggplot() +
  geom_line(aes(x = 연도, y = 업소수, color = "업소수"), 
            stat='identity', linewidth = 0.8) +
  geom_point(aes(x = 연도, y = 업소수, color = "업소수"), 
             stat='identity', size = 2) +
  geom_line(aes(x = 연도, y = 폐수발생량/13, color = "폐수발생량"), 
            stat='identity', linewidth = 0.8) +
  geom_point(aes(x = 연도, y = 폐수발생량/13, color = "폐수발생량"), 
             stat='identity', size = 2) +
  geom_line(aes(x = 연도, y = 폐수방류량/13, color = "폐수방류량"), 
            stat='identity', linewidth = 0.8) +
  geom_point(aes(x = 연도, y = 폐수방류량/13, color = "폐수방류량"), 
             stat='identity', size = 2) +
  geom_text(aes(x = 연도, y = 업소수, label = comma(업소수)), 
            size = 3.5, vjust = -0.8, 
            color = "black", check_overlap = TRUE) +
  geom_text(aes(x = 연도, y = 폐수발생량/13, label = comma(폐수발생량)), 
            size = 3.5, vjust = 1.8, 
            color = "black", check_overlap = TRUE) +
  geom_text(aes(x = 연도, y = 폐수방류량/13, label = comma(폐수방류량)), 
            size = 3.5, vjust = 1.8, 
            color = "black", check_overlap = TRUE) +
  scale_color_manual(values = c("업소수" = "red", "폐수발생량" = "mediumblue", 
                                "폐수방류량" = "darkgreen"),
                     breaks = c("업소수", "폐수발생량", "폐수방류량")) +
  scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "업소수(개소)", 
                     limits = c(0, 25),
                     labels = scales::comma,
                     sec.axis = sec_axis(~.*13, name = "폐수 발생·방류량(톤/일)", 
                                         labels = scales::comma)) +
  theme_few(base_family = "notosanskr") +
  theme(
    line = element_line(linewidth = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.ticks.x.top = element_line(),
    axis.ticks.y.right = element_line(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal"
  )


## 그래프_산업계_연도추이_주천A -----
산업계 %>%
  filter(단위유역 == "주천A") %>%
  ggplot() +
  geom_line(aes(x = 연도, y = 업소수, color = "업소수"), 
            stat='identity', linewidth = 0.8) +
  geom_point(aes(x = 연도, y = 업소수, color = "업소수"), 
             stat='identity', size = 2) +
  geom_line(aes(x = 연도, y = 폐수발생량/400, color = "폐수발생량"), 
            stat='identity', linewidth = 0.8) +
  geom_point(aes(x = 연도, y = 폐수발생량/400, color = "폐수발생량"), 
             stat='identity', size = 2) +
  geom_line(aes(x = 연도, y = 폐수방류량/400, color = "폐수방류량"), 
            stat='identity', linewidth = 0.8) +
  geom_point(aes(x = 연도, y = 폐수방류량/400, color = "폐수방류량"), 
             stat='identity', size = 2) +
  geom_text(aes(x = 연도, y = 업소수, label = comma(업소수)), 
            size = 3.5, vjust = -0.8, 
            color = "black", check_overlap = TRUE) +
  geom_text(aes(x = 연도, y = 폐수발생량/400, label = comma(폐수발생량)), 
            size = 3.5, vjust = 1.8, 
            color = "black", check_overlap = TRUE) +
  geom_text(aes(x = 연도, y = 폐수방류량/400, label = comma(폐수방류량)), 
            size = 3.5, vjust = 1.8, 
            color = "black", check_overlap = TRUE) +
  scale_color_manual(values = c("업소수" = "red", "폐수발생량" = "mediumblue", 
                                "폐수방류량" = "darkgreen"),
                     breaks = c("업소수", "폐수발생량", "폐수방류량")) +
  scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "업소수(개소)", 
                     limits = c(0, 30),
                     labels = scales::comma,
                     sec.axis = sec_axis(~.*415, name = "폐수 발생·방류량(톤/일)", 
                                         labels = scales::comma)) +
  theme_few(base_family = "notosanskr") +
  theme(
    line = element_line(linewidth = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.ticks.x.top = element_line(),
    axis.ticks.y.right = element_line(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal"
  )

## 그래프_산업계_연도추이_섬강A -----
산업계 %>%
  filter(단위유역 == "섬강A") %>%
  ggplot() +
  geom_line(aes(x = 연도, y = 업소수, color = "업소수"), 
            stat='identity', linewidth = 0.8) +
  geom_point(aes(x = 연도, y = 업소수, color = "업소수"), 
             stat='identity', size = 2) +
  geom_line(aes(x = 연도, y = 폐수발생량/70, color = "폐수발생량"), 
            stat='identity', linewidth = 0.8) +
  geom_point(aes(x = 연도, y = 폐수발생량/70, color = "폐수발생량"), 
             stat='identity', size = 2) +
  geom_line(aes(x = 연도, y = 폐수방류량/70, color = "폐수방류량"), 
            stat='identity', linewidth = 0.8) +
  geom_point(aes(x = 연도, y = 폐수방류량/70, color = "폐수방류량"), 
             stat='identity', size = 2) +
  geom_text(aes(x = 연도, y = 업소수, label = comma(업소수)), 
            size = 3.5, vjust = -0.8, 
            color = "black", check_overlap = TRUE) +
  geom_text(aes(x = 연도, y = 폐수발생량/70, label = comma(폐수발생량)), 
            size = 3.5, vjust = 1.8, 
            color = "black", check_overlap = TRUE) +
  geom_text(aes(x = 연도, y = 폐수방류량/70, label = comma(폐수방류량)), 
            size = 3.5, vjust = 1.8, 
            color = "black", check_overlap = TRUE) +
  scale_color_manual(values = c("업소수" = "red", "폐수발생량" = "mediumblue", 
                                "폐수방류량" = "darkgreen"),
                     breaks = c("업소수", "폐수발생량", "폐수방류량")) +
  scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "업소수(개소)", 
                     limits = c(0, 150),
                     labels = scales::comma,
                     sec.axis = sec_axis(~.*70, name = "폐수 발생·방류량(톤/일)", 
                                         labels = scales::comma)) +
  theme_few(base_family = "notosanskr") +
  theme(
    line = element_line(linewidth = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.ticks.x.top = element_line(),
    axis.ticks.y.right = element_line(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal"
  )

## 그래프_산업계_연도추이_홍천A -----
산업계 %>%
  filter(단위유역 == "홍천A") %>%
  ggplot() +
  geom_line(aes(x = 연도, y = 업소수, color = "업소수"), 
            stat='identity', linewidth = 0.8) +
  geom_point(aes(x = 연도, y = 업소수, color = "업소수"), 
             stat='identity', size = 2) +
  geom_line(aes(x = 연도, y = 폐수발생량/110, color = "폐수발생량"), 
            stat='identity', linewidth = 0.8) +
  geom_point(aes(x = 연도, y = 폐수발생량/110, color = "폐수발생량"), 
             stat='identity', size = 2) +
  geom_line(aes(x = 연도, y = 폐수방류량/110, color = "폐수방류량"), 
            stat='identity', linewidth = 0.8) +
  geom_point(aes(x = 연도, y = 폐수방류량/110, color = "폐수방류량"), 
             stat='identity', size = 2) +
  geom_text(aes(x = 연도, y = 업소수, label = comma(업소수)), 
            size = 3.5, vjust = -0.8, 
            color = "black", check_overlap = TRUE) +
  geom_text(aes(x = 연도, y = 폐수발생량/110, label = comma(폐수발생량)), 
            size = 3.5, vjust = 1.8, 
            color = "black", check_overlap = TRUE) +
  geom_text(aes(x = 연도, y = 폐수방류량/110, label = comma(폐수방류량)), 
            size = 3.5, vjust = 1.8, 
            color = "black", check_overlap = TRUE) +
  scale_color_manual(values = c("업소수" = "red", "폐수발생량" = "mediumblue", 
                                "폐수방류량" = "darkgreen"),
                     breaks = c("업소수", "폐수발생량", "폐수방류량")) +
  scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "업소수(개소)", 
                     limits = c(0, 180),
                     labels = scales::comma,
                     sec.axis = sec_axis(~.*110, name = "폐수 발생·방류량(톤/일)", 
                                         labels = scales::comma)) +
  theme_few(base_family = "notosanskr") +
  theme(
    line = element_line(linewidth = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.ticks.x.top = element_line(),
    axis.ticks.y.right = element_line(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal"
  )


dev.off()


#_______________________________________________________________________________


## 그래프_토지계_시군별 지목 면적 -----
토지계_total %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(연도 == 기준연도, 시군 != "강원도", 지목 != "합계") %>% 
  # "시", "군" 삭제
  mutate(시군 = str_remove(시군, "(시|군)")) %>% 
  ggplot(aes(x = reorder(시군, -면적), y = 면적, fill = 지목)) +
  geom_bar(stat='identity', width = 0.7) +
  geom_text(aes(label =comma(면적)), size = 3, 
            position = position_stack(vjust = 0.5),
            color = "black", check_overlap = TRUE) +
  geom_text(aes(y = 총면적, label = comma(총면적)), size = 3.5, vjust = -0.5,
            check_overlap = TRUE) +
  scale_y_continuous(name = "토지면적(㎢)", breaks = seq(0, 100000, by = 500), 
                     limits = c(0, 1900), labels = scales::comma) +
  theme_few(base_family = "notosanskr") +
  theme(
    line = element_line(linewidth = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.ticks.x.top = element_line(),
    axis.ticks.y.right = element_line(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal"
  )


#_______________________________________________________________________________


## 그래프_양식계_연도추이 -----
양식계_total %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(시군 == "평창군", 종류 != "합계") %>% 
  ggplot(aes(x = 연도, y = 양식장수, fill = 종류)) +
  geom_bar(stat='identity', width = 0.7) +
  geom_text(aes(label = comma(양식장수)), size = 3.5, 
            position = position_stack(vjust = 0.5),
            color = "black", check_overlap = TRUE) +
  scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "양식장수(개소)", breaks = seq(0, 30, by = 10), 
                     limits = c(0, 30), labels = scales::comma) +
  theme_few(base_family = "notosanskr") +
  theme(
    line = element_line(linewidth = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.ticks.x.top = element_line(),
    axis.ticks.y.right = element_line(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal"
  )


#_______________________________________________________________________________


## 그래프_환경기초시설_처리량 연도추이 -----
환경기초시설_total %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(시군 == "강원도", 종류 != "합계") %>% 
  ggplot(aes(x = 연도, y = 처리량/1000, fill = 종류)) +
  geom_line(data = . %>% filter(종류 == "공공하수"),
            aes(y = 처리량/1000/12, color = 종류), 
            stat='identity', linewidth = 0.8) +
  geom_point(data = . %>% filter(종류 == "공공하수"),
            aes(y = 처리량/1000/12, color = 종류), 
            stat='identity', shape = 21, size = 2.5, stroke = 1, fill = "white") +
  geom_text(data = . %>% filter(종류 == "공공하수"),
            aes(y = 처리량/1000/12, label =comma(round(처리량/1000, 2))), 
            size = 3.5, vjust = -0.9,
            color = "black", check_overlap = TRUE) +
  
  geom_bar(data = . %>% filter(종류 != "공공하수"), 
           stat='identity', width = 0.7) +
  geom_text(data = . %>% filter(종류 != "공공하수"),
            aes(label =comma(round(처리량/1000, 2))), size = 3.5,
            position = position_stack(vjust = 0.5),
            color = "black", check_overlap = TRUE) +
  
  scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "처리량(천톤/일)", breaks = seq(0, 100, by = 10), 
                     limits = c(0, 60), labels = scales::comma,
                     sec.axis = sec_axis(~.*12, name = "공공하수 처리량(톤/일)",
                                         breaks = seq(0, 1000, by = 200),
                                         labels = scales::comma)) +
  scale_fill_manual(values = c("소규모하수" = "orange", "공공폐수" = "deepskyblue",
                               "분뇨(생활가축)" = "green", "매립장" = "violet"),
                    labels = c("소규모하수", "공공폐수", "분뇨(생활가축)", "매립장")) +

  theme_few(base_family = "notosanskr") +
  theme(
    line = element_line(linewidth = 0.1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.ticks.x.top = element_line(),
    axis.ticks.y.right = element_line(),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
    legend.direction = "horizontal"
  )


  dev.off()
  
  
  
  
  
  