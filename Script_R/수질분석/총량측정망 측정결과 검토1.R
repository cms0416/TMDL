##########  라이브러리 로드  ###################################################
library(tidyverse)
library(readxl)
library(writexl)

# 그래프 작성
library(showtext)

## 그래프용 Noto Sans KR 폰트 추가
font_add_google("Noto Sans KR", "notosanskr")
showtext_auto()


##########  파일 불러오기  #####################################################
수질_원본 <- read_excel("수질분석/총량측정망_강원_2007_2024.xlsx")
오염원 <- read_excel("전국오염원조사/Output/전국오염원조사 자료 정리(강원도전체시군기준).xlsx")
기준년도 <- 2024

##########  수질 데이터 정리  ##################################################
수질_정리 <- 수질_원본 %>% 
  # BOD열에서 결측치 제거(결측치가 아닌 값들만 필터)
  filter(!is.na(BOD)) %>%
  mutate(
    월 = month(일자),
    계절 = ifelse(월 >= 3 & 월 <= 5, "봄",
                ifelse(월 >= 6 & 월 <= 8, "여름",
                       ifelse(월 >= 9 & 월 <= 11, "가을", "겨울")
                )
    )
  ) %>%
  # '계절' 요인형(factor)으로 변환 후 계절 순서로 요인 순서 변경
  mutate(
    계절 = factor(계절, levels = c("봄", "여름", "가을", "겨울")),
    총량지점명 = factor(총량지점명, levels = c(
      "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A", "섬강B",
      "북한A", "북한B", "소양A", "인북A", "소양B", "북한C", "홍천A", "한탄A", 
      "제천A", "한강B", "한강D", "북한D", "임진A", "한탄B", "낙본A"
    ))
    )

## 유역별 평균 값 및 수질등급 정리
유역별평균 <- 수질_정리 %>% 
  filter(연도 %in% c((기준년도-2):기준년도), 월 %in% c(1:6)) %>% 
  select(-c(일자, 월, 계절)) %>% 
  group_by(총량지점명, 연도) %>% 
  summarise(across(c(BOD, TP), mean), .groups = "drop") %>% 
  mutate(BOD = round(BOD, 1),
         TP = round(TP, 3),
         BOD_등급 = case_when(
           BOD <= 1 ~ "Ia",
           BOD <= 2 ~ "Ib",
           BOD <= 3 ~ "II",
           BOD <= 5 ~ "III",
           BOD <= 8 ~ "IV",
           BOD <= 10 ~ "V",
           .default = "VI"
         ), 
         TP_등급 = case_when(
           TP <= 0.02 ~ "Ia",
           TP <= 0.04 ~ "Ib",
           TP <= 0.10 ~ "II",
           TP <= 0.20 ~ "III",
           TP <= 0.30 ~ "IV",
           TP <= 0.50 ~ "V",
           .default = "VI"
         )
  ) %>% 
  relocate(BOD_등급, .after = BOD)
  



##########  유역별 주요 오염원 현황  ###########################################
오염원현황 <- 오염원 %>% 
  filter(시군 == "강원도", !(단위유역 %in% c("소계", "기타")),
         오염원 %in% c("생활계", "축산계", "산업계_업소수", 
                    "산업계_폐수방류량", "토지계"),
         분류 %in% c("인구", "물사용량", "한우", "젖소", "돼지", "가금", 
                   "전", "답", "소계"),
         !(str_c(오염원, 분류) %in% c("축산계소계", "토지계소계"))) %>% 
  mutate(분류 = ifelse(오염원 %in% c("산업계_업소수", "산업계_폐수방류량"),
                     오염원, 분류)
         ) %>% 
  select(권역:분류, `2022`, `2023`)



##########  유역별 수질 그래프  ################################################

##### BOD 그래프 ---------------------------------------------------------------
pdf("E:/Coding/TMDL/수질분석/Output/Plot/총량관리 수질측정망 측정결과 평가/총량측정망_BOD_그래프(7.5x3.5).pdf",
    width = 7.5, height = 3.5)

## 남한강 수계 + 섬강 수계 별 박스플롯 BOD
수질_정리 %>%
  filter(연도 >= (기준년도-2),
         총량지점명 %in% c("골지A", "오대A", "주천A", "평창A", 
                      "옥동A", "한강A", "섬강A", "섬강B")) %>% 
  ggplot(aes(x = 총량지점명, y = BOD)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절),
              shape = 21, alpha = 0.6, size = 2.3,
              position = position_jitter(0.2)
  ) +
  scale_y_continuous(name = 'BOD (mg/L)', trans='log10',limits=c(0.15, 10)) +
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


## 북한강 수계, 홍천A, 한탄A 유역 별 박스플롯 BOD
수질_정리 %>%
  filter(연도 >= (기준년도-2),
         총량지점명 %in% c("북한A", "북한B", "소양A", "인북A", 
                      "소양B", "북한C", "홍천A", "한탄A")) %>% 
  ggplot(aes(x = 총량지점명, y = BOD)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절),
              shape = 21, alpha = 0.6, size = 2.3,
              position = position_jitter(0.2)
  ) +
  scale_y_continuous(name = 'BOD (mg/L)', trans='log10',limits=c(0.15, 10)) +
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

## 타시도 관할 및 낙동강 수계 유역 별 박스플롯 BOD
수질_정리 %>%
  filter(연도 >= (기준년도-2),
         총량지점명 %in% c("제천A", "한강B", "한강D", "북한D", 
                      "임진A", "한탄B", "낙본A")) %>% 
  ggplot(aes(x = 총량지점명, y = BOD)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절),
              shape = 21, alpha = 0.6, size = 2.3,
              position = position_jitter(0.2)
  ) +
  scale_y_continuous(name = 'BOD (mg/L)', trans='log10',limits=c(0.15, 10)) +
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


##### T-P 그래프 ---------------------------------------------------------------
pdf("E:/Coding/TMDL/수질분석/Output/Plot/총량관리 수질측정망 측정결과 평가/총량측정망_TP_그래프(7.5x3.5).pdf",
    width = 7.5, height = 3.5)

## 남한강 수계 + 섬강 수계 별 박스플롯 T-P
수질_정리 %>%
  filter(연도 >= (기준년도-2),
         총량지점명 %in% c("골지A", "오대A", "주천A", "평창A", 
                      "옥동A", "한강A", "섬강A", "섬강B")) %>% 
  ggplot(aes(x = 총량지점명, y = TP)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절),
              shape = 21, alpha = 0.6, size = 2.3,
              position = position_jitter(0.2)
  ) +
  scale_y_continuous(name = 'T-P (mg/L)', trans='log10',limits=c(0.003, 10)) +
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


## 북한강 수계, 홍천A, 한탄A 유역 별 박스플롯 T-P
수질_정리 %>%
  filter(연도 >= (기준년도-2),
         총량지점명 %in% c("북한A", "북한B", "소양A", "인북A", 
                      "소양B", "북한C", "홍천A", "한탄A")) %>% 
  ggplot(aes(x = 총량지점명, y = TP)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절),
              shape = 21, alpha = 0.6, size = 2.3,
              position = position_jitter(0.2)
  ) +
  scale_y_continuous(name = 'T-P (mg/L)', trans='log10',limits=c(0.003, 10)) +
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

## 타시도 관할 및 낙동강 수계 유역 별 박스플롯 T-P
수질_정리 %>%
  filter(연도 >= (기준년도-2),
         총량지점명 %in% c("제천A", "한강B", "한강D", "북한D", 
                      "임진A", "한탄B", "낙본A")) %>% 
  ggplot(aes(x = 총량지점명, y = TP)) +
  # staplewidth : 박스플롯이 수염이 끝나는 지점 표시 너비
  geom_boxplot(outliers = FALSE, staplewidth = 0.3) +
  geom_jitter(aes(fill = 계절),
              shape = 21, alpha = 0.6, size = 2.3,
              position = position_jitter(0.2)
  ) +
  scale_y_continuous(name = 'T-P (mg/L)', trans='log10',limits=c(0.003, 10)) +
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


##########  유역별 오염원 현황 그래프  #########################################

pdf("E:/Coding/TMDL/수질분석/Output/Plot/총량관리 수질측정망 측정결과 평가/2024_하반기_오염원(5x5).pdf",
    width = 5, height = 5)

## 그래프_생활계_인구 -----
오염원현황 %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(분류 == "인구") %>% 
  # 단위유역을 오염원 수치 기준으로 내림차순 정렬
  mutate(단위유역 = fct_reorder(단위유역, `2023`, .desc = T)) %>%
  ggplot(aes(x = 단위유역, y = `2023` / 10000)) +
  geom_bar(stat='identity', width = 0.6, fill = "deepskyblue3") +
  scale_y_continuous(name = "인구(만명)", expand = expansion(add = c(0, 1)),
                     breaks = seq(0, 35, by = 5)) +
  ggtitle("인구") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 17),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    # x축 라벨을 옆으로 눕히기
    axis.text.x = element_text(face = "bold", angle = 90, hjust = 1, vjust = 0.5 ),
    axis.text.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA)
  )


## 그래프_생활계_물사용량(2022년 기준) -----
오염원현황 %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(분류 == "물사용량") %>% 
  # 단위유역을 오염원 수치 기준으로 내림차순 정렬 
  mutate(단위유역 = fct_reorder(단위유역, `2022`, .desc = T)) %>%
  ggplot(aes(x = 단위유역, y = `2022` / 10000)) +
  geom_bar(stat='identity', width = 0.6, fill = "deepskyblue3") +
  # expansion() : 축 범위 확장
  scale_y_continuous(name = "물사용량(만㎥/일)", 
                     expand = expansion(add = c(0, 2)),
                     breaks = seq(0, 30, by = 5)) +
  ggtitle("물사용량") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 17),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    # x축 라벨을 옆으로 눕히기
    axis.text.x = element_text(face = "bold", angle = 90, hjust = 1, vjust = 0.5 ),
    axis.text.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA)
  )


## 그래프_축산계_한우 -----
오염원현황 %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(분류 == "한우") %>% 
  # 단위유역을 오염원 수치 기준으로 내림차순 정렬 
  mutate(단위유역 = fct_reorder(단위유역, `2023`, .desc = T)) %>%
  ggplot(aes(x = 단위유역, y = `2023` / 10000)) +
  geom_bar(stat='identity', width = 0.6, fill = "deepskyblue3") +
  # expansion() : 축 범위 확장
  scale_y_continuous(name = "한우(만두)",
                     expand = expansion(add = c(0, 0.6)),
                     breaks = seq(0, 5, by = 1)) +
  ggtitle("한우") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 17),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    # x축 라벨을 옆으로 눕히기
    axis.text.x = element_text(face = "bold", angle = 90, hjust = 1, vjust = 0.5 ),
    axis.text.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA)
  )


## 그래프_축산계_젖소 -----
오염원현황 %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(분류 == "젖소") %>% 
  # 단위유역을 오염원 수치 기준으로 내림차순 정렬 
  mutate(단위유역 = fct_reorder(단위유역, `2023`, .desc = T)) %>%
  ggplot(aes(x = 단위유역, y = `2023` / 10000)) +
  geom_bar(stat='identity', width = 0.6, fill = "deepskyblue3") +
  # expansion() : 축 범위 확장
  scale_y_continuous(name = "젖소(만두)",  
                     expand = expansion(add = c(0, 0.05)), 
                     breaks = seq(0, 2, by = 0.2)) +
  ggtitle("젖소") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 17),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    # x축 라벨을 옆으로 눕히기
    axis.text.x = element_text(face = "bold", angle = 90, hjust = 1, vjust = 0.5 ),
    axis.text.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA)
  )


## 그래프_축산계_돼지 -----
오염원현황 %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(분류 == "돼지") %>% 
  # 단위유역을 오염원 수치 기준으로 내림차순 정렬 
  mutate(단위유역 = fct_reorder(단위유역, `2023`, .desc = T)) %>%
  ggplot(aes(x = 단위유역, y = `2023` / 10000)) +
  geom_bar(stat='identity', width = 0.6, fill = "deepskyblue3") +
  # expansion() : 축 범위 확장
  scale_y_continuous(name = "돼지(만두)",  
                     expand = expansion(add = c(0, 1.5)),
                     breaks = seq(0, 20, by = 2)) +
  ggtitle("돼지") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 17),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    # x축 라벨을 옆으로 눕히기
    axis.text.x = element_text(face = "bold", angle = 90, hjust = 1, vjust = 0.5 ),
    axis.text.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA)
  )


## 그래프_축산계_가금 -----
오염원현황 %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(분류 == "가금") %>% 
  # 단위유역을 오염원 수치 기준으로 내림차순 정렬 
  mutate(단위유역 = fct_reorder(단위유역, `2023`, .desc = T)) %>%
  ggplot(aes(x = 단위유역, y = `2023` / 10000)) +
  geom_bar(stat='identity', width = 0.6, fill = "deepskyblue3") +
  # expansion() : 축 범위 확장
  scale_y_continuous(name = "가금(만두)", 
                     expand = expansion(add = c(0, 8)), 
                     breaks = seq(0, 200, by = 20)) +
  ggtitle("가금") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 17),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    # x축 라벨을 옆으로 눕히기
    axis.text.x = element_text(face = "bold", angle = 90, hjust = 1, vjust = 0.5 ),
    axis.text.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA)
  )


## 그래프_산업계_업소수 -----
오염원현황 %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(분류 == "산업계_업소수") %>% 
  # 단위유역을 오염원 수치 기준으로 내림차순 정렬 
  mutate(단위유역 = fct_reorder(단위유역, `2023`, .desc = T)) %>%
  ggplot(aes(x = 단위유역, y = `2023`)) +
  geom_bar(stat='identity', width = 0.6, fill = "deepskyblue3") +
  # expansion() : 축 범위 확장
  scale_y_continuous(name = "산업계 업소수(개소)", 
                     expand = expansion(add = c(0, 10)), 
                     breaks = seq(0, 300, by = 50)) +
  ggtitle("산업계 업소수") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 17),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    # x축 라벨을 옆으로 눕히기
    axis.text.x = element_text(face = "bold", angle = 90, hjust = 1, vjust = 0.5 ),
    axis.text.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA)
  )

## 그래프_산업계_폐수방류량 -----
오염원현황 %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(분류 == "산업계_폐수방류량") %>% 
  # 단위유역을 오염원 수치 기준으로 내림차순 정렬 
  mutate(단위유역 = fct_reorder(단위유역, `2023`, .desc = T)) %>%
  ggplot(aes(x = 단위유역, y = `2023` / 1000)) +
  geom_bar(stat='identity', width = 0.6, fill = "deepskyblue3") +
  # expansion() : 축 범위 확장
  scale_y_continuous(name = "산업계 폐수방류량(천㎥/일)", 
                     expand = expansion(add = c(0, 0.7)), 
                     breaks = seq(0, 10, by = 1)) +
  ggtitle("산업계 폐수방류량") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 17),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    # x축 라벨을 옆으로 눕히기
    axis.text.x = element_text(face = "bold", angle = 90, hjust = 1, vjust = 0.5 ),
    axis.text.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA)
  )

## 그래프_토지계_논(답) 면적 -----
오염원현황 %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(분류 == "답") %>% 
  # 단위유역을 오염원 수치 기준으로 내림차순 정렬 
  mutate(단위유역 = fct_reorder(단위유역, `2023`, .desc = T)) %>%
  ggplot(aes(x = 단위유역, y = `2023`)) +
  geom_bar(stat='identity', width = 0.6, fill = "deepskyblue3") +
  # expansion() : 축 범위 확장
  scale_y_continuous(name = "답(㎢)", expand = expansion(add = c(0, 5)),
                     breaks = seq(0, 100, by = 20)) +
  ggtitle("논(답) 면적") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 17),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    # x축 라벨을 옆으로 눕히기
    axis.text.x = element_text(face = "bold", angle = 90, hjust = 1, vjust = 0.5 ),
    axis.text.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA)
  )


## 그래프_토지계_전(밭) 면적 -----
오염원현황 %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(분류 == "전") %>% 
  # 단위유역을 오염원 수치 기준으로 내림차순 정렬 
  mutate(단위유역 = fct_reorder(단위유역, `2023`, .desc = T)) %>%
  ggplot(aes(x = 단위유역, y = `2023`)) +
  geom_bar(stat='identity', width = 0.6, fill = "deepskyblue3") +
  # expansion() : 축 범위 확장
  scale_y_continuous(name = "전(㎢)", expand = expansion(add = c(0, 6)),
                     breaks = seq(0, 120, by = 20)) +
  ggtitle("전(밭) 면적") +
  theme_bw(base_family = "notosanskr", base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 17),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    # x축 라벨을 옆으로 눕히기
    axis.text.x = element_text(face = "bold", angle = 90, hjust = 1, vjust = 0.5 ),
    axis.text.y = element_text(face = "bold"),
    panel.border = element_rect(linewidth = 0.5, fill = NA)
  )

dev.off()
