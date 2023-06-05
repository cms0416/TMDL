################################################################################
# 관련 패키지 로드
library(tidyverse)
library(data.table)
library(readxl)
library(ggthemes)
library(extrafont)
library(writexl)

################################################################################


### 반올림 사용자 정의 함수 로드
source("Script/Function/round2func.R")


### Excel 파일 불러오기_readxl
obs <- read_excel("Data/총량측정망0721.xlsx")

target <- read_excel("Data/목표수질.xlsx") %>%
  filter(총량지점명 %in% c(
    "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A", "섬강B",
    "북한A", "북한B", "소양A", "인북A", "소양B", "북한C", "홍천A", "한탄A",
    "제천A", "한강B", "한강C", "달천A", "달천B", "한강D"
  ))

### obs_측정값 정리
obs <- obs %>%
  filter(총량지점명 %in% c(
    "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A", "섬강B",
    "북한A", "북한B", "소양A", "인북A", "소양B", "북한C", "홍천A", "한탄A",
    "제천A", "한강B", "한강C", "달천A", "달천B", "한강D"
  )) %>%
  filter(연도 >= 2014) %>%
  filter(!is.na(BOD)) %>%
  # BOD열에서 결측치 제거(결측치가 아닌 값들만 필터)
  select(총량지점명, 일자, BOD, TP, TOC, 유량, 연도)

# 수계 순서에 맞춰 데이터 순서 조정(지점 추가 및 변경 시 수정)
총량지점명 <- c(
  "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A", "섬강B",
  "북한A", "북한B", "소양A", "인북A", "소양B", "북한C", "홍천A", "한탄A",
  "제천A", "한강B", "한강C", "달천A", "달천B", "한강D"
)
ID <- c(1:22)
obsrank <- data.frame(총량지점명, ID)
obs <- obs %>%
  left_join(obsrank, by = "총량지점명") %>%
  arrange(ID) %>%
  select(ID, everything()) # 열 순서 조정("ID"를 첫번째로)

# obs_월 및 계절 추가_lubridate, dplyr
obs <- obs %>% mutate(월 = month(obs$일자)) %>% # 월 추가
  mutate(계절 = ifelse(월 >= 3 & 월 <= 5, "봄", # 계절 추가
    ifelse(월 >= 6 & 월 <= 8, "여름",
      ifelse(월 >= 9 & 월 <= 11, "가을", "겨울")
    )
  ))

# '계절' 요인형(factor)으로 변환 후 계절 순서로 요인 순서 변경
obs$월 <- as.factor(obs$월)
obs$계절 <- as.factor(obs$계절)
obs <- obs %>% mutate(계절 = factor(계절,
  levels = c("봄", "여름", "가을", "겨울")
))

# '총량지점명' 요인형(factor)으로 변환 후 지점 순서로 요인 순서 변경
obs$총량지점명 <- as.factor(obs$총량지점명)
obs <- obs %>% mutate(총량지점명 = factor(총량지점명, levels = c(
  "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A", "섬강B",
  "북한A", "북한B", "소양A", "인북A", "소양B", "북한C", "홍천A", "한탄A",
  "제천A", "한강B", "한강C", "달천A", "달천B", "한강D"
)))

target$총량지점명 <- as.factor(obs$총량지점명)
target <- target %>% mutate(총량지점명 = factor(총량지점명, levels = c(
  "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A", "섬강B",
  "북한A", "북한B", "소양A", "인북A", "소양B", "북한C", "홍천A", "한탄A",
  "제천A", "한강B", "한강C", "달천A", "달천B", "한강D"
)))

# 권역 추가
# obs <- obs %>% mutate(권역 = ifelse(총량지점명 %in% c("골지A", "오대A", "주천A", "평창A", "옥동A", "한강A"), "남한강",
#   ifelse(총량지점명 %in% c("섬강A", "섬강B"), "섬강",
#     ifelse(총량지점명 %in% c("북한A", "북한B", "소양A", "인북A", "소양B", "북한C"), "북한강",
#       ifelse(총량지점명 %in% "홍천A", "홍천강",
#         ifelse(총량지점명 %in% "한탄A", "한탄강", "충북")
#       )
#     )
#   )
# ))

# 목표수질 추가 및 달성여부 확인
BOD <- obs %>%
  left_join(target, by = "총량지점명") %>%
  mutate(달성여부 = ifelse(BOD <= BOD_목표수질, 1, 0)) %>%
  select(-TP, -TP_목표수질, -TOC, -TOC_목표수질)

TP <- obs %>%
  left_join(target, by = "총량지점명") %>%
  mutate(달성여부 = ifelse(TP <= TP_목표수질, 1, 0)) %>%
  select(-BOD, -BOD_목표수질, -TOC, -TOC_목표수질)

TOC <- obs %>%
  left_join(target, by = "총량지점명") %>%
  mutate(달성여부 = ifelse(TOC <= TOC_목표수질, 1, 0)) %>%
  select(-BOD, -BOD_목표수질, -TP, -TP_목표수질)

# 지점별 연평균 계산_dplyr, tidyr
BOD_ymean <- obs %>%
  group_by(총량지점명, 연도) %>% # 지점별, 연도별 분리
  summarise(BOD = round2(mean(`BOD`, na.rm = TRUE), 1)) %>% # BOD 연평균(소수점 1자리로 표시)
  spread(연도, BOD) # # # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)_tidyr

TP_ymean <- obs %>%
  group_by(총량지점명, 연도) %>% # 지점별, 연도별 분리
  summarise(TP = round2(mean(`TP`, na.rm = TRUE), 3)) %>% # TP 연평균(소수점 3자리로 표시)
  spread(연도, TP) # # # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)_tidyr

TOC_ymean <- obs %>%
  group_by(총량지점명, 연도) %>% # 지점별, 연도별 분리
  summarise(TOC = round2(mean(`TOC`, na.rm = TRUE), 1)) %>% # TP 연평균(소수점 3자리로 표시)
  spread(연도, TOC) # # # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)_tidyr

####################################################################################################################################
BOD_group0 <- data.frame()
TP_group0 <- data.frame()
TOC_group0 <- data.frame()

n <- max(obs$연도) - 2

for (i in 2014:n) {
  ## BOD -----------------------------------------------------------------------------------------------------------------------
  assign( # assign(변수명, 값) 변수명에 값 할당
    paste0("BOD_", i - 2000, i - 1998), # 변수명 지정
    BOD %>%
      group_by(총량지점명) %>%
      # 지점별 그룹지정
      filter(연도 %in% c(i, i + 1, i + 2)) %>%
      # 해당연도만 추출
      mutate(BOD_RANK = rank(BOD, ties.method = c("first"))) %>%
      # 데이터 순위 매기기(오름차순/동일값은 앞에 있는 값이 우선순위)
      mutate(Total = n()) %>%
      # 전체 데이터 갯수 세기
      mutate(달성률만족횟수 = ceiling(Total * 0.625)) %>%
      # 달성률 62.5% 만족하는 데이터 순위 확인
      mutate(달성기준수질 = ifelse(BOD_RANK == 달성률만족횟수, BOD, 0)) %>%
      # 달성률 62.5% 만족하는 데이터 입력(데이터가 없는 경우 0)
      mutate(목표달성횟수 = sum(달성여부)) %>%
      mutate(달성률 = round2((목표달성횟수 / Total) * 100, 1)) %>%
      mutate(평가기간 = ifelse(달성기준수질 != 0, paste0(i - 2000, "~", i - 1998), 0)) # 데이터가 있는 행에만 평가기간 입력
  ) -> BOD_temp

  BOD_group0 <- rbind(BOD_group0, BOD_temp)

  ## TP -----------------------------------------------------------------------------------------------------------------------
  assign(
    paste0("TP_", i - 2000, i - 1998),
    TP %>%
      group_by(총량지점명) %>% # 지점별 그룹지정
      filter(연도 %in% c(i, i + 1, i + 2)) %>% # 해당연도만 추출
      mutate(TP_RANK = rank(TP, ties.method = c("first"))) %>% # 데이터 순위 매기기(오름차순/동일값은 앞에 있는 값이 우선순위)
      mutate(Total = n()) %>% # 전체 데이터 갯수 세기
      mutate(달성률만족횟수 = ceiling(Total * 0.625)) %>% # 달성률 62.5% 만족하는 데이터 순위 확인
      mutate(달성기준수질 = ifelse(TP_RANK == 달성률만족횟수, TP, 0)) %>% # 달성률 62.5% 만족하는 데이터 입력(데이터가 없는 경우 0)
      mutate(목표달성횟수 = sum(달성여부)) %>%
      mutate(달성률 = round2((목표달성횟수 / Total) * 100, 1)) %>%
      mutate(평가기간 = ifelse(달성기준수질 != 0, paste0(i - 2000, "~", i - 1998), 0)) # 데이터가 있는 행에만 평가기간 입력
  ) -> TP_temp

  TP_group0 <- rbind(TP_group0, TP_temp)

  ## TOC -----------------------------------------------------------------------------------------------------------------------
  assign(
    paste0("TOC_", i - 2000, i - 1998),
    TOC %>%
      group_by(총량지점명) %>% # 지점별 그룹지정
      filter(연도 %in% c(i, i + 1, i + 2)) %>% # 해당연도만 추출
      mutate(TOC_RANK = rank(TOC, ties.method = c("first"))) %>% # 데이터 순위 매기기(오름차순/동일값은 앞에 있는 값이 우선순위)
      mutate(Total = n()) %>% # 전체 데이터 갯수 세기
      mutate(달성률만족횟수 = ceiling(Total * 0.625)) %>% # 달성률 62.5% 만족하는 데이터 순위 확인
      mutate(달성기준수질 = ifelse(TOC_RANK == 달성률만족횟수, TOC, 0)) %>% # 달성률 62.5% 만족하는 데이터 입력(데이터가 없는 경우 0)
      mutate(목표달성횟수 = sum(달성여부)) %>%
      mutate(달성률 = round2((목표달성횟수 / Total) * 100, 1)) %>%
      mutate(평가기간 = ifelse(달성기준수질 != 0, paste0(i - 2000, "~", i - 1998), 0)) # 데이터가 있는 행에만 평가기간 입력
  ) -> TOC_temp

  TOC_group0 <- rbind(TOC_group0, TOC_temp)
}

####################################################################################################################################


# 자료 정리
BOD_group <- BOD_group0 %>%
  filter(평가기간 != 0) %>%
  select(권역, 총량지점명, 평가기간, BOD_목표수질, 달성률, 달성기준수질)

TP_group <- TP_group0 %>%
  filter(평가기간 != 0) %>%
  select(권역, 총량지점명, 평가기간, TP_목표수질, 달성률, 달성기준수질)

TOC_group <- TOC_group0 %>%
  filter(평가기간 != 0) %>%
  select(권역, 총량지점명, 평가기간, TOC_목표수질, 달성률, 달성기준수질)

BOD_group$달성률 <- paste0(sprintf("%.1f", BOD_group$달성률), "%")
TP_group$달성률 <- paste0(sprintf("%.1f", TP_group$달성률), "%")
TOC_group$달성률 <- paste0(sprintf("%.1f", TOC_group$달성률), "%")

# 달성률 정리
BOD_ach.rate <- BOD_group %>%
  select(-달성기준수질, -권역, -BOD_목표수질) %>%
  spread(평가기간, 달성률)

TP_ach.rate <- TP_group %>%
  select(-달성기준수질, -권역, -TP_목표수질) %>%
  spread(평가기간, 달성률)

TOC_ach.rate <- TOC_group %>%
  select(-달성기준수질, -권역, -TOC_목표수질) %>%
  spread(평가기간, 달성률)

# 열이름에 접미사 "달성률" 추가 (1열(총량지정명)은 제외)
colnames(BOD_ach.rate)[2:7] <- paste(colnames(BOD_ach.rate)[2:7], " 달성률")
colnames(TP_ach.rate)[2:7] <- paste(colnames(TP_ach.rate)[2:7], " 달성률")
colnames(TOC_ach.rate)[2:7] <- paste(colnames(TOC_ach.rate)[2:7], " 달성률")

# 달성기준수질 정리
BOD_ach.conc <- BOD_group %>%
  select(-달성률, -권역, -BOD_목표수질) %>%
  spread(평가기간, 달성기준수질)

TP_ach.conc <- TP_group %>%
  select(-달성률, -권역, -TP_목표수질) %>%
  spread(평가기간, 달성기준수질)

TOC_ach.conc <- TOC_group %>%
  select(-달성률, -권역, -TOC_목표수질) %>%
  spread(평가기간, 달성기준수질)

# 열이름에 접미사 "달성기준수질" 추가 (1열(총량지정명)은 제외)
colnames(BOD_ach.conc)[2:7] <- paste(colnames(BOD_ach.conc)[2:7], " 달성기준수질")
colnames(TP_ach.conc)[2:7] <- paste(colnames(TP_ach.conc)[2:7], " 달성기준수질")
colnames(TOC_ach.conc)[2:7] <- paste(colnames(TOC_ach.conc)[2:7], " 달성기준수질")

# 목표수질, 연평균수질, 달성률, 달성기준수질 데이터 합쳐서 정리
BOD_assessment <- target %>%
  select(-TP_목표수질, -TOC_목표수질) %>%
  left_join(BOD_ymean, by = "총량지점명") %>%
  left_join(BOD_ach.rate, by = "총량지점명") %>%
  left_join(BOD_ach.conc, by = "총량지점명") %>%
  arrange(총량지점명)

TP_assessment <- target %>%
  select(-BOD_목표수질, -TOC_목표수질) %>%
  left_join(TP_ymean, by = "총량지점명") %>%
  left_join(TP_ach.rate, by = "총량지점명") %>%
  left_join(TP_ach.conc, by = "총량지점명") %>%
  arrange(총량지점명)

TOC_assessment <- target %>%
  select(-BOD_목표수질, -TP_목표수질) %>%
  left_join(TOC_ymean, by = "총량지점명") %>%
  left_join(TOC_ach.rate, by = "총량지점명") %>%
  left_join(TOC_ach.conc, by = "총량지점명") %>%
  arrange(총량지점명)

# 엑셀 파일 내보내기_writexl
write_xlsx(BOD_assessment, path = "Output/Data/달성률평가_BOD.xlsx")
write_xlsx(TP_assessment, path = "Output/Data/달성률평가_TP.xlsx")
write_xlsx(TOC_assessment, path = "Output/Data/달성률평가_TOC.xlsx")


####################################################################################################################################

## ------------------
##      그래프
## ------------------

TP_plot <- TP_1921 %>%
  filter(총량지점명 == "한강A")

## 유역별 T-P 그래프
TP_plot %>%
  ggplot() +
  geom_point(aes(x = TP_RANK, y = TP, fill = 계절), color = "black", shape = 21, alpha = 0.6, size = 3) +
  scale_y_log10(limits = c(0.001, 1), expand = c(0, 0)) +
  geom_hline(aes(yintercept = TP_목표수질), colour = "red", linetype = "dashed", size = 0.7) +
  geom_point(aes(x = 30, y = TP_목표수질), alpha = 0.0) +
  geom_text(aes(
    x = 45, y = TP_목표수질,
    label = paste("목표수질 :", sprintf("%.3f", TP_목표수질), "mg/L,", "달성률 :", sprintf("%.1f", 달성률), "%")
  ),
  vjust = -0.5, family = "NanumBarunGothic", size = 5
  ) +
  labs(title = paste("'19~'21 ", 총량지점명, " T-P (계절별)"), x = "자료순위", y = "T-P (mg/L)") +
  theme_bw() +
  theme(
    plot.title = element_text(family = "NanumBarunGothic", size = 17, face = "bold", hjust = 0.5), # 그래프 제목 설정
    axis.title = element_text(family = "NanumBarunGothic", size = 13, face = "bold"), # 축 제목 설정
    axis.text.x = element_text(size = 12), # x축 수치 사이즈
    axis.text.y = element_text(size = 12), # y축 수치 사이즈
    legend.title = element_text(family = "NanumBarunGothic", size = 10, face = "bold"),
    legend.text = element_text(family = "NanumBarunGothic", size = 10),
    legend.position = c(0.20, 0.90), # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

ggsave(filename = "한강A_TP.jpg", scale = 1.3, width = 18, height = 11, dpi = 600, units = "cm")

## 남한강수계 T-P 그래프
TP_1921 %>%
  filter(권역 == "남한강") %>%
  ggplot() +
  geom_point(aes(x = TP_RANK, y = TP, fill = 계절), color = "black", shape = 21, alpha = 0.6, size = 3) +
  scale_y_log10(limits = c(0.001, 1), expand = c(0, 0)) +
  geom_hline(aes(yintercept = TP_목표수질), colour = "red", linetype = "dashed", size = 0.7) +
  geom_point(aes(x = 25, y = TP_목표수질), alpha = 0.0) +
  geom_text(aes(x = 5, y = TP_목표수질, label = paste("목표수질 :", sprintf("%.3f", TP_목표수질), "mg/L")),
    vjust = -2, hjust = 0, family = "NanumBarunGothic", size = 5
  ) +
  geom_text(aes(x = 5, y = TP_목표수질, label = paste("달성률 :", sprintf("%.1f", 달성률), "%")),
    vjust = -0.5, hjust = 0, family = "NanumBarunGothic", size = 5
  ) +
  labs(title = "'19~'21 남한강수계 T-P (계절별)", x = "자료순위", y = "T-P (mg/L)") +
  theme_bw() +
  theme(
    plot.title = element_text(family = "NanumBarunGothic", size = 17, face = "bold", hjust = 0.5), # 그래프 제목 설정
    axis.title = element_text(family = "NanumBarunGothic", size = 13, face = "bold"), # 축 제목 설정
    axis.text.x = element_text(size = 12), # x축 수치 사이즈
    axis.text.y = element_text(size = 12), # y축 수치 사이즈
    legend.title = element_text(family = "NanumBarunGothic", size = 10, face = "bold"),
    legend.text = element_text(family = "NanumBarunGothic", size = 10),
    legend.position = c(0.18, 0.95), # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5),
    strip.text = element_text(family = "NanumBarunGothic", size = 13, face = "bold")
  ) +
  facet_wrap(~총량지점명, ncol = 2, scale = "free_x")

ggsave(filename = "Graph_남한강수계_TP.jpg", path = "Output/Plot", scale = 1.3, width = 21, height = 29.7, dpi = 600, units = "cm")

## 북한강수계 T-P 그래프
TP_1921 %>%
  filter(권역 == "북한강") %>%
  ggplot() +
  geom_point(aes(x = TP_RANK, y = TP, fill = 계절), color = "black", shape = 21, alpha = 0.6, size = 3) +
  scale_y_log10(limits = c(0.001, 1), expand = c(0, 0)) +
  geom_hline(aes(yintercept = TP_목표수질), colour = "red", linetype = "dashed", size = 0.7) +
  geom_point(aes(x = 25, y = TP_목표수질), alpha = 0.0) +
  geom_text(aes(x = 5, y = TP_목표수질, label = paste("목표수질 :", sprintf("%.3f", TP_목표수질), "mg/L")),
    vjust = -2, hjust = 0, family = "NanumBarunGothic", size = 5
  ) +
  geom_text(aes(x = 5, y = TP_목표수질, label = paste("달성률 :", sprintf("%.1f", 달성률), "%")),
    vjust = -0.5, hjust = 0, family = "NanumBarunGothic", size = 5
  ) +
  labs(title = "'19~'21 북한강수계 T-P (계절별)", x = "자료순위", y = "T-P (mg/L)") +
  theme_bw() +
  theme(
    plot.title = element_text(family = "NanumBarunGothic", size = 17, face = "bold", hjust = 0.5), # 그래프 제목 설정
    axis.title = element_text(family = "NanumBarunGothic", size = 13, face = "bold"), # 축 제목 설정
    axis.text.x = element_text(size = 12), # x축 수치 사이즈
    axis.text.y = element_text(size = 12), # y축 수치 사이즈
    legend.title = element_text(family = "NanumBarunGothic", size = 10, face = "bold"),
    legend.text = element_text(family = "NanumBarunGothic", size = 10),
    legend.position = c(0.18, 0.95), # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5),
    strip.text = element_text(family = "NanumBarunGothic", size = 13, face = "bold")
  ) +
  facet_wrap(~총량지점명, ncol = 2, scale = "free_x")

ggsave(filename = "Graph_북한강수계_TP.jpg", path = "Output/Plot", scale = 1.3, width = 21, height = 29.7, dpi = 600, units = "cm")

## 섬강수계 T-P 그래프
TP_1921 %>%
  filter(권역 == "섬강") %>%
  ggplot() +
  geom_point(aes(x = TP_RANK, y = TP, fill = 계절), color = "black", shape = 21, alpha = 0.6, size = 3) +
  scale_y_log10(limits = c(0.001, 1), expand = c(0, 0)) +
  geom_hline(aes(yintercept = TP_목표수질), colour = "red", linetype = "dashed", size = 0.7) +
  geom_point(aes(x = 25, y = TP_목표수질), alpha = 0.0) +
  geom_text(aes(x = 5, y = TP_목표수질, label = paste("목표수질 :", sprintf("%.3f", TP_목표수질), "mg/L")),
    vjust = -2, hjust = 0, family = "NanumBarunGothic", size = 5
  ) +
  geom_text(aes(x = 5, y = TP_목표수질, label = paste("달성률 :", sprintf("%.1f", 달성률), "%")),
    vjust = -0.5, hjust = 0, family = "NanumBarunGothic", size = 5
  ) +
  labs(title = "'19~'21 섬강수계 T-P (계절별)", x = "자료순위", y = "T-P (mg/L)") +
  theme_bw() +
  theme(
    plot.title = element_text(family = "NanumBarunGothic", size = 17, face = "bold", hjust = 0.5), # 그래프 제목 설정
    axis.title = element_text(family = "NanumBarunGothic", size = 13, face = "bold"), # 축 제목 설정
    axis.text.x = element_text(size = 12), # x축 수치 사이즈
    axis.text.y = element_text(size = 12), # y축 수치 사이즈
    legend.title = element_text(family = "NanumBarunGothic", size = 10, face = "bold"),
    legend.text = element_text(family = "NanumBarunGothic", size = 10),
    legend.position = c(0.15, 0.90), # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5),
    strip.text = element_text(family = "NanumBarunGothic", size = 13, face = "bold")
  ) +
  facet_wrap(~총량지점명, ncol = 2, scale = "free_x")

ggsave(filename = "Graph_섬강수계_TP.jpg", path = "Output/Plot", scale = 1.3, width = 21, height = 11, dpi = 600, units = "cm")

## 홍천, 한탄 T-P 그래프
TP_1921 %>%
  filter(총량지점명 %in% c("홍천A", "한탄A")) %>%
  ggplot() +
  geom_point(aes(x = TP_RANK, y = TP, fill = 계절), color = "black", shape = 21, alpha = 0.6, size = 3) +
  scale_y_log10(limits = c(0.001, 1), expand = c(0, 0)) +
  geom_hline(aes(yintercept = TP_목표수질), colour = "red", linetype = "dashed", size = 0.7) +
  geom_point(aes(x = 25, y = TP_목표수질), alpha = 0.0) +
  geom_text(aes(x = 5, y = TP_목표수질, label = paste("목표수질 :", sprintf("%.3f", TP_목표수질), "mg/L")),
    vjust = -2, hjust = 0, family = "NanumBarunGothic", size = 5
  ) +
  geom_text(aes(x = 5, y = TP_목표수질, label = paste("달성률 :", sprintf("%.1f", 달성률), "%")),
    vjust = -0.5, hjust = 0, family = "NanumBarunGothic", size = 5
  ) +
  labs(title = "'19~'21 홍천강 / 한탄강 수계 T-P (계절별)", x = "자료순위", y = "T-P (mg/L)") +
  theme_bw() +
  theme(
    plot.title = element_text(family = "NanumBarunGothic", size = 17, face = "bold", hjust = 0.5), # 그래프 제목 설정
    axis.title = element_text(family = "NanumBarunGothic", size = 13, face = "bold"), # 축 제목 설정
    axis.text.x = element_text(size = 12), # x축 수치 사이즈
    axis.text.y = element_text(size = 12), # y축 수치 사이즈
    legend.title = element_text(family = "NanumBarunGothic", size = 10, face = "bold"),
    legend.text = element_text(family = "NanumBarunGothic", size = 10),
    legend.position = c(0.15, 0.95), # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5),
    strip.text = element_text(family = "NanumBarunGothic", size = 13, face = "bold")
  ) +
  facet_wrap(~총량지점명, ncol = 2)

ggsave(filename = "Graph_홍천강,한탄강수계_TP.jpg", path = "Output/Plot", scale = 1.3, width = 21, height = 11, dpi = 600, units = "cm")

## 홍천A 그래프
TP_1921 %>%
  filter(총량지점명 == "홍천A") %>%
  ggplot() +
  geom_point(aes(x = TP_RANK, y = TP, fill = 계절), color = "black", shape = 21, alpha = 0.6, size = 3) +
  scale_y_log10(limits = c(0.001, 1), expand = c(0, 0)) +
  geom_hline(aes(yintercept = TP_목표수질), colour = "red", linetype = "dashed", size = 0.7) +
  geom_point(aes(x = 30, y = TP_목표수질), alpha = 0.0) +
  geom_text(aes(x = 5, y = TP_목표수질, label = paste("목표수질 :", sprintf("%.3f", TP_목표수질), "mg/L")),
    vjust = -2, hjust = 0, family = "NanumBarunGothic", size = 5
  ) +
  geom_text(aes(x = 5, y = TP_목표수질, label = paste("달성률 :", sprintf("%.1f", 달성률), "%")),
    vjust = -0.5, hjust = 0, family = "NanumBarunGothic", size = 5
  ) +
  labs(title = paste("'19~'21 홍천A T-P (계절별)"), x = "자료순위", y = "T-P (mg/L)") +
  theme_bw() +
  theme(
    plot.title = element_text(family = "NanumBarunGothic", size = 17, face = "bold", hjust = 0.5), # 그래프 제목 설정
    axis.title = element_text(family = "NanumBarunGothic", size = 13, face = "bold"), # 축 제목 설정
    axis.text.x = element_text(size = 12), # x축 수치 사이즈
    axis.text.y = element_text(size = 12), # y축 수치 사이즈
    legend.title = element_text(family = "NanumBarunGothic", size = 10, face = "bold"),
    legend.text = element_text(family = "NanumBarunGothic", size = 10),
    legend.position = c(0.20, 0.90), # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

ggsave(filename = "Graph_홍천A_TP.jpg", path = "Output/Plot", scale = 1.3, width = 18, height = 11, dpi = 600, units = "cm")

## 한탄A 그래프
TP_1921 %>%
  filter(총량지점명 == "한탄A") %>%
  ggplot() +
  geom_point(aes(x = TP_RANK, y = TP, fill = 계절), color = "black", shape = 21, alpha = 0.6, size = 3) +
  scale_y_log10(limits = c(0.001, 1), expand = c(0, 0)) +
  geom_hline(aes(yintercept = TP_목표수질), colour = "red", linetype = "dashed", size = 0.7) +
  geom_point(aes(x = 30, y = TP_목표수질), alpha = 0.0) +
  geom_text(aes(x = 5, y = TP_목표수질, label = paste("목표수질 :", sprintf("%.3f", TP_목표수질), "mg/L")),
    vjust = -2, hjust = 0, family = "NanumBarunGothic", size = 5
  ) +
  geom_text(aes(x = 5, y = TP_목표수질, label = paste("달성률 :", sprintf("%.1f", 달성률), "%")),
    vjust = -0.5, hjust = 0, family = "NanumBarunGothic", size = 5
  ) +
  labs(title = paste("'19~'21 한탄A T-P (계절별)"), x = "자료순위", y = "T-P (mg/L)") +
  theme_bw() +
  theme(
    plot.title = element_text(family = "NanumBarunGothic", size = 17, face = "bold", hjust = 0.5), # 그래프 제목 설정
    axis.title = element_text(family = "NanumBarunGothic", size = 13, face = "bold"), # 축 제목 설정
    axis.text.x = element_text(size = 12), # x축 수치 사이즈
    axis.text.y = element_text(size = 12), # y축 수치 사이즈
    legend.title = element_text(family = "NanumBarunGothic", size = 10, face = "bold"),
    legend.text = element_text(family = "NanumBarunGothic", size = 10),
    legend.position = c(0.20, 0.90), # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )

ggsave(filename = "Graph_한탄A_TP.jpg", path = "Output/Plot", scale = 1.3, width = 18, height = 11, dpi = 600, units = "cm")
