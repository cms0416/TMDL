################################################################################
## 라이브러리 로드
library(tidyverse)
library(data.table)
library(readxl)
library(writexl)

# 반올림 사용자 정의 함수 로드
source("Script/Function/round2func.R")
################################################################################

## 할당부하량 및 오염원별 배출부하량 불러오기
data_bod <- read_excel("Data/배출부하량.xlsx")
data_tp <- read_excel("Data/배출부하량.xlsx", sheet = 2)
pol_bod <- read_excel("Data/배출부하량.xlsx", sheet = 3)
pol_tp <- read_excel("Data/배출부하량.xlsx", sheet = 4)

data <- rbind(data_bod, data_tp)
pol <- rbind(pol_bod, pol_tp)


## 할당부하량 데이터 정리
data <- data %>%
  select(1:4, 33, 34, 37, 38, 39, 43) %>%
  filter(시도 == "강원도", 단위유역 != "소계") %>%
  group_by(대상물질, 시군, 단위유역) %>%
  summarise_at(vars(할당량_점:할당지역개발_비점), funs(sum)) %>%
  pivot_longer(cols = c(할당량_점:할당지역개발_비점), names_to = "구분", values_to = "부하량") %>%
  separate(구분, into = c("구분", "점비점"), sep = "_")

## 오염원별 배출부하량 데이터 정리
pol <- pol %>%
  select(1:6, 18, 20) %>%
  mutate(부하량 = `2030년` - `2030년 개발`) %>% 
  filter(시도 == "강원도", 시군 != "소계", 오염원 != "총계") %>%
  group_by(대상물질, 시군, 단위유역, 오염원, 점비점) %>%
  summarise(부하량 = sum(부하량)) %>%
  setnames(., "오염원", "구분")

## 오염원별 배출부하량 합계 계산
pol_sum <- pol %>% 
  group_by(대상물질, 시군, 단위유역, 점비점) %>% 
  summarise(합계 = sum(부하량))

## 잔여량 계산(할당오염원 - 오염원별 배출부하량 합계)
jan <- data %>% filter(구분 == "할당오염원") %>% 
  left_join(pol_sum, by = c("대상물질", "시군", "단위유역", "점비점")) %>% 
  mutate(잔여량 = round2(부하량 - 합계, 3), 구분 = "잔여량") %>% select( -부하량, -합계) %>%
  rename(부하량 = 잔여량)

## 배출부하량, 오염원, 잔여량 데이터 합치기
total <- rbind(data, pol, jan)

## 시군별 합계 계산
total_sum <- total %>% 
  group_by(대상물질, 시군, 구분, 점비점) %>% 
  summarise(부하량 = sum(부하량)) %>% 
  mutate(단위유역 = "합계")

## 단위유역별 합계 합치기
total <- rbind(total, total_sum)

## 최종 자료 정리
total <- total %>%
  mutate_at(vars(시군, 단위유역, 구분, 점비점), as.factor) %>%
  mutate(
    시군 = factor(시군, levels = c(
      "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군",
      "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군",
      "양구군", "인제군", "고성군"
    )),
    단위유역 = factor(단위유역, levels = c(
      "합계", "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A", "섬강B",
      "북한A", "북한B", "소양A", "인북A", "소양B", "북한C", "홍천A", "한탄A",
      "한강B", "제천A", "한강D", "북한D", "한탄B", "임진A", "낙본A"
    )),
    구분 = factor(구분, levels = c(
      "할당량", "할당지역개발", "할당오염원",
      "생활계", "축산계", "산업계",
      "토지계", "양식계", "매립계", "잔여량"
    )),
    점비점 = factor(점비점, levels = c("점", "비점"))
  ) %>%
  arrange(대상물질, 시군, 단위유역, 점비점, 구분)


### 파일 내보내기
write_xlsx(total, path = "Output/Data/오염원현황 정리_시군기준.xlsx")
