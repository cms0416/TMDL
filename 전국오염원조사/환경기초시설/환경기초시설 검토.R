#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(data.table)
library(readxl)
library(writexl)

### 반올림 사용자 정의 함수 로드
source("Script_R/Function/func_round2.R")



##########  자료 정리  #########################################################

## *****  파일 불러오기  *******************************************************

# 데이터 경로지정 및 데이터 목록
dir <- ("전국오염원조사/환경기초시설")
files <- list.files(dir, pattern = "*.xlsx")

# 데이터 불러오기 및 합치기
stp_현황_원본 <- tibble()

for (file in files) {
  print(file)
  temp <- read_excel(file.path(dir, file), skip = 3, col_names = F) %>%
    # str_sub(문자열, 시작위치, 끝위치) : 연도 추출
    mutate(연도 = str_sub(file, 1, 4), .before = 1)
  stp_현황_원본 <- bind_rows(stp_현황_원본, temp)
}
## *****************************************************************************

## 환경기초시설 유역 현황
stp_유역 <- read_excel(
  "전국오염원조사/환경기초시설/환경기초시설 현황/환경기초시설_현황.xlsx"
  ) %>% 
  select(처리시설명, 처리시설코드, 단위유역) %>% 
  rename(시설코드 = 처리시설코드)


## 현황 데이터 정리
stp_현황_정리 <- stp_현황_원본 %>%
  mutate(across(c(18:20), ~ replace(., is.na(.), 0))) %>%
  mutate(across(c(18:20), as.numeric)) %>%
  # 처리시설명 기준으로 추가 자료 결합
  rename(시설코드 = 4) %>%
  left_join(stp_유역 %>% select(시설코드, 단위유역), by = "시설코드") %>%
  # 기타자료 추가
  mutate(시설용량 = `...18` + `...19` + `...20`) %>%
  select(
    연도, 2:11, 시설용량, 26, 단위유역
  ) %>%
  set_names(
    c(
      "연도", "법정동코드", "처리시설명", "시설코드","구분", 
      "시도", "시군구", "읍면동", "리", "본번", "부번", "시설용량", 
      "가동개시일자", "단위유역"
    )
  ) %>%
  mutate(종류 = str_sub(시설코드, 7, 7)) %>%
  mutate(종류 = case_when(
    종류 == "W" ~ "하수처리시설", # 500톤 이상 공공하수처리시설
    종류 == "V" ~ "마을하수도", # 500톤 미만 소규모 하수처리시설
    종류 == "A" ~ "농공단지폐수처리시설",
    종류 == "I" ~ "산업단지폐수처리시설",
    종류 == "F" ~ "분뇨처리시설",
    종류 == "S" ~ "축산폐수처리시설",
    종류 == "H" ~ "오수처리시설"
  ))



### 환경기초시설방류량 -------------------------------------------------

## 환경기초시설 현황 자료 불러오기
stp_방류량_원본 <- tibble()

for (file in files) {
  print(file)
  temp <- read_excel(file.path(dir, file), sheet = 5, skip = 3, col_names = F)
  stp_방류량_원본 <- bind_rows(stp_방류량_원본, temp)
}


stp_방류량_정리 <- stp_방류량_원본 %>%
  select(1, 2, 4, 3, 7, 10, 11, 13, 14) %>%
  mutate_at(vars(5:9), as.numeric) %>%
  mutate(재이용유량 = 0) %>%
  set_names(
    c(
      "처리시설명", "시설코드", "방류구번호", "운영일", "방류유량",
      "방류농도BOD", "방류농도COD", "방류농도TN", "방류농도TP", "재이용유량"
    )
  ) %>%
  mutate(
    운영일 = as.Date(운영일),
    운영년도 = year(운영일),
    운영월 = month(운영일),
    운영일 = day(운영일)
  ) %>%
  relocate(c(운영년도, 운영월), .after = 방류구번호) %>% 
  distinct()

##### 365일 날짜 데이터 생성 ---------------------------------------------------
## 2014년 ~ 2024년
date <- data.frame(date = seq(
  as.Date("2014/01/01", "%Y/%m/%d"), 
  as.Date("2024/12/31", "%Y/%m/%d"),
  1
)) %>%
  mutate(
    운영년도 = year(date),
    운영월 = month(date),
    운영일 = day(date)
  ) %>%
  select(-date)

### 365일 데이터 존재 여부 확인 및 없는 경우 추가 ++++++++++++++++++++++++++++++
## 측정데이터 갯수 확인
stp_방류량_정리 %<>%
  left_join(stp_방류량_정리 %>% count(처리시설명, 방류구번호, 운영년도),
            by = c("처리시설명", "방류구번호", "운영년도")
  )
  

## 365일 자료가 없는 시설은 365일로 확장
stp_방류량_정리_365일 <- stp_방류량_정리 %>%
  filter(n != 365, n != 366) %>%  # 윤년 고려 366일도 제외
  select(처리시설명, 시설코드, 방류구번호, 운영년도) %>%
  distinct() %>%
  left_join(date, by = "운영년도") %>%
  left_join(stp_방류량_정리,
            by = c(
              "처리시설명", "시설코드", "방류구번호",
              "운영년도", "운영월", "운영일"
            )
  )

## 365일 자료가 없는 시설은 제외 후 날짜 확장한 자료로 대체
stp_방류량_정리 %<>%
  filter(n == 365 | n == 366) %>%
  rbind(stp_방류량_정리_365일) %>%
  select(-n)
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 환경기초시설 방류량 연평균값 산정(미측정시 대입 용도)
stp_방류량_정리_mean <- stp_방류량_정리 %>%
  group_by(처리시설명, 방류구번호, 운영년도) %>%
  summarise(across(c(방류유량:방류농도TP), list(mean = ~ mean(., na.rm = T))),
            .groups = "drop")

## 방류 유량 및 농도 미측정시 평균값으로 입력
stp_방류량_최종 <- stp_방류량_정리 %>%
  left_join(stp_방류량_정리_mean, 
            by = c("처리시설명", "방류구번호", "운영년도")) %>%
  mutate(
    방류농도BOD = ifelse(is.na(방류농도BOD), 방류농도BOD_mean, 방류농도BOD),
    방류농도COD = ifelse(is.na(방류농도COD), 방류농도COD_mean, 방류농도COD),
    방류농도TN = ifelse(is.na(방류농도TN), 방류농도TN_mean, 방류농도TN),
    방류농도TP = ifelse(is.na(방류농도TP), 방류농도TP_mean, 방류농도TP),
    # 유량의 경우 수질측정값은 있는데 유량이 0인 경우도 평균값으로 입력
    방류유량 = ifelse(is.na(방류유량),
                  방류유량_mean,
                  ifelse(방류유량 == 0 & 방류농도BOD > 0, 방류유량_mean, 방류유량)
    )
  ) %>%
  mutate_at(vars(방류농도COD), ~ replace(., is.na(.), 0)) %>%
  select(1, 3:12) %>%
  rowid_to_column(var = "ID") %>%
  mutate_at(vars(ID, 재이용유량), as.character)


