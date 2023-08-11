#####  라이브러리 로드  #############################################################
library(tidyverse)
library(magrittr)
library(data.table)
library(readxl)
library(writexl)

### 반올림 사용자 정의 함수 로드
source("Script_R/Function/round2func.R")

### 주소DB 관련 자료 로드
source("Script_R/주소 검토/주소DB 정리2.R")


##########  당해 연도 자료(2022년) 정리  #############################################

## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/0시군제출자료 검토/생활계",
  pattern = "*.xlsx", full.names = T
)

files_기존 <- grep("기존", files, value = TRUE)
files_푸른물 <- grep("푸른물", files, value = TRUE)
files_wims <- grep("WIMS", files, value = TRUE)

# _______________________________________________________________________________
## 기존 양식 상수도 자료 합치기
waterusage_기존 <- data.frame()

for (file in files_기존) {
  print(file)
  temp <- read_excel(file, skip = 4, col_names = F, guess_max = 100000) %>%
    select(1:21) %>%
    # substr(문자열, 시작위치, 끝위치) : 시군 이름 추출
    mutate(시군 = str_sub(file, -8, -6))
  waterusage_기존 <- rbind(waterusage_기존, temp)
}

## 기존 양식 지하수 자료 합치기
waterusage_기존_지하수 <- data.frame()

for (file in files_기존) {
  print(file)
  temp <- read_excel(file, sheet = 2, skip = 4, col_names = F, guess_max = 100000)

  # 자료가 없는 경우 대비 test열(NA값 입력) 추가
  temp %<>% mutate(test = NA)

  # 자료가 있는 경우 해당열 선택 및 시군 이름 입력
  if (is.na(temp[1, 1])) {
    waterusage_기존_지하수 <- rbind(waterusage_기존_지하수, temp)
  } else {
    temp %<>% select(1:20) %>%
      # substr(문자열, 시작위치, 끝위치) : 시군 이름 추출
      mutate(시군 = str_sub(file, -8, -6))

    waterusage_기존_지하수 <- rbind(waterusage_기존_지하수, temp)
  }
}

# _______________________________________________________________________________
## wims 양식 상수도 자료 합치기
waterusage_wims <- data.frame()

for (file in files_wims) {
  print(file)
  temp <- read_excel(file, skip = 2, col_names = F, guess_max = 100000) %>%
    select(1, 3:6, 8, 9, 22:33, 46, 47) %>%
    # substr(문자열, 시작위치, 끝위치) : 시군 이름 추출
    mutate(시군 = str_sub(file, -8, -6))
  waterusage_wims <- rbind(waterusage_wims, temp)
}

## wims 양식 지하수 자료 합치기
waterusage_wims_지하수 <- data.frame()

for (file in files_wims) {
  print(file)
  temp <- read_excel(file, sheet = 2, skip = 2, col_names = F, guess_max = 100000) %>%
    select(1, 3:6, 9, 22:33, 46, 47) %>%
    # substr(문자열, 시작위치, 끝위치) : 시군 이름 추출
    mutate(시군 = str_sub(file, -8, -6))
  waterusage_wims_지하수 <- rbind(waterusage_wims_지하수, temp)
}

# _______________________________________________________________________________
## 푸른물 양식 상수도 자료 합치기
waterusage_푸른물 <- data.frame()

for (file in files_푸른물) {
  print(file)
  temp <- read_excel(file, col_names = T, guess_max = 100000) %>%
    select(1, 3:7, 14, 22, 87, 128) %>%
    # substr(문자열, 시작위치, 끝위치) : 시군 이름 추출
    mutate(시군 = str_sub(file, -8, -6))
  waterusage_푸른물 <- rbind(waterusage_푸른물, temp)
}

## 푸른물 양식 지하수 자료 합치기
waterusage_푸른물_지하수 <- data.frame()

for (file in files_푸른물) {
  print(file)
  temp <- read_excel(file, sheet = 2, col_names = T, guess_max = 100000)

  # 자료가 없는 경우 대비 test열(NA값 입력) 추가
  temp %<>% mutate(test = NA)

  # 자료가 있는 경우 해당열 선택 및 시군 이름 입력
  if (is.na(temp[1, 1])) {
    waterusage_푸른물_지하수 <- rbind(waterusage_푸른물_지하수, temp)
  } else {
    temp %<>% select(1, 3:6, 9, 12, 55) %>%
      # substr(문자열, 시작위치, 끝위치) : 시군 이름 추출
      mutate(시군 = str_sub(file, -8, -6))

    waterusage_푸른물_지하수 <- rbind(waterusage_푸른물_지하수, temp)
  }
}


## *****  데이터 정리  *********************************************************

## 기존 양식 상수도 자료 정리
waterusage_기존1 <- waterusage_기존 %>%
  # 숫자 데이터 숫자형으로 변환
  mutate(across(c(1, 8, 10:21), as.numeric)) %>%
  # 사용량 측정 자료 NA인 경우 0으로 변환
  mutate(across(c(10:21), ~ replace(., is.na(.), 0))) %>%
  # 사용량 합계(일자료) 계산 - 전체 합계 / 365일
  mutate(사용량합계 = round2(pmap_dbl(select(., 10:21), sum) / 365, 1)) %>%
  select(1, 시군, 2:9, 사용량합계) %>%
  set_names(c(
    "연도", "시군", "수용가번호", "주소1", "주소2",
    "수용가명", "상호명", "업종", "가구수", "하수도요금여부",
    "사용량합계"
  )) %>%
  mutate(구분 = "상수도")

## 기존 양식 지하수 자료 정리
waterusage_기존_지하수1 <- waterusage_기존_지하수 %>%
  mutate(하수도요금여부 = NA, .after = 8) %>%
  # 숫자 데이터 숫자형으로 변환
  mutate(across(c(1, 8, 10:21), as.numeric)) %>%
  # 사용량 측정 자료 NA인 경우 0으로 변환
  mutate(across(c(10:21), ~ replace(., is.na(.), 0))) %>%
  # 사용량 합계(일자료) 계산 - 전체 합계 / 365일
  mutate(사용량합계 = round2(pmap_dbl(select(., 10:21), sum) / 365, 1)) %>%
  select(1, 시군, 2:9, 사용량합계) %>%
  set_names(c(
    "연도", "시군", "수용가번호", "주소1", "주소2",
    "수용가명", "상호명", "업종", "가구수", "하수도요금여부",
    "사용량합계"
  )) %>%
  mutate(구분 = "지하수")


# _______________________________________________________________________________
## wims 양식 상수도 자료 정리
waterusage_wims1 <- waterusage_wims %>%
  # 숫자 데이터 숫자형으로 변환
  mutate(across(c(1, 6, 8:19), as.numeric)) %>%
  # 사용량 측정 자료 NA인 경우 0으로 변환
  mutate(across(c(8:19), ~ replace(., is.na(.), 0))) %>%
  # 사용량 합계(일자료) 계산 - 전체 합계 / 365일
  mutate(사용량합계 = round2(pmap_dbl(select(., 8:19), sum) / 365, 1)) %>%
  select(1, 시군, 2, 20, 4, 3, 21, 5:7, 사용량합계) %>%
  set_names(c(
    "연도", "시군", "수용가번호", "주소1", "주소2",
    "수용가명", "상호명", "업종", "가구수", "하수도요금여부",
    "사용량합계"
  )) %>%
  mutate(구분 = "상수도")

## wims 양식 지하수 자료 정리
waterusage_wims_지하수1 <- waterusage_wims_지하수 %>%
  mutate(하수도요금여부 = NA, .after = 6) %>%
  # 숫자 데이터 숫자형으로 변환
  mutate(across(c(1, 6, 8:19), as.numeric)) %>%
  # 사용량 측정 자료 NA인 경우 0으로 변환
  mutate(across(c(8:19), ~ replace(., is.na(.), 0))) %>%
  # 사용량 합계(일자료) 계산 - 전체 합계 / 365일
  mutate(사용량합계 = round2(pmap_dbl(select(., 8:19), sum) / 365, 1)) %>%
  select(1, 시군, 2, 20, 4, 3, 21, 5:7, 사용량합계) %>%
  set_names(c(
    "연도", "시군", "수용가번호", "주소1", "주소2",
    "수용가명", "상호명", "업종", "가구수", "하수도요금여부",
    "사용량합계"
  )) %>%
  mutate(구분 = "지하수")

# _______________________________________________________________________________
## 푸른물 양식 상수도 자료 정리
waterusage_푸른물1 <- waterusage_푸른물 %>%
  # 사용량 및 하수료 "-" 및 NA인 경우 0으로 변환
  # 숫자에서 쉼표 제거
  mutate(
    사용량합계 = ifelse(사용량합계 == "-" | is.na(사용량합계), 0, 사용량합계),
    하수료합계 = ifelse(하수료합계 == "-" | is.na(하수료합계), 0, 하수료합계),
    사용량합계 = str_remove_all(사용량합계, ","),
    하수료합계 = str_remove_all(하수료합계, ",")
  ) %>%
  # 숫자 데이터 숫자형으로 변환
  mutate(across(c(연도, 세대수, 사용량합계, 하수료합계), as.numeric)) %>%
  # 사용량 합계(일자료) 계산 - 전체 합계 / 365일
  mutate(사용량합계 = round2(사용량합계 / 365, 1)) %>%
  select(
    연도, 시군, 관리번호, 주소, 새주소, 성명, 상호, 상수업종, 세대수,
    하수료합계, 사용량합계
  ) %>%
  set_names(c(
    "연도", "시군", "수용가번호", "주소1", "주소2",
    "수용가명", "상호명", "업종", "가구수", "하수도요금여부",
    "사용량합계"
  )) %>%
  mutate(구분 = "상수도")

## 푸른물 양식 지하수 자료 정리
waterusage_푸른물_지하수1 <- waterusage_푸른물_지하수 %>%
  # 사용량 "-" 및 NA인 경우 0으로 변환
  # 숫자에서 쉼표 제거
  mutate(
    사용량합계 = ifelse(사용량합계 == "-" | is.na(사용량합계), 0, 사용량합계),
    사용량합계 = str_remove_all(사용량합계, ","),
    하수료합계 = NA,
    상호 = NA
  ) %>%
  # 숫자 데이터 숫자형으로 변환
  mutate(across(c(연도, 가구수, 사용량합계), as.numeric)) %>%
  # 사용량 합계(일자료) 계산 - 전체 합계 / 365일
  mutate(사용량합계 = round2(사용량합계 / 365, 1)) %>%
  select(
    연도, 시군, 관리번호, 주소, 새주소, 성명, 상호, 업종, 가구수,
    하수료합계, 사용량합계
  ) %>%
  set_names(c(
    "연도", "시군", "수용가번호", "주소1", "주소2",
    "수용가명", "상호명", "업종", "가구수", "하수도요금여부",
    "사용량합계"
  )) %>%
  mutate(구분 = "지하수")




# _______________________________________________________________________________
## 전체 파일 합치기
waterusage <- rbind(
  waterusage_기존1, waterusage_기존_지하수1,
  waterusage_wims1, waterusage_wims_지하수1,
  waterusage_푸른물1, waterusage_푸른물_지하수1
)


##########  주소 정리  ##############################################################


### 주소검토  -----
addr_test <- waterusage %>% select(-c(연도, 하수도요금여부)) %>%
  # filter(시군 == "화천군") %>%
  mutate(
    # 시군, 수용가번호 합쳐서 코드 생성(시군별 수용가번호 동일한 경우 방지)
    코드 = str_c(시군, 수용가번호, 구분, 업종),
    주소1수정 =
    # '00번길' 또는 '00길'앞에 띄어쓰기가 되어 있는 경우 공백 제거
      str_replace(주소1, " 번길", "번길") %>%
        str_replace(., "\\s\\d{1,}(번길)", str_extract(주소1, "\\d{1,}(번길)")) %>%
        str_replace(., "\\s\\d{1,}(길)", str_extract(주소1, "\\d{1,}(길)")) %>%
        # 동, 리 앞에 숫자가 있는 경우('XX1동', 'XX1리') 숫자 삭제
        str_replace(., "\\d{1,}(동)", "동") %>%
        str_replace(., "\\d{1,}(리)", "리") %>%
        # '00반' 삭제
        str_remove(., "\\d{1,}(반)") %>%
        str_remove(., "강원도") %>%
        str_remove(., 시군) %>%
        str_trim(),
    주소2수정 =
    # '00번길' 또는 '00길'앞에 띄어쓰기가 되어 있는 경우 공백 제거
      str_replace(주소2, " 번길", "번길") %>%
        str_replace(., "\\s\\d{1,}(번길)", str_extract(주소2, "\\d{1,}(번길)")) %>%
        str_replace(., "\\s\\d{1,}(길)", str_extract(주소2, "\\d{1,}(길)")) %>%
        # 동, 리 앞에 숫자가 있는 경우('XX1동', 'XX1리') 숫자 삭제
        str_replace(., "\\d{1,}(동)", "동") %>%
        str_replace(., "\\d{1,}(리)", "리") %>%
        # '00반' 삭제
        str_remove(., "\\d{1,}(반)") %>%
        str_remove(., "강원도") %>%
        str_remove(., 시군) %>%
        str_trim(),
    # 읍면
    읍면1 = str_extract(주소1수정, "[가-힣0-9]{1,}(읍|면)(?!읍|면|동|가|리|로|길)") %>%
      str_remove(., "\\d{1,}") %>%
      str_trim(),
    읍면2 = str_extract(주소2수정, "[가-힣0-9]{1,}(읍|면)(?!읍|면|동|가|리|로|길)") %>%
      str_remove(., "\\d{1,}") %>%
      str_trim(),
    # 읍면 확인(앞뒤로 여백 추가해서 일부만 일치하는 경우 방지)
    읍면1 = ifelse(str_detect(읍면_check, str_c(" ", 읍면1, " ")), 읍면1, NA),
    읍면2 = ifelse(str_detect(읍면_check, str_c(" ", 읍면2, " ")), 읍면2, NA),
    # 동리(주소에서 읍면 삭제 후 추출)
    동리1 = case_when(
      is.na(읍면1) & is.na(읍면2) ~ 주소1수정,
      !is.na(읍면1) & is.na(읍면2) ~ str_remove(주소1수정, 읍면1),
      is.na(읍면1) & !is.na(읍면2) ~ str_remove(주소1수정, 읍면2),
      !is.na(읍면1) & !is.na(읍면2) ~ str_remove(주소1수정, 읍면1)
    ),
    동리1 = ifelse(str_sub(str_extract(동리1, "[가-힣0-9]{1,}(동|가|리)") %>%
      str_trim(), -1, -1) == "가",
    str_extract(동리1, "[가-힣0-9]{1,}(가)(?!읍|면|동|가|리|로|길)") %>% str_trim(),
    str_extract(동리1, "[가-힣0-9]{1,}(동|리)(?!읍|면|동|가|리|로|길)") %>% str_trim()
    ),
    동리2 = case_when(
      is.na(읍면1) & is.na(읍면2) ~ 주소2수정,
      !is.na(읍면1) & is.na(읍면2) ~ str_remove(주소2수정, 읍면1),
      is.na(읍면1) & !is.na(읍면2) ~ str_remove(주소2수정, 읍면2),
      !is.na(읍면1) & !is.na(읍면2) ~ str_remove(주소2수정, 읍면2)
    ),
    동리2 = ifelse(str_sub(str_extract(동리2, "[가-힣0-9]{1,}(동|가|리)") %>%
      str_trim(), -1, -1) == "가",
    str_extract(동리2, "[가-힣0-9]{1,}(가)(?!읍|면|동|가|리|로|길)") %>% str_trim(),
    str_extract(동리2, "[가-힣0-9]{1,}(동|리)(?!읍|면|동|가|리|로|길)") %>% str_trim()
    ),
    # 동리 확인(앞뒤로 여백 추가해서 일부만 일치하는 경우 방지)
    동리1 = ifelse(str_detect(동리_check, str_c(" ", 시군, 동리1, " ")), 동리1, NA),
    동리2 = ifelse(str_detect(동리_check, str_c(" ", 시군, 동리2, " ")), 동리2, NA),
    # 도로명
    도로명1 = str_extract(주소1수정, "[가-힣A-Za-z0-9]{1,}(로|길)(?!읍|면|동|\\d{1,}가|리|로|길)"),
    도로명2 = str_extract(주소2수정, "[가-힣A-Za-z0-9]{1,}(로|길)(?!읍|면|동|\\d{1,}가|리|로|길)"),
    # 도로명 확인(앞뒤로 여백 추가해서 일부만 일치하는 경우 방지)
    도로명1 = ifelse(str_detect(도로명_check, str_c(" ", 도로명1, " ")), 도로명1, NA),
    도로명2 = ifelse(str_detect(도로명_check, str_c(" ", 도로명2, " ")), 도로명2, NA),
    # 본번/부번
    산1 = str_extract(주소1수정, "(?<=동|리|가)[:blank:]*(산)") %>% str_trim(),
    본번1 = str_extract(주소1수정, "(?<=동|리|가|산)[:blank:]*(\\d{1,})(?!로|길|호)"),
    부번1 = str_extract(주소1수정, "(?<=동|리|가|산)[:blank:]*([\\d\\-]{1,})") %>%
      str_extract(., "(?<=\\-)\\d{1,}"),
    산2 = str_extract(주소2수정, "(?<=동|리|가)[:blank:]*(산)") %>% str_trim(),
    본번2 = str_extract(주소2수정, "(?<=동|리|가|산)[:blank:]*(\\d{1,})(?!로|길|호)"),
    부번2 = str_extract(주소2수정, "(?<=동|리|가|산)[:blank:]*([\\d\\-]{1,})") %>%
      str_extract(., "(?<=\\-)\\d{1,}"),
    건물본번1 = ifelse(is.na(도로명1), NA,
      주소1수정 %>%
        str_split_i(., 도로명1, 2) %>%
        str_extract(., "\\d{1,}")
    ),
    건물부번1 = ifelse(is.na(도로명1), NA,
      주소1수정 %>%
        str_split_i(., 도로명1, 2) %>%
        str_extract(., "[0-9\\-]{1,}") %>%
        str_extract(., "(?<=\\-)\\d{1,}")
    ),
    건물본번2 = ifelse(is.na(도로명2), NA,
      주소2수정 %>%
        str_split_i(., 도로명2, 2) %>%
        str_extract(., "\\d{1,}")
    ),
    건물부번2 = ifelse(is.na(도로명2), NA,
      주소2수정 %>%
        str_split_i(., 도로명2, 2) %>%
        str_extract(., "[0-9\\-]{1,}") %>%
        str_extract(., "(?<=\\-)\\d{1,}")
    ),
    읍면 = ifelse(is.na(읍면1) | 읍면1 == "", 읍면2, 읍면1),
    동리 = ifelse(is.na(동리1) | 동리1 == "", 동리2, 동리1),
    읍면동 = str_extract(동리, "[가-힣0-9]{1,}(동|가)(?!리)"),
    리 = str_extract(동리, "[가-힣0-9]{1,}(리)"),
    읍면동 = ifelse(is.na(읍면동), 읍면, 읍면동),
    산 = ifelse(is.na(산1) | 산1 == "", 산2, 산1),
    본번 = ifelse(is.na(본번1) | 본번1 == "", 본번2, 본번1),
    부번 = ifelse(is.na(부번1) | 부번1 == "", 부번2, 부번1),
    도로명 = ifelse(is.na(도로명1) | 도로명1 == "", 도로명2, 도로명1),
    건물본번 = ifelse(is.na(건물본번1) | 건물본번1 == "", 건물본번2, 건물본번1),
    건물부번 = ifelse(is.na(건물부번1) | 건물부번1 == "", 건물부번2, 건물부번1),

    # 본번/부번 앞에 0이 있는 경우 제거
    본번 = ifelse(str_sub(본번, 1, 1) == 0, str_replace(본번, "0", ""), 본번),
    부번 = ifelse(str_sub(부번, 1, 1) == 0, str_replace(부번, "0", ""), 부번),
    건물본번 = ifelse(str_sub(건물본번, 1, 1) == 0, str_replace(건물본번, "0", ""), 건물본번),
    건물부번 = ifelse(str_sub(건물부번, 1, 1) == 0, str_replace(건물부번, "0", ""), 건물부번),
    across(c(본번, 부번, 건물본번, 건물부번), ~ replace(., . == "", NA)),
    across(c(본번, 부번, 건물본번, 건물부번), as.numeric),

    # 주소 합치기
    도로명주소 =
      str_c(
        "강원도", " ", 시군, " ",
        # '시'는 동인 경우 읍면이 없어도 가능
        # '군'은 동이 없기 때문에 무조건 읍면이 있어야 함
        case_when(
          str_sub(시군, -1) == "시" ~ ifelse(is.na(읍면), "", str_c(읍면, " ")),
          str_sub(시군, -1) == "군" ~ str_c(읍면, " ")
        ),
        도로명, " ", 건물본번,
        ifelse(건물부번 == 0 | is.na(건물부번), "", str_c("-", 건물부번))
      ),
    지번주소 =
      str_c(
        "강원도", " ", 시군, " ",
        case_when(
          str_sub(동리, -1) == "동" | str_sub(동리, -1) == "가" ~ "",
          str_sub(동리, -1) == "리" ~ str_c(읍면, " ")
        ),
        동리, " ",
        ifelse(is.na(산), "", str_c("산", " ")),
        본번,
        ifelse(부번 == 0 | is.na(부번), "", str_c("-", 부번))
      ),
    
    # 주소 확인
    동리확인 = ifelse(is.na(동리), "X", ""),
    주소확인 = ifelse(is.na(동리) & is.na(도로명주소) & is.na(지번주소), "X", "")
  )


### 주소외 기타 내용 정리
addr_test1 <- addr_test %>%
    mutate(
      기타1 = 
        str_remove(주소1수정, ifelse(is.na(읍면), "test", 읍면)) %>%
        str_remove(., ifelse(is.na(동리), "test", 동리)) %>%
        str_remove(., ifelse(is.na(도로명), "test", 도로명)) %>%
        str_remove(., ifelse(is.na(산), "test", 산)) %>%
        str_remove(., ifelse(is.na(본번), "test", ifelse(is.na(부번), 본번, str_c(본번, "-", 부번)))) %>%
        str_remove(., ifelse(is.na(건물본번), "test",
                           ifelse(is.na(건물부번), 건물본번, str_c(건물본번, "-", 건물부번)))) %>% 
        str_remove(.,"\\(\\)") %>% str_trim(),
                 
      기타2 =
        str_remove(주소2수정, ifelse(is.na(읍면), "test", 읍면)) %>%
        str_remove(., ifelse(is.na(동리), "test", 동리)) %>%
        str_remove(., ifelse(is.na(도로명), "test", 도로명)) %>%
        str_remove(., ifelse(is.na(산), "test", 산)) %>%
        str_remove(., ifelse(is.na(본번), "test", ifelse(is.na(부번), 본번, str_c(본번, "-", 부번)))) %>%
        str_remove(., ifelse(is.na(건물본번), "test",
                             ifelse(is.na(건물부번), 건물본번, str_c(건물본번, "-", 건물부번)))) %>% 
        str_remove(.,"\\(\\)") %>% str_trim()
    )



### 주소 확인  -----
addr_test1 <- addr_test %>%
  # 도로명 주소 기준 확인
  left_join(도로명주소 %>% select(도로명주소, 도로명주소확인), by = "도로명주소") %>%
  # 지번 주소 기준 확인
  left_join(지번주소 %>% select(지번주소, 지번주소확인), by = "지번주소") %>%
  mutate(across(c(도로명주소확인, 지번주소확인), ~ replace(., is.na(.), "X"))) %>%
  mutate(
    도로명주소확인 = ifelse(is.na(도로명주소), "N", 도로명주소확인),
    지번주소확인 = ifelse(is.na(지번주소), "N", 지번주소확인)
  ) %>%
  select(-c(주소1수정:건물부번2), -읍면동, -리, -동리확인, -주소확인) %>%
  # mutate(
  #   도로명주소추가확인 = case_when(
  #     도로명주소확인 == "X" & 지번주소확인 == "X" ~ "Y",
  #     도로명주소확인 == "X" & 지번주소확인 == "N" ~ "Y",
  #     TRUE ~ "N"
  #   ),
  #   지번주소추가확인 = case_when(
  #     도로명주소확인 == "X" & 지번주소확인 == "X" ~ "Y",
  #     도로명주소확인 == "N" & 지번주소확인 == "X" ~ "Y",
  #     TRUE ~ "N"
  #   )
  # ) %>%
  # 도로명 주소 기준 확인
  left_join(
    도로명주소 %>%
      filter(지하여부 == 0) %>%
      select(시군, 도로명, 건물본번, 건물부번,
        도로명주소DB_도로명주소 = 도로명주소,
        도로명주소DB_지번주소 = 지번주소
      ),
    by = c("시군", "도로명", "건물본번", "건물부번")
  ) %>%
  left_join(
    지번주소 %>%
      select(시군:부번,
        지번주소DB_도로명주소 = 도로명주소,
        지번주소DB_지번주소 = 지번주소
      ),
    by = c("시군", "읍면", "동리", "산", "본번", "부번")
  ) %>%
  mutate(
    도로명주소DB_도로명주소 = ifelse(도로명주소DB_도로명주소 == 도로명주소, NA, 도로명주소DB_도로명주소),
    도로명주소DB_지번주소 = ifelse(도로명주소DB_지번주소 == 지번주소, NA, 도로명주소DB_지번주소),
    지번주소DB_도로명주소 = ifelse(지번주소DB_도로명주소 == 도로명주소, NA, 지번주소DB_도로명주소),
    지번주소DB_지번주소 = ifelse(지번주소DB_지번주소 == 지번주소, NA, 지번주소DB_지번주소)
  )
# group_by(코드) %>% mutate(중복 = length(코드))


### 주소DB자료와 비교 확인  -----
addr_test2 <- addr_test %>%
  # 읍면을 제외한 나머지 도로명 주소를 이용하여 주소DB자료 결합(읍면 틀린 경우 확인)
  left_join(
    도로명주소 %>%
      filter(지하여부 == 0) %>%
      select(시군, 도로명, 건물본번, 건물부번,
        도로명주소_DB = 도로명주소,
        도로명주소기준_지번주소 = 지번주소
      ),
    by = c("시군", "도로명", "건물본번", "건물부번")
  ) %>%
  # 도로명주소확인
  mutate(도로명주소확인 = case_when(
    is.na(도로명) & is.na(도로명주소) ~ "미기재",  # 제출 자료가 없는 경우 N
    !is.na(도로명) & is.na(도로명주소) ~ "X", # 도로명은 있으나 도로명 주소가 생성이 안된경우 X
    is.na(도로명주소_DB) ~ "X",        # 도로명주소_DB에서 검색이 안되는 경우 X
    도로명주소_DB != 도로명주소 ~ "X", # 제출자료와 주소DB자료가 불일치하는 경우 X
    도로명주소_DB == 도로명주소 ~ "O"  # 제출자료와 주소DB자료가 일치하는 경우 O
  )) %>%
  # 지번주소확인 : 제출자료와 주소DB자료가 일치하는 경우 O
  left_join(
    지번주소 %>% select(지번주소, 지번주소확인,
      지번주소기준_도로명주소 = 도로명주소
    ),
    by = "지번주소"
  ) %>%
  # 지번주소확인 : 제출자료 없거나 주소DB와 불일치하는 경우 확인
  mutate(지번주소확인 = case_when(
    is.na(지번주소) ~ "미기재", # 제출 자료가 없는 경우 N
    !is.na(동리) & is.na(지번주소) ~ "X", # 동리는 있으나 도로명 주소가 생성이 안된경우 X
    !is.na(지번주소) & is.na(지번주소확인) ~ "X", # 제출자료와 주소DB자료가 불일치하는 경우 X
    TRUE ~ 지번주소확인 # 나머지 자료는 주소DB와 일치
  )) %>%
  # 지번주소 추가 확인 : 도로명주소기준 지번주소와 제출자료 일치 여부 확인
  # 지번주소확인2 = ifelse(지번주소 != 도로명주소기준_지번주소, "X", "O")) %>%
  group_by(코드) %>%
  mutate(중복 = length(코드)) %>%
  ungroup() %>%
  relocate(구분, .after = 업종) %>% 
  select(-c(가구수:건물부번2), -읍면동, -리, -동리확인, -주소확인) %>%
  relocate(c(도로명주소확인, 지번주소확인), .after = 지번주소)


## 시군별 자료 개수 확인  -----
시군별주소확인개수 <- addr_test2 %>%
  group_by(시군) %>%
  summarise(총개수 = n(), .groups = "drop") %>%
  left_join(
    addr_test2 %>%
      filter(도로명주소확인 != "미기재") %>%
      group_by(시군) %>%
      summarise(도로명주소_총개수 = n(), .groups = "drop"),
    by = "시군"
  ) %>%
  left_join(
    addr_test2 %>%
      filter(도로명주소확인 == "X") %>%
      group_by(시군) %>%
      summarise(도로명주소_오류개수 = n(), .groups = "drop"),
    by = "시군"
  ) %>%
  left_join(
    addr_test2 %>%
      filter(지번주소확인 != "미기재") %>%
      group_by(시군) %>%
      summarise(지번주소_총개수 = n(), .groups = "drop"),
    by = "시군"
  ) %>% 
  left_join(
    addr_test2 %>%
      filter(지번주소확인 == "X") %>%
      group_by(시군) %>%
      summarise(지번주소_오류개수 = n(), .groups = "drop"),
    by = "시군"
  )

시군별주소확인개수 %<>%
  bind_rows(시군별주소확인개수 %>%
    summarise(
      총개수 = sum(총개수),
      도로명주소_총개수 = sum(도로명주소_총개수),
      도로명주소_오류개수 = sum(도로명주소_오류개수),
      지번주소_총개수 = sum(지번주소_총개수),
      지번주소_오류개수 = sum(지번주소_오류개수)
    ) %>%
    mutate(시군 = "합계", .before = 1)) %>% 
  mutate(도로명주소_오류율 = round(도로명주소_오류개수 / 도로명주소_총개수, 4),
         .after = 도로명주소_오류개수) %>% 
  mutate(지번주소_오류율 = round(지번주소_오류개수 / 지번주소_총개수, 4),
         .after = 지번주소_오류개수) %>%
  mutate(
    시군 = factor(시군, levels = c(
      "합계", "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군",
      "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군",
      "양구군", "인제군", "고성군", "동해시", "속초시", "양양군"
    ))) %>% 
  arrange(시군)


### 파일 내보내기  -----
write_xlsx(addr_test2, path = "전국오염원조사/Output/addr_test2.xlsx")



##########  주소 좌표 변환  ###############################################################

library(httr)
library(jsonlite) # fromJSON()
library(progress)

## REST API 키(https://developers.kakao.com/)
KAKAO_MAP_API_KEY <- "9e9a85a9ec8362e009da2f7bc4b3a09c"

## ------ 도로명 주소 ----------------------------------------------------------
## 수용가번호 및 대표주소만 추출
address_doro <- addr_test1 %>%
  # filter(!(시군 %in% c("강릉시", "고성군", "삼척시"))) %>%
  filter(도로명주소확인 == "X") %>%
  select(코드, 도로명주소) %>%
  filter(!is.na(도로명주소)) %>%
  rowid_to_column(var = "ID")

address_doro_list <- address_doro$도로명주소

## progress bar 설정
pb_doro <- progress_bar$new(
  format = " Progress: [:bar] :current / :total (:percent), Estimated completion time::eta",
  total = nrow(address_doro), # 총 tick 개수 (default 100)
  clear = FALSE, # 종료 후에도 진행 경과 출력 결과 유지 (default TRUE)
  width = 80 # 진행 경과 막대 너비
)

result_doro <- data.frame()

for (i in 1:nrow(address_doro)) {
  place_list <- GET(
    url = "https://dapi.kakao.com/v2/local/search/address.json",
    query = list(query = address_doro_list[i]),
    add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY))
  ) %>%
    content(as = "text") %>%
    fromJSON()

  # 수용가번호 및 기존 주소 불러오기
  temp_addr_doro <- address_doro %>% filter(ID == i)

  ## 도로명주소 : 주소 검색결과가 없는 경우 대비 test열(NA값 입력) 추가
  temp_doro <- bind_cols(place_list$documents$road_address, tibble(test = NA))

  # 도로명주소가 없는 경우(test열이 1열에 위치) 모든 열 NA로 입력
  if (is.na(temp_doro[1, 1])) {
    temp_doro <- tibble(
      address_name = NA, building_name = NA, road_name = NA,
      main_building_no = NA, sub_building_no = NA,
      region_3depth_name = NA, region_2depth_name = NA,
      x = NA, y = NA
    )
  } else {
    temp_doro <- temp_doro %>%
      select(
        address_name, building_name, road_name,
        main_building_no, sub_building_no, region_2depth_name,
        region_3depth_name, x, y
      )
  }

  temp_doro <- bind_cols(temp_addr_doro, temp_doro)

  result_doro <- bind_rows(result_doro, temp_doro)


  ## 진행상황 확인
  pb_doro$tick()
}


## ------ 지번 주소 ------------------------------------------------------------
## 수용가번호 및 대표주소만 추출
address_jibun <- addr_test1 %>%
  # filter(!(시군 %in% c("강릉시", "고성군", "삼척시"))) %>%
  filter(지번주소확인 == "X") %>%
  select(코드, 지번주소) %>%
  filter(!is.na(지번주소)) %>%
  rowid_to_column(var = "ID") # %>% filter(ID > 105)

address_jibun_list <- address_jibun$지번주소

## progress bar 설정
pb_jibun <- progress_bar$new(
  format = " Progress: [:bar] :current / :total (:percent), Estimated completion time::eta",
  total = nrow(address_jibun), # 총 tick 개수 (default 100)
  clear = FALSE, # 종료 후에도 진행 경과 출력 결과 유지 (default TRUE)
  width = 80 # 진행 경과 막대 너비
)

result_jibun <- data.frame()

for (i in 1:nrow(address_jibun)) {
  place_list <- GET(
    url = "https://dapi.kakao.com/v2/local/search/address.json",
    query = list(query = address_jibun_list[i]),
    add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY))
  ) %>%
    content(as = "text") %>%
    fromJSON()

  # 수용가번호 및 기존 주소 불러오기
  temp_addr_jibun <- address_jibun %>% filter(ID == i)

  temp <- bind_cols(place_list$documents$road_address, tibble(test = NA))

  ## 지번주소
  # temp_jibun <- bind_cols(temp_addr_jibun, place_list$documents$address)
  # place_list$documents$road_address$address_name

  if (is.na(temp[1, 1])) {
    temp_jibun <- bind_cols(temp_addr_jibun, place_list$documents$address)
  } else {
    temp_jibun <- bind_cols(
      temp_addr_jibun, place_list$documents$address,
      place_list$documents$road_address$address_name
    )
  }

  result_jibun <- bind_rows(result_jibun, temp_jibun)

  ## 진행상황 확인
  pb_jibun$tick()
}


## ------ 검색 결과 정리 -------------------------------------------------------
## 도로명 주소
result_doro2 <- result_doro %>%
  select(코드, 도로명주소, address_name, region_3depth_name) %>%
  filter(!is.na(address_name)) %>%
  mutate(
    address_name = str_replace(address_name, "강원특별자치도", "강원도"),
    확인 = ifelse(도로명주소 != address_name, "X", "O")
  )

## 지번 주소
result_jibun2 <- result_jibun %>%
  select(코드, 지번주소, address_name, region_3depth_h_name, region_3depth_name) %>%
  filter(!is.na(address_name)) %>%
  mutate(
    address_name = str_replace(address_name, "강원특별자치도", "강원도"),
    확인 = ifelse(지번주소 != address_name, "X", "O"),
    행정동정리 = ifelse(str_detect(region_3depth_name, region_3depth_h_name), "O", "X")
  )

addr_test2 <- addr_test1 %>%
  left_join(result_doro2, by = c("코드", "도로명주소")) %>%
  left_join(result_jibun2, by = c("코드", "지번주소")) %>%
  mutate(
    도로명주소확인 = ifelse(is.na(도로명검색결과), 도로명주소확인, 도로명검색결과),
    지번주소확인 = ifelse(is.na(지번검색결과), 지번주소확인, 지번검색결과),
    주소확인 = case_when(
      도로명주소확인 == "O" & 지번주소확인 == "O" ~ "OK",
      도로명주소확인 == "O" | 지번주소확인 == "O" ~ "OK",
      TRUE ~ "주소 수정 필요"
    )
  ) %>%
  select(-c(업종:사용량합계), -코드, -c(도로명주소추가확인:지번검색결과)) %>%
  relocate(c(구분, 수용가명, 상호명), .after = 수용가번호)



##########  물사용량 및 주소코드 최종 정리  ############################################

waterusage_2022 <- addr_test1 %>%
  select(시군, 코드, 주소코드, 업종, 사용량합계, 주소코드확인) %>%
  left_join(result_doro2 %>% select(코드, 주소코드), by = "코드") %>%
  mutate(
    주소 = ifelse(is.na(주소코드.x), 주소코드.y, 주소코드.x),
    업종2 = ifelse(str_detect(업종, "가정"), "가정용", "영업용"),
    연도 = 2022
  ) %>%
  group_by(연도, 시군, 주소, 업종2) %>%
  summarise(물사용량 = sum(사용량합계), .groups = "drop")

waterusage_2022_시군 <- waterusage_2022 %>%
  group_by(시군) %>%
  summarise(물사용량 = sum(물사용량), .groups = "drop")


### 파일 내보내기
write_xlsx(list("동리" = waterusage_2022, "시군" = waterusage_2022_시군),
  path = "전국오염원조사/Output/waterusage_2022.xlsx"
)

write_xlsx(waterusage_2022_시군, path = "전국오염원조사/Output/waterusage_2022_시군.xlsx")
