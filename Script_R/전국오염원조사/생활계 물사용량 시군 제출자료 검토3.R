#####  라이브러리 로드  #############################################################
library(tidyverse)
library(magrittr)
library(data.table)
library(readxl)
library(writexl)

### 반올림 사용자 정의 함수 로드
source("Script_R/Function/round2func.R")



##########  코드 정리  ##############################################################

### 법정동코드 불러오기
dongcode <- read_excel("주소 검토/법정동코드 전체자료.xlsx", guess_max = 3000) %>%
  filter(시도 == "강원도", 폐지여부 == "존재") %>%
  select(-폐지여부) %>%
  rename(시군 = 시군구) %>%
  mutate(
    읍면 = str_extract(읍면동, "[가-힣0-9]{1,}(읍|면)"),
    동리 = ifelse(is.na(읍면), 읍면동, 리)
  ) %>%
  filter(!is.na(동리))

### 도로명코드 불러오기
dorocode <- read.table("주소 검토/주소DB/도로명코드.txt",
  header = F, quote = "", sep = "|", fill = T,
  encoding = "UTF-8", fileEncoding = "EUC-KR"
) %>%
  select(1, 2, 5) %>%
  set_names(c(
    "도로명코드", "도로명", "시도"
  )) %>%
  filter(시도 == "강원도") %>%
  select(-시도) %>%
  distinct(도로명, .keep_all = TRUE)

### 읍면, 동리, 도로명 확인용 벡터 생성(제일 앞에 여백 추가)
읍면_check <- paste(" ", dongcode$읍면, collapse = " ")
동리_check <- paste(" ", dongcode$동리, collapse = " ")
도로명_check <- paste(" ", dorocode$도로명, collapse = " ")


##########  도로명주소 전자지도 기준 주소 DB 구축  ##################################

### 도로명주소 전자지도 건물정보 DB 불러오기
juso_db_doro <- read.table("주소 검토/match_build_gangwon.txt",
  header = F, quote = "", sep = "|", fill = T,
  encoding = "UTF-8", fileEncoding = "EUC-KR"
)

### 도로명주소 전자지도 도로명매칭코드 생성
juso_code_doro <- juso_db_doro %>%
  select(1:9, 11, 12, 20, 24, 25) %>%
  set_names(c(
    "법정동코드", "시도", "시군구", "읍면동", "도로명코드",
    "도로명", "지하여부", "본번", "부번", "건물관리번호",
    "건물명", "상세건물명", "건물중심점_x좌표", "건물중심점_y좌표"
  )) %>%
  mutate(
    읍면 = str_extract(읍면동, "[가-힣0-9]{1,}(읍|면)"),
    # 동리 = ifelse(is.na(읍면), 읍면동, NA),
    도로명매칭코드 =
      str_c(
        "D",
        도로명코드,
        # str_sub(법정동코드, 6, 8),
        지하여부,
        str_pad(str_replace_na(본번, "0"), width = 5, side = "left", pad = "0"),
        str_pad(str_replace_na(부번, "0"), width = 5, side = "left", pad = "0")
      ),
    도로명주소 =
      str_c(
        시도, " ", 시군구, " ",
        ifelse(is.na(읍면), "", str_c(읍면, " ")),
        도로명, " ", 본번,
        ifelse(부번 == 0 | is.na(부번), "", str_c("-", 부번))
      )
  ) %>%
  # 도로명매칭코드가 여러개인 경우 하나만 남기고 정리
  distinct(도로명매칭코드, .keep_all = TRUE)


### 도로명주소 전자지도 지번정보 DB 불러오기
juso_db_jibun <- read.table("주소 검토/match_jibun_gangwon.txt",
  header = F, sep = "|",
  encoding = "UTF-8", fileEncoding = "EUC-KR"
)

### 도로명주소 전자지도 지번주소 매칭코드 생성
juso_code_jibun <- juso_db_jibun %>%
  select(1:12) %>%
  set_names(c(
    "법정동코드", "시도", "시군구", "읍면동", "리", "산", "번지", "호",
    "도로명코드", "지하여부", "본번", "부번"
  )) %>%
  mutate(
    읍면 = str_extract(읍면동, "[가-힣0-9]{1,}(읍|면)"),
    동리 = ifelse(is.na(읍면), 읍면동, 리),
    도로명매칭코드 =
      str_c(
        "D",
        도로명코드,
        # str_sub(법정동코드, 6, 8),
        지하여부,
        str_pad(str_replace_na(본번, "0"), width = 5, side = "left", pad = "0"),
        str_pad(str_replace_na(부번, "0"), width = 5, side = "left", pad = "0")
      ),
    지번매칭코드 =
      str_c(
        "J",
        법정동코드,
        산,
        str_pad(번지, width = 4, side = "left", pad = "0"),
        str_pad(str_replace_na(호, "0"), width = 4, side = "left", pad = "0")
      ),
    지번주소 =
      str_c(
        시도, " ", 시군구, " ", 읍면동,
        ifelse(is.na(리) | 리 == "", "", str_c(" ", 리)),
        ifelse(산 == 1, " 산 ", " "), 번지,
        ifelse(호 == 0 | is.na(호), "", str_c("-", 호))
      )
  )


## 주소코드 최종 정리(도로명 기준)
final_juso_code_jibun <- juso_code_jibun %>%
  left_join(
    juso_code_doro %>%
      select(도로명매칭코드, 도로명주소, 건물중심점_x좌표, 건물중심점_y좌표),
    by = "도로명매칭코드"
  )

final_juso_code_doro <- juso_code_doro %>%
  left_join(
    final_juso_code_jibun %>%
      select(도로명매칭코드, 동리) %>%
      distinct(도로명매칭코드, .keep_all = TRUE),
    by = "도로명매칭코드"
  ) %>%
  # 도로명주소 중복 삭제
  # (지하주소가 있는 경우 도로명주소는 동일하나 도로명매칭코드는 다른 경우 발생)
  distinct(도로명주소, .keep_all = TRUE) %>%
  mutate(도로명주소확인 = "O")

final_juso_code_jibun %<>%
  # 지번코드 중복 삭제
  distinct(지번매칭코드, .keep_all = TRUE) %>%
  mutate(지번주소확인 = "O")


##########  과거 자료(2021년) 확인  #################################################

## *****  파일 불러오기  *******************************************************
### 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/생활계 물사용량 주소 검증/2021년 기준 물사용량 제출 자료",
  pattern = "*.xlsx", full.names = T
)

### 기존 양식 파일 합치기
waterusage_2021 <- data.frame()

for (file in files) {
  print(file)
  ## 상수 자료
  temp <- read_excel(file, skip = 6, col_names = F) %>%
    select(2:3, 5:9, 12:19) %>%
    mutate(구분 = "상수도")

  waterusage_2021 <- rbind(waterusage_2021, temp)

  ## 지하수 자료
  temp <- read_excel(file, sheet = 3, skip = 6, col_names = F)

  # 자료가 없는 경우 대비 test열(NA값 입력) 추가
  temp %<>% mutate(test = NA)

  # 자료가 있는 경우 해당열 선택 및 구분열 추가
  if (is.na(temp[1, 1])) {
    waterusage_2021 <- rbind(waterusage_2021, temp)
  } else {
    temp %<>% select(2:3, 5:9, 12:19) %>%
      mutate(구분 = "지하수")

    waterusage_2021 <- rbind(waterusage_2021, temp)
  }
}
## *****************************************************************************

### 과거 파일 정리
waterusage_2021 %<>%
  set_names(c(
    "법정동코드", "수용가번호", "시군", "읍면동", "법정리",
    "본번", "부번", "읍면", "도로명", "건물본번", "건물부번",
    "수용가명", "상호명", "업종", "가구수", "구분"
  ))
waterusage_2021_dong <- waterusage_2021 %>%
  mutate(across("법정동코드", as.numeric)) %>%
  left_join(dongcode %>% select(법정동코드, 동리), by = "법정동코드") %>%
  mutate(
    법정리2 = ifelse(is.na(법정리) | 법정리 == "", 동리, 법정리),
    주소코드 = str_c(시군, 읍면동, 법정리2, sep = " "),
    코드 = str_c(시군, 수용가번호, 구분)
  ) %>%
  select(-c(법정동코드, 법정리:동리)) %>%
  distinct(코드, .keep_all = TRUE) %>%
  mutate(중복자료 = ifelse(duplicated(코드), "중복", ""))



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


### 주소검토
addr_test <- waterusage %>% select(-c(연도, 하수도요금여부)) %>%
  # filter(시군 == "철원군") %>%
  mutate(
    주소1수정 =
    # '00번길' 또는 '00길'앞에 띄어쓰기가 되어 있는 경우 공백 제거
      str_replace(주소1, " 번길", "번길") %>%
        str_replace(., "[\\s][0-9]{1,}(번길)", str_extract(주소1, "[0-9]{1,}(번길)")) %>%
        str_replace(., "[\\s][0-9]{1,}(길)", str_extract(주소1, "[0-9]{1,}(길)")) %>%
        # 동, 리 앞에 숫자가 있는 경우('XX1동', 'XX1리') 숫자 삭제
        str_replace(., "[0-9]{1,}(동)", "동") %>%
        str_replace(., "[0-9]{1,}(리)", "리") %>%
        # '00반' 삭제
        str_remove(., "[0-9]{1,}(반)") %>%
        str_remove(., 시군) %>%
        str_trim(),
    주소2수정 =
    # '00번길' 또는 '00길'앞에 띄어쓰기가 되어 있는 경우 공백 제거
      str_replace(주소2, " 번길", "번길") %>%
        str_replace(., "[\\s][0-9]{1,}(번길)", str_extract(주소2, "[0-9]{1,}(번길)")) %>%
        str_replace(., "[\\s][0-9]{1,}(길)", str_extract(주소2, "[0-9]{1,}(길)")) %>%
        # 동, 리 앞에 숫자가 있는 경우('XX1동', 'XX1리') 숫자 삭제
        str_replace(., "[0-9]{1,}(동)", "동") %>%
        str_replace(., "[0-9]{1,}(리)", "리") %>%
        # '00반' 삭제
        str_remove(., "[0-9]{1,}(반)") %>%
        str_remove(., 시군) %>%
        str_trim(),
    # 읍면
    읍면1 = str_extract(주소1수정, "[가-힣0-9]{1,}(읍|면)(?!읍|면|동|가|리|로|길)") %>%
      str_remove(., "[0-9]{1,}") %>%
      str_trim(),
    읍면2 = str_extract(주소2수정, "[가-힣0-9]{1,}(읍|면)(?!읍|면|동|가|리|로|길)") %>%
      str_remove(., "[0-9]{1,}") %>%
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
    동리1 = ifelse(str_detect(동리_check, str_c(" ", 동리1, " ")), 동리1, NA),
    동리2 = ifelse(str_detect(동리_check, str_c(" ", 동리2, " ")), 동리2, NA),
    # 도로명
    도로명1 = str_extract(주소1수정, "[가-힣A-Za-z0-9]{1,}(로|길)(?!읍|면|동|[0-9]{1,}가|리|로|길)"),
    도로명2 = str_extract(주소2수정, "[가-힣A-Za-z0-9]{1,}(로|길)(?!읍|면|동|[0-9]{1,}가|리|로|길)"),
    # 도로명 확인(앞뒤로 여백 추가해서 일부만 일치하는 경우 방지)
    도로명1 = ifelse(str_detect(도로명_check, str_c(" ", 도로명1, " ")), 도로명1, NA),
    도로명2 = ifelse(str_detect(도로명_check, str_c(" ", 도로명2, " ")), 도로명2, NA),
    # 본번/부번
    산1 = str_extract(주소1수정, "(?<=동 |리 |가 )(산)"),
    본번1 = str_extract(주소1수정, "(?<=동 |동|리 |리|가 |가|산 |산)[0-9]{1,}(?!로 |길 )"),
    부번1 = str_extract(주소1수정, "(?<=동 |동|리 |리|가 |가|산 |산)[0-9\\-]{1,}") %>%
      str_extract(., "(?<=\\-)[0-9]{1,}"),
    산2 = str_extract(주소2수정, "(?<=동 |리 |가 )(산)"),
    본번2 = str_extract(주소2수정, "(?<=동 |동|리 |리|가 |가|산 |산)[0-9]{1,}(?!로 |길 )"),
    부번2 = str_extract(주소2수정, "(?<=동 |동|리 |리|가 |가|산 |산)[0-9\\-]{1,}") %>%
      str_extract(., "(?<=\\-)[0-9]{1,}"),
    건물본번1 = ifelse(is.na(도로명1), NA,
      주소1수정 %>%
        str_split_i(., 도로명1, 2) %>%
        str_extract(., "[0-9]{1,}")
    ),
    건물부번1 = ifelse(is.na(도로명1), NA,
      주소1수정 %>%
        str_split_i(., 도로명1, 2) %>%
        str_extract(., "[0-9\\-]{1,}") %>%
        str_extract(., "(?<=\\-)[0-9]{1,}")
    ),
    건물본번2 = ifelse(is.na(도로명2), NA,
      주소2수정 %>%
        str_split_i(., 도로명2, 2) %>%
        str_extract(., "[0-9]{1,}")
    ),
    건물부번2 = ifelse(is.na(도로명2), NA,
      주소2수정 %>%
        str_split_i(., 도로명2, 2) %>%
        str_extract(., "[0-9\\-]{1,}") %>%
        str_extract(., "(?<=\\-)[0-9]{1,}")
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
        ifelse(is.na(읍면), "", str_c(읍면, " ")),
        ifelse(is.na(동리), "", str_c(동리, " ")),
        ifelse(is.na(산), "", str_c(산, " ")),
        본번,
        ifelse(부번 == 0 | is.na(부번), "", str_c("-", 부번))
      ),
    동리확인 = ifelse(is.na(동리), "X", ""),
    주소확인 = ifelse(is.na(동리) & is.na(도로명주소) & is.na(지번주소), "X", ""),
    # 시군, 수용가번호 합쳐서 코드 생성(시군별 수용가번호 동일한 경우 방지)
    코드 = str_c(시군, 수용가번호, 구분)
  )

### 주소 확인
addr_test1 <- addr_test %>%
  # 도로명 주소 기준 확인
  left_join(final_juso_code_doro %>% select(도로명주소, 도로명주소확인), by = "도로명주소") %>%
  # 지번 주소 기준 확인
  left_join(final_juso_code_jibun %>% select(지번주소, 지번주소확인), by = "지번주소") %>%
  mutate(across(c(도로명주소확인, 지번주소확인), ~ replace(., is.na(.), "X"))) %>%
  mutate(
    도로명주소확인 = ifelse(is.na(도로명주소), "N", 도로명주소확인),
    지번주소확인 = ifelse(is.na(지번주소), "N", 지번주소확인)
  ) %>%
  select(-c(주소1수정:건물부번2), -읍면동, -리, -동리확인, -주소확인) %>%
  mutate(
    도로명주소추가확인 = case_when(
      도로명주소확인 == "X" & 지번주소확인 == "X" ~ "Y",
      도로명주소확인 == "X" & 지번주소확인 == "N" ~ "Y",
      TRUE ~ "N"
    ),
    지번주소추가확인 = case_when(
      도로명주소확인 == "X" & 지번주소확인 == "X" ~ "Y",
      도로명주소확인 == "N" & 지번주소확인 == "X" ~ "Y",
      TRUE ~ "N"
    )
  )




### 주소코드 합치기
addr_test1 <- addr_test %>%
  left_join(waterusage_2021_dong %>% select(코드, 주소코드), by = "코드") %>%
  # mutate(읍면동 = ifelse(is.na(읍면동.x), 읍면동.y, 읍면동.x)) %>%
  rename(주소코드1 = 주소코드) %>%
  mutate(주소코드2 = str_c(시군, 읍면동, 동리, sep = " ")) %>%
  # 도로명 주소 기준 동리 확인
  left_join(final_juso_code_doro %>% select(도로명주소, 동리), by = "도로명주소") %>%
  # 지번 주소 기준 동리 확인
  left_join(final_juso_code_jibun %>% select(지번주소, 동리), by = "지번주소") %>%
  mutate(동리 = ifelse(is.na(동리.x),
    ifelse(is.na(동리.y), 동리, 동리.y),
    동리.x
  )) %>%
  # 리만 있고 읍면이 없는 경우 읍면 추가
  left_join(dongcode %>% select(시군, 읍면동, 동리), by = c("시군", "동리")) %>%
  # 중복자료 확인 위해 시군, 수용가번호 합쳐서 생성된 코드 활용
  group_by(코드) %>%
  mutate(
    중복 = length(코드),
    # 중복 자료 중 읍면동 기존자료와 신규자료가 다른 경우 삭제
    삭제 = ifelse(중복 > 1 & 읍면동.x != 읍면동.y, "x", ""),
    # 철원군 양지리의 경우 근남면인 경우 삭제
    삭제 = ifelse(시군 == "철원군" & 동리 == "양지리" & 읍면동.y == "근남면", "x", 삭제),
    # 철원군 자료의 경우 읍면동 자료가 누락되어 NA값이 되므로 빈칸으로 변경
    삭제 = ifelse(is.na(삭제), "", 삭제)
  ) %>%
  ungroup() %>%
  # 확인된 중복 자료 삭제
  filter(삭제 != "x") %>%
  # 읍면동 및 주소코드 정리
  mutate(
    읍면동 = ifelse(is.na(읍면동.x), 읍면동.y, 읍면동.x),
    주소코드3 = str_c(시군, 읍면동, 동리, sep = " "),
    주소코드 = ifelse(is.na(주소코드3),
      ifelse(is.na(주소코드2), 주소코드1, 주소코드2),
      주소코드3
    ),
    # 주소코드가 없는 경우 확인
    주소코드확인 = ifelse(is.na(주소코드), "x", "")
    # 과거자료 주소코드와 올해 주소코드가 불일치 하는 경우
    # 주소코드확인2 = ifelse(주소코드1 != 주소코드2, "x", "")
  ) %>%
  select(
    시군:주소2, 업종, 가구수, 사용량합계, 읍면, 동리, 읍면동, 리:지번주소,
    코드, 주소코드, 주소코드확인
  )

### 도로명 기준 동리 정리
doro_dongri <- addr_test1 %>%
  filter(!is.na(도로명), !is.na(주소코드)) %>%
  select(도로명, 시군, 읍면, 동리, 읍면동, 리, 주소코드) %>%
  mutate(도로명코드 = str_c(도로명, 주소코드, sep = " ")) %>%
  distinct(도로명코드, .keep_all = TRUE) %>%
  mutate(도로명중복 = ifelse(duplicated(도로명), "중복", ""))


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
  # filter(도로명주소추가확인 == "Y") %>%
  select(코드, 도로명주소, 사용량합계) %>%
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
  # filter(지번주소추가확인 == "Y") %>%
  select(코드, 지번주소, 사용량합계) %>%
  filter(!is.na(지번주소)) %>%
  rowid_to_column(var = "ID") 

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
  
  ## 지번주소
  temp_jibun <- bind_cols(temp_addr_jibun, place_list$documents$address)
  
  result_jibun <- bind_rows(result_jibun, temp_jibun)
  
  ## 진행상황 확인
  pb_jibun$tick()
}


## ------ 검색 결과 정리 -------------------------------------------------------
## 도로명 주소
result_doro2 <- result_doro %>%
  mutate(도로명검색결과 = ifelse(is.na(address_name), "X", "O")) %>% 
  select(코드, 도로명주소, 도로명검색결과)

## 지번 주소
result_jibun2 <- result_jibun %>%
  mutate(지번검색결과 = ifelse(is.na(address_name), "X", "O")) %>% 
  select(코드, 지번주소, 지번검색결과)


addr_test2 <- addr_test1 %>% 
  left_join(result_doro2, by = c("코드", "도로명주소")) %>% 
  left_join(result_jibun2, by = c("코드", "지번주소")) %>% 
  mutate(도로명주소확인 = ifelse(is.na(도로명검색결과), 도로명주소확인, 도로명검색결과),
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
  path = "전국오염원조사/waterusage_2022.xlsx"
)

write_xlsx(waterusage_2022_시군, path = "전국오염원조사/waterusage_2022_시군.xlsx")



# _________#_________#_________#_________#_________#_________#_________#_________#


place_list <- GET(
  url = "https://dapi.kakao.com/v2/local/search/address.json",
  query = list(query = "강원도 영월군 영월읍 청령포로 229"),
  add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY))
) %>%
  content(as = "text") %>%
  fromJSON()



row_temp1 <- tibble(
  "address$address_name" = ifelse(is.null(place_list$documents$address$address_name),
    NA,
    place_list$documents$address$address_name
  )
)

row_temp2 <- ifelse(is.null(place_list$documents),
  tibble(address$address_name == address_list[1]),
  row_temp1
)

row_temp <- tibble(
  address_name = ifelse(is.null(place_list$documents$address$address_name), address_list[i], place_list$documents$address$address_name),
  region_2depth_name = ifelse(is.null(place_list$documents$address$region_2depth_name), "", place_list$documents$address$region_2depth_name),
  region_3depth_h_name = ifelse(is.null(place_list$documents$address$region_3depth_h_name), "", place_list$documents$address$region_3depth_h_name),
  region_3depth_name = ifelse(is.null(place_list$documents$address$region_3depth_name), "", place_list$documents$address$region_3depth_name),
  x = ifelse(is.null(place_list$documents$address$x), "", place_list$documents$address$x),
  y = ifelse(is.null(place_list$documents$address$y), "", place_list$documents$address$y)
)
