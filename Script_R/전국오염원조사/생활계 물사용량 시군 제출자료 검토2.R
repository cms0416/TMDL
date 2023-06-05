#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(data.table)
library(readxl)
library(writexl)

## 반올림 사용자 정의 함수 로드
source("Script_R/Function/round2func.R")

################################################################################



## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/0시군제출자료 검토/생활계",
  pattern = "*.xlsx", full.names = T
)

files_기존 <- grep("기존", files, value = TRUE)
files_푸른물 <- grep("푸른물", files, value = TRUE)
files_wims <- grep("WIMS", files, value = TRUE)

# 기존 양식 파일 합치기
waterusage_기존 <- data.frame()

for (file in files_기존) {
  print(file)
  temp <- read_excel(file, skip = 4, col_names = F) %>%
    select(1:21) %>%
    # substr(문자열, 시작위치, 끝위치) : 시군 이름 추출
    mutate(시군 = str_sub(file, -8, -6))
  waterusage_기존 <- rbind(waterusage_기존, temp)
}

# wims 양식 파일 합치기
waterusage_wims <- data.frame()

for (file in files_wims) {
  print(file)
  temp <- read_excel(file, skip = 2, col_names = F) %>%
    select(1, 3:6, 8, 9, 22:33, 46, 47) %>%
    # substr(문자열, 시작위치, 끝위치) : 시군 이름 추출
    mutate(시군 = str_sub(file, -8, -6))
  waterusage_wims <- rbind(waterusage_wims, temp)
}

# 푸른물 양식 파일 합치기
waterusage_푸른물 <- data.frame()

for (file in files_푸른물) {
  print(file)
  temp <- read_excel(file, col_names = T) %>%
    select(1, 3:7, 14, 22, 87, 128) %>%
    # substr(문자열, 시작위치, 끝위치) : 시군 이름 추출
    mutate(시군 = str_sub(file, -8, -6))
  waterusage_푸른물 <- rbind(waterusage_푸른물, temp)
}

## *****************************************************************************


## 기존 양식 파일 정리
waterusage_기존1 <- waterusage_기존 %>%
  mutate_at(vars(1, 8, 10:21), as.numeric) %>%
  mutate(사용량합계 = round2(pmap_dbl(select(., 10:21), sum) / 365, 3)) %>%
  select(1, 시군, 2:9, 사용량합계) %>%
  set_names(c(
    "연도", "시군", "수용가번호", "주소1", "주소2",
    "수용가명", "상호명", "업종", "가구수", "하수도요금여부",
    "사용량합계"
  ))


## wims 양식 파일 정리
waterusage_wims1 <- waterusage_wims %>%
  mutate_at(vars(1, 6, 8:19), as.numeric) %>%
  mutate(사용량합계 = round2(pmap_dbl(select(., 8:19), sum) / 365, 3)) %>%
  select(1, 시군, 2, 20, 4, 3, 21, 5:7, 사용량합계) %>%
  set_names(c(
    "연도", "시군", "수용가번호", "주소1", "주소2",
    "수용가명", "상호명", "업종", "가구수", "하수도요금여부",
    "사용량합계"
  ))

## 푸른물 양식 파일 정리
waterusage_푸른물1 <- waterusage_푸른물 %>%
  # 사용량 및 하수료 "-"으로 표기된 경우 0으로 수정
  # 숫자에서 쉼표 제거
  mutate(
    사용량합계 = ifelse(사용량합계 == "-", 0, 사용량합계),
    하수료합계 = ifelse(하수료합계 == "-", 0, 하수료합계),
    사용량합계 = str_remove_all(사용량합계, ","),
    하수료합계 = str_remove_all(하수료합계, ",")
  ) %>%
  mutate_at(vars(연도, 세대수, 사용량합계, 하수료합계), as.numeric) %>%
  select(
    연도, 시군, 관리번호, 주소, 새주소, 성명, 상호, 상수업종, 세대수,
    하수료합계, 사용량합계
  ) %>%
  set_names(c(
    "연도", "시군", "수용가번호", "주소1", "주소2",
    "수용가명", "상호명", "업종", "가구수", "하수도요금여부",
    "사용량합계"
  ))

## 전체 파일 합치기
waterusage <- rbind(waterusage_기존1, waterusage_wims1, waterusage_푸른물1)


##########  코드 정리  ##############################################################

### 법정동코드 불러오기
dongcode <- read_excel("주소 검토/법정동코드 전체자료.xlsx", guess_max = 3000) %>% 
  filter(시도 == "강원도", 폐지여부 == "존재") %>% 
  select(-폐지여부) %>% 
  rename(시군 = 시군구) %>% 
  mutate(
    읍면 = str_extract(읍면동, "[가-힣0-9]{1,}(읍|면)"),
    동리 = ifelse(is.na(읍면), 읍면동, 리)) %>% 
  filter(!is.na(동리))

### 도로명코드 불러오기
dorocode <- read.table("주소 검토/주소DB/도로명코드.txt", 
                       header = F, quote = "", sep = "|", fill = T,
                       encoding = "UTF-8", fileEncoding = "EUC-KR") %>% 
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




##########  과거 자료(2021년) 확인  #################################################

## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/생활계 물사용량 주소 검증/2021년 기준 물사용량 제출 자료",
  pattern = "*.xlsx", full.names = T
)

# 기존 양식 파일 합치기
waterusage_2021 <- data.frame()

for (file in files) {
  print(file)
  temp <- read_excel(file, skip = 6, col_names = F) %>%
    select(2:3, 5:9, 12:19)
  
  waterusage_2021 <- rbind(waterusage_2021, temp)
}
## *****************************************************************************

## 과거 파일 정리
waterusage_2021 %<>%
  set_names(c(
    "법정동코드", "수용가번호", "시군", "읍면동", "법정리",
    "본번", "부번", "읍면", "도로명", "건물본번", "건물부번",
    "수용가명", "상호명", "업종", "가구수"
  ))
waterusage_2021_dong <- waterusage_2021 %>% 
  mutate_at("법정동코드", as.numeric) %>%
  left_join(dongcode %>% select(법정동코드, 동리), by = "법정동코드") %>% 
  mutate(법정리2 = ifelse(is.na(법정리) | 법정리 == "", 동리, 법정리),
         주소코드 = str_c(시군, 읍면동, 법정리2, sep = " "),
         코드 = str_c(시군, 수용가번호)) %>% 
  select(-c(법정동코드, 법정리:동리))%>% 
  distinct(코드, .keep_all = TRUE) %>%
  mutate(중복자료 = ifelse(duplicated(코드), "중복", ""))

  


##########  주소 정리  ##############################################################


### 주소검토
test <- waterusage %>% select(시군, 수용가번호, 주소1, 주소2) %>% 
  # filter(시군 == "철원군") %>% 
  mutate(주소1수정 =
           # '00번길' 또는 '00길'앞에 띄어쓰기가 되어 있는 경우 공백 제거
           str_replace(주소1, " 번길", "번길") %>%
           str_replace(., "[\\s][0-9]{1,}(번길)", str_extract(주소1, "[0-9]{1,}(번길)")) %>%
           str_replace(., "[\\s][0-9]{1,}(길)", str_extract(주소1, "[0-9]{1,}(길)")) %>%
           # 리 앞에 숫자가 있는 경우('00리') 숫자 삭제
           str_replace(., "[0-9]{1,}(리)", "리") %>%
           # '00반' 삭제
           str_remove(., "[0-9]{1,}(반)") %>%
           str_trim(),
         주소2수정 =
           # '00번길' 또는 '00길'앞에 띄어쓰기가 되어 있는 경우 공백 제거
           str_replace(주소2, " 번길", "번길") %>%
           str_replace(., "[\\s][0-9]{1,}(번길)", str_extract(주소2, "[0-9]{1,}(번길)")) %>%
           str_replace(., "[\\s][0-9]{1,}(길)", str_extract(주소2, "[0-9]{1,}(길)")) %>%
           # 리 앞에 숫자가 있는 경우('00리') 숫자 삭제
           str_replace(., "[0-9]{1,}(리)", "리") %>%
           # '00반' 삭제
           str_remove(., "[0-9]{1,}(반)") %>%
           str_trim(),
         # 읍면
         읍면1 = str_extract(주소1수정, "[가-힣0-9]{1,}(읍|면)(?![0-9](가)|동|가|리|로|길)") %>% 
           str_remove(., "[0-9]{1,}") %>% 
           str_trim(), 
         읍면2 = str_extract(주소2수정, "[가-힣0-9]{1,}(읍|면)(?![0-9](가)|동|가|리|로|길)") %>% 
           str_remove(., "[0-9]{1,}") %>% 
           str_trim(),
         # 읍면 확인(앞뒤로 여백 추가해서 일부만 일치하는 경우 방지)
         읍면1 = ifelse(str_detect(읍면_check, str_c(" ", 읍면1, " ")), 읍면1, NA),
         읍면2 = ifelse(str_detect(읍면_check, str_c(" ", 읍면2, " ")), 읍면2, NA),
         # 동리(주소에서 읍면 삭제 후 추출)
         동리1 = ifelse(is.na(읍면1), 
                      주소1수정,
                      str_remove(주소1수정, 읍면1)), 
         동리1 = ifelse(str_sub(str_extract(동리1, "[가-힣0-9]{1,}(동|가|리)") %>% 
                                str_trim(), -1, -1) == "가",
                      str_extract(동리1, "[가-힣0-9]{1,}(가)") %>% str_trim(),
                      str_extract(동리1, "[가-힣0-9]{1,}(동|리)") %>% str_trim()),
         동리2 = ifelse(is.na(읍면2), 
                      주소2수정,
                      str_remove(주소2수정, 읍면2)),
         동리2 = ifelse(str_sub(str_extract(동리2, "[가-힣0-9]{1,}(동|가|리)") %>%
                                str_trim(), -1, -1) == "가",
                      str_extract(동리2, "[가-힣0-9]{1,}(가)") %>% str_trim(),
                      str_extract(동리2, "[가-힣0-9]{1,}(동|리)") %>% str_trim()), 
         # 동리 확인(앞뒤로 여백 추가해서 일부만 일치하는 경우 방지)
         동리1 = ifelse(str_detect(동리_check, str_c(" ", 동리1, " ")), 동리1, NA),
         동리2 = ifelse(str_detect(동리_check, str_c(" ", 동리2, " ")), 동리2, NA),
         # 도로명
         도로명1 = str_extract(주소1수정, "[가-힣A-Za-z0-9]{1,}(로|길)(?![0-9](가)|읍|면|동|가|리)"),
         도로명2 = str_extract(주소2수정, "[가-힣A-Za-z0-9]{1,}(로|길)(?![0-9](가)|읍|면|동|가|리)"),
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
           str_extract(., "[0-9]{1,}")),
         건물부번1 = ifelse(is.na(도로명1), NA,
           주소1수정 %>% 
           str_split_i(., 도로명1, 2) %>%
           str_extract(., "[0-9\\-]{1,}") %>%
           str_extract(., "(?<=\\-)[0-9]{1,}")),
         건물본번2 = ifelse(is.na(도로명2), NA,
           주소2수정 %>% 
           str_split_i(., 도로명2, 2) %>%
           str_extract(., "[0-9]{1,}")),
         건물부번2 = ifelse(is.na(도로명2), NA,
           주소2수정 %>% 
           str_split_i(., 도로명2, 2) %>%
           str_extract(., "[0-9\\-]{1,}") %>%
           str_extract(., "(?<=\\-)[0-9]{1,}")),
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
             ifelse(is.na(읍면), "", str_c(읍면, " ")),
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
         동리확인 = ifelse(is.na(동리), "x", ""),
         주소확인 = ifelse(is.na(동리) & is.na(도로명주소) & is.na(지번주소), "x", ""),
         코드 = str_c(시군, 수용가번호)
  )


### 주소코드 합치기
test1 <- test %>% 
  left_join(waterusage_2021_dong %>% select(코드, 주소코드), by = "코드") %>% 
  mutate(주소코드2 = str_c(시군, 읍면동, 동리, sep = " "), 
    주소코드확인 = ifelse(is.na(주소코드) & is.na(주소코드2), "x", ""), 
    주소코드확인2 = ifelse(주소코드 != 주소코드2, "x", ""))








###########################################################################################
waterusage1 <- waterusage %>%
  mutate(
    # 주소 1, 2 합치기
    # 괄호, 쉼표, 문장부호로 시작하는 경우 삭제
    주소1 = ifelse(is.na(주소1), "", 주소1),
    주소2 = ifelse(is.na(주소2), "", 주소2),
    주소 = str_c(str_replace(주소1, '(\\[|\\(|\\,|\\")[:print:]{2,}', ""),
      str_replace(주소2, '(\\[|\\(|\\,|\\")[:print:]{2,}', ""),
      sep = " "
    ),
    # 주소 1차 수정 :
    주소수정 =
    # '00번길' 또는 '00길'앞에 띄어쓰기가 되어 있는 경우 공백 제거
      str_replace(주소, " 번길", "번길") %>%
        str_replace(., "[\\s][0-9]{1,}(번길)", str_extract(주소, "[0-9]{1,}(번길)")) %>%
        str_replace(., "[\\s][0-9]{1,}(길)", str_extract(주소, "[0-9]{1,}(길)")) %>%
        # 리 앞에 숫자가 있는 경우('00리') 숫자 삭제
        str_replace(., "[0-9]{1,}(리)", "리") %>%
        # '00반' 삭제
        str_remove(., "[0-9]{1,}(반)") %>%
        str_trim(),
    읍면 = str_extract(주소수정, "[가-힣0-9]{1,}(읍 |면 )") %>% 
      str_remove(., "[0-9]{1,}") %>% 
      str_trim(),
    # 읍면 기준으로 주소 분리(읍면이 없는 경우 무시)
    # 뒤에 '로', '길'이 없는경우로 한정 : (?![가-힣]{0,}(로|길))
    동리 = ifelse(is.na(읍면), 주소수정, str_split_i(주소수정, 읍면, 2)),
    동리 = ifelse(str_sub(str_extract(동리, "[가-힣0-9]{1,}(동 |가 |리 )") %>% str_trim(), -1, -1) == "가",
      str_extract(동리, "[가-힣0-9]{1,}(가 )") %>% str_trim(),
      str_extract(동리, "[가-힣0-9]{1,}(동 |리 )") %>%
        str_remove_all(., "[0-9]{1,}") %>% str_trim()
    ),

    # 도로명 : "로" 또는 "길"로 끝나고 뒤에 "00가" 없는 경우
    도로명 = str_extract(주소수정, "[가-힣A-Za-z0-9]{1,}(로|길)(?![0-9](가))"),
    건물본번 = 
      # ifelse(is.na(동리), 주소수정, str_split_i(주소수정, 동리, 2)) %>%
      주소수정 %>% 
      str_split_i(., 도로명, 2) %>%
      str_extract(., "[0-9]{1,}"),
    건물부번 = 
      # ifelse(is.na(동리), 주소수정, str_split_i(주소수정, 동리, 2)) %>%
      주소수정 %>% 
      str_split_i(., 도로명, 2) %>%
      str_extract(., "[0-9\\-]{1,}") %>%
      str_extract(., "(?<=\\-)[0-9]{1,}"),
    읍면동 = ifelse(str_sub(str_extract(주소수정, "[가-힣0-9]{1,}(읍|면|동|가)"), -1, -1) == "가",
      str_extract(주소수정, "[가-힣0-9]{1,}(읍|면|동|가)"),
      str_extract(주소수정, "[가-힣0-9]{1,}(읍|면|동|가)") %>%
        str_remove(., "[0-9]")) %>% 
      str_trim(),
    # 읍면동 기준으로 주소 분리(읍면동이 없는 경우 무시)
    # 리 = ifelse(is.na(읍면동), 주소수정, str_split_i(주소수정, 읍면동, 2)),
    리 = str_extract(주소수정, "(?<=읍 |면 )[가-힣0-9]{1,}(리)") %>%
      str_remove_all(., "[0-9]{1,}"),
    산 = str_extract(주소수정, "(?<=동 |리 |가 )(산)"),
    본번 = str_extract(주소수정, "(?<=동 |동|리 |리|가 |가|산 |산)[0-9]{1,}(?!로 |길 )"),
    부번 = str_extract(주소수정, "(?<=동 |동|리 |리|가 |가|산 |산)[0-9\\-]{1,}") %>%
      str_extract(., "(?<=\\-)[0-9]{1,}"),
    # 주소 합치기
    도로명주소 =
      str_c(
        "강원도", " ", 시군, " ",
        ifelse(is.na(읍면), "", str_c(읍면, " ")),
        도로명, " ", 건물본번,
        ifelse(건물부번 == 0 | is.na(건물부번), "", str_c("-", 건물부번))
      ),
    지번주소 =
      str_c(
        "강원도", " ", 시군, " ", 읍면동, " ",
        ifelse(is.na(리), "", str_c(리, " ")),
        ifelse(is.na(산), "", str_c(산, " ")),
        본번,
        ifelse(부번 == 0 | is.na(부번), "", str_c("-", 부번))
      ),
    대표주소 =
      ifelse(is.na(도로명주소), 지번주소, 도로명주소),
    확인 = ifelse(is.na(대표주소), 1, 0)
  )


##########  주소 좌표 변환  ###############################################################

library(httr)
library(jsonlite) # fromJSON()
library(progress)

## REST API 키(https://developers.kakao.com/)
KAKAO_MAP_API_KEY <- "9e9a85a9ec8362e009da2f7bc4b3a09c"

## 수용가번호 및 대표주소만 추출
address <- waterusage1 %>% 
  select(수용가번호, 대표주소) %>% 
  filter(!is.na(대표주소)) %>% 
  rowid_to_column(var = "ID") %>% filter(ID > 90631) %>% select(-ID) %>% rowid_to_column(var = "ID")

address_list <- address$대표주소

## progress bar 설정
pb <- progress_bar$new(
  format = " Progress: [:bar] :current / :total (:percent), Estimated completion time::eta",
  total = nrow(address), # 총 tick 개수 (default 100)
  clear = FALSE, # 종료 후에도 진행 경과 출력 결과 유지 (default TRUE)
  width= 80 # 진행 경과 막대 너비
)

result_doro <- data.frame()
result_jibun <- data.frame()

for (i in 1:nrow(address)) {
  place_list <- GET(
    url = "https://dapi.kakao.com/v2/local/search/address.json",
    query = list(query = address_list[i]),
    add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY))) %>%
    content(as = "text") %>%
    fromJSON()
  
  # 수용가번호 및 기존 주소 불러오기
  temp_addr <- address %>% filter(ID == i)
  
  ## 도로명주소 : 주소 검색결과가 없는 경우 대비 test열(NA값 입력) 추가
  temp_doro <- bind_cols(place_list$documents$road_address, tibble(test = NA))
  
  # 도로명주소가 없는 경우(test열이 1열에 위치) 모든 열 NA로 입력
  if (is.na(temp_doro[1, 1])) {
    temp_doro <- tibble(address_name = NA, building_name = NA, road_name = NA, 
                        main_building_no = NA, sub_building_no = NA, 
                        x = NA, y = NA)
  } else {
    temp_doro <- temp_doro %>% 
      select(address_name, building_name, road_name, 
             main_building_no, sub_building_no, x, y)
  }
  
  temp_doro <-  bind_cols(temp_addr, temp_doro)
  
  result_doro <- bind_rows(result_doro, temp_doro)
  
  ## 지번주소
  temp_jibun <- bind_cols(temp_addr, place_list$documents$address)
  
  result_jibun <- bind_rows(result_jibun, temp_jibun) 
  
  ## 진행상황 확인
  pb$tick()
}


result_jibun2 <- result_jibun %>% 
  select(-6, -9) %>% 
  set_names("ID", "수용가번호", "원본주소", "지번주소", "법정동코드", "본번", "산", "시군", 
            "읍면동", "동리", "부번", "경도_지번", "위도_지번")


result_doro2 <- result_doro %>%  
  set_names("ID", "수용가번호", "원본주소", "도로명주소", "건물명", "도로명", 
            "건물본번", "건물부번", "경도_도로", "위도_도로")





# temp_doro <- bind_cols(address %>% filter(ID == i),
#                       place_list$documents$road_address) %>%
#   select(-starts_with("."))


##################################################################################


place_list <- GET(
  url = "https://dapi.kakao.com/v2/local/search/address.json",
  query = list(query = "강원도 영월군 영월읍 영흥리 963-1"),
  add_headers(Authorization = paste0("KakaoAK ", KAKAO_MAP_API_KEY))) %>%
  content(as = "text") %>%
  fromJSON()



row_temp1 <- tibble(
  'address$address_name' = ifelse(is.null(place_list$documents$address$address_name),
                                  NA, 
                                  place_list$documents$address$address_name))

row_temp2 <- ifelse(is.null(place_list$documents),
                    tibble(address$address_name = address_list[1]), 
                    row_temp1)

row_temp <- tibble(
  address_name = ifelse(is.null(place_list$documents$address$address_name), address_list[i], place_list$documents$address$address_name),
  region_2depth_name = ifelse(is.null(place_list$documents$address$region_2depth_name), "", place_list$documents$address$region_2depth_name), 
  region_3depth_h_name = ifelse(is.null(place_list$documents$address$region_3depth_h_name), "", place_list$documents$address$region_3depth_h_name), 
  region_3depth_name = ifelse(is.null(place_list$documents$address$region_3depth_name), "", place_list$documents$address$region_3depth_name), 
  x = ifelse(is.null(place_list$documents$address$x), "", place_list$documents$address$x), 
  y = ifelse(is.null(place_list$documents$address$y), "", place_list$documents$address$y)
)