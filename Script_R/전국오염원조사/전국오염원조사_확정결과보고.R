##### 관련 패키지 로드   #######################################################
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

#####  함수 정의  ##############################################################
## 반올림 사용자 정의 함수 로드
source("Script_R/Function/round2func.R")

##### 1. 소계 계산 함수(분류가 없는 경우)  ###### ------------------------------
subtotal_1 <- function(data) {
  data %>%
    # 시군별 합계
    group_by(연도) %>%
    group_modify(~ .x %>% adorn_totals(where = "row", name = "강원도"))
}


##### 2. 소계 계산 함수(분류가 있는 경우)  ###### ------------------------------
subtotal_2 <- function(data, class) {
  data %>%
    # 시군별 합계
    group_by(연도, !!sym(class)) %>%
    group_modify(~ .x %>% adorn_totals(where = "row", name = "강원도")) %>%
    group_by(연도, 시군) %>%
    group_modify(~ .x %>% adorn_totals(where = "row", name = "합계"))
}


##### 3. 단위유역/시군 순서 지정 함수  ###### ----------------------------------
order_func <- function(data, class = "") {
  data %>% 
    mutate(
      시군 = factor(시군, levels = c(
        "강원도", "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군",
        "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군",
        "양구군", "인제군", "고성군", "동해시", "속초시", "양양군"
      )),
    ) %>%
    arrange(시군, !!sym(class))
}




##**************************************************************************** ##
###############################  생활계 - 인구  ################################
##**************************************************************************** ##


## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/생활계/",
  pattern = "*.xls", full.names = T
)

# map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
인구_원본 <- files %>%
  map_dfr(read_excel, skip = 4, col_names = F) %>%
  select(-c(7:18)) # 필요없는 열 삭제
## *****************************************************************************


## 인구데이터 정리
인구_정리 <- 인구_원본 %>%
  # 수치데이터 및 연도 숫자로 지정
  mutate(across(c(1, 7:23), as.numeric)) %>% 
  # 변수명 지정
  set_names(c(
    "연도", "행정구역코드", "시도", "시군", "법정동", "법정리", "가정인구합계",
    "시가지역인구합계", "시가인구_하수처리구역_소계", "시가인구_하수처리구역_분류식",
    "시가인구_하수처리구역_합류식", "시가인구_하수미처리구역_소계",
    "시가인구_하수미처리구역_오수처리", "시가인구_하수미처리구역_정화조",
    "시가인구_하수미처리구역_수거식", "비시가지역인구합계",
    "비시가인구_하수처리구역_소계", "비시가인구_하수처리구역_분류식",
    "비시가인구_하수처리구역_합류식", "비시가인구_하수미처리구역_소계",
    "비시가인구_하수미처리구역_오수처리", "비시가인구_하수미처리구역_정화조",
    "비시가인구_하수미처리구역_수거식"
  ))%>%
  # 주소코드 추가("리"가 없는 "동"의 경우 "법정리"칸에 "동"으로 추가)
  mutate(주소 = str_c(시군, 법정동, ifelse(is.na(법정리), 법정동, 법정리), sep = " "))


인구_total <- 인구_정리 %>%
  group_by(연도, 시군) %>%
  summarise(
    가정인구합계 = sum(가정인구합계), 
    하수처리인구 = sum(시가인구_하수처리구역_소계) + sum(비시가인구_하수처리구역_소계),
    분류식 = sum(시가인구_하수처리구역_분류식) + sum(비시가인구_하수처리구역_분류식),
    합류식 = sum(시가인구_하수처리구역_합류식) + sum(비시가인구_하수처리구역_합류식),
    하수미처리인구 = sum(시가인구_하수미처리구역_소계) + sum(비시가인구_하수미처리구역_소계),
    오수처리 = sum(시가인구_하수미처리구역_오수처리) + sum(비시가인구_하수미처리구역_오수처리),
    정화조 = sum(시가인구_하수미처리구역_정화조) + sum(비시가인구_하수미처리구역_정화조),
    수거식 = sum(시가인구_하수미처리구역_수거식) + sum(비시가인구_하수미처리구역_수거식),
    .groups = "drop"
  ) %>%
  subtotal_1() %>% 
  mutate(처리율 = round(하수처리인구 / 가정인구합계 * 100)) %>% 
  order_func()


인구_처리 <- 인구_total %>%
  select(연도, 시군, 가정인구합계, 처리율, 하수처리인구, 하수미처리인구) %>% 
  mutate(총인구 = 가정인구합계) %>% 
  pivot_longer(
    cols = c(총인구, 하수처리인구, 하수미처리인구),         
    names_to = "구분",
    values_to = "인구"
  ) %>% 
  mutate(라벨위치 = ifelse(구분 == "하수처리인구", 인구 - 인구*0.5, 가정인구합계 - 인구*0.5))

##**************************************************************************** ##
###################################  축산계  ###################################
##**************************************************************************** ##


## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/축산계",
  pattern = "*.xls", full.names = T
)

# 데이터 불러오기 및 합치기
축산계_원본 <- files %>%
  map_dfr(~ {
    data <- read_excel(.x, skip = 6, col_names = F) 
    year <- str_sub(basename(.x), 1, 4) %>% as.integer()
    data <- data %>% mutate(연도 = year, .before = 1) 
  })
## *****************************************************************************


## 변수명 지정 및 데이터 정리
축산계_정리 <- 축산계_원본 %>%
  select(연도, 12:14, 17, 18) %>% 
  set_names(c(
    "연도", "시군", "법정동", "법정리", "축종", "사육두수"
  )) %>%
  # 사육두수, 연도 숫자로 지정
  mutate(across(c(사육두수, 연도), as.numeric)) %>%
  # 사육두수 결측인 경우 0으로 수정
  mutate(across(c(사육두수), ~ replace(., is.na(.), 0))) %>%
  mutate(
    # 주소코드 추가("리"가 없는 "동"의 경우 "법정리"칸에 "동"으로 추가)
    주소 = str_c(시군, 법정동, ifelse(is.na(법정리), 법정동, 법정리), sep = " "),
    # 축산계 축종 변환(총량 기술지침 발생원단위 기준 축종명으로 변경)
    축종 = case_when(
      축종 == "한우(소)" ~ "한우",
      축종 == "유우(젖소)" ~ "젖소",
      축종 == "돼지" ~ "돼지",
      축종 == "마필(말)" ~ "말",
      축종 == "산양(염소포함)" | 축종 == "면양(육양포함)" | 축종 == "사슴" ~ "양, 사슴",
      축종 == "개" ~ "개",
      축종 == "닭" | 축종 == "오리" | 축종 == "타조" | 축종 == "가금기타" ~ "가금",
      TRUE ~ "-"
    )
  )

## 연도 및 시군별 농가수, 사육두수 합계 정리
축산계_total <- 축산계_정리 %>%
  group_by(연도, 시군, 축종) %>%
  # 농가수
  tally(name = "농가수") %>% 
  # 사육두수
  left_join(축산계_정리 %>%
              group_by(연도, 시군, 축종) %>%
              summarise(사육두수 = sum(사육두수), .groups = "drop"), 
            by = c("연도", "시군", "축종")) %>% 
  subtotal_2(., "축종") %>% 
  mutate(축종 = factor(축종, levels = c(
    "합계", "젖소", "한우", "말", "돼지", "양, 사슴", "개", "가금"
  ))) %>% 
  order_func(., "축종")



##**************************************************************************** ##
###################################  산업계  ###################################
##**************************************************************************** ##

## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/산업계",
  pattern = "*.xls", full.names = T
)

## Excel 파일을 읽어와 데이터 합치기
산업계_원본 <- files %>%
  map_dfr(~ {
    data <- read_excel(.x, skip = 5, col_names = F) 
    year <- str_sub(basename(.x), 1, 4) %>% as.integer()
    data <- data %>% mutate(연도 = year, .before = 1) 
    # 2022년 이전 자료의 경우 부분위탁량(76열) 뒤에 폐기물처리 항목 추가
    if (year < 2022) {
      data %<>% mutate(폐기물처리 = "", .after = 77)
    } else {
      data %<>% rename(폐기물처리 = ...77)
    }
  })
## *****************************************************************************


## 변수명 지정 및 데이터 정리
산업계_정리 <- 산업계_원본 %>%
  select(연도, 4, 6, 8, 9, 10, 11, 12, 13, 24, 73, 82) %>%
  set_names(c(
    "연도", "휴업", "업소명", "시도", "시군", "법정동", "법정리", 
    "본번", "부번", "규모", "폐수발생량", "폐수방류량"
  )) %>% 
  # 휴업인 경우 삭제
  filter(is.na(휴업)) %>%
  # 폐수발생량, 폐수방류량, 연도 숫자로 지정 및 결측값 0으로 수정
  mutate(across(c(폐수발생량, 폐수방류량, 연도), as.numeric)) %>%
  mutate(across(c(폐수발생량, 폐수방류량, 연도), ~ replace(., is.na(.), 0))) %>%
  # 폐수방류량이 음수인 경우 0으로 수정
  mutate(
    폐수방류량 = ifelse(폐수방류량 < 0, 0, 폐수방류량),
    # 주소코드 추가("리"가 없는 "동"의 경우 "법정리"칸에 "동"으로 추가)
    주소 = str_c(시군, 법정동, ifelse(is.na(법정리), 법정동, 법정리), sep = " ")
  )

## 연도 및 시군별 합계 정리
산업계_total <- 산업계_정리 %>%
  mutate(업소수 = 1) %>%
  group_by(연도, 시군, 규모) %>%
  summarise(
    업소수 = sum(업소수),
    폐수발생량 = sum(폐수발생량),
    폐수방류량 = sum(폐수방류량),
    .groups = "drop"
  ) %>% 
  subtotal_2(., "규모") %>% 
  order_func()


##**************************************************************************** ##
###################################  토지계  ###################################
##**************************************************************************** ##

## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/토지계",
  pattern = "*.xls", full.names = T
)

## Excel 파일을 읽어와 데이터 합치기
토지계_원본 <- files %>%
  map_dfr(~ {
    data <- read_excel(.x, skip = 2, col_names = F) 
    year <- str_sub(basename(.x), 1, 4) %>% as.integer()
    data <- data %>% mutate(연도 = year, .before = 1) 
  })
## *****************************************************************************

## 데이터 정리
토지계_정리 <- 토지계_원본 %>%
  select(-2) %>%
  set_names(c(
    "연도", "시도", "시군", "법정동", "법정리", "총면적", "전", "답", "과수원",
    "목장용지", "임야", "광천지", "염전", "대지", "공장용지", "학교용지",
    "주차장", "주유소용지", "창고용지", "도로", "철도용지", "제방", "하천",
    "구거", "유지", "양어장", "수도용지", "공원", "체육용지", "유원지",
    "종교용지", "사적지", "묘지", "잡종지"
  )) %>% 
  # 강원도 외 타 시도 제외
  filter(시도 == "강원도") %>% 
  # 지목별 면적 및 연도 숫자로 지정
  mutate(across(c(연도, 총면적:잡종지), as.numeric)) %>%
  mutate(
    # 주소코드 추가("리"가 없는 "동"의 경우 "법정리"칸에 "동"으로 추가)
    주소 = str_c(시군, 법정동, ifelse(is.na(법정리), 법정동, 법정리), sep = " "),
    # 대표지목으로 정리 : 대지, 임야, 전(과수원 포함), 답, 기타(그 외 나머지)
    전 = 전 + 과수원,
    기타 = 목장용지 + 공원 + 묘지 + 사적지 + 광천지 + 염전 + 제방 + 하천 + 
      구거 + 유지 + 양어장 + 잡종지 + 학교용지 + 창고용지 + 종교용지 + 
      주차장 + 도로 + 철도용지 + 수도용지 + 공장용지 + 주유소용지 + 체육용지 +
      유원지
  ) %>%
  # 기타 등으로 합쳐진 지목 제외
  select(연도, 주소, everything(), -c(
    과수원, 목장용지, 공원, 묘지, 사적지, 광천지, 염전, 제방, 하천, 구거, 유지,
    양어장, 잡종지, 학교용지, 창고용지, 종교용지, 주차장, 도로, 철도용지, 
    수도용지, 공장용지, 주유소용지, 체육용지, 유원지
  )) %>%
  pivot_longer(cols = 전:기타, names_to = "지목", values_to = "면적")


## 연도 및 시군별 지목 면적 정리
토지계_total <- 토지계_정리 %>%
  group_by(연도, 시군, 지목) %>%
  summarise(면적 = sum(면적), .groups = "drop") %>% 
  subtotal_2(., "지목") %>% 
  mutate(
    면적 = round(면적 / (10^6), 2),
    지목 = factor(지목, levels = c(
    "합계", "전", "답", "임야", "대지", "기타"
  ))) %>% 
  order_func(., "지목")


##**************************************************************************** ##
###################################  양식계  ###################################
##**************************************************************************** ##

## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/양식계",
  pattern = "*.xls", full.names = T
)

# 데이터 불러오기 및 합치기
양식계_원본 <- files %>%
  map_dfr(~ {
    data <- read_excel(.x, skip = 3, col_names = F) 
    year <- str_sub(basename(.x), 1, 4) %>% as.integer()
    data <- data %>% mutate(연도 = year, .before = 1) 
    # 2021년 이전 자료의 경우 인허가관리번호 항목 3개 추가(1열 뒤)
    if (year < 2021) {
      data %<>% mutate(
        어업양식업면허대장 = NA,
        양식업허가대장 = NA,
        새올행정시스템 = NA,
        .after = 2
      )
    } else {
      data %<>% rename(
        어업양식업면허대장 = ...2,
        양식업허가대장 = ...3,
        새올행정시스템 = ...4
      )
    }
    
  })

## *****************************************************************************


## 변수명 지정 및 데이터 정리
양식계_정리 <- 양식계_원본 %>%
  select(연도, 7, 10:12, 18, 22, 28, 30) %>%
  set_names(c(
    "연도", "업소명", "시군", "법정동", "법정리", "종류",
    "시설면적", "방류하천", "휴업"
  )) %>%
  # 시설면적, 연도 숫자로 지정
  mutate(across(c(시설면적, 연도), as.numeric)) %>%
  # 주소코드 추가("리"가 없는 "동"의 경우 "법정리"칸에 "동"으로 추가)
  mutate(주소 = str_c(시군, 법정동, ifelse(is.na(법정리), 법정동, 법정리), sep = " "))


## 연도 및 시군별 양식장수 합계 정리
양식계_total <- 양식계_정리 %>%
  # 휴업인 경우 제외
  filter(is.na(휴업)) %>% 
  group_by(연도, 시군, 종류) %>%
  # 양식장수
  tally(name = "양식장수") %>% 
  subtotal_2(., "종류") %>% 
  mutate(종류 = factor(종류, levels = c(
    "합계", "가두리", "유수식", "지수식", "도전양식"
  ))) %>% 
  order_func(., "종류")



##**************************************************************************** ##
###################################  매립계  ###################################
##**************************************************************************** ##

## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/매립계",
  pattern = "*.xls", full.names = T
)

# 데이터 불러오기 및 합치기
매립계_현황 <- files %>%
  map_dfr(~ {
    data <- read_excel(.x, skip = 3, col_names = F) 
    year <- str_sub(basename(.x), 1, 4) %>% as.integer()
    data <- data %>% mutate(연도 = year, .before = 1) 
  })

# 데이터 불러오기 및 합치기
매립계_발생량 <- files %>%
  map_dfr(~ {
    data <- read_excel(.x, , sheet = 2, skip = 2, col_names = F) 
    year <- str_sub(basename(.x), 1, 4) %>% as.integer()
    data <- data %>% mutate(연도 = year, .before = 1) 
  })
## *****************************************************************************

## 매립장현황 데이터 정리
매립계_현황 %<>%
  select(연도, 2, 3, 6, 18) %>%
  set_names(c(
    "연도", "시설명", "시설코드", "시군", "가동유무"
  )) %>%
  mutate(연도 = as.numeric(연도))


## 침출수 발생유량 연평균 계산
매립계_정리 <- 매립계_발생량 %>%
  select(1:3, 5) %>%
  set_names(c("연도", "시설명", "시설코드", "발생유량")) %>% 
  mutate(across(c(연도, 발생유량), as.numeric)) %>%
  group_by(연도, 시설명, 시설코드) %>%
  summarise(처리량 = round2(mean(발생유량), 1), .groups = "drop") %>%
  mutate_all(~ replace(., is.na(.), 0)) %>% 
  mutate(종류 = "매립장", .after = 연도) %>% 
  left_join(매립계_현황 %>% select(연도, 시설명, 시설코드, 시군),
            by = c("연도", "시설명", "시설코드"))



##**************************************************************************** ##
################################  환경기초시설  ################################
##**************************************************************************** ##

## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/환경기초시설",
  pattern = "*.xls", full.names = T
)

## 시설현황 자료 불러오기
환경기초시설_현황_원본 <- files %>%
  map_dfr(~ {
    data <- read_excel(.x, skip = 3, col_names = F) 
    year <- str_sub(basename(.x), 1, 4) %>% as.integer()
    data <- data %>% mutate(연도 = year, .before = 1) 
  })

## 유입량 자료 불러오기
환경기초시설_유입량_원본 <- files %>%
  map_dfr(~ {
    data <- read_excel(.x, sheet = 2, skip = 3, col_names = F) 
    year <- str_sub(basename(.x), 1, 4) %>% as.integer()
    data <- data %>% mutate(연도 = year, .before = 1) 
  })
## *****************************************************************************

## 현황 데이터 정리
환경기초시설_현황_정리 <- 환경기초시설_현황_원본 %>%
  mutate(across(c(18:20), ~ replace(., is.na(.), 0))) %>%
  mutate(across(c(18:20), as.numeric)) %>%
  # 시설명 기준으로 추가 자료 결합
  rename(시설코드 = 4) %>%
  # 기타자료 추가
  mutate(시설용량 = `...18` + `...19` + `...20`) %>%
  select(
    연도, 2:11, 시설용량, 26
  ) %>%
  set_names(
    c(
      "연도", "법정동코드", "시설명", "시설코드", "구분", 
      "시도", "시군", "읍면동", "리", "본번", "부번", "시설용량", 
      "가동개시일자"
    )
  ) %>%
  mutate(종류 = str_sub(시설코드, 7, 7)) %>%
  mutate(종류 = case_when(
    종류 == "W" ~ "공공하수", # 500톤 이상 공공하수처리시설
    종류 == "V" ~ "소규모하수", # 500톤 미만 소규모 하수처리시설
    종류 == "A" ~ "공공폐수",
    종류 == "I" ~ "공공폐수",
    종류 == "F" ~ "분뇨(생활가축)",
    종류 == "S" ~ "분뇨(생활가축)",
    종류 == "H" ~ "소규모하수"
  ))


## 유입량 데이터 정리
환경기초시설_유입량_정리 <- 환경기초시설_유입량_원본 %>%
  select(1:6) %>%
  mutate(across(c(5:6), as.numeric)) %>%
  set_names(
    c(
      "연도", "시설명", "시설코드", "운영일", "유입구번호", "유입유량")
  ) %>%
  # mutate(
  #   운영일 = as.Date(운영일),
  #   운영년도 = year(운영일),
  #   운영월 = month(운영일),
  #   운영일 = day(운영일)
  # ) %>%
  # relocate(c(운영년도, 운영월), .after = 시설코드) %>% 
  distinct() %>%
  mutate(종류 = str_sub(시설코드, 7, 7)) %>%
  mutate(종류 = case_when(
    종류 == "W" ~ "공공하수", # 500톤 이상 공공하수처리시설
    종류 == "V" ~ "소규모하수", # 500톤 미만 소규모 하수처리시설
    종류 == "A" ~ "공공폐수",
    종류 == "I" ~ "공공폐수",
    종류 == "F" ~ "분뇨(생활가축)",
    종류 == "S" ~ "분뇨(생활가축)",
    종류 == "H" ~ "소규모하수"
  ))

  
## 환경기초시설 시설수 및 처리량(유입량)
환경기초시설_total <- 환경기초시설_유입량_정리 %>%
  # 유입구가 여러개인 경우 합치기
  group_by(연도, 종류, 시설명, 시설코드, 운영일) %>% 
  summarise(유입유량 = sum(유입유량), .groups = "drop") %>% 
  # 연평균 유입유량 산정
  group_by(연도, 종류, 시설명, 시설코드) %>% 
  summarise(처리량 = round2(mean(유입유량), 1), .groups = "drop") %>% 
  left_join(환경기초시설_현황_정리 %>% select(연도, 시설명, 시설코드, 시군),
            by = c("연도", "시설명", "시설코드")) %>% 
  # 매립장 합치기
  bind_rows(., 매립계_정리) %>% 
  group_by(연도, 시군, 종류) %>% 
  summarise(시설수 = sum(!is.na(시설명)),
            처리량 = sum(처리량),
            .groups = "drop") %>% 
  subtotal_2(., "종류") %>% 
  mutate(종류 = factor(종류, levels = c(
    "합계", "공공하수", "소규모하수", "공공폐수",
    "분뇨(생활가축)", "매립장"
  ))) %>% 
  order_func(., "종류")
  


##**************************************************************************** ##
###################################  그래프  ###################################
##**************************************************************************** ##

## 그래프_생활계_지역별인구현황 -----
인구_total %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(연도 == 2021, 시군 != "강원도") %>% 
  # "시", "군" 삭제
  mutate(시군 = str_remove(시군, "(시|군)")) %>% 
  ggplot(aes(x = 시군, y = 가정인구합계 * 0.001, fill = 시군)) +
  geom_bar(stat='identity') +
  geom_text(aes(label = round(가정인구합계 * 0.001)), vjust = 1.3,
            position = position_dodge(0.9), size = 4)+
  scale_x_discrete(limits = c("춘천", "원주", "강릉", "동해", "태백", "속초",
                              "삼척", "홍천", "횡성", "영월", "평창", "정선",
                              "철원", "화천", "양구", "인제", "고성", "양양")) +
  scale_y_continuous(name = "인구(천명)", breaks = seq(0, 350, by=50)) +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = "none")


## 그래프_생활계_지역별처리인구현황 -----
인구_처리 %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(연도 == 2021, 시군 != "강원도", 구분 != "총인구") %>% 
  # "시", "군" 삭제
  mutate(시군 = str_remove(시군, "(시|군)")) %>% 
  ggplot(aes(x = 시군, y = 인구 * 0.001, fill = 구분)) +
  geom_bar(stat='identity') +
  geom_text(aes(y = 라벨위치 * 0.001, label = round(인구 * 0.001)),
            size = 4, check_overlap = TRUE) +
  geom_text(aes(y = 가정인구합계 * 0.001, label = round(가정인구합계 * 0.001)),
            vjust = -1.6, size = 4.3, color = "blue")+
  geom_text(aes(y = 가정인구합계 * 0.001, label = str_c("(", 처리율, "%)")),
            vjust = -0.6, size = 3.5, color = "blue")+
  scale_x_discrete(limits = c("춘천", "원주", "강릉", "동해", "태백", "속초",
                              "삼척", "홍천", "횡성", "영월", "평창", "정선",
                              "철원", "화천", "양구", "인제", "고성", "양양")) +
  scale_y_continuous(name = "인구(천명)", breaks = seq(0, 400, by = 50), limits = c(0, 400)) +
  theme_classic(base_family = "notosanskr") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = c(0.80, 0.80),
    panel.border = element_rect(fill = NA)
  )


## 그래프_생활계_연도별인구변화 -----
인구_처리 %>% 
  filter(시군 == "강원도") %>% 
  ggplot(aes(x = 연도, y = 인구 * 0.001, color = 구분)) +
  geom_line(stat='identity', linewidth = 0.8) +
  geom_point(stat='identity', size = 2) +
  geom_text(aes(label = comma(round(인구 * 0.001))), size = 3.5, vjust = -0.5, 
            color = "black", check_overlap = TRUE) +
  scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "인구(천명)", breaks = seq(0, 1800, by = 400), 
                     limits = c(0, 1800), labels = scales::comma) +
  theme_calc(base_family = "notosanskr") +
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

## 그래프_축산계_가축사육두수변화 -----
축산계_total %>% 
  filter(시군 == "강원도", 축종 %in% c("합계", "한우", "젖소", "돼지", "가금")) %>% 
  ggplot() +
  geom_bar(data = . %>% filter(축종 == "합계"), 
           aes(x = 연도, y = 사육두수 * 0.00009, fill = "총 사육두수"), 
           stat='identity', width = 0.7) +
  geom_line(data = . %>% filter(축종 == "가금"),
            aes(x = 연도, y = 사육두수 * 0.00009, color = 축종), 
            stat='identity', linewidth = 0.8) +
  geom_point(data = . %>% filter(축종 == "가금"), 
             aes(x = 연도, y = 사육두수 * 0.00009, color = 축종), 
             stat='identity', size = 2) +
  geom_line(data = . %>% filter(축종 %in% c("한우", "젖소", "돼지")),
            aes(x = 연도, y = 사육두수 * 0.001, color = 축종), 
            stat='identity', linewidth = 0.8) +
  geom_point(data = . %>% filter(축종 %in% c("한우", "젖소", "돼지")), 
             aes(x = 연도, y = 사육두수 * 0.001, color = 축종), 
             stat='identity', size = 2) +
  geom_text(data = . %>% filter(축종 == "합계"),
            aes(x = 연도, y = 사육두수 * 0.00009, 
                label = comma(round(사육두수 * 0.001))), 
            size = 3.5, vjust = -0.5, 
            color = "black", check_overlap = TRUE) +
  geom_text(data = . %>% filter(축종 == "가금"),
            aes(x = 연도, y = 사육두수 * 0.00009, 
                label = comma(round(사육두수 * 0.001))), 
            size = 3.5, vjust = 1.3, 
            color = "black", check_overlap = TRUE) +
  geom_text(data = . %>% filter(축종 %in% c("한우", "젖소", "돼지")),
            aes(x = 연도, y = 사육두수 * 0.001, 
                label = comma(round(사육두수 * 0.001))), 
            size = 3.5, vjust = -0.5, 
            color = "black", check_overlap = TRUE) +
  scale_fill_manual(values = c("총 사육두수" = "wheat"), breaks = "총 사육두수") +
  scale_color_manual(values = c("한우" = "red", "젖소" = "mediumblue", 
                                "돼지" = "darkgreen", "가금" = "darkviolet"),
                     breaks = c("한우", "젖소", "돼지", "가금")) +
  scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "한우, 젖소, 돼지(천두)", 
                     breaks = seq(0, 1200, by = 200), limits = c(0, 1200),
                     labels = scales::comma,
                     sec.axis = sec_axis(~./0.09, name = "총 사육두수, 가금(천두)", 
                                         labels = scales::comma)) +
  theme_calc(base_family = "notosanskr") +
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

## 그래프_축산계_농가수변화 -----
축산계_total %>% 
  filter(시군 == "강원도", 축종 %in% c("합계", "한우", "젖소", "돼지", "가금")) %>% 
  ggplot() +
  geom_bar(data = . %>% filter(축종 == "합계"), 
           aes(x = 연도, y = 농가수/2, fill = "총 농가수"), 
           stat='identity', width = 0.7) +
  geom_line(data = . %>% filter(축종 == "한우"),
            aes(x = 연도, y = 농가수/2, color = 축종), 
            stat='identity', linewidth = 0.8) +
  geom_point(data = . %>% filter(축종 == "한우"), 
             aes(x = 연도, y = 농가수/2, color = 축종), 
             stat='identity', size = 2) +
  geom_line(data = . %>% filter(축종 %in% c("젖소", "돼지", "가금")),
            aes(x = 연도, y = 농가수, color = 축종), 
            stat='identity', linewidth = 0.8) +
  geom_point(data = . %>% filter(축종 %in% c("젖소", "돼지", "가금")), 
             aes(x = 연도, y = 농가수, color = 축종), 
             stat='identity', size = 2) +
  geom_text(data = . %>% filter(축종 == "합계"),
            aes(x = 연도, y = 농가수/2, label = comma(농가수)), 
            size = 3.5, vjust = -0.5, 
            color = "black", check_overlap = TRUE) +
  geom_text(data = . %>% filter(축종 == "한우"),
            aes(x = 연도, y = 농가수/2, label = comma(농가수)), 
            size = 3.5, vjust = 1.3, 
            color = "black", check_overlap = TRUE) +
  geom_text(data = . %>% filter(축종 %in% c("젖소", "가금")),
            aes(x = 연도, y = 농가수, label = comma(농가수)), 
            size = 3.5, vjust = -0.5, 
            color = "black", check_overlap = TRUE) +
  geom_text(data = . %>% filter(축종 == "돼지"),
            aes(x = 연도, y = 농가수, label = comma(농가수)), 
            size = 3.5, vjust = 1.5, 
            color = "black", check_overlap = TRUE) +
  scale_fill_manual(values = c("총 농가수" = "wheat"), breaks = "총 농가수") +
  scale_color_manual(values = c("한우" = "red", "젖소" = "mediumblue", 
                                "돼지" = "darkgreen", "가금" = "darkviolet"),
                     breaks = c("한우", "젖소", "돼지", "가금")) +
  scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "젖소, 돼지, 가금(호)", 
                     breaks = seq(0, 10000, by = 1000), limits = c(0, 8000),
                     labels = scales::comma,
                     sec.axis = sec_axis(~.*2, name = "총 농가수, 한우(호)", 
                                         labels = scales::comma)) +
  theme_calc(base_family = "notosanskr") +
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


## 그래프_축산계_시군별 한우 사육두수 -----
축산계_total %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(연도 == 2021, 시군 != "강원도", 축종 == "한우") %>% 
  # "시", "군" 삭제
  mutate(시군 = str_remove(시군, "(시|군)")) %>% 
  ggplot(aes(x = reorder(시군, -사육두수), y = 사육두수)) +
  geom_bar(stat='identity', fill = "deepskyblue3", width = 0.7) +
  geom_text(aes(label = comma(사육두수)), size = 3.5, vjust = -0.5, 
            color = "black", check_overlap = TRUE) +

  scale_y_continuous(name = "사육두수", breaks = seq(0, 100000, by = 10000), 
                     limits = c(0, 70000), labels = scales::comma) +
  theme_calc(base_family = "notosanskr") +
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

## 그래프_축산계_시군별 젖소 사육두수 -----
축산계_total %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(연도 == 2021, 시군 != "강원도", 축종 == "젖소") %>% 
  # "시", "군" 삭제
  mutate(시군 = str_remove(시군, "(시|군)")) %>% 
  ggplot(aes(x = reorder(시군, -사육두수), y = 사육두수)) +
  geom_bar(stat='identity', fill = "deepskyblue3", width = 0.7) +
  geom_text(aes(label = comma(사육두수)), size = 3.5, vjust = -0.5, 
            color = "black", check_overlap = TRUE) +
  
  scale_y_continuous(name = "사육두수", breaks = seq(0, 100000, by = 2000), 
                     limits = c(0, 14000), labels = scales::comma) +
  theme_calc(base_family = "notosanskr") +
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

## 그래프_축산계_시군별 돼지 사육두수 -----
축산계_total %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(연도 == 2021, 시군 != "강원도", 축종 == "돼지") %>% 
  # "시", "군" 삭제
  mutate(시군 = str_remove(시군, "(시|군)")) %>% 
  ggplot(aes(x = reorder(시군, -사육두수), y = 사육두수)) +
  geom_bar(stat='identity', fill = "deepskyblue3", width = 0.7) +
  geom_text(aes(label = comma(사육두수)), size = 3.5, vjust = -0.5, 
            color = "black", check_overlap = TRUE) +
  
  scale_y_continuous(name = "사육두수", breaks = seq(0, 1000000, by = 20000), 
                     limits = c(0, 160000), labels = scales::comma) +
  theme_calc(base_family = "notosanskr") +
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


## 그래프_축산계_시군별 가금 사육두수 -----
축산계_total %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(연도 == 2021, 시군 != "강원도", 축종 == "가금") %>% 
  # "시", "군" 삭제
  mutate(시군 = str_remove(시군, "(시|군)")) %>% 
  ggplot(aes(x = reorder(시군, -사육두수*0.0001), y = 사육두수*0.0001)) +
  geom_bar(stat='identity', fill = "deepskyblue3", width = 0.7) +
  geom_text(aes(label = comma(사육두수*0.0001)), size = 3.5, vjust = -0.5, 
            color = "black", check_overlap = TRUE) +
  
  scale_y_continuous(name = "사육두수(만두)", breaks = seq(0, 10000, by = 20), 
                     limits = c(0, 200), labels = scales::comma) +
  theme_calc(base_family = "notosanskr") +
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


## 그래프_산업계_연도추이 -----
산업계_total %>% 
  filter(시군 == "강원도", 규모 == "합계") %>% 
  ggplot() +
  geom_line(aes(x = 연도, y = 업소수, color = "업소수"), 
            stat='identity', linewidth = 0.8) +
  geom_point(aes(x = 연도, y = 업소수, color = "업소수"), 
             stat='identity', size = 2) +
  geom_line(aes(x = 연도, y = 폐수발생량/120, color = "폐수발생량"), 
            stat='identity', linewidth = 0.8) +
  geom_point(aes(x = 연도, y = 폐수발생량/120, color = "폐수발생량"), 
             stat='identity', size = 2) +
  geom_line(aes(x = 연도, y = 폐수방류량/120, color = "폐수방류량"), 
            stat='identity', linewidth = 0.8) +
  geom_point(aes(x = 연도, y = 폐수방류량/120, color = "폐수방류량"), 
             stat='identity', size = 2) +
  geom_text(aes(x = 연도, y = 업소수, label = comma(업소수)), 
            size = 3.5, vjust = -0.8, 
            color = "black", check_overlap = TRUE) +
  geom_text(aes(x = 연도, y = 폐수발생량/120, label = comma(폐수발생량/1000)), 
            size = 3.5, vjust = 1.8, 
            color = "black", check_overlap = TRUE) +
  geom_text(aes(x = 연도, y = 폐수방류량/120, label = comma(폐수방류량/1000)), 
            size = 3.5, vjust = 1.8, 
            color = "black", check_overlap = TRUE) +
  scale_color_manual(values = c("업소수" = "red", "폐수발생량" = "mediumblue", 
                                "폐수방류량" = "darkgreen"),
                     breaks = c("업소수", "폐수발생량", "폐수방류량")) +
  scale_x_continuous(breaks = seq(0, 10000, 1)) +
  scale_y_continuous(name = "업소수(개소)", 
                     breaks = seq(0, 10000, by = 500), limits = c(0, 2500),
                     labels = scales::comma,
                     sec.axis = sec_axis(~./10, name = "폐수 발생·방류량(천톤/일)", 
                                         labels = scales::comma)) +
  theme_calc(base_family = "notosanskr") +
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


## 그래프_산업계_시군별 업소수 -----
산업계_total %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(연도 == 2021, 시군 != "강원도", 규모 == "합계") %>% 
  # "시", "군" 삭제
  mutate(시군 = str_remove(시군, "(시|군)")) %>% 
  ggplot(aes(x = reorder(시군, -업소수), y = 업소수)) +
  geom_bar(stat='identity', fill = "deepskyblue3", width = 0.7) +
  geom_text(aes(label = comma(업소수)), size = 3.5, vjust = -0.5, 
            color = "black", check_overlap = TRUE) +
  scale_y_continuous(name = "업소수", breaks = seq(0, 100000, by = 50), 
                     limits = c(0, 400), labels = scales::comma) +
  theme_calc(base_family = "notosanskr") +
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

## 그래프_산업계_시군별 폐수 발생, 방류량 -----
산업계_total %>% 
  # 연도 선택 및 도전체 합계 삭제
  filter(연도 == 2021, 시군 != "강원도", 규모 == "합계") %>% 
  pivot_longer(
    cols = 폐수발생량:폐수방류량,
    names_to = "구분",   
    values_to = "폐수량"
  ) %>% 
  # "시", "군" 삭제
  mutate(시군 = str_remove(시군, "(시|군)")) %>% 
  ggplot(aes(x = reorder(시군, -폐수량), y = 폐수량, fill = 구분)) +
  geom_bar(position = position_dodge(0.8),
           stat='identity', width = 0.7) +

  # geom_text(aes(label = comma(폐수량)), size = 3.5, vjust = -0.5, 
  #           color = "black", check_overlap = TRUE) +
  scale_y_continuous(name = "폐수 발생·방류량(톤/일)", breaks = seq(0, 100000, by = 10000), 
                     limits = c(0, 70000), labels = scales::comma) +
  theme_calc(base_family = "notosanskr") +
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
