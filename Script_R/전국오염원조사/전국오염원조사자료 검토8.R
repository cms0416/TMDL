#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(janitor)
library(readxl)
library(writexl)

##### 공통파일(단위유역별 점유율) 불러오기  ####################################
share <- read_excel("전국오염원조사/단위유역별 점유율.xlsx")


#####  함수 정의  ##############################################################
## 반올림 사용자 정의 함수 로드
source("Script_R/Function/round2func.R")

##### 1. 소계 계산 함수(분류가 없는 경우)  ###### ------------------------------
subtotal_1 <- function(data) {
  data %>%
    # 유역별 합계
    group_by(연도, 단위유역) %>%
    group_modify(~ .x %>% adorn_totals(where = "row", fill = "합계", name = "강원도")) %>%
    group_by(연도, 시군) %>%
    group_modify(~ .x %>% adorn_totals(where = "row", name = "소계"))
}

##### 2. 소계 계산 함수(분류가 있는 경우)  ###### ------------------------------
subtotal_2 <- function(data, class) {
  data %>%
    # 유역별 합계
    group_by(연도, 단위유역, !!sym(class)) %>%
    group_modify(~ .x %>% adorn_totals(where = "row", fill = "소계", name = "강원도")) %>%
    group_by(연도, 단위유역, 시군) %>%
    group_modify(~ .x %>% adorn_totals(where = "row", fill = "소계", name = "소계")) %>%
    group_by(연도, 시군, !!sym(class)) %>%
    group_modify(~ .x %>% adorn_totals(where = "row", fill = "소계", name = "소계")) %>% 
    relocate(단위유역, .after = 시군)
}

##### 3. 단위유역/시군 순서 지정 함수  ###### ----------------------------------
order_func <- function(data, value, class = "") {
  data %>% 
    mutate(
      단위유역 = factor(단위유역, levels = c(
        "소계", "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A",
        "섬강A", "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
        "홍천A", "한탄A", "한강B", "제천A", "한강D", "북한D", "한탄B", "임진A",
        "낙본A", "기타"
      )),
      시군 = factor(시군, levels = c(
        "강원도", "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군",
        "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군",
        "양구군", "인제군", "고성군", "동해시", "속초시", "양양군"
      )),
      # 권역 추가
      권역 = case_when(
        단위유역 == "골지A" | 단위유역 == "오대A" | 단위유역 == "주천A" |
          단위유역 == "평창A" | 단위유역 == "옥동A" | 단위유역 == "한강A" ~ "남한강",
        단위유역 == "섬강A" | 단위유역 == "섬강B" ~ "섬강",
        단위유역 == "북한A" | 단위유역 == "북한B" | 단위유역 == "소양A" |
          단위유역 == "인북A" | 단위유역 == "소양B" | 단위유역 == "북한C" ~ "북한강",
        단위유역 == "홍천A" ~ "홍천강",
        단위유역 == "한탄A" ~ "한탄강",
        단위유역 == "한강B" | 단위유역 == "제천A" | 단위유역 == "한강D" ~ "충청북도",
        단위유역 == "북한D" | 단위유역 == "한탄B" | 단위유역 == "임진A" ~ "경기도",
        단위유역 == "낙본A" ~ "낙동강",
        TRUE ~ "기타"
      )
    ) %>%
    select(권역, everything()) %>%
    # 각 연도를 열로 변경(wide 포맷)
    pivot_wider(names_from = 연도, values_from = value) %>%
    arrange(시군, 단위유역, !!sym(class))
}

##**************************************************************************** ##
###############################  생활계 - 인구  ################################
##**************************************************************************** ##


## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/생활계",
  pattern = "*.xls", full.names = T
)

# 경로지정된 생활계 파일 합치기
population <- files %>%
  # map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
  map_dfr(read_excel, skip = 4, col_names = F) %>%
  # 필요없는 열 삭제
  select(-c(7:18))
## *****************************************************************************


## 변수명 지정 및 데이터 정리
population <- population %>%
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
  )) %>%
  # 수치데이터 및 연도 숫자로 지정
  mutate(across(c(1, 7:23), as.numeric)) %>%
  # 주소코드 추가("리"가 없는 "동"의 경우 "법정리"칸에 "동"으로 추가)
  mutate(주소 = str_c(시군, 법정동, ifelse(is.na(법정리), 법정동, 법정리), sep = " "))

## 연도 및 동리별 가정인구합계 정리
population_total <- population %>%
  group_by(연도, 주소) %>%
  summarise(가정인구합계 = sum(가정인구합계), .groups = "drop")

## 각 동리별 단위유역 점유율 계산(소유역 점유율 합계)
share_population <- share %>%
  group_by(주소, 단위유역, 시군) %>%
  summarise(생활계 = sum(생활계) / 100, .groups = "drop")

## 유역/시군 기준 인구 합계 연도별 정리
population_sum <- tibble()

for (i in 2014:2022) {
  temp <- share_population %>%
    mutate(연도 = i) %>%
    left_join(population_total, by = c("주소", "연도")) %>%
    mutate(총인구 = round(생활계 * 가정인구합계)) %>% # 동리별 가정인구합계와 유역 점유율 계산
    group_by(연도, 단위유역, 시군) %>%
    summarise(총인구 = sum(총인구, na.rm = T), .groups = "drop")

  population_sum <- bind_rows(population_sum, temp)
}

## *****  소계 계산  ***********************************************************

## 소계 계산, 단위유역/시군 순서 지정, 연도 기준 wide 포맷 변환
population_sum %<>% subtotal_1() %>% order_func(., "총인구")

## 권역 제외
population_sum_s <- population_sum %>% select(-권역)


## ______________________________________________________________________________

## 권역별 정리
population_sum_a <- population_sum %>%
  filter(시군 == "강원도") %>%
  group_by(권역) %>%
  summarise_at(vars(`2014`:`2022`), ~ sum(.)) %>%
  mutate(권역 = factor(권역, levels = c(
    "남한강", "섬강", "북한강", "홍천강", "한탄강",
    "충청북도", "경기도", "낙동강", "기타"
  ))) %>%
  arrange(권역)


#####  엑셀 파일 내보내기_writexl
write_xlsx(
  list(
    "유역별정리" = population_sum %>% 
      arrange(단위유역, 시군) %>% 
      filter(단위유역 != "소계"),
    "시군별정리" = population_sum,
    "권역별정리" = population_sum_a
  ),
  path = "전국오염원조사/Output/생활계_인구.xlsx"
)



##**************************************************************************** ##
#############################  생활계 - 물사용량  ##############################
##**************************************************************************** ##

## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/생활계",
  pattern = "*.xls", full.names = T
)

# 경로지정된 생활계 파일 합치기
waterusage <- files %>%
  # map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
  map_dfr(read_excel, sheet = 2, skip = 4, col_names = F) %>%
  # 필요없는 열 삭제
  select(1:6, 19, 36)
## *****************************************************************************


## 변수명 지정 및 데이터 정리
waterusage %<>%
  # 변수명 지정
  set_names(c(
    "연도", "행정구역코드", "시도", "시군", "법정동", "법정리",
    "가정용물사용합계", "영업용물사용합계"
  )) %>%
  # 수치데이터 및 연도 숫자로 지정
  mutate(across(c(연도, 가정용물사용합계, 영업용물사용합계), as.numeric)) %>%
  mutate(
    # 물사용량 합계 계산
    물사용량 = 가정용물사용합계 + 영업용물사용합계,
    # 주소코드 추가("리"가 없는 "동"의 경우 "법정리"칸에 "동"으로 추가)
    주소 = str_c(시군, 법정동, ifelse(is.na(법정리), 법정동, 법정리), sep = " ")
  )

## 연도 및 동리별 물사용량합계 정리
waterusage_total <- waterusage %>%
  group_by(연도, 주소) %>%
  summarise(물사용량 = sum(물사용량), .groups = "drop")

## 각 동리별 단위유역 점유율 계산(소유역 점유율 합계)
share_waterusage <- share %>%
  group_by(주소, 단위유역, 시군) %>%
  summarise(생활계 = sum(생활계) / 100, .groups = "drop")

## 유역/시군 기준 물사용량 합계 연도별 정리
waterusage_sum <- tibble()

for (i in 2014:2022) {
  temp <- share_waterusage %>%
    mutate(연도 = i) %>%
    left_join(waterusage_total, by = c("주소", "연도")) %>%
    # 동리별 물사용량합계와 유역 점유율 계산
    mutate(물사용량 = round2(생활계 * 물사용량, 1)) %>%
    group_by(연도, 단위유역, 시군) %>%
    summarise(물사용량 = sum(물사용량, na.rm = T), .groups = "drop")

  waterusage_sum <- bind_rows(waterusage_sum, temp)
}

## *****  소계 계산  ***********************************************************

## 소계 계산, 단위유역/시군 순서 지정, 연도 기준 wide 포맷 변환
waterusage_sum %<>% subtotal_1() %>% order_func(., "물사용량")

## 시군 기준 정리
waterusage_sum_s <- waterusage_sum %>% select(-권역)


## ______________________________________________________________________________
waterusage_sum <- waterusage_sum %>% filter(단위유역 != "합계")

## 권역별 정리
waterusage_sum_a <- waterusage_sum %>%
  filter(시군 == "합계") %>%
  group_by(권역) %>%
  summarise_at(vars(`2014`:`2022`), ~ sum(.)) %>%
  mutate(권역 = factor(권역, levels = c(
    "남한강", "섬강", "북한강", "홍천강", "한탄강",
    "충청북도", "경기도", "낙동강", "기타"
  ))) %>%
  arrange(권역)


#####  엑셀 파일 내보내기_writexl
write_xlsx(list(
  "유역별정리" = waterusage_sum %>% 
    arrange(단위유역, 시군) %>% 
    filter(단위유역 != "소계"),
  "시군별정리" = waterusage_sum_s,
  "권역별정리" = waterusage_sum_a
), path = "전국오염원조사/Output/생활계_물사용량.xlsx")


#####  생활계 전체 엑셀 파일 내보내기_writexl
sheets <- list(
  "생활계_인구_유역별" = population_sum %>% 
    arrange(단위유역, 시군) %>% 
    filter(단위유역 != "소계"),
  "생활계_인구_시군별" = population_sum,
  "생활계_인구_권역별" = population_sum_a,
  "생활계_물사용량_유역별" = waterusage_sum %>% 
    arrange(단위유역, 시군) %>% 
    filter(단위유역 != "소계"),
  "생활계_물사용량_시군별" = waterusage_sum,
  "생활계_물사용량_권역별" = waterusage_sum_a
)

write_xlsx(sheets, path = "전국오염원조사/Output/생활계.xlsx")


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
  livestock <- files %>%
    map_dfr(~ {
      data <- read_excel(.x, skip = 6, col_names = F) 
      year <- str_sub(basename(.x), 1, 4) %>% as.integer()
      data <- data %>% mutate(연도 = year, .before = 1) 
    })
## *****************************************************************************


## 변수명 지정 및 데이터 정리
livestock %<>%
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

## 연도 및 동리별 사육두수합계 정리
livestock_total <- livestock %>%
  group_by(연도, 주소, 축종) %>%
  summarise(사육두수 = sum(사육두수), .groups = "drop")

livestock_summary <- livestock %>%
  group_by(연도, 시군) %>%
  summarise(사육두수 = sum(사육두수), .groups = "drop")

## 각 동리별 단위유역 점유율 계산(소유역 점유율 합계) 및 축종추가
share_livestock <- share %>%
  group_by(주소, 단위유역, 시군) %>%
  summarise(축산계 = sum(축산계) / 100, .groups = "drop") %>%
  mutate(
    젖소 = "젖소", 한우 = "한우", 말 = "말", 돼지 = "돼지",
    `양, 사슴` = "양, 사슴", 개 = "개", 가금 = "가금"
  ) %>%
  pivot_longer(cols = 젖소:가금, names_to = "축종", values_to = "임시") %>%
  select(-임시)


## 유역/시군 기준 합계 연도별 정리
livestock_sum <- tibble()

for (i in 2014:2022) {
  temp <- share_livestock %>%
    mutate(연도 = i) %>%
    left_join(livestock_total, by = c("주소", "연도", "축종")) %>%
    # 동리별 사육두수합계와 유역 점유율 계산
    mutate(총사육두수 = round(축산계 * 사육두수)) %>%
    group_by(연도, 단위유역, 시군, 축종) %>%
    summarise(총사육두수 = sum(총사육두수, na.rm = T), .groups = "drop")

  livestock_sum <- bind_rows(livestock_sum, temp)
}

## *****  소계 계산  ***********************************************************

## 소계 계산, 단위유역/시군 순서 지정, 연도 기준 wide 포맷 변환
livestock_sum <- livestock_sum %>% 
  subtotal_2(., "축종") %>% 
  mutate(축종 = factor(축종, levels = c(
    "젖소", "한우", "말", "돼지", "양, 사슴", "개", "가금", "소계"
  ))) %>% 
  order_func(., "총사육두수", "축종")


## 권역 제외
livestock_sum_s <- livestock_sum %>% select(-권역)
 


## ______________________________________________________________________________

### 권역별 합계
livestock_sum_a <- livestock_sum %>%
  filter(시군 == "합계") %>%
  group_by(권역, 축종) %>%
  summarise_at(vars(`2014`:`2022`), ~ sum(.)) %>%
  mutate(권역 = factor(권역, levels = c(
    "남한강", "섬강", "북한강", "홍천강", "한탄강",
    "충청북도", "경기도", "낙동강", "기타"
  ))) %>%
  arrange(권역)

livestock_sum_a2 <- livestock_sum_a %>%
  group_by(축종) %>%
  summarise_at(vars(`2014`:`2022`), ~ sum(.))

livestock_sum_a3 <- livestock_sum_a %>% filter(축종 == "소계")

### 단위유역별 전체 축종 합계
livestock_sum_b <- livestock_sum %>%
  filter(시군 == "합계", 축종 == "소계") %>%
  ungroup() %>%
  select(-권역, -시군, -축종)

### 시군별 전체 축종 합계
livestock_sum_c <- livestock_sum %>%
  filter(축종 == "소계") %>%
  group_by(시군) %>%
  summarise_at(vars(`2014`:`2022`), ~ sum(.))

#####  엑셀 파일 내보내기_writexl
write_xlsx(
  list(
    "유역별정리" = livestock_sum,
    "시군별정리" = livestock_sum_s %>% filter(시군 != "합계"),
    "권역별정리" = livestock_sum_a
  ),
  path = "전국오염원조사/Output/축산계.xlsx"
)


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
industry <- files %>%
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
industry2 <- industry %>%
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

## 연도 및 동리별 합계 정리
industry_total <- industry2 %>%
  mutate(업소수 = 1) %>%
  group_by(연도, 주소, 시군, 규모) %>%
  summarise(
    업소수 = sum(업소수),
    폐수발생량 = sum(폐수발생량),
    폐수방류량 = sum(폐수방류량),
    .groups = "drop"
  )

industry_summary1 <- industry_total %>%
  group_by(연도, 시군) %>%
  summarise(업소수 = sum(업소수), .groups = "drop")

## 각 동리별 단위유역 점유율 계산(소유역 점유율 합계)
share_industry <- share %>%
  group_by(주소, 단위유역, 시군) %>%
  summarise(산업계 = sum(산업계) / 100, .groups = "drop") %>%
  mutate(
    `1종` = "1종", `2종` = "2종", `3종` = "3종", `4종` = "4종", `5종` = "5종"
  ) %>%
  pivot_longer(cols = `1종`:`5종`, names_to = "규모", values_to = "임시") %>%
  select(-임시)

## 유역/시군 기준 합계 연도별 정리
industry_sum <- tibble()

for (i in 2014:2022) {
  temp <- share_industry %>%
    mutate(연도 = i) %>%
    left_join(industry_total, by = c("주소", "연도", "시군", "규모")) %>%
    mutate(
      # 동리별 업소수합계와 유역 점유율 계산
      업소수 = round(산업계 * 업소수),
      # 동리별 폐수발생량합계와 유역 점유율 계산
      폐수발생량 = round2(산업계 * 폐수발생량, 1),
      # 동리별 폐수방류량합계와 유역 점유율 계산
      폐수방류량 = round2(산업계 * 폐수방류량, 1)
    ) %>%
    group_by(연도, 단위유역, 시군, 규모) %>%
    summarise(
      업소수 = sum(업소수, na.rm = T),
      폐수발생량 = sum(폐수발생량, na.rm = T),
      폐수방류량 = sum(폐수방류량, na.rm = T),
      .groups = "drop"
    )

  industry_sum <- bind_rows(industry_sum, temp)
}

industry_summary2 <- industry_sum %>%
  group_by(연도, 시군) %>%
  summarise(업소수 = sum(업소수), .groups = "drop")

## *****  소계 계산  ***********************************************************

## 소계 계산, 단위유역/시군 순서 지정, 연도 기준 wide 포맷 변환
industry_sum <- industry_sum %>% 
  subtotal_2(., "규모") 


## 각 연도를 열로 변경(wide 포맷)
## 업소수 기준 정리
industry_sum1 <- industry_sum %>%
  select(-폐수발생량, -폐수방류량) %>%
  order_func(., "업소수", "")

## 폐수발생량 기준 정리
industry_sum2 <- industry_sum %>%
  select(-업소수, -폐수방류량) %>%
  order_func(., "폐수발생량", "")

## 폐수방류량 기준 정리
industry_sum3 <- industry_sum %>%
  select(-업소수, -폐수발생량) %>%
  order_func(., "폐수방류량", "")


## 권역 제외
industry_sum1_s <- industry_sum1 %>% select(-권역)
industry_sum2_s <- industry_sum2 %>% select(-권역)
industry_sum3_s <- industry_sum3 %>% select(-권역)


## ______________________________________________________________________________

## 권역별 정리
industry_sum1_a <- industry_sum1 %>%
  filter(시군 == "합계") %>%
  group_by(권역, 규모) %>%
  summarise_at(vars(`2014`:`2022`), ~ sum(.)) %>%
  mutate(권역 = factor(권역, levels = c(
    "남한강", "섬강", "북한강", "홍천강", "한탄강",
    "충청북도", "경기도", "낙동강", "기타"
  ))) %>%
  arrange(권역)

industry_sum2_a <- industry_sum2 %>%
  filter(시군 == "합계") %>%
  group_by(권역, 규모) %>%
  summarise_at(vars(`2014`:`2022`), ~ sum(.)) %>%
  mutate(권역 = factor(권역, levels = c(
    "남한강", "섬강", "북한강", "홍천강", "한탄강",
    "충청북도", "경기도", "낙동강", "기타"
  ))) %>%
  arrange(권역)

industry_sum3_a <- industry_sum3 %>%
  filter(시군 == "합계") %>%
  group_by(권역, 규모) %>%
  summarise_at(vars(`2014`:`2022`), ~ sum(.)) %>%
  mutate(권역 = factor(권역, levels = c(
    "남한강", "섬강", "북한강", "홍천강", "한탄강",
    "충청북도", "경기도", "낙동강", "기타"
  ))) %>%
  arrange(권역)


#####  엑셀 파일 내보내기_writexl
sheets <- list(
  "산업계_업소수_유역별" = industry_sum1,
  "산업계_업소수_시군별" = industry_sum1_s %>% filter(시군 != "합계"),
  "산업계_업소수_권역별" = industry_sum1_a,
  "산업계_폐수발생량_유역별" = industry_sum2,
  "산업계_폐수발생량_시군별" = industry_sum2_s %>% filter(시군 != "합계"),
  "산업계_폐수발생량_권역별" = industry_sum2_a,
  "산업계_폐수방류량_유역별" = industry_sum3,
  "산업계_폐수방류량_시군별" = industry_sum3_s %>% filter(시군 != "합계"),
  "산업계_폐수방류량_권역별" = industry_sum3_a
)

write_xlsx(sheets, path = "전국오염원조사/Output/산업계.xlsx")


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
landuse <- files %>%
  map_dfr(~ {
    data <- read_excel(.x, skip = 2, col_names = F) 
    year <- str_sub(basename(.x), 1, 4) %>% as.integer()
    data <- data %>% mutate(연도 = year, .before = 1) 
  })
## *****************************************************************************

## 데이터 정리
landuse2 <- landuse %>%
  select(-2) %>%
  set_names(c(
    "연도", "시도", "시군", "법정동", "법정리", "총면적", "전", "답", "과수원",
    "목장용지", "임야", "광천지", "염전", "대지", "공장용지", "학교용지",
    "주차장", "주유소용지", "창고용지", "도로", "철도용지", "제방", "하천",
    "구거", "유지", "양어장", "수도용지", "공원", "체육용지", "유원지",
    "종교용지", "사적지", "묘지", "잡종지"
  )) %>% 
  # 지목별 면적 및 연도 숫자로 지정
  mutate(across(c(연도, 총면적:잡종지), as.numeric)) %>%
  mutate(
    # 주소코드 추가("리"가 없는 "동"의 경우 "법정리"칸에 "동"으로 추가)
    주소 = str_c(시군, 법정동, ifelse(is.na(법정리), 법정동, 법정리), sep = " "),
    # 토지계 지목 변환(총량 기술지침 발생원단위 지목에 맞춰서 조정)
    기타초지 = 목장용지 + 공원 + 묘지 + 사적지,
    기타 = 광천지 + 염전 + 제방 + 하천 + 구거 + 유지 + 양어장 + 잡종지,
    공공시설지역 = 학교용지 + 창고용지 + 종교용지,
    교통지역 = 주차장 + 도로 + 철도용지 + 수도용지,
  ) %>%
  select(연도, 주소, everything(), -c(
    목장용지, 공원, 묘지, 사적지, 광천지, 염전, 제방, 하천, 구거, 유지,
    양어장, 잡종지, 학교용지, 창고용지, 종교용지, 주차장, 도로,
    철도용지, 수도용지
  )) %>%
  pivot_longer(cols = 전:교통지역, names_to = "지목", values_to = "면적")


## 연도 및 동리 별 지목 면적 정리
landuse_total <- landuse2 %>%
  group_by(연도, 주소, 시군, 지목) %>%
  summarise(면적 = sum(면적), .groups = "drop")

## 각 동리별 단위유역 점유율 계산(소유역 점유율 합계)
share_landuse <- share %>%
  select(-3, -4, -c(6:11), -25, -26) %>%
  pivot_longer(cols = 4:16, names_to = "지목", values_to = "토지계") %>%
  group_by(주소, 단위유역, 시군, 지목) %>%
  summarise(토지계 = sum(토지계) / 100, .groups = "drop")


## 유역/시군 기준 인구 합계 연도별 정리
landuse_sum <- tibble()

for (i in 2014:2022) {
  temp <- share_landuse %>%
    mutate(연도 = i) %>%
    left_join(landuse_total, by = c("주소", "연도", "시군", "지목")) %>%
    mutate(총면적 = 토지계 * 면적) %>%
    group_by(연도, 단위유역, 시군, 지목) %>%
    summarise(총면적 = round2(sum(총면적, na.rm = T) * 10^-6, 3), .groups = "drop")

  landuse_sum <- bind_rows(landuse_sum, temp)
}


## *****  소계 계산 1  *********************************************************
# 유역별 합계
landuse_sum <- landuse_sum %>%
  bind_rows(landuse_sum %>%
    group_by(연도, 단위유역, 지목) %>%
    summarise(총면적 = sum(총면적), .groups = "drop") %>%
    mutate(시군 = "강원도") %>%
    select(연도, 시군, 단위유역, 지목, 총면적))

# 시군및 유역별 소계
landuse_sum <- landuse_sum %>%
  bind_rows(landuse_sum %>%
    group_by(연도, 단위유역, 시군) %>%
    summarise(총면적 = sum(총면적), .groups = "drop") %>%
    mutate(지목 = "소계") %>%
    select(연도, 시군, 단위유역, 지목, 총면적))

## *****************************************************************************



## *****  토지계 소계 전망값과 동일하게 계산  **********************************
# 오염원 전망 자료 불러오기
prospect <- read_excel("오염원증감현황/Output/오염원 전망(시군별 정리).xlsx")

# 전망자료 내 토지계 "소계" 만 분리
prospect <- prospect %>%
  filter(오염원 == "토지계", 분류 == "소계") %>%
  select(-오염원, -c(`2018년`:`2030년`)) %>%
  rename(지목 = 분류, 전망 = `2017년`)

# 전망자료와 당해년도 토지계 자료 "소계" 차이값 계산
prospect_a <- landuse_sum %>%
  filter(연도 == 2022) %>%
  left_join(prospect, by = c("시군", "단위유역", "지목")) %>%
  mutate(차이 = 전망 - 총면적) %>%
  filter(지목 == "소계") %>%
  select(-총면적, -전망)

# "소계"의 차이값을 지목 중 "임야"에 반영할 수 있도록 추가
prospect_b <- prospect_a %>%
  bind_rows(prospect_a %>% mutate(지목 = "임야"))

# "소계"와 "임야"에 차이값 반영
landuse_sum <- landuse_sum %>%
  left_join(prospect_b, by = c("연도", "시군", "단위유역", "지목")) %>%
  mutate(
    차이 = replace(차이, is.na(차이), 0),
    총면적 = 총면적 + 차이
  ) %>%
  select(-차이)
## *****************************************************************************



## *****  소계 계산 2  *********************************************************
# 시군별 합계
landuse_sum <- landuse_sum %>%
  bind_rows(landuse_sum %>%
    group_by(연도, 시군, 지목) %>%
    summarise(총면적 = sum(총면적), .groups = "drop") %>%
    mutate(단위유역 = "소계") %>%
    select(연도, 단위유역, 시군, 지목, 총면적))

## *****************************************************************************


## 권역 추가, 단위유역, 시군 순서 설정 및 각 연도를 열로 변경(wide 포맷)
landuse_sum <- landuse_sum %>% 
  mutate(
    지목 = factor(지목, levels = c(
      "전", "답", "과수원", "기타초지", "임야", "기타", "대지", "공장용지",
      "공공시설지역", "교통지역", "주유소용지", "체육용지", "유원지", "소계"
    ))
  ) %>%
  order_func(., "총면적", "지목")


## 권역 제외
landuse_sum_s <- landuse_sum %>% select(-권역)

## ______________________________________________________________________________

## 권역별 정리
landuse_sum_a <- landuse_sum %>%
  filter(시군 == "합계") %>%
  group_by(권역, 지목) %>%
  summarise_at(vars(`2014`:`2022`), ~ sum(.)) %>%
  mutate(권역 = factor(권역, levels = c(
    "남한강", "섬강", "북한강", "홍천강", "한탄강",
    "충청북도", "경기도", "낙동강", "기타"
  ))) %>%
  arrange(권역)

#####  엑셀 파일 내보내기_writexl
write_xlsx(
  list(
    "유역별정리" = landuse_sum,
    "시군별정리" = landuse_sum_s %>% filter(시군 != "합계"),
    "권역별정리" = landuse_sum_a
  ),
  path = "전국오염원조사/Output/토지계.xlsx"
)


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
fishfarm <- files %>%
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
fishfarm <- fishfarm %>%
  select(연도, 7, 10:12, 18, 22, 28, 30) %>%
  set_names(c(
    "연도", "업소명", "시군", "법정동", "법정리", "종류",
    "시설면적", "방류하천", "휴업"
  )) %>%
  # 시설면적, 연도 숫자로 지정
  mutate(across(c(시설면적, 연도), as.numeric)) %>%
  # 주소코드 추가("리"가 없는 "동"의 경우 "법정리"칸에 "동"으로 추가)
  mutate(주소 = str_c(시군, 법정동, ifelse(is.na(법정리), 법정동, 법정리), sep = " "))

## 연도 및 동리별 시설면적 합계 정리
fishfarm_total <- fishfarm %>%
  group_by(연도, 주소, 시군, 종류) %>%
  summarise(시설면적 = sum(시설면적), .groups = "drop")

## 각 동리별 단위유역 점유율 계산(소유역 점유율 합계)
share_fishfarm <- share %>%
  group_by(주소, 단위유역, 시군) %>%
  summarise(양식계 = sum(양식계) / 100, .groups = "drop") %>%
  mutate(
    가두리 = "가두리",
    유수식 = "유수식",
    도전양식 = "도전양식",
    지수식 = "지수식"
  ) %>%
  pivot_longer(cols = 가두리:지수식, names_to = "종류", values_to = "임시") %>%
  select(-임시)

## 유역/시군 기준 시설면적 합계 연도별 정리
fishfarm_sum <- tibble()

for (i in 2014:2022) {
  temp <- share_fishfarm %>%
    mutate(연도 = i) %>%
    left_join(fishfarm_total, by = c("주소", "연도", "시군", "종류")) %>%
    # 동리별 시설면적 합계와 유역 점유율 계산
    mutate(시설면적 = round2(양식계 * 시설면적, 2)) %>%
    group_by(연도, 단위유역, 시군, 종류) %>%
    summarise(시설면적 = sum(시설면적, na.rm = T), .groups = "drop")

  fishfarm_sum <- bind_rows(fishfarm_sum, temp)
}


## *****  소계 계산  ***********************************************************
## 소계 계산, 단위유역/시군 순서 지정, 연도 기준 wide 포맷 변환
fishfarm_sum <- fishfarm_sum %>% 
  subtotal_2(., "종류") %>% 
  mutate(종류 = factor(종류, levels = c(
    "가두리", "유수식", "도전양식", "지수식", "소계"
  ))) %>% 
  order_func(., "시설면적", "종류")


## 권역 제외
fishfarm_sum_s <- fishfarm_sum %>% select(-권역)


## ______________________________________________________________________________
fishfarm_sum <- fishfarm_sum %>% filter(단위유역 != "합계")

## 권역별 정리
fishfarm_sum_a <- fishfarm_sum %>%
  filter(시군 == "합계") %>%
  group_by(권역, 종류) %>%
  summarise_at(vars(`2014`:`2022`), ~ sum(.)) %>%
  mutate(권역 = factor(권역, levels = c(
    "남한강", "섬강", "북한강", "홍천강", "한탄강",
    "충청북도", "경기도", "낙동강", "기타"
  ))) %>%
  arrange(권역)

#####  엑셀 파일 내보내기_writexl
write_xlsx(
  list(
    "유역별정리" = fishfarm_sum,
    "시군별정리" = fishfarm_sum_s %>% filter(시군 != "합계"),
    "권역별정리" = fishfarm_sum_a
  ),
  path = "전국오염원조사/Output/양식계.xlsx"
)


##**************************************************************************** ##
###################################  매립계  ###################################
##**************************************************************************** ##


## 매립장 현황 데이터 불러오기
landfill_a <- read_excel("전국오염원조사/매립장 현황.xlsx") %>%
  select(매립시설명, 시군, 단위유역)


##### ===== 매립장 시설수 ======================================================

## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/매립계",
  pattern = "*.xls", full.names = T
)

# 데이터 불러오기 및 합치기
landfill1 <- files %>%
  map_dfr(~ {
    data <- read_excel(.x, skip = 3, col_names = F) 
    year <- str_sub(basename(.x), 1, 4) %>% as.integer()
    data <- data %>% mutate(연도 = year, .before = 1) 
  })
## *****************************************************************************


## 변수명 지정 및 데이터 정리
landfill1 <- landfill1 %>%
  select(연도, 2, 6:8, 18) %>%
  set_names(c(
    "연도", "매립시설명", "시군", "법정동", "법정리", "가동유무"
  )) %>%
  # 단위유역 추가 및 연도 숫자로 변환
  left_join(landfill_a, by = c("매립시설명", "시군")) %>%
  mutate(연도 = as.numeric(연도))

## 유역/시군 기준 매립장 시설수 연도별 정리
landfill_count <- tibble()

for (i in 2014:2022) {
  temp <- share %>%
    group_by(단위유역, 시군) %>%
    summarise(연도 = i, .groups = "drop") %>%
    left_join(landfill1, by = c("단위유역", "시군", "연도")) %>%
    group_by(연도, 단위유역, 시군) %>%
    summarise(시설수 = sum(!is.na(매립시설명)), .groups = "drop")

  landfill_count <- bind_rows(landfill_count, temp)
}


## *****  소계 계산  ***********************************************************
## 소계 계산, 단위유역/시군 순서 지정, 연도 기준 wide 포맷 변환
landfill_count %<>% subtotal_1() %>% order_func(., "시설수")


## 권역 제외
landfill_count_s <- landfill_count %>% select(-권역)




##### ===== 매립장 침출수 발생유량 =============================================

## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/매립계",
  pattern = "*.xls", full.names = T
)

# 데이터 불러오기 및 합치기
landfill2 <- files %>%
  map_dfr(~ {
    data <- read_excel(.x, , sheet = 2, skip = 2, col_names = F) 
    year <- str_sub(basename(.x), 1, 4) %>% as.integer()
    data <- data %>% mutate(연도 = year, .before = 1) 
  })
## *****************************************************************************


## 변수명 지정
landfill2 %<>%
  select(연도, 2, 5) %>%
  set_names(c("연도", "매립시설명", "발생유량"))

## 침출수 발생유량 연평균 계산
landfill_mean <- landfill2 %>%
  mutate(across(c(연도, 발생유량), as.numeric)) %>%
  group_by(매립시설명, 연도) %>%
  summarise(발생유량 = round2(mean(발생유량), 1), .groups = "drop") %>%
  mutate_all(~ replace(., is.na(.), 0))
  # mutate(시설수 = 1)

## 침출수 발생유량 자료에 시군, 단위유역 추가
landfill_mean <- landfill_mean %>%
  left_join(landfill_a, by = "매립시설명") %>%
  filter(!is.na(시군))


## 유역/시군 기준 침출수 발생유량 연도별 정리
landfill_sum <- tibble()

for (i in 2014:2022) {
  temp <- share %>%
    group_by(단위유역, 시군) %>%
    summarise(연도 = i, .groups = "drop") %>%
    left_join(landfill_mean, by = c("단위유역", "시군", "연도")) %>%
    group_by(연도, 단위유역, 시군) %>%
    summarise(발생유량 = sum(발생유량), .groups = "drop")

  landfill_sum <- bind_rows(landfill_sum, temp)
}

## 결측치 0으로 변환
landfill_sum <- landfill_sum %>% mutate_all(~ replace(., is.na(.), 0))


## *****  소계 계산  ***********************************************************
## 소계 계산, 단위유역/시군 순서 지정, 연도 기준 wide 포맷 변환
landfill_sum %<>% subtotal_1() %>% order_func(., "발생유량")


## 권역 제외
landfill_sum_s <- landfill_sum %>% select(-권역)



##**************************************************************************** ##
############################  전체 통합 자료 정리  #############################
##**************************************************************************** ##

## 생활계 인구 자료 오염원 및 분류 열 추가
population_sum_s1 <- population_sum_s %>%
  mutate(오염원 = "생활계", 분류 = "인구") %>%
  select(시군, 단위유역, 오염원, 분류, everything())

## 생활계 물사용량 자료 오염원 및 분류 열 추가
waterusage_sum_s1 <- waterusage_sum_s %>%
  mutate(오염원 = "생활계", 분류 = "물사용량") %>%
  select(시군, 단위유역, 오염원, 분류, everything())

## 축산계 자료 오염원 열 추가 및 "축종" 열 이름 "분류"로 변경
livestock_sum_s1 <- livestock_sum_s %>%
  mutate(오염원 = "축산계") %>%
  rename(분류 = 축종) %>%
  select(시군, 단위유역, 오염원, 분류, everything())

## 산업계 업소수 자료 오염원 열 추가 및 "규모" 열 이름 "분류"로 변경
industry_sum1_s1 <- industry_sum1_s %>%
  mutate(오염원 = "산업계_업소수") %>%
  rename(분류 = 규모) %>%
  select(시군, 단위유역, 오염원, 분류, everything())

## 산업계 폐수발생량 자료 오염원 열 추가 및 "규모" 열 이름 "분류"로 변경
industry_sum2_s1 <- industry_sum2_s %>%
  mutate(오염원 = "산업계_폐수발생량") %>%
  rename(분류 = 규모) %>%
  select(시군, 단위유역, 오염원, 분류, everything())

## 산업계 폐수방류량 자료 오염원 열 추가 및 "규모" 열 이름 "분류"로 변경
industry_sum3_s1 <- industry_sum3_s %>%
  mutate(오염원 = "산업계_폐수방류량") %>%
  rename(분류 = 규모) %>%
  select(시군, 단위유역, 오염원, 분류, everything())

## 토지계 자료 오염원 열 추가 및 "지목" 열 이름 "분류"로 변경
landuse_sum_s1 <- landuse_sum_s %>%
  mutate(오염원 = "토지계") %>%
  rename(분류 = 지목) %>%
  select(시군, 단위유역, 오염원, 분류, everything())

## 양식계 자료 오염원 열 추가 및 "종류" 열 이름 "분류"로 변경
fishfarm_sum_s1 <- fishfarm_sum_s %>%
  mutate(오염원 = "양식계_시설면적") %>%
  rename(분류 = 종류) %>%
  select(시군, 단위유역, 오염원, 분류, everything())

## 매립계 시설수 자료 오염원 및 분류 열 추가
landfill_count_s1 <- landfill_count_s %>%
  mutate(오염원 = "매립계_시설수", 분류 = "소계") %>%
  select(시군, 단위유역, 오염원, 분류, everything())

## 매립계 침출수발생량 자료 오염원 및 분류 열 추가
landfill_sum_s1 <- landfill_sum_s %>%
  mutate(오염원 = "매립계_침출수발생량", 분류 = "소계") %>%
  select(시군, 단위유역, 오염원, 분류, everything())

## 전체 계별 데이터 합치기
alldata_s <- bind_rows(
  population_sum_s1, waterusage_sum_s1, livestock_sum_s1,
  industry_sum1_s1, industry_sum2_s1, industry_sum3_s1,
  landuse_sum_s1, fishfarm_sum_s1, landfill_count_s1,
  landfill_sum_s1
)

## 최종 데이터 정리
alldata_s %<>%
  filter(
    분류 != "1종", 분류 != "2종", 분류 != "3종", 분류 != "4종", 분류 != "5종",
    분류 != "가두리", 분류 != "도전양식", 분류 != "유수식", 분류 != "지수식"
  ) %>%
  mutate(
    단위유역 = factor(단위유역, levels = c(
      "소계", "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A",
      "섬강A", "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
      "홍천A", "한탄A", "한강B", "제천A", "한강D", "북한D", "한탄B", "임진A",
      "낙본A", "기타"
    )),
    시군 = factor(시군, levels = c(
      "강원도", "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군",
      "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군",
      "양구군", "인제군", "고성군", "동해시", "속초시", "양양군"
    )),
    오염원 = factor(오염원, levels = c(
      "생활계", "축산계", "산업계_업소수", "산업계_폐수발생량", "산업계_폐수방류량",
      "토지계", "양식계_시설면적", "매립계_시설수", "매립계_침출수발생량"
    )),
    분류 = factor(분류, levels = c(
      "인구", "물사용량", "젖소", "한우", "말", "돼지", "양, 사슴", "개", "가금",
      "전", "답", "과수원", "기타초지", "임야", "기타", "대지", "공장용지",
      "공공시설지역", "교통지역", "주유소용지", "체육용지", "유원지", "소계"
    ))
  ) %>%
  arrange(시군, 단위유역, 오염원, 분류)


## 수질개선사업계획 추진실적 기준으로 정리(기타수계 제외)
alldata_s1 <- alldata_s %>%
  filter(
    !시군 %in% c("동해시", "속초시", "양양군"),
    !단위유역 %in% c("기타", "소계")
  ) %>%
  # 기타수계 제외한 합계 재산성
  bind_rows(
    (.) %>%
      group_by(시군, 오염원, 분류) %>%
      summarise_at(vars(`2014`:`2022`), ~ sum(.)) %>%
      mutate(
        단위유역 = "소계",
        단위유역 = factor(단위유역, levels = c(
          "소계", "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A",
          "섬강A", "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
          "홍천A", "한탄A", "한강B", "제천A", "한강D", "북한D", "한탄B", "임진A",
          "낙본A", "기타"
        ))
      )
  ) %>%
  arrange(시군, 단위유역, 오염원, 분류)



###  엑셀 파일 내보내기_writexl
write_xlsx(alldata_s, path = "전국오염원조사/Output/전국오염원조사 자료 정리(강원도전체시군기준).xlsx")
write_xlsx(alldata_s1, path = "전국오염원조사/Output/전국오염원조사 자료 정리(총량대상시군기준).xlsx")

write.csv(alldata_s1, "전국오염원조사/Output/전국오염원조사 자료 정리(총량대상시군기준).csv", row.names = F, fileEncoding = "cp949")

##### ===== 계별 대표값 정리 ===================================================
## 축산계 정리(젖소 + 한우 + 돼지)
livestock_sum_s2 <- livestock_sum_s1 %>%
  filter(분류 %in% c("젖소", "한우", "돼지")) %>%
  group_by(단위유역, 시군, 오염원) %>%
  summarise_at(vars(`2014`:`2022`), ~ sum(.)) %>%
  mutate(분류 = "소계")

industry_sum3_s2 <- industry_sum3_s1 %>%
  filter(분류 == "소계")

landuse_sum_s2 <- landuse_sum_s1 %>%
  filter(분류 == "소계")

fishfarm_sum_s2 <- fishfarm_sum_s1 %>%
  filter(분류 == "소계")

alldata_s2 <- bind_rows(
  population_sum_s1, livestock_sum_s2, industry_sum3_s2,
  landuse_sum_s2, fishfarm_sum_s2, landfill_sum_s1
)

## 시군기준 정리
alldata_s2_시군 <- alldata_s1 %>%
  filter(
    오염원 %in% c("생활계", "축산계", "산업계_폐수방류량"),
    분류 %in% c("인구", "물사용량", "젖소", "한우", "돼지", "소계")
  ) %>%
  filter(오염원 != "축산계" | 분류 != "소계") %>%
  bind_rows(., alldata_s1 %>%
    filter(분류 %in% c("젖소", "한우", "돼지")) %>%
    group_by(단위유역, 시군, 오염원) %>%
    summarise(across(c(`2014`:`2022`), ~ sum(.)), .groups = "drop") %>%
    mutate(분류 = "소계")) %>%
  # filter(단위유역 == "합계") %>%
  select(시군, 단위유역, 오염원, 분류, `2019`:`2022`) %>%
  mutate(오염원 = factor(오염원, levels = c(
    "생활계", "축산계", "산업계_폐수방류량"
  ))) %>%
  arrange(시군, 단위유역, 오염원)


###  엑셀 파일 내보내기_writexl
write_xlsx(alldata_s2_시군, path = "전국오염원조사/Output/전국오염원조사자료 계별 대표값 정리(시군기준).xlsx")


## 유역기준 정리
alldata_s2_유역 <- alldata_s2 %>%
  select(단위유역, 시군, everything(), -분류) %>%
  filter(단위유역 != "합계", 단위유역 != "기타") %>%
  mutate(
    단위유역 = factor(단위유역, levels = c(
      "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A", "섬강B",
      "북한A", "북한B", "소양A", "인북A", "소양B", "북한C", "홍천A", "한탄A",
      "한강B", "제천A", "한강D", "북한D", "한탄B", "임진A", "낙본A"
    )),
    시군 = factor(시군, levels = c(
      "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군",
      "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군",
      "양구군", "인제군", "고성군", "동해시", "속초시", "양양군"
    )),
    오염원 = factor(오염원, levels = c(
      "생활계", "축산계", "산업계_폐수방류량",
      "토지계", "양식계_시설면적", "매립계_침출수발생량"
    ))
  ) %>%
  arrange(단위유역, 시군, 오염원)




###################################   END   ####################################
