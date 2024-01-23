#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(janitor)
library(readxl)
library(writexl)


#####  축산계 자료 불러오기  ###################################################
# livestock <- read_excel("전국오염원조사/축산계/2022년기준_전국오염원_조사자료_축산계_가축분뇨현황_(가확정).xlsx",
#                         skip = 6, col_names = F)

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


#####  데이터 정리  ############################################################

## 변수명 지정 및 데이터 정리
livestock_1 <- livestock %>% 
  # mutate(연도 = 2022, .before = 1) %>% 
  select(연도, 10, 12:14, 17, 18) %>% 
  set_names(c(
    "연도", "법정동코드", "시군", "법정동", "법정리", "축종", "사육두수"
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

## 특정 축종, 시군, 연도에 따라 정리
livestock_2 <- livestock_1 %>% 
  filter(축종 %in% c("한우", "돼지", "가금"), 
         시군 %in% c("원주시", "횡성군", "철원군"), 
         연도 > 2019) %>% 
  group_by(연도, 법정동코드, 시군, 법정동, 법정리, 주소, 축종) %>% 
  summarise(사육두수 = sum(사육두수, na.rm=TRUE), .groups = "drop") %>% 
  pivot_wider(
    names_from = c(연도, 축종),
    names_glue = "{연도}_{축종}",
    names_sort = TRUE,
    names_vary = "slowest",
    values_from = 사육두수
  ) %>% 
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))


#####  파일 내보내기  ##########################################################
write_xlsx(livestock_2, path = "전국오염원조사/Output/축산계_동리기준_원주횡성철월.xlsx")  
  
  