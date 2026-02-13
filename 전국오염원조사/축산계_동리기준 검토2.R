#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(janitor)
library(readxl)
library(writexl)



#####  축산계 자료 불러오기  ###################################################
livestock <- read_excel("전국오염원조사/축산계/2022년기준_전국오염원_조사자료_축산계_가축분뇨현황.xlsx",
                        skip = 6, col_names = F) %>% 
  mutate(연도 = 2022, .before = 1)



#####  데이터 정리  ############################################################

## 변수명 지정 및 데이터 정리
livestock1 <- livestock %>%
  select(연도, 2, 12:17, 18) %>% 
  set_names(c(
    "연도", "농장명", "시군", "법정동", "법정리", "본번", "부번", "축종", "사육두수"
  )) %>%
  # 사육두수, 연도 숫자로 지정
  mutate(across(c(사육두수, 연도), as.numeric)) %>%
  # 사육두수 결측인 경우 0으로 수정
  mutate(across(c(사육두수), ~ replace(., is.na(.), 0))) %>%
  mutate(
    # 동리코드 추가("리"가 없는 "동"의 경우 "법정리"칸에 "동"으로 추가)
    동리 = str_c(시군, 법정동, ifelse(is.na(법정리), 법정동, 법정리), sep = " "),
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
livestock_total <- livestock1 %>%
  group_by(연도, 동리, 축종) %>%
  summarise(사육두수 = sum(사육두수), .groups = "drop")

livestock_summary <- livestock1 %>%
  group_by(연도, 시군) %>%
  summarise(사육두수 = sum(사육두수), .groups = "drop")

## 각 동리별 단위유역 점유율 계산(소유역 점유율 합계) 및 축종추가
share_livestock <- share %>%
  group_by(동리, 단위유역, 시군) %>%
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
    left_join(livestock_total, by = c("동리", "연도", "축종")) %>%
    # 동리별 사육두수합계와 유역 점유율 계산
    mutate(총사육두수 = round(축산계 * 사육두수)) %>%
    group_by(연도, 단위유역, 동리, 축종) %>%
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






## 특정 축종, 시군, 연도에 따라 정리
livestock_2 <- livestock_1 %>% 
  filter(축종 %in% c("한우", "돼지", "가금"), 
         시군 %in% c("원주시", "횡성군", "철원군"), 
         연도 > 2019) %>% 
  group_by(연도, 법정동코드, 시군, 법정동, 법정리, 동리, 축종) %>% 
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
  
  