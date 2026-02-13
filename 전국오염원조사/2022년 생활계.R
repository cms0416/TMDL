### 전국오염원조사자료 검토용 물사용량 자료 편집
waterusage2 <- waterusage %>%
  group_by(연도, 시군, 법정동, 법정리, 주소) %>%
  summarise(across(c(가정용물사용합계, 영업용물사용합계, 물사용량), ~sum(.)), .groups = "drop")


### 2022년 통계인구 자료 정리(과학원 제공)
통계인구 <- read_excel("전국오염원조사/2022년_통계인구_강원도.xlsx", skip = 4, col_names = T) %>% 
  mutate(동리 = ifelse(is.na(법정리), 법정동, 법정리),
         주소 = str_c(시군, 법정동, 동리, sep = " ")) %>% 
  select(주소, 인구_22년 = 총인구, 법정동, 동리)


### 1. 21년도 전국오염원조사자료 검토용 물사용량 및 인구 자료를 주소 기준으로 결합
### 2. 21년 기준 동리별 LPCD(물사용량 / 인구) 산정
### 3. 22년 통계인구 자료에 LPCD 적용하여 22년 가정용 물사용량 산정
### 4. 22년 가정용 물사용량과 21년 영업용 물사용량으로 22년 전체 물사용량 산정
생활계_인구물사용량 <- waterusage2 %>% 
  filter(연도 == 2021) %>% 
  left_join(population_total, by = c("연도", "주소")) %>%
  left_join(통계인구, by = "주소") %>% 
  mutate(LPCD = 가정용물사용합계 / 가정인구합계,
         LPCD = replace(LPCD, is.na(LPCD), 0),
         가정용물사용량_22년 = 인구_22년 * LPCD, 
         물사용량_22년 = 가정용물사용량_22년 + 영업용물사용합계) %>% 
  mutate(연도 = 2022)


### 시군별 현황 확인
생활계_인구물사용량_시군 <- 생활계_인구물사용량 %>% 
  group_by(시군) %>% 
  summarise(across(c(인구_22년, 물사용량_22년), ~sum(.)))


### 2022년 인구 및 물사용량 자료 생성
population_2022 <- 생활계_인구물사용량 %>% 
  select(연도, 주소, 가정인구합계 = 인구_22년)

waterusage_2022 <- 생활계_인구물사용량 %>% 
  select(연도, 주소, 물사용량 = 물사용량_22년)


### 2022년 자료와 기존 자료 합치기
population_total %<>% rbind(., population_2022)

waterusage_total %<>% rbind(., waterusage_2022)


### 파일 내보내기
write_xlsx(생활계_인구물사용량, path = "전국오염원조사/Output/생활계_2022년.xlsx")

