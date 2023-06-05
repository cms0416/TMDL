#####  라이브러리 로드  ########################################################
library(tidyverse)
library(data.table)
library(readxl)
library(writexl)
library(lubridate)
################################################################################


### 점유율 자료 불러오기
share <- read_excel("D:/1. 수질총량/10. 할당부하량 관리/2021/B3오염원별점유율.xlsx") %>%
  mutate_at(vars(행정구역코드), as.character) %>% 
  select(행정구역코드) %>% 
  distinct() %>% 
  mutate(총량대상 = "대상") 


######  축산계 검토  ###########################################################

### 현재 데이터 불러오기
data_livestock_2021 <- read_excel("Data/부하량 검토/축산계검토/P3축산현황.xlsx") %>% 
  left_join(share, by = "행정구역코드") %>% 
  filter(총량대상 == "대상") %>% 
  select(년도, 시군구, 축종, 사육두수)

### 기본계획 데이터(2017년) 불러오기
data_livestock_2017 <- read_excel("Data/부하량 검토/축산계검토/축산현황(기본계획).xlsx") %>% 
  filter(시도 == "강원도") %>% 
  mutate_at(vars(행정구역코드), as.character) %>% 
  left_join(share, by = "행정구역코드") %>% 
  filter(총량대상 == "대상") %>% 
  select(년도, 시군구, 축종, 사육두수)

### 데이터 합치고 년도, 시군, 축종별 합계
data_livestock <- data_livestock_2017 %>% 
  rbind(data_livestock_2021) %>% 
  group_by(년도, 시군구, 축종) %>% 
  summarise(사육두수 = sum(사육두수), .groups = "drop") %>% 
  filter(시군구 != "속초시", 시군구 != "동해시", 시군구 != "양양군")

### 소, 양/사슴 합치기
data_livestock <- data_livestock %>% 
  rbind(
    data_livestock %>% 
      filter(축종 %in% c("젖소", "한우")) %>% 
      group_by(년도, 시군구) %>% 
      summarise(사육두수 = sum(사육두수), .groups = "drop") %>% 
      mutate(축종 = "소")
  ) %>% 
  rbind(
    data_livestock %>% 
      filter(축종 %in% c("양", "산양", "사슴")) %>% 
      group_by(년도, 시군구) %>% 
      summarise(사육두수 = sum(사육두수), .groups = "drop") %>% 
      mutate(축종 = "양/사슴")
  ) %>% 
  filter(축종 != "젖소", 축종 != "한우", 축종 != "양", 
         축종 != "산양", 축종 != "사슴") 

### 강원도전체 합계 계산
data_livestock <- data_livestock %>% 
  rbind(
    data_livestock %>% 
      group_by(년도, 축종) %>% 
      summarise(사육두수 = sum(사육두수), .groups = "drop") %>% 
      mutate(시군구 = "합계")
  ) %>% 
  mutate(
    시군구 = factor(시군구, levels = c(
      "합계", "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군",
      "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군", "양구군",
      "인제군", "고성군"
    )), 
    축종 = factor(축종, levels = c(
      "소", "돼지", "말", "양/사슴", "개", "가금"
    ))
  ) %>% 
  arrange(시군구, 년도, 축종)

### 변화율 계산
data_livestock_pivot <- data_livestock %>% 
  pivot_wider(
    names_from = 년도,
    values_from = 사육두수
  ) %>% 
  mutate_at(vars(`2017`, `2021`), ~ replace(., is.na(.), 0)) %>% 
  mutate(변화율 = round((`2021` - `2017`) / `2017` * 100, 2))


### 파일 내보내기
write_xlsx(data_livestock_pivot, path = "Output/Data/부하량산정 기초자료/부하량초과검토.xlsx")


### 그래프
plot_cow <- data %>% 
  filter(축종 == "소") %>% 
  filter(시군구 != "합계") 

plot_cow %>% 
  ggplot() +
  geom_bar(aes(x = 시군구, y = 사육두수, fill = factor(년도)), 
           stat = "identity", position = "dodge", colour = "black") +
  scale_fill_brewer(palette = "Pastel1")

plot_pig <- data %>% 
  filter(축종 == "돼지") %>% 
  filter(시군구 != "합계")

plot_pig %>% 
  ggplot() +
  geom_bar(aes(x = 시군구, y = 사육두수, fill = factor(년도)), 
           stat = "identity", position = "dodge", colour = "black") +
  scale_fill_brewer(palette = "Pastel1")



######  생활계 검토  ###########################################################

### 점유율 자료 불러오기
share <- read_excel("D:/1. 수질총량/10. 할당부하량 관리/2021/B3오염원별점유율.xlsx") %>%
  mutate_at(vars(행정구역코드), as.character) %>% 
  select(행정구역코드) %>% 
  distinct() %>% 
  mutate(총량대상 = "대상") 

### 2017년 ###

### 분류식 유량 / 수거식 유량 분리 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
data_water_2017 <- read_excel("Data/부하량 검토/생활계검토/생활계물사용량.xlsx") %>% 
  filter(시도 == "강원도") %>% 
  select(년도, 시군구, 물사용량합계, 가정시가분류, 가정시가수거, 
         가정비시가분류, 가정비시가수거, 영업시가분류, 영업시가수거, 
         영업비시가분류, 영업비시가수거) %>% 
  mutate(유량_분류 = 가정시가분류 + 가정비시가분류 + 영업시가분류 + 영업비시가분류,
         유량_수거 = 가정시가수거 + 가정비시가수거 + 영업시가수거 + 영업비시가수거,
         합계 = 유량_분류 + 유량_수거,
         검증 = 물사용량합계 - 합계) %>% 
  group_by(년도, 시군구) %>% 
  summarise(유량_분류 = round(sum(유량_분류), 2), 
            유량_수거 = round(sum(유량_수거), 2),
            .groups = "drop") 

### 분류식 인구 / 수거식 인구 분리 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
data_population_2017 <- read_excel("Data/부하량 검토/생활계검토/생활계가정인구현황.xlsx") %>% 
  filter(시도 == "강원도") %>% 
  select(년도, 시군구, 총인구, 시가분류식인구, 시가수거식인구, 
         비시가분류식인구, 비시가수거식인구) %>% 
  mutate(인구_분류 = 시가분류식인구 + 비시가분류식인구, 
         인구_수거 = 시가수거식인구 + 비시가수거식인구,
         합계 = 인구_분류 + 인구_수거,
         검증 = 총인구 - 합계) %>% 
  group_by(년도, 시군구) %>% 
  summarise(인구_분류 = sum(인구_분류), 
            인구_수거 = sum(인구_수거), 
            .groups = "drop") %>% 
  left_join(data_water_2017, by = c("년도", "시군구"))


data_iSTP_2017 <- read_excel("Data/부하량 검토/생활계검토/오수처리시설.xlsx") %>% 
  filter(시도 == "강원도") %>% 
  select(연도, 시군구, 인구, 사용유량) %>% 
  rename(년도 = 연도) %>% 
  group_by(년도, 시군구) %>% 
  summarise(인구_개인오수 = sum(인구), 유량_개인오수 = round(sum(사용유량), 2), .groups = "drop") 


data_septic_2017 <- read_excel("Data/부하량 검토/생활계검토/정화조.xlsx") %>% 
  filter(시도 == "강원도") %>% 
  select(연도, 시군구, 인구, 사용유량) %>% 
  rename(년도 = 연도) %>% 
  group_by(년도, 시군구) %>%  
  summarise(인구_정화조 = sum(인구), 유량_정화조 = round(sum(사용유량), 2), .groups = "drop") 
  

data_2017 <- data_population_2017 %>% 
  left_join(data_iSTP_2017, by = c("년도", "시군구")) %>%
  left_join(data_septic_2017, by = c("년도", "시군구")) %>% 
  mutate(인구_총계 = 인구_분류 + 인구_수거 + 인구_개인오수 + 인구_정화조, 
         유량_총계 = 유량_분류 + 유량_수거 + 유량_개인오수 + 유량_정화조) %>% 
  group_by(년도, 시군구) %>%
  rbind(
    (.) %>% 
      group_by(년도) %>% 
      summarise(인구_분류 = sum(인구_분류), 인구_수거 = sum(인구_수거), 
                인구_개인오수 = sum(인구_개인오수), 인구_정화조 = sum(인구_정화조),
                인구_총계 = sum(인구_총계),
                유량_분류 = sum(유량_분류), 유량_수거 = sum(유량_수거), 
                유량_개인오수 = sum(유량_개인오수), 유량_정화조 = sum(유량_정화조), 
                유량_총계 = sum(유량_총계)) %>% 
      mutate(시군구 = "합계")
  )


### 2021년 ###

### 분류식 유량 / 수거식 유량 분리 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
data_water_2021 <- read_excel("Data/부하량 검토/생활계검토/P2생활계물사용량.xlsx") %>% 
  left_join(share, by = "행정구역코드") %>% 
  filter(총량대상 == "대상") %>% 
  select(년도, 시군구, 물사용량합계, 가정시가분류, 가정시가수거, 
         가정비시가분류, 가정비시가수거, 영업시가분류, 영업시가수거, 
         영업비시가분류, 영업비시가수거) %>% 
  mutate(유량_분류 = 가정시가분류 + 가정비시가분류 + 영업시가분류 + 영업비시가분류,
         유량_수거 = 가정시가수거 + 가정비시가수거 + 영업시가수거 + 영업비시가수거,
         합계 = 유량_분류 + 유량_수거,
         검증 = 물사용량합계 - 합계) %>% 
  group_by(년도, 시군구) %>% 
  summarise(유량_분류 = round(sum(유량_분류), 2), 
            유량_수거 = round(sum(유량_수거), 2),
              .groups = "drop") 

### 분류식 인구 / 수거식 인구 분리 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
data_population_2021 <- read_excel("Data/부하량 검토/생활계검토/P1생활계가정인구현황.xlsx") %>% 
  left_join(share, by = "행정구역코드") %>% 
  filter(총량대상 == "대상") %>% 
  select(년도, 시군구, 총인구, 시가분류식인구, 시가수거식인구, 비시가분류식인구, 비시가수거식인구) %>% 
  mutate(인구_분류 = 시가분류식인구 + 비시가분류식인구, 
         인구_수거 = 시가수거식인구 + 비시가수거식인구,
         합계 = 인구_분류 + 인구_수거,
         검증 = 총인구 - 합계) %>% 
  group_by(년도, 시군구) %>% 
  summarise(인구_분류 = sum(인구_분류), 
            인구_수거 = sum(인구_수거), 
              .groups = "drop")  %>% 
  left_join(data_water_2021, by = c("년도", "시군구"))
  

data_iSTP_2021 <- read_excel("Data/부하량 검토/생활계검토/P2_1오수처리시설.xlsx") %>% 
  left_join(share, by = "행정구역코드") %>% 
  filter(총량대상 == "대상") %>% 
  select(연도, 시군구, 인구, 사용유량) %>% 
  rename(년도 = 연도) %>% 
  group_by(년도, 시군구) %>%
  summarise(인구_개인오수 = sum(인구), 유량_개인오수 = round(sum(사용유량), 2), .groups = "drop")
  
  
data_septic_2021 <- read_excel("Data/부하량 검토/생활계검토/P2_2정화조.xlsx") %>% 
  left_join(share, by = "행정구역코드") %>% 
  filter(총량대상 == "대상") %>% 
  select(연도, 시군구, 인구, 사용유량) %>% 
  rename(년도 = 연도) %>% 
  group_by(년도, 시군구) %>%
  summarise(인구_정화조 = sum(인구), 유량_정화조 = round(sum(사용유량), 2), .groups = "drop") 


data_2021 <- data_population_2021 %>% 
  left_join(data_iSTP_2021, by = c("년도", "시군구")) %>%
  left_join(data_septic_2021, by = c("년도", "시군구")) %>% 
  mutate(인구_총계 = 인구_분류 + 인구_수거 + 인구_개인오수 + 인구_정화조, 
         유량_총계 = 유량_분류 + 유량_수거 + 유량_개인오수 + 유량_정화조) %>% 
  group_by(년도, 시군구) %>%
  rbind(
    (.) %>% 
      group_by(년도) %>% 
      summarise(인구_분류 = sum(인구_분류), 인구_수거 = sum(인구_수거), 
                인구_개인오수 = sum(인구_개인오수), 인구_정화조 = sum(인구_정화조),
                인구_총계 = sum(인구_총계),
                유량_분류 = sum(유량_분류), 유량_수거 = sum(유량_수거), 
                유량_개인오수 = sum(유량_개인오수), 유량_정화조 = sum(유량_정화조), 
                유량_총계 = sum(유량_총계)) %>% 
      mutate(시군구 = "합계")
  )
  
data_compare <- data_2017 %>% rbind(data_2021) %>% filter(시군구 == "합계")
