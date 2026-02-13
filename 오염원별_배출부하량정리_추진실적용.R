##  라이브러리 로드  
library(tidyverse)
library(magrittr)
library(readxl)
library(writexl)

##  데이터 불러오기
bod <- read_excel("D:/1. 수질총량/4. 기본계획/0. 한강수계 2단계 기본계획 보고서/0.최종/#한강수계_최종(인쇄)/기본계획 총괄표.xlsx", 
                  sheet = "오염원별 BOD 배출부하량(5장)", skip = 5) %>% 
  mutate(대상물질 = "BOD", .before = 오염원)

tp <- read_excel("D:/1. 수질총량/4. 기본계획/0. 한강수계 2단계 기본계획 보고서/0.최종/#한강수계_최종(인쇄)/기본계획 총괄표.xlsx", 
                  sheet = "오염원별 T-P 배출부하량(5장)", skip = 5) %>% 
  mutate(대상물질 = "T-P", .before = 오염원)

##  bod, tp 데이터 합치고 필요한 열만 선택
bind <- bod %>% bind_rows(tp) %>% 
  select(단위유역:`2020년`, `2025년`, `2030년`) %>% 
  relocate(단위유역, .after = 시군) %>% 
  mutate(오염원 = ifelse(오염원 == "총계", "합계", 오염원)) 

## 항목별 순서 지정
order_func <- function(data) {
  data %>%
    mutate(
      시군 = factor(시군, levels = c(
        "춘천시", "원주시", "강릉시", "태백시", "삼척시", 
        "홍천군", "횡성군", "영월군", "평창군", "정선군", 
        "철원군", "화천군", "양구군", "인제군", "고성군"
      )),
      단위유역 = factor(단위유역, levels = c(
        "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A", "섬강B", 
        "북한A", "북한B", "소양A", "인북A", "소양B", "북한C", "홍천A", "한탄A", 
        "제천A", "한강B", "한강D", "북한D", "임진A", "한탄B", "낙본A"
      )),
      오염원 = factor(오염원, levels = c(
        "생활계", "축산계", "산업계", "토지계", "양식계", "매립계", "합계"
      ))
    )
}

##  데이터 정렬
data <- bind %>% 
  filter(시도 == "강원도", 시군 != "소계") %>% 
  select(-시도) %>% 
  order_func %>% 
  arrange(시군, 단위유역, 대상물질, 오염원) 

##  파일 내보내기
write_xlsx(data, path = "D:/1. 수질총량/7. 수질개선사업/추진실적/오염원별_배출부하량.xlsx")


