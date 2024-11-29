##########  라이브러리 로드  ###################################################
library(tidyverse)
library(readxl)
library(writexl)


##########  파일 불러오기  #####################################################

### 데이터 불러오기 및 전처리(측정 자료에 목표수질, 권역, 강원도 여부 등 추가)
총량측정망 <- read_excel("수질분석/총량측정망_2007_2024.xlsx")

변경지점 <- read_excel("수질분석/총량측정망운영계획변경검토.xlsx", 
                   skip = 2, col_names = F) %>% 
  select(1, 2, 7, 11, 13) %>% 
  set_names(c("총량지점명", "일자", "BOD", "TP", "유량")) %>% 
  mutate(일자 = str_replace_all(일자, "\\.", "-") %>% as.Date(.),
         연도 = year(일자),
         월 = month(일자),
         유달부하_BOD_b = BOD * 유량 * 86.4,
         유달부하_TP_b = TP * 유량 * 86.4)

변경지점_연도별유달부하 <- 변경지점 %>% 
  filter(!is.na(유달부하_TP_b)) %>% 
  group_by(총량지점명, 연도) %>% 
  summarise(across(c(유달부하_BOD_b, 유달부하_TP_b), mean), .groups = "drop") %>% 
  mutate(단위유역 = str_remove_all(총량지점명, "[0-9]"), .before = 1)


총량측정망_연도별유달부하 <- 총량측정망 %>%
  select(총량지점명, 일자, 연도, BOD, TP, 유량) %>% 
  mutate(유달부하_BOD_a = BOD * 유량 * 86.4,
         유달부하_TP_a = TP * 유량 * 86.4) %>% 
  filter(!is.na(유달부하_TP_a), 
         연도 > 2018, 
         연도 < 2024,
         총량지점명 %in% c("골지A", "한강A", "주천A", "옥동A", 
                      "섬강A", "섬강B", "인북A", "홍천A")) %>% 
  group_by(총량지점명, 연도) %>% 
  summarise(across(c(유달부하_BOD_a, 유달부하_TP_a), mean), .groups = "drop") %>% 
  rename(단위유역 = 총량지점명) %>% 
  left_join(변경지점_연도별유달부하, by = c("단위유역", "연도")) %>% 
  filter(!is.na(총량지점명)) %>% 
  mutate(영향_BOD = round(유달부하_BOD_b / 유달부하_BOD_a * 100, 2),
         영향_TP = round(유달부하_TP_b / 유달부하_TP_a * 100, 2)) %>% 
  relocate(c(유달부하_BOD_b, 영향_BOD), .after = 유달부하_BOD_a) %>% 
  relocate(총량지점명, .after = 단위유역)



