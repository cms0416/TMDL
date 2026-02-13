#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(janitor)
library(readxl)
library(writexl)
library(sf)
library(units)  # 단위 설정


#####  파일 불러오기  ##########################################################

법정동코드 <- read_excel("주소 검토/법정동코드 전체자료_240619.xlsx",
                    guess_max = Inf) %>% 
  filter(폐지여부 == "존재", 시도 == "강원특별자치도", !is.na(읍면동)) %>% 
  mutate(동리 = ifelse(is.na(리), 읍면동, 리)) %>% 
  select(-c(폐지여부, 시도))

지목 <- read_excel("D:/GIS/토지이용/연속지적도/지목부호.xlsx")

## 연속지적도 파일
연속지적도 <- st_read("D:/GIS/토지이용/연속지적도/2024년 6월/연속지적도_강원_한강_240614(유역도결합).shp")

연속지적도 %<>% filter(면적 != 0)

연속지적도_정리 <- 연속지적도 %>% 
  rename(단위유역 = SW_NAME) %>% 
  filter(단위유역 == "섬강A") %>% 
  # select(-c(BCHK, SGG_OID, COL_ADM_SE, LI_CD, 시도명, 면적1)) %>% 
  mutate(
    법정동코드 = str_sub(PNU, 1, 10) %>% as.numeric()) %>% 
  left_join(법정동코드, by = "법정동코드") %>% 
  mutate(
    지번 = str_sub(JIBUN, 1, -2) %>% str_trim(),
    지목약자 = str_sub(JIBUN, -1),
    .after = 리
    ) %>% 
  left_join(지목, by = "지목약자")



연속지적도_정리2 <- 연속지적도_정리 %>% 
  filter(is.na(PNU))


## 면적계산
연속지적도_동리_유역_면적 <- 연속지적도_동리_유역 %>% 
  mutate(area = set_units(st_area(.), km^2) %>% as.numeric())


