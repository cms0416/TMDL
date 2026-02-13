
#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(janitor)
library(readxl)
library(writexl)
library(sf)


#####  파일 불러오기  ##########################################################
## 법정동 코드
법정동코드 <- read_excel("주소 검토/법정동코드_강원_240619.xlsx") %>%
  mutate(동리 = ifelse(
    str_sub(읍면동, -1) == "읍" | str_sub(읍면동, -1) == "면",
    리,
    읍면동
  )) %>%
  filter(
     !is.na(동리) # 폐지여부 == "존재", 시도 == "강원특별자치도"
  ) 
  # select(-c(폐지여부, 시도, 리))

법정동코드_존재 <- 법정동코드 %>% 
  filter(폐지여부 == "존재") %>% 
  mutate(주소코드 = str_c(시군구, 읍면동, 동리))


법정동코드_폐지 <- 법정동코드 %>% 
  filter(폐지여부 == "폐지") %>% 
  select(-법정동코드) %>% 
  left_join(법정동코드_존재 %>% 
              select(법정동코드, 시군구, 동리), 
            by = c("시군구", "동리"))
  
  

## 섬강A 오염원
오염원 <- read_excel("전국오염원조사/Output/섬강A_2023_축산_농지.xlsx")
  


## 섬강A 동리 파일
섬강A_동리 <- st_read("D:/GIS/행정구역/2017법정동리_섬강A(원주, 횡성).shp") 

 

#####  데이터 정리  ############################################################

## 데이터 정리
섬강A_동리_정리 <- 섬강A_동리 %>% 
  # 지명 변경된 경우 변경 지명 적용
  mutate(
    읍면동명 = case_when(
    str_c(시군구명, 읍면동명) == "홍천군동면" ~ "영귀미면",
    .default = 읍면동명),
    주소코드 = str_c(시군구명, 읍면동명, 동리명)) %>% 
  select(-c(LI_CD, EMD_CD, 면적)) %>% 
  left_join(오염원 %>% select(주소코드, 소, 돼지, 가금, 농지, 농지비율), 
            by = "주소코드") %>% 
  select(-주소코드) %>% 
  set_names(c("sido", "sigun", "emd", "dongli", "cow", "pig", "poultry", "farm", 
              "farm_ratio", "geometry")) %>%
  mutate(across(where(is.factor), as.character))

## shp 파일 내보내기
st_write(섬강A_동리_정리, "D:/GIS/오염원, 시설/2024 섬강A/섬강A_오염원.shp",
         layer_options = "ENCODING=UTF-8",
         append = FALSE)

