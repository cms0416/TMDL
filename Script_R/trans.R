##########  라이브러리 로드  ###################################################
library(tidyverse)
library(readxl)
library(writexl)
library(janitor)

##########  파일 불러오기  #####################################################
trans <- read_excel("trans.xlsx")

split <- read_excel("trans.xlsx", sheet = 2)

sec <- read_excel("trans.xlsx", sheet = 3)


##########  데이터 정리  #######################################################
trans_1 <- trans %>% 
  left_join(sec, join_by(거래일자 >= 날짜))
