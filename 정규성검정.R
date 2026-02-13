# 관련 패키지 로드
library(readxl)
library(writexl)
library(lubridate)
library(tidyverse)
library(data.table)
library(broom)
library(nortest)


sewer <- read_excel("Data/하수처리장.xlsx")

sewer1 <- sewer %>% mutate(BOD = log(BOD), TP = log(TP)) %>% 
  select(처리장명, BOD, TP)

stest_bod <- sewer1 %>% group_by(처리장명) %>% 
  do(tidy(shapiro.test(.$BOD))) %>% 
  mutate(statistic = round(statistic, 3), p.value = round(p.value, 3))

kstest_bod <- sewer1 %>% group_by(처리장명) %>% 
  do(tidy(ks.test(.$BOD, pnorm, mean = mean(.$BOD), sd = sd(.$BOD)))) %>% 
  mutate(statistic = round(statistic, 3), p.value = round(p.value, 3))

ltest_bod <- sewer1 %>% group_by(처리장명) %>% 
  do(tidy(lillie.test(.$BOD))) %>% 
  mutate(statistic = round(statistic, 3), p.value = round(p.value, 3))
  
adtest_bod <- sewer1 %>% group_by(처리장명) %>% 
  do(tidy(ad.test(.$BOD))) %>% 
  mutate(statistic = round(statistic, 3), p.value = round(p.value, 3))


write_xlsx(ltest_bod, path = "Output/Data/ltest_bod.xlsx")


tests <- read_excel("Output/Data/test.xlsx")

tests1 <- tests %>% mutate(BOD = log(BOD), TP = log(TP)) %>% 
  select(처리장명, BOD, TP)

shapiro.test(tests1$BOD)
lillie.test(tests1$BOD)
