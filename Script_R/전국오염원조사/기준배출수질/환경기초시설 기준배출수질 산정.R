# 가. 기준배출수질_BOD
rm(list = ls())

setwd("C:/Users/user/Desktop/R/전오사")                                  #경로설정

# Load Library
library("dplyr")                                                         #라이브러리 Load
#data load
rawdata <- read.table('환경기초_2021.csv',sep = ',',header = TRUE)       # 파일 Load
rawdata <- select(rawdata, TWW, BOD)                                     # 데이터 선택
rawdata <- na.omit(rawdata)                                              # 빈값 제거
rawdata <- rawdata[!(rawdata$BOD == 0),]                                 # 0값 제거
rawdata <- rawdata %>% mutate(ln_BOD=log(BOD,base = exp(1)))             # 로그변환

Result_가_BOD <- rawdata %>% group_by(TWW) %>% summarise(mean_BOD = mean(ln_BOD), sd_BOD = sd(ln_BOD), ave_BOD = exp(1)^(mean_BOD+sd_BOD*1.645))                                                 # 환경기초시설별 산식으로 계산(정규성분포)

write.csv(Result_가_BOD,"Result_환경기초_21_가_BOD.csv")                 # 파일 save  

# 가. 기준배출수질_TP
rm(list = ls())

setwd("C:/Users/user/Desktop/R/전오사")

# Load Library
library("dplyr")
#data load
rawdata <- read.table('환경기초_2021.csv',sep = ',',header = TRUE)
rawdata <- select(rawdata, TWW, TP)

rawdata <- na.omit(rawdata)
rawdata <- rawdata[!(rawdata$TP == 0),]
rawdata <- rawdata %>% mutate(ln_TP=log(TP,base = exp(1)))

Result_가_TP <- rawdata %>% group_by(TWW) %>% summarise(mean_TP = mean(ln_TP), sd_TP = sd(ln_TP), ave_TP = exp(1)^(mean_TP+sd_TP*1.645))

write.csv(Result_가_TP,"Result_환경기초_21_가_TP.csv")



# 나. 기준배출수질_BOD
rm(list = ls())             

setwd("C:/Users/user/Desktop/R/전오사")                   #경로설정

# Load Library
library("dplyr")                                          #라이브러리 Load
#data load
rawdata <- read.table('환경기초_2021.csv',sep = ',',header = TRUE)         # 파일 Load
rawdata <- select(rawdata, TWW, BOD)                                       # 데이터 선택
rawdata <- na.omit(rawdata)                                                # 빈값 제거
rawdata <- rawdata[!(rawdata$BOD == 0),]                                   # 0값 제거


BOD_result <- rawdata %>% group_by(TWW) %>% arrange(BOD, .by_group = TRUE) %>% 
  summarise(count = n(), NO_BOD = 1+0.95*(count-1), a_BOD = floor(NO_BOD), b_BOD = NO_BOD - a_BOD, Xa_BOD = nth(BOD, a_BOD), Xaa_BOD = nth(BOD, a_BOD+1), BOD_1 = (1-b_BOD)*Xa_BOD+b_BOD*Xaa_BOD)       # 환경기초시설별 산식으로 계산(비정규성분포)


write.csv(BOD_result,"Result_환경기초_21_나_BOD.csv")

# 나. 기준배출수질_TP
rm(list = ls())

setwd("C:/Users/user/Desktop/R/전오사")

# Load Library
library("dplyr")
#data load
rawdata <- read.table('환경기초_2021.csv',sep = ',',header = TRUE)
rawdata <- select(rawdata, TWW, TP)

rawdata <- na.omit(rawdata)
rawdata <- rawdata[!(rawdata$TP == 0),]



TP_result <- rawdata %>% group_by(TWW) %>% arrange(TP, .by_group = TRUE) %>% summarise(count = n(), NO_TP = 1+0.95*(count-1), a_TP = floor(NO_TP), b_TP = NO_TP - a_TP, Xa_TP = nth(TP, a_TP), Xaa_TP = nth(TP, a_TP+1), TP_1 = (1-b_TP)*Xa_TP+b_TP*Xaa_TP)

# head(result)

write.csv(TP_result,"Result_환경기초_21_나_TP.csv")