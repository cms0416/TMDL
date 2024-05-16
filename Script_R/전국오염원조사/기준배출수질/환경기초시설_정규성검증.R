# 정규성검사 BOD

rm(list = ls())

library('dplyr')
library('nortest')
library(readxl)

# Setting 
setwd("C:/Users/user/Desktop/R/전오사")

data <- read_excel('전국오염원조사/기준배출수질/환경기초시설_22.xlsx')
data <- select(data, TWW, BOD)

data <- na.omit(data)
data <- data[!(data$BOD == 0),]

data<- data %>% mutate(ln_BOD=log(BOD,base = exp(1)))


drowname <- table(data$TWW)    #하수처리장 측정 빈도 확인

drowname1 <- data.frame(drowname)

colnames(drowname1) <- c("TWW","측정횟수")

drowname2 <- drowname1 %>% filter(측정횟수 >= 30)      # 측정 횟수 30회 이상만 필터링

R1 <- filter(.data = data, TWW == "간동오음리공공하수도")  

A1 <- lillie.test(R1$ln_BOD)        #Kolmogorov-Smirnov
A2 <- unlist(A1)
result_KS <- data.frame(A2)

B1 <- shapiro.test(R1$ln_BOD)       #Shapiro-Wilk
B2 <- unlist(B1)
result_SW <- data.frame(B2)

C1 <- ad.test(R1$ln_BOD)            #Anderson-Darling
C2 <- unlist(C1)
result_AD <- data.frame(C2)


# Calculation

 f <- for (i in 2:255){            #기초시설수 확인
    a = drowname2[i,1]
    RAW <- filter(.data = data, TWW == a)
    
    KS2 <- lillie.test(RAW$ln_BOD)
    KS3 <- unlist(KS2)
    KS4 <- data.frame(KS3)
    result_KS[,i] <- KS4
    
    SW2 <- shapiro.test(RAW$ln_BOD)
    SW3 <- unlist(SW2)
    SW4 <- data.frame(SW3)
    result_SW[,i] <- SW4
    
    AD2 <- ad.test(RAW$ln_BOD)
    AD3 <- unlist(AD2)
    AD4 <- data.frame(AD3)
    result_AD[,i] <- AD4
    
 }
 
colnames(result_KS) <- drowname2$TWW
colnames(result_SW) <- drowname2$TWW
colnames(result_AD) <- drowname2$TWW

result_KS <- t(result_KS)
result_SW <- t(result_SW)
result_AD <- t(result_AD)

result_cbind_KS_SW <- cbind(result_KS, result_SW)
result_cbind_KS_SW_AD <- cbind(result_cbind_KS_SW, result_AD)


write.csv(result_cbind_KS_SW_AD,"Result_환경기초시설_정규성검증_2021_result_BOD.csv",
          fileEncoding = "EUC-KR") #정규성 검정 BOD 결과


# 환경기초시설 정규성검증 TP

rm(list = ls())

library('dplyr')
library('nortest')

# Setting
setwd("C:/Users/user/Desktop/R/전오사")

data <- read.csv('환경기초_2021.csv', header = TRUE)
data <- select(data, TWW, TP)

data <- na.omit(data)
data <- data[!(data$TP == 0),]

data<- data %>% mutate(ln_TP=log(TP,base = exp(1)))

drowname <- table(data$TWW)

drowname1 <- data.frame(drowname)

colnames(drowname1) <- c("TWW","측정횟수")

drowname2 <- drowname1 %>% filter(측정횟수 >= 30)

R1 <- filter(.data = data, TWW == "간동오음리공공하수도")  

A1 <- lillie.test(R1$ln_TP)        #Kolmogorov-Smirnov
A2 <- unlist(A1)
result_KS <- data.frame(A2)

B1 <- shapiro.test(R1$ln_TP)       #Shapiro-Wilk 
B2 <- unlist(B1)
result_SW <- data.frame(B2)

C1 <- ad.test(R1$ln_TP)            #Anderson-Darling
C2 <- unlist(C1)
result_AD <- data.frame(C2)


# Calculation

f <- for (i in 2:224){              #기초시설수 확인
    a = drowname2[i,1]

    RAW <- filter(.data = data, TWW == a)
    
    KS2 <- lillie.test(RAW$ln_TP)
    KS3 <- unlist(KS2)
    KS4 <- data.frame(KS3)
    result_KS[,i] <- KS4
    
    SW2 <- shapiro.test(RAW$ln_TP)
    SW3 <- unlist(SW2)
    SW4 <- data.frame(SW3)
    result_SW[,i] <- SW4
    
    AD2 <- ad.test(RAW$ln_TP)
    AD3 <- unlist(AD2)
    AD4 <- data.frame(AD3)
    result_AD[,i] <- AD4
    
}

colnames(result_KS) <- drowname2$TWW
colnames(result_SW) <- drowname2$TWW
colnames(result_AD) <- drowname2$TWW

result_KS <- t(result_KS)
result_SW <- t(result_SW)
result_AD <- t(result_AD)


result_cbind_KS_SW <- cbind(result_KS, result_SW)
result_cbind_KS_SW_AD <- cbind(result_cbind_KS_SW, result_AD)


write.csv(result_cbind_KS_SW_AD,"Result_환경기초시설_21_정규성검증_TP.csv")  #정규성 검정 TP 결과


