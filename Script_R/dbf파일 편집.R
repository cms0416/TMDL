
################################################################################
## 라이브러리 로드
library(tidyverse)
library(foreign)
library(readxl)
################################################################################

data <- read.dbf("D:/GIS/행정구역/2017법정동리.dbf", as.is = TRUE)

Encoding(data$X.Ãµµ.í) <-"EUC-KR"

################################################################################

data <- read.dbf("D:/GIS/토지이용/연속지적도/점유율산정용/점유율산정 연속지적도(2022.04).dbf")

# 지번과 지목 분리해서 정리 : str_sub(문자열, 시작위치, 끝위치)
data1 <- data %>% mutate(
  JIBUN2 = str_sub(JIBUN, 1, -2),
  JIMOK = str_sub(JIBUN, -1, -1),
  PNU2 = str_sub(PNU, 1, 10),
  PNU3 = str_sub(PNU, 11, 11)
)

code <- read_excel("D:/GIS/토지이용/연속지적도/법정동코드 전체자료.xlsx")


###  엑셀 파일 내보내기_writexl
write.dbf(data, "D:/GIS/토지이용/연속지적도/점유율산정용/점유율산정 연속지적도(2022.04)_원본백업2.dbf")
write.dbf(data1, "D:/GIS/토지이용/연속지적도/점유율산정용/점유율산정 연속지적도(2022.04).dbf")
