### librarian 패키지 : CRAN+Github에서 패키지를 한번에 다운로드-설치-불러오기
install.packages("devtools")
devtools::install_github("DesiQuintans/librarian")


### 주요 패키지
pkg <- c(
  "tidyverse", "data.table", "ggthemes", "writexl", "openxlsx",
  "showtext", "rvest", "RSelenium", "seleniumPipes", "styler", 
  "remotes", "progress", "datapasta", "sf", "sp", "janitor", "gt",
  "psych", "ggcorrplot", "scales", "nortest", "ggplotify", "cowplot",
  "installr", "patchwork", "slider"
)

### 패키지 설치
## install.packages 함수에 "repos" 추가
## (https://stackoverflow.com/questions/25721884/how-should-i-deal-with-package-xxx-is-not-available-for-r-version-x-y-z-wa)

## 전체 패키지 설치
install.packages(pkg, dependencies = TRUE, repos='http://cran.rstudio.com/')

## 신규 패키지만 설치
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]

if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE, repos='http://cran.rstudio.com/')
}


library(tidyverse)
library(data.table)
library(ggthemes)
library(showtext)  # 그래프에 구글 폰트 적용
library(patchwork) # 그래프 레이아웃 관리
library(ggplotify) # 'cowplot'을 사용하여 생성된 플롯을 'ggplot' 객체로 변환하여 정렬 가능
library(cowplot)   # 그래프 테마, 정렬하고 배열 기능, 플롯에 주석 및 이미지 추가 등
library(GGally)

library(writexl)
library(openxlsx)
library(rvest)
library(RSelenium)
library(seleniumPipes)
library(styler)
library(remotes)
library(progress)
library(datapasta)  # 엑셀에 있는 데이터 바로 tibble이나 data.frame으로 붙여넣기
library(sf)  # GIS 관련
library(sp)  # GIS 관련
library(janitor)  # 데이터 분석 도구
library(gt)  # 표 꾸미기
library(psych)  # 상관분석
library(ggcorrplot) # 상관관계 그래프
library(scales)
library(nortest) # 정규성 검증
library(installr) # R 업데이트
library(slider)
# library(extrafont)

shelf("tidyverse", "data.table", "ggthemes", "writexl", "extrafont", "rvest", 
      "RSelenium", "seleniumPipes", "styler", "remotes", "progress", "sf", "sp",
      "janitor", "gt", "ggcorrplot")


### 퀀트 관련 패키지
pkg <- c(
  "quantmod", "httr", "jsonlite", "corrplot", "dygraphs",
  "highcharter", "plotly", "PerformanceAnalytics",
  "nloptr", "quadprog", "RiskPortfolios", "cccp",
  "timetk", "stargazer", "timeSeries"
)

new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]

if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)
}

shelf("quantmod", "httr", "jsonlite", "corrplot", "dygraphs",
      "highcharter", "plotly", "PerformanceAnalytics",
      "nloptr", "quadprog", "RiskPortfolios", "cccp",
      "timetk", "stargazer", "timeSeries")

library(quantmod)
library(httr)
library(jsonlite)
library(corrplot)
library(dygraphs)
library(highcharter)
library(plotly)
library(PerformanceAnalytics)
library(nloptr)
library(quadprog)
library(RiskPortfolios)
library(cccp)
library(timetk)
library(stargazer)
library(timeSeries)
