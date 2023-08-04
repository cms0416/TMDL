### librarian 패키지 : CRAN+Github에서 패키지를 한번에 다운로드-설치-불러오기
install.packages("devtools")
devtools::install_github("DesiQuintans/librarian")


### 주요 패키지
pkg <- c(
  "tidyverse", "data.table", "ggthemes", "writexl", "openxlsx",
  "extrafont", "rvest", "RSelenium", "seleniumPipes", "styler", 
  "remotes", "progress"
)

new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)
}

library(tidyverse)
library(data.table)
library(ggthemes)
library(writexl)
library(openxlsx)
library(extrafont)
library(rvest)
library(RSelenium)
library(seleniumPipes)
library(styler)
library(remotes)
library(progress)

shelf("tidyverse", "data.table", "ggthemes", "writexl", "extrafont", "rvest", 
      "RSelenium", "seleniumPipes", "styler", "remotes", "progress")


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
