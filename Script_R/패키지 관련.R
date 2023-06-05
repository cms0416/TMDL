### 주요 패키지
pkg <- c(
  "tidyverse", "data.table", "ggthemes", "writexl",
  "extrafont", "rvest", "RSelenium", "seleniumPipes",
  "styler", "remotes", "progress"
)

new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)
}

library("tidyverse")
library("data.table")
library("ggthemes")
library("writexl")
library("extrafont")
library("rvest")
library("RSelenium")
library("seleniumPipes")
library("styler")
library("remotes")
library("progress")


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

library("quantmod")
library("httr")
library("jsonlite")
library("corrplot")
library("dygraphs")
library("highcharter")
library("plotly")
library("PerformanceAnalytics")
library("nloptr")
library("quadprog")
library("RiskPortfolios")
library("cccp")
library("timetk")
library("stargazer")
library("timeSeries")
