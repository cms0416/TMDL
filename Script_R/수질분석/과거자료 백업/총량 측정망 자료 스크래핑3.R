###############################################################################################################
#
# selenium 실행 : 윈도우에서 cmd를 통해 명령 프롬프트를 연 후, 아래 명령어를 입력(cmd창은 계속 열어둔다.)
#
# cd C:\Rselenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-2.jar -port 4445
#
###############################################################################################################

################################################################################
## 라이브러리 로드
library(tidyverse)
library(data.table)
library(RSelenium)
library(seleniumPipes)
library(rvest)
library(httr)
library(writexl)

################################################################################


## 4445번 포트와 크롬 연결
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "chrome"
)

## 크롬 실행
remDr$open()

## 물환경정보시스템 총량측정망 자료조회 접속
remDr$navigate("http://water.nier.go.kr/web/totalMeasure?pMENU_NO=3")

## 지점 선택 팝업창 열기
remDr$findElement(using = "xpath", value = '//*[@id="searchForm"]/div[2]/div[2]/a')$clickElement()

################################################################################
## 지점 선택을 위한 순서 확인

## 페이지 로딩 시간 대기
Sys.sleep(0.5)

## 페이지 소스 읽어오기
page_parse <- remDr$getPageSource()[[1]]

## 페이지 소스 중 HTML 정보만 읽어오기
page_html <- page_parse %>% read_html()

## 페이지 내 테이블 데이터 추출
Sys.setlocale("LC_ALL", "English")  # 로케일 언어를 English로 변경
table <- page_html %>% html_table(fill = TRUE)  # 테이블 데이터만 추출
Sys.setlocale("LC_ALL", "Korean")  # 다시 로케일 언어를 Korean으로 변경

## 테이블 데이터 중 측정지점 목록이 포함된 테이블 만 추출(4번 테이블)
station <- table[[4]]

## 지점 번호 추가 및 지점 선택
station <- station %>%
  select(-1) %>%
  mutate(ID = row_number()) %>%
  filter(측정소명 %in% c(
    "가평A", "경안A", "경안B", "골지A", "공릉A", "굴포A", "달천A", "달천B",
    "문산A", "복하A", "북한A", "북한B", "북한C", "북한D", "섬강A", "섬강B",
    "소양A", "소양B", "신천A", "안양A", "양화A", "영평A", "오대A", "옥동A",
    "왕숙A", "인북A", "임진A", "임진B", "제천A", "조종A", "주천A", "중랑A",
    "청미A", "탄천A", "평창A", "한강A", "한강B", "한강C", "한강D", "한강E",
    "한강F", "한강G", "한강H", "한강I", "한탄A", "한탄B", "홍천A", "흑천A",
    "낙본A"
  ))

## 지점 번호 추출 후 벡터로 변환(pull(열 번호))
stationid <- station %>%
  select(ID) %>%
  pull(1)

################################################################################


## 확인 된 지점 번호로 지점 선택
for (i in stationid) {
  remDr$findElement(
    using = "xpath",
    value = paste0("/html/body/div[3]/div[2]/form[1]/div[2]/div[2]/div[1]/div[3]/div[1]/div/div/div[2]/table/tbody/tr[", i, "]/td[1]/input")
  )$clickElement()
}

Sys.sleep(1)

## 지점 선택 완료(확인 클릭)
remDr$findElement(using = "xpath", value = '//*[@id="dataSearchPop"]/div[4]/a[1]')$clickElement()

## 자료 조회 기간 월 설정(한번 설정 후 고정으로 반복문 제외)
remDr$findElement(using = "xpath", value = '//*[@id="s_month"]/option[text() = "01"]')$clickElement() # 시작 월
remDr$findElement(using = "xpath", value = '//*[@id="e_month"]/option[text() = "12"]')$clickElement() # 종료 월
# remDr$findElement(using = "xpath", value = '//*[@id="s_year"]/option[text() = "2020"]')$clickElement()  # 시작 연도
# remDr$findElement(using = "xpath", value = '//*[@id="e_year"]/option[text() = "2020"]')$clickElement()  # 종료 연도


################################################################################


## 지정된 기간 동안 자료 조회 후 추출하여 연차별 병합
for (i in 2007:2021) {  # <--------------- 시작연도 변경 시 아래 obs 병합 시작 연도도 동일하게 변경
  ## 자료 조회 기간 연도 설정
  remDr$findElement(using = "xpath", value = paste0('//*[@id="s_year"]/option[text() = ', i, "]"))$clickElement() # 시작 연도
  remDr$findElement(using = "xpath", value = paste0('//*[@id="e_year"]/option[text() = ', i, "]"))$clickElement() # 종료 연도

  ## 자료 검색(검색버튼 클릭)
  remDr$findElement(using = "xpath", value = '//*[@id="searchForm"]/div[2]/a')$clickElement()

  ## 페이지 로딩 시간 대기
  Sys.sleep(5)

  ## 페이지 소스 읽어오기
  page_parse <- remDr$getPageSource()[[1]]

  ## 페이지 소스 중 HTML 정보만 읽어오기
  page_html <- page_parse %>% read_html()

  ## 페이지 내 테이블 데이터 추출
  Sys.setlocale("LC_ALL", "English") # 로케일 언어를 English로 변경
  table <- page_html %>% html_table(fill = TRUE) # 테이블 데이터만 추출
  Sys.setlocale("LC_ALL", "Korean") # 다시 로케일 언어를 Korean으로 변경

  ## 테이블 데이터 중 총량측정망 자료가 포함된 테이블 만 추출(7번 테이블)
  ## 2007년에 최초 obs 변수 생성, 이후 연도 부터 아래에 데이터 병합
  if (i == 2007) {  # <--------------- 시작연도 변경 시 반드시 동일하게 변경
    obs0 <- table[[7]]
  } else {
    obs0 <- rbind(obs0, table[[7]])
  }
}

################################################################################

## 결측 행 제외
obs <- obs0 %>% filter(!is.na(번호))

## 변수명 변경(R 변수명 규칙에 맞게 조정)_data.table
obs <- obs %>% select(-번호)

names(obs) <- c(
  "총량지점명", "일자", "수온", "pH", "EC", "DO", "BOD", "COD",
  "SS", "TN", "TP", "TOC", "유량"
)

## 열 순서 변경
obs <- obs[c(1, 2, 7, 11, 13, 12, 3, 4, 5, 6, 8, 9, 10)]

## 날짜 형식 변경
obs$일자 <- gsub("\\.", "-", obs$일자)

## 문자열로 표현된 날짜를 Date 객체로 변환_lubridate
obs$일자 <- as.Date(obs$일자, "%Y-%m-%d")

## 연도 추가_dplyr
obs <- obs %>% mutate(연도 = year(obs$일자))

## 엑셀 파일 내보내기_writexl
write_xlsx(obs, path = "Data/총량측정망0721.xlsx")


