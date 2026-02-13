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

################################################################################


## 수질오염총량관리시스템 접속
remDr$navigate("https://tmdl.nier.go.kr/")

## ID 입력
remDr$findElement(using = "id", value = "userId")$setElementAttribute("value", "gwondo")

## 비밀번호 입력
remDr$findElement(using = "id", value = "pw")$setElementAttribute("value", "gw33")

## 로그인 버튼 클릭
remDr$findElement(using = "xpath", value = "/html/body/form[1]/div/div/div/div/div[3]/div")$clickElement()

Sys.sleep(1)

## 개발부하량 조회 클릭
remDr$findElement(using = "xpath", value = '//*[@id="acdn01"]/ul/li[2]/a/div[1]')$clickElement()


################################################################################

## 시군 및 단위유역 분류 지정
station <- c(
  "춘천시북한B", "춘천시소양B", "춘천시북한C", "춘천시홍천A", "춘천시북한D",
  "원주시주천A", "원주시섬강A", "원주시섬강B", "원주시제천A", "원주시한강D",
  "강릉시골지A", "태백시골지A", "태백시옥동A", "삼척시골지A", "삼척시한강A",
  "홍천군섬강A", "홍천군소양A", "홍천군홍천A", "횡성군주천A", "횡성군섬강A",
  "횡성군섬강B", "영월군주천A", "영월군평창A", "영월군옥동A", "영월군한강A",
  "영월군한강B", "평창군골지A", "평창군오대A", "평창군평창A", "평창군한강A",
  "정선군골지A", "정선군오대A", "정선군한강A", "철원군북한A", "철원군한탄A",
  "철원군임진A", "철원군한탄B", "화천군북한A", "화천군북한B", "양구군북한A",
  "양구군인북A", "양구군소양B", "인제군소양A", "인제군인북A", "인제군소양B",
  "고성군인북A"
)

## 항목별 데이터프레임 생성
bod <- data.frame()
tp <- data.frame()

################################################################################

## 시군 및 단위유역별 지역개발부하량 자료 조회 및 추출
for (i in station) {
  ## 시군 선택
  remDr$findElement(using = "xpath", value = paste0('//*[@id="search_sigungu_cd"]/option[text() = "', substr(i, 1, 3), '"]'))$clickElement()
  Sys.sleep(1)

  ## 단위유역 선택
  remDr$findElement(using = "xpath", value = paste0('//*[@id="search_unitdgr"]/option[text() = "', substr(i, 4, 6), '"]'))$clickElement()
  Sys.sleep(1)

  ## 검색하기 버튼 클릭
  remDr$findElement(using = "xpath", value = '//*[@id="btn_tabChange"]')$clickElement()
  Sys.sleep(2)

  ## 페이지 소스 읽어오기
  page_parse <- remDr$getPageSource()[[1]]

  ## 페이지 소스 중 HTML 정보만 읽어오기
  page_html <- page_parse %>% read_html()

  ## 페이지 내 테이블 데이터 추출
  Sys.setlocale("LC_ALL", "English") # 로케일 언어를 English로 변경
  table <- page_html %>% html_table(fill = TRUE) # 테이블 데이터만 추출
  Sys.setlocale("LC_ALL", "Korean") # 다시 로케일 언어를 Korean으로 변경

  ## 테이블 데이터 중 측정지점 목록이 포함된 테이블 만 추출(4번 테이블)
  bod_table <- table[[3]]
  tp_table <- table[[4]]

  ## 테이블 열 이름 변경
  names(bod_table) <- c("연도", 2:7, "점", "비점", 8:10)
  names(tp_table) <- c("연도", 2:7, "점", "비점", 8:10)

  ## 테이블 내에서 누적부하량 자료만 추출 및 시군, 단위유역 추가
  bod_table <- bod_table %>%
    filter(연도 %in% c(2021:2030)) %>%
    mutate(시군 = substr(i, 1, 3), 단위유역 = substr(i, 4, 6)) %>%
    select("시군", "단위유역", "연도", "점", "비점") %>%
    mutate(점 = gsub("0.00", "", gsub("[()]", "", 점)), 비점 = gsub("0.00", "", gsub("[()]", "", 비점))) # 점, 비점 전환량 삭제(0.00)

  tp_table <- tp_table %>%
    filter(연도 %in% c(2021:2030)) %>%
    mutate(시군 = substr(i, 1, 3), 단위유역 = substr(i, 4, 6)) %>%
    select("시군", "단위유역", "연도", "점", "비점") %>%
    mutate(점 = gsub("0.000", "", gsub("[()]", "", 점)), 비점 = gsub("0.000", "", gsub("[()]", "", 비점))) # 점, 비점 전환량 삭제(0.00)

  ## 연도 및 데이터 숫자로 전환
  bod_table[, c("연도", "점", "비점")] <- map(bod_table[, c("연도", "점", "비점")], as.numeric)
  tp_table[, c("연도", "점", "비점")] <- map(tp_table[, c("연도", "점", "비점")], as.numeric)

  bod <- bod %>% rbind(bod_table)
  tp <- tp %>% rbind(tp_table)
  Sys.sleep(0.5)
}

## NA를 0으로 수정
bod[is.na(bod)] <- 0
tp[is.na(tp)] <- 0

################################################################################

## BOD 데이터 wide format 으로 변경
bod1 <- bod %>%
  pivot_longer(cols = c(점, 비점), names_to = "점비점") %>% 
  pivot_wider(names_from = 연도, values_from = value) %>% 
  setnames(c("2021", "2022", "2023", "2024", "2025", "2026", "2027", "2028", "2029", "2030"), 
           c("x2021", "x2022", "x2023", "x2024", "x2025", "x2026", "x2027", "x2028", "x2029", "x2030"))

## BOD 시군별 소계 계산
bod_subtotal <- bod1 %>%
  group_by(시군, 점비점) %>%
  summarise_at(vars(c("x2021":"x2030")), funs(sum)) %>%
  mutate(단위유역 = "소계") %>%
  select("시군", "단위유역", everything())

## BOD 전체 데이터와 소계 합친 후 시군 및 단위유역 순서로 정리
bod1 <- bod1 %>%
  rbind(bod_subtotal) %>%
  mutate_at(vars(시군, 단위유역, 점비점), as.factor) %>%
  mutate(시군 = factor(시군, levels = c(
    "춘천시", "원주시", "강릉시", "태백시",
    "삼척시", "홍천군", "횡성군", "영월군", "평창군", "정선군",
    "철원군", "화천군", "양구군", "인제군", "고성군"
  ))) %>%
  mutate(단위유역 = factor(단위유역, levels = c(
    "소계", "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A", 
    "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C", "홍천A", 
    "한탄A", "한강B", "제천A", "한강D", "북한D", "임진A", "한탄B"
  ))) %>% 
  mutate(점비점 = factor(점비점, levels = c(
    "점", "비점"
  ))) %>% 
  arrange(시군, 단위유역, 점비점)

## 누적부하량 외에 연별 부하량 추가(x : 연차별 누적 소진량, y : 연차별 소진량)
bod1 %<>% mutate(y2021 = x2021, 
                 y2022 = x2022 - x2021, y2023 = x2023 - x2022, 
                 y2024 = x2024 - x2023, y2025 = x2025 - x2024, 
                 y2026 = x2026 - x2025, y2027 = x2027 - x2026, 
                 y2028 = x2028 - x2027, y2029 = x2029 - x2028, 
                 y2030 = x2030 - x2029) %>% 
  select(시군, 단위유역, 점비점, c(y2021:y2030), c(x2021:x2030))

#------------------------------------------------------------------------------#
## TP 데이터 wide format 으로 변경
tp1 <- tp %>%
  pivot_longer(cols = c(점, 비점), names_to = "점비점") %>% 
  pivot_wider(names_from = 연도, values_from = value) %>% 
  setnames(c("2021", "2022", "2023", "2024", "2025", "2026", "2027", "2028", "2029", "2030"), 
           c("x2021", "x2022", "x2023", "x2024", "x2025", "x2026", "x2027", "x2028", "x2029", "x2030"))

## TP 시군별 소계 계산
tp_subtotal <- tp1 %>%
  group_by(시군, 점비점) %>%
  summarise_at(vars(c("x2021":"x2030")), funs(sum)) %>%
  mutate(단위유역 = "소계") %>%
  select("시군", "단위유역", everything())

## TP 전체 데이터와 소계 합친 후 시군 및 단위유역 순서로 정리
tp1 <- tp1 %>%
  rbind(tp_subtotal) %>%
  mutate_at(vars(시군, 단위유역, 점비점), as.factor) %>%
  mutate(시군 = factor(시군, levels = c(
    "춘천시", "원주시", "강릉시", "태백시",
    "삼척시", "홍천군", "횡성군", "영월군", "평창군", "정선군",
    "철원군", "화천군", "양구군", "인제군", "고성군"
  ))) %>%
  mutate(단위유역 = factor(단위유역, levels = c(
    "소계", "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A", 
    "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C", "홍천A", 
    "한탄A", "한강B", "제천A", "한강D", "북한D", "임진A", "한탄B"
  ))) %>% 
  mutate(점비점 = factor(점비점, levels = c(
    "점", "비점"
  ))) %>% 
  arrange(시군, 단위유역, 점비점)

## 누적부하량 외에 연별 부하량 추가
tp1 %<>% mutate(y2021 = x2021, 
                y2022 = x2022 - x2021, y2023 = x2023 - x2022, 
                y2024 = x2024 - x2023, y2025 = x2025 - x2024, 
                y2026 = x2026 - x2025, y2027 = x2027 - x2026, 
                y2028 = x2028 - x2027, y2029 = x2029 - x2028, 
                y2030 = x2030 - x2029) %>% 
  select(시군, 단위유역, 점비점, c(y2021:y2030), c(x2021:x2030))



################################################################################
###  엑셀 파일 내보내기_writexl
write_xlsx(list("BOD" = bod1, "T-P" = tp1), path = "Output/Data/지역개발부하량소진현황.xlsx")



