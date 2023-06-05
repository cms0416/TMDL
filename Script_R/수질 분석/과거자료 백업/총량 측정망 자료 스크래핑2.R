library(tidyverse)
library(data.table)
library(RSelenium)
library(seleniumPipes)
library(rvest)
library(httr)
library(writexl)

## 최초년도(2005년) 자료 검색 후 url 확보_rvest
## (3년이상 설정 시 에러 발생 하여 1년 단위 추출 후 병합)
data <- POST("http://water.nier.go.kr/waterData/totalSearch.do",
  query = list(
    pType = "S",
    menuIdx = "3_1_6",
    s_date = "200501",
    e_date = "200512",
    searchOrder = "",
    docuType = "",
    stat_id = "1013A90,1016A45,1016A75,1001A30,1019A60,1019A40,2001A35,1004A45,
    1004A70,1023A50,1007A57,1009A10,1010A35,1013A60,1015A10,1006A45,1006A80,
    1012A20,1012A40,1022A55,1018A82,1007A44,1022A35,1001A40,1003A15,1025A51,
    1011A20,1021A20,1023A22,1003A71,1015A30,1002A20,1018A30,1007A15,1018A16,
    1002A55,1003A17,1003A47,1003A75,1005A60,1007A60,1025A03,1025A69,1018A52,
    1019A25,1022A07,1022A80,1014A70,1007A70",
    stat_nm = "가평A,경안A,경안B,골지A,공릉A,굴포A,낙본A,달천A,달천B,문산A,
    복하A,북한A,북한B,북한C,북한D,섬강A,섬강B,소양A,소양B,신천A,안양A,양화A,
    영평A,오대A,옥동A,왕숙A,인북A,임진A,임진B,제천A,조종A,주천A,중랑A,청미A,
    탄천A,평창A,한강A,한강B,한강C,한강D,한강E,한강F,한강G,한강H,한강I,한탄A,
    한탄B,홍천A,흑천A",
    searchRGubun = "1",
    stat_cd = "1013A90",
    stat_nm = "가평A",
    stat_cd = "1016A45",
    stat_nm = "경안A",
    stat_cd = "1016A75",
    stat_nm = "경안B",
    stat_cd = "1001A30",
    stat_nm = "골지A",
    stat_cd = "1019A60",
    stat_nm = "공릉A",
    stat_cd = "1019A40",
    stat_nm = "굴포A",
    stat_cd = "1004A45",
    stat_nm = "낙본A",
    stat_cd = "2001A35",
    stat_nm = "달천A",
    stat_cd = "1004A70",
    stat_nm = "달천B",
    stat_cd = "1023A50",
    stat_nm = "문산A",
    stat_cd = "1007A57",
    stat_nm = "복하A",
    stat_cd = "1009A10",
    stat_nm = "북한A",
    stat_cd = "1010A35",
    stat_nm = "북한B",
    stat_cd = "1013A60",
    stat_nm = "북한C",
    stat_cd = "1015A10",
    stat_nm = "북한D",
    stat_cd = "1006A45",
    stat_nm = "섬강A",
    stat_cd = "1006A80",
    stat_nm = "섬강B",
    stat_cd = "1012A20",
    stat_nm = "소양A",
    stat_cd = "1012A40",
    stat_nm = "소양B",
    stat_cd = "1022A55",
    stat_nm = "신천A",
    stat_cd = "1018A82",
    stat_nm = "안양A",
    stat_cd = "1007A44",
    stat_nm = "양화A",
    stat_cd = "1022A35",
    stat_nm = "영평A",
    stat_cd = "1001A40",
    stat_nm = "오대A",
    stat_cd = "1003A15",
    stat_nm = "옥동A",
    stat_cd = "1025A51",
    stat_nm = "왕숙A",
    stat_cd = "1011A20",
    stat_nm = "인북A",
    stat_cd = "1021A20",
    stat_nm = "임진A",
    stat_cd = "1023A22",
    stat_nm = "임진B",
    stat_cd = "1003A71",
    stat_nm = "제천A",
    stat_cd = "1015A30",
    stat_nm = "조종A",
    stat_cd = "1002A20",
    stat_nm = "주천A",
    stat_cd = "1018A30",
    stat_nm = "중랑A",
    stat_cd = "1007A15",
    stat_nm = "청미A",
    stat_cd = "1018A16",
    stat_nm = "탄천A",
    stat_cd = "1002A55",
    stat_nm = "평창A",
    stat_cd = "1003A17",
    stat_nm = "한강A",
    stat_cd = "1003A47",
    stat_nm = "한강B",
    stat_cd = "1003A75",
    stat_nm = "한강C",
    stat_cd = "1005A60",
    stat_nm = "한강D",
    stat_cd = "1007A60",
    stat_nm = "한강E",
    stat_cd = "1025A03",
    stat_nm = "한강F",
    stat_cd = "1025A69",
    stat_nm = "한강G",
    stat_cd = "1018A52",
    stat_nm = "한강H",
    stat_cd = "1019A25",
    stat_nm = "한강I",
    stat_cd = "1022A07",
    stat_nm = "한탄A",
    stat_cd = "1022A80",
    stat_nm = "한탄B",
    stat_cd = "1014A70",
    stat_nm = "홍천A",
    stat_cd = "1007A70",
    stat_nm = "흑천A"
  )
)

# url 변수 지정
url <- data$url

# 4445번 포트와 크롬 연결
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "chrome"
)

# 크롬 실행
remDr$open()

## 1년단위 데이터 추출 후 병합
for (i in 2005:2021) {
  # 년도 변경_gsub 함수
  url <- gsub(paste0(i - 1, "01"), paste0(i, "01"), url)
  url <- gsub(paste0(i - 1, "12"), paste0(i, "12"), url)

  # 사이트 접속(접속할 사이트 주소 입력)
  remDr$navigate(url)

  # 페이지 소스 읽어오기
  page_parse <- remDr$getPageSource()[[1]]

  # 페이지 소스 중 HTML 정보만 읽어오기
  page_html <- page_parse %>% read_html()

  # 페이지 내 테이블 데이터 추출
  Sys.setlocale("LC_ALL", "English")  # 로케일 언어를 English로 변경
  table <- page_html %>% html_table(fill = TRUE)  # 테이블 데이터만 추출
  Sys.setlocale("LC_ALL", "Korean")  # 다시 로케일 언어를 Korean으로 변경

  # 2005년에 최초 obs 변수 생성, 이후 년도 부터 아래에 데이터 병합
  if (i == 2005) {
    obs <- table[[5]]
  } else {
    obs <- bind_rows(obs, table[[5]])
  }
}

# 변수명 변경(R 변수명 규칙에 맞게 조정)_data.table
obs <- obs %>% select(-번호)
setnames(
  obs, c(
    "수온(℃)", "수소이온농도(ph)", "용존산소(㎎/L)", "BOD(㎎/L)", "COD(㎎/L)",
    "부유물질(㎎/L)", "총질소(T-N)(㎎/L)", "총인(T-P)(㎎/L)", 
    "총유기탄소(TOC)(㎎/L)", "유량(㎥/s)"
  ),
  c("수온", "pH", "DO", "BOD", "COD", "SS", "TN", "TP", "TOC", "유량")
)

# 열 순서 변경
obs <- obs[c(1, 2, 7, 11, 13, 12, 3, 4, 5, 6, 8, 9, 10)]

# 날짜 형식 변경
obs$일자 <- gsub("\\.", "-", obs$일자)

# 문자열로 표현된 날짜를 Date 객체로 변환_lubridate
obs$일자 <- as.Date(obs$일자, "%Y-%m-%d")

# 연도 추가_dplyr
obs <- obs %>% mutate(연도 = year(obs$일자))

# 엑셀 파일 내보내기_writexl
write_xlsx(obs, path = "Data/총량측정망0521.xlsx")
