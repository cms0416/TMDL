library(RSelenium)
library(seleniumPipes)
library(httr)
library(rvest)
library(readr)


data <- POST("http://water.nier.go.kr/waterData/totalSearch.do",
  query = list(
    pType = "E",
    menuIdx = "3_1_6",
    s_date = "202101",
    e_date = "202104",
    searchOrder = "",
    docuType = "csv",
    stat_id = "1013A90,1016A45,1001A30,1019A60,1019A40,1004A45,1004A70,1023A50,1013A60,1015A10,1006A45,1006A80,1012A20,1012A40,1022A55,1022A35,1001A40,1003A15,1025A51,1011A20,1021A20,1023A22,1015A30,1018A30,1002A55,1003A17,1003A47,1003A75,1005A60,1007A60,1025A03,1018A52,1019A25,1022A07,1022A80,1014A70,1007A70",
    stat_nm = "",
    searchRGubun = "1",
    stat_cd = "1013A90",
    stat_nm = "가평A",
    stat_cd = "1016A45",
    stat_nm = "경안A",
    stat_cd = "1001A30",
    stat_nm = "골지A",
    stat_cd = "1019A60",
    stat_nm = "공릉A",
    stat_cd = "1019A40",
    stat_nm = "굴포A",
    stat_cd = "1004A45",
    stat_nm = "달천A",
    stat_cd = "1004A70",
    stat_nm = "달천B",
    stat_cd = "1023A50",
    stat_nm = "문산A",
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
    stat_cd = "1015A30",
    stat_nm = "조종A",
    stat_cd = "1018A30",
    stat_nm = "중랑A",
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

data_value <- read_html(data$url)
data_value <- html_text(data_value)
data_value <- read_csv(data_value)

download.file(data$url, "Data/test.csv")

# 4445번 포트와 크롬 연결
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "chrome"
)

# 크롬 실행
remDr$open()

# 사이트 접속(접속할 사이트 주소 입력)
remDr$navigate(data$url)
