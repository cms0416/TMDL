### 내비게이션용DB - https://business.juso.go.kr/addrlink/elctrnMapProvd/geoDBDwldList.do#this
### 주소 DB - https://business.juso.go.kr/addrlink/attrbDBDwld/attrbDBDwldList.do#this

#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)


##########  도로명주소 전자지도 기준 주소 DB 구축  ##################################

### 도로명코드 불러오기
도로명코드 <- read.table("주소 검토/주소DB/개선_도로명코드_전체분.txt",
  header = F, quote = "", sep = "|", fill = T,
  encoding = "UTF-8", fileEncoding = "EUC-KR"
) %>%
  select(1, 2, 5) %>%
  set_names(c(
    "도로명코드", "도로명", "시도"
  )) %>%
  filter(시도 == "강원도") %>%
  select(-시도) %>%
  distinct(도로명코드, .keep_all = TRUE)


### 도로명주소 전자지도 건물정보 DB 불러오기
도로명주소_db <- read.table("주소 검토/match_build_gangwon.txt",
  header = F, quote = "", sep = "|", fill = T,
  encoding = "UTF-8", fileEncoding = "EUC-KR"
)

### 도로명주소 전자지도 자료 정리
도로명주소_정리 <- 도로명주소_db %>%
  select(1:9, 11, 12, 20, 24, 25) %>%
  set_names(c(
    "법정동코드", "시도", "시군", "읍면동", "도로명코드",
    "도로명", "지하여부", "본번", "부번", "건물관리번호",
    "건물명", "상세건물명", "건물중심점_x좌표", "건물중심점_y좌표"
  )) %>%
  mutate(
    읍면 = str_extract(읍면동, "[가-힣0-9]{1,}(읍|면)"),
    동 = ifelse(is.na(읍면), 읍면동, NA),
    도로명주소 =
      str_c(
        시도, " ", 시군, " ",
        ifelse(is.na(읍면), "", str_c(읍면, " ")),
        도로명, " ",
        ifelse(지하여부 == 1, "지하 ", ""),
        본번,
        ifelse(부번 == 0 | is.na(부번), "", str_c("-", 부번))
      )
  )


### 도로명주소 전자지도 지번정보 DB 불러오기
지번주소_db <- read.table("주소 검토/match_jibun_gangwon.txt",
  header = F, sep = "|",
  encoding = "UTF-8", fileEncoding = "EUC-KR"
)

### 도로명주소 전자지도 지번주소 매칭코드 생성
지번주소_정리 <- 지번주소_db %>%
  select(1:13) %>%
  set_names(c(
    "법정동코드", "시도", "시군", "읍면동", "리", "산", "번지", "호",
    "도로명코드", "지하여부", "본번", "부번", "지번일련번호"
  )) %>%
  left_join(도로명코드, by = "도로명코드") %>%
  mutate(
    읍면 = str_extract(읍면동, "[가-힣0-9]{1,}(읍|면)"),
    동리 = ifelse(is.na(읍면), 읍면동, 리),
    도로명주소 =
      str_c(
        시도, " ", 시군, " ",
        ifelse(is.na(읍면), "", str_c(읍면, " ")),
        도로명, " ",
        ifelse(지하여부 == 1, "지하 ", ""),
        본번,
        ifelse(부번 == 0 | is.na(부번), "", str_c("-", 부번))
      ),
    지번주소 =
      str_c(
        시도, " ", 시군, " ", 읍면동,
        ifelse(is.na(리) | 리 == "", "", str_c(" ", 리)),
        ifelse(산 == 1, " 산 ", " "), 번지,
        ifelse(호 == 0 | is.na(호), "", str_c("-", 호))
      )
  )

도로명주소_대표지번 <- 지번주소_정리 %>%
  filter(지번일련번호 == 0) %>%
  select(도로명주소, 지번주소, 동리)

도로명주소 <- 도로명주소_정리 %>%
  select(도로명주소, 건물명) %>%
  distinct(도로명주소, .keep_all = TRUE) %>%
  left_join(도로명주소_대표지번, by = "도로명주소") %>%
  relocate(건물명, .after = last_col()) %>%
  mutate(도로명주소확인 = "O")

지번주소 <- 지번주소_정리 %>%
  select(지번주소, 도로명주소, 지번일련번호) %>%
  distinct(지번주소, .keep_all = TRUE) %>%
  mutate(지번주소확인 = "O")


### 법정동코드 불러오기
법정동코드 <- read_excel("주소 검토/법정동코드 전체자료.xlsx", guess_max = 3000) %>%
  filter(시도 == "강원도", 폐지여부 == "존재") %>%
  select(-폐지여부) %>%
  rename(시군 = 시군구) %>%
  mutate(
    읍면 = str_extract(읍면동, "[가-힣0-9]{1,}(읍|면)"),
    동리 = ifelse(is.na(읍면), 읍면동, 리)
  ) %>%
  filter(!is.na(동리))

### 읍면, 동리, 도로명 확인용 벡터 생성(제일 앞에 여백 추가)
읍면_check <- paste(" ", 법정동코드$읍면, collapse = " ")
동리_check <- paste(" ", 법정동코드$동리, collapse = " ")
도로명_check <- paste(" ", 도로명코드$도로명, collapse = " ")
