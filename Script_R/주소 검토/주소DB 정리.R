### 주소 DB 출처 - https://business.juso.go.kr/addrlink/attrbDBDwld/attrbDBDwldList.do#this

#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)

#####  주소 DB 불러오기  #######################################################
주소DB <- read.table("주소 검토/주소DB/주소_강원도.txt",
  header = F, quote = "", sep = "|", fill = T,
  encoding = "UTF-8", fileEncoding = "EUC-KR"
) %>%
  set_names(c(
    "관리번호", "도로명코드", "읍면동일련번호", "지하여부",
    "건물본번", "건물부번", "기초구역번호", "변경사유코드",
    "고시일자", "변경전도로명주소", "상세주소부여 여부"
  ))

지번DB <- read.table("주소 검토/주소DB/지번_강원도.txt",
  header = F, quote = "", sep = "|", fill = T,
  encoding = "UTF-8", fileEncoding = "EUC-KR"
) %>%
  set_names(c(
    "관리번호", "일련번호", "법정동코드", "시도", "시군", "읍면동", "리",
    "산", "본번", "부번", "대표여부"
  ))

도로명코드2 <- read.table("주소 검토/주소DB/개선_도로명코드_전체분.txt",
  header = F, quote = "", sep = "|", fill = T,
  encoding = "UTF-8", fileEncoding = "EUC-KR"
) %>%
  select(1, 2, 4, 5, 7, 9) %>%
  set_names(c(
    "도로명코드", "도로명", "읍면동일련번호", "시도", "시군", "읍면동"
  )) %>%
  filter(시도 == "강원도") %>%
  select(-시도)

부가정보 <- read.table("주소 검토/주소DB/부가정보_강원도.txt",
                 header = F, quote = "", sep = "|", fill = T,
                 encoding = "UTF-8", fileEncoding = "EUC-KR"
)

#####  주소 정리  ##############################################################
주소DB_도로명 <- 주소DB %>%
  left_join(도로명코드2, by = c("도로명코드", "읍면동일련번호")) %>%
  select(관리번호, 시군, 읍면동, 도로명, 건물본번, 건물부번) %>%
  mutate(
    읍면 = str_extract(읍면동, "[가-힣0-9]{1,}(읍|면)"),
    동 = ifelse(is.na(읍면), 읍면동, NA),
    .after = 읍면동,
  ) %>%
  mutate(
    도로명주소 =
      str_c(
        "강원도 ", 시군, " ",
        ifelse(is.na(읍면), "", str_c(읍면, " ")),
        도로명, " ", 건물본번,
        ifelse(건물부번 == 0 | is.na(건물부번), "", str_c("-", 건물부번))
      )
  )

주소DB_지번 <- 지번DB %>%
  mutate(
    지번주소 =
      str_c(
        시도, " ", 시군, " ", 읍면동, " ",
        ifelse(리 == "", 리, str_c(리, " ")),
        ifelse(산 == 1, "산 ", ""),
        본번,
        ifelse(부번 == 0, "", str_c("-", 부번))
      )
  )


#####  도로명주소 및 지번주소 매칭  ############################################
주소DB_도로명1 <- 주소DB_도로명 %>% 
  left_join(지번주소 %>% select(관리번호, 지번주소), by = "관리번호")


