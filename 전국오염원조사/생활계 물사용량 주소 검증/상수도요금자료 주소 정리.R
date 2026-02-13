#####  라이브러리 로드  ########################################################
library(tidyverse)
library(readxl)
library(writexl)
################################################################################


## 도로명코드 불러오기
dorocode <- read.table("Data/생활계 부하량 산정/주소DB/도로명코드.txt", 
                       header = F, quote = "", sep = "|", fill = T,
                       encoding = "UTF-8", fileEncoding = "EUC-KR") %>% 
  select(1, 2, 5) %>%
  set_names(c(
    "도로명코드", "도로명", "시도"
  )) %>% 
  filter(시도 == "강원도") %>% 
  select(-시도) %>% 
  distinct(도로명, .keep_all = TRUE)

## 법정동코드 불러오기
dongcode <- read_excel("Data/생활계 부하량 산정/법정동코드 전체자료.xlsx") %>% 
  filter(시도 == "강원도", 폐지여부 == "존재") %>% select(-폐지여부) %>% 
  rename(시군 = 시군구)

# 상수도요금자료 불러오기
ws_address <- read_excel("Data/생활계 부하량 산정/상하수도 요금 자료/상수도 요금자료 원본_강원도.xlsx")

ws_address1 <- ws_address %>%
  select(수용가번호, 시도, 시군, 주소) %>%
  mutate(
    # 괄호, 쉼표, 문장부호로 시작하는 경우 삭제
    # '00번길' 또는 '00길'앞에 띄어쓰기가 되어 있는 경우 공백 제거 
    주소수정 = str_replace(주소, '(\\(|\\,|\\")[:print:]{2,}', "") %>%
      str_replace(., " 번길", "번길") %>% 
      str_replace(., "[\\s][0-9]{1,}(번길)", str_extract(주소, "[0-9]{1,}(번길)")) %>%
      str_replace(., "[\\s][0-9]{1,}(길)", str_extract(주소, "[0-9]{1,}(길)")) %>%
      str_trim(),
    도로명 = str_extract(주소수정, "[가-힣A-Za-z0-9]{2,}(로|길)"),
    건물본번 = str_replace(주소수정, 도로명, "") %>% 
      str_extract(., "[0-9]{1,}"),
    건물부번 = str_replace(주소수정, 도로명, "") %>% 
      str_extract(., "[0-9\\-]{1,}") %>%
      str_extract(., "(?<=\\-)[0-9]{1,}"),
    읍면동 = ifelse(str_sub(str_extract(주소수정, "[가-힣A-Za-z0-9]{1,}(읍|면|동|가)"), -1, -1) == "가", 
             str_extract(주소수정, "[가-힣A-Za-z0-9]{1,}(읍|면|동|가)"),
             str_extract(주소수정, "[가-힣A-Za-z0-9]{1,}(읍|면|동|가)") %>% 
             str_remove(., '[0-9]')),
    리 = str_extract(주소수정, "[가-힣A-Za-z0-9]{1,}(리)") %>% str_remove(., '[0-9]'),
    산 = str_extract(주소수정, "(?<=동 |리 |가 )(산)"),
    본번 = str_extract(주소수정, "(?<=동 |동|리 |리|가 |가|산 |산)[0-9]{1,}"),
    부번 = str_extract(주소수정, "(?<=동 |동|리 |리|가 |가|산 |산)[0-9\\-]{1,}") %>%
      str_extract(., "(?<=\\-)[0-9]{1,}"),
    # 주소 합치기
    도로명주소 =
      str_c(
        시도, " ", 시군, " ", 도로명, " ", 건물본번,
        ifelse(건물부번 == 0 | is.na(건물부번), "", str_c("-", 건물부번))
      ),
    지번주소 =
      str_c(
        시도, " ", 시군, " ", 읍면동,
        ifelse(is.na(리), "", str_c(" ", 리)),
        ifelse(is.na(산), "", str_c(" ", 산)), 
        " ", 본번,
        ifelse(부번 == 0 | is.na(부번), "", str_c("-", 부번))
      )
  ) 
  

  # 도로명코드 추가
  left_join(dorocode, by = "도로명") %>%
  # 법정동코드 추가
  left_join(dongcode, by = c("시도", "시군", "읍면동", "리")) %>%
  # 매칭코드 추가
  mutate(
    도로명매칭코드 =
      str_c(
        "D",
        도로명코드,
        0,
        str_pad(str_replace_na(건물본번, "0"), width = 5, side = "left", pad = "0"),
        str_pad(str_replace_na(건물부번, "0"), width = 5, side = "left", pad = "0")
      ),
    지번매칭코드 =
      str_c(
        "J",
        법정동코드,
        str_replace_na(ifelse(산 == "산", "1", "0"), "0"),
        str_pad(본번, width = 4, side = "left", pad = "0"),
        str_pad(str_replace_na(부번, "0"), width = 4, side = "left", pad = "0")
      )) 
  
  
  # 도로명주소 기준 주소매칭코드 결합
  left_join(final_juso_code_doro %>% select(도로명주소, 도로명매칭코드, 법정동코드), by = "도로명주소") %>% 
  # 지번주소 기준 주소매칭코드 결합
  left_join(juso_code_jibun %>% select(지번주소, 지번매칭코드, 법정동코드), by = "지번주소") %>% 
  # 코드 정리
  mutate(법정동코드 = ifelse(is.na(법정동코드), 
                        ifelse(is.na(법정동코드.y), 법정동코드.x, 법정동코드.y), 
                        법정동코드),
         도로명매칭코드 = ifelse(is.na(도로명매칭코드.y), 도로명매칭코드.x, 도로명매칭코드.y),
         지번매칭코드 = ifelse(is.na(지번매칭코드.y), 지번매칭코드.x, 지번매칭코드.y), 
         # 주소 확인 가능 여부 체크
         주소확인 = ifelse(is.na(도로명매칭코드) & is.na(지번매칭코드), 1, ""), 
         # 중복 자료 확인 및 제거
         코드 = str_c(주소, 수용가번호)) %>%
  distinct(코드, .keep_all = TRUE)


## 수용가번호, 주소 중복확인
ws_address2 <- ws_address1 %>% 
  mutate(코드 = str_c(주소, 수용가번호)) %>%
  group_by(코드) %>% mutate(중복 = length(코드))
