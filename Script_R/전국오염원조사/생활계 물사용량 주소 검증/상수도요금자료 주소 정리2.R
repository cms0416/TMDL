#####  라이브러리 로드  ########################################################
library(tidyverse)
library(readxl)
library(writexl)
################################################################################


##########  도로명주소 전자지도 기준 주소 DB 구축  ###############################################################

### ----- 도로명 기준 자료 정리 ------------------------------------------------
## 도로명코드 불러오기
dorocode <- read.table("주소 검토/주소DB/도로명코드.txt", 
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
dongcode <- read_excel("주소 검토/법정동코드 전체자료.xlsx", guess_max = 3000) %>% 
  filter(시도 == "강원도", 폐지여부 == "존재") %>% 
  select(-폐지여부) %>% 
  rename(시군 = 시군구) %>% 
  mutate(
    읍면 = str_extract(읍면동, "[가-힣0-9]{1,}(읍|면)"),
    동리 = str_extract(읍면동, "[가-힣0-9]{1,}(동|가)"),
    동리 = ifelse(is.na(동리), 리, 동리)) %>% 
  filter(!is.na(동리))

## 도로명주소 전자지도 건물정보 DB 불러오기
juso_db_doro <- read.table("주소 검토/match_build_gangwon.txt",
                           header = F, quote = "", sep = "|", fill = T,
                           encoding = "UTF-8", fileEncoding = "EUC-KR"
)

# 도로명주소 전자지도 도로명매칭코드 생성
juso_code_doro <- juso_db_doro %>%
  select(1:9, 11, 12, 20, 24, 25) %>%
  set_names(c(
    "법정동코드", "시도", "시군구", "읍면동", "도로명코드",
    "도로명", "지하여부", "본번", "부번", "건물관리번호",
    "건물명", "상세건물명", "건물중심점_x좌표", "건물중심점_y좌표"
  )) %>%
  mutate(
    읍면 = str_extract(읍면동, "[가-힣0-9]{1,}(읍|면)"),
    동리 = str_extract(읍면동, "[가-힣0-9]{1,}(동|가)"),
    도로명매칭코드 =
      str_c(
        "D",
        도로명코드,
        # str_sub(법정동코드, 6, 8),
        지하여부,
        str_pad(str_replace_na(본번, "0"), width = 5, side = "left", pad = "0"),
        str_pad(str_replace_na(부번, "0"), width = 5, side = "left", pad = "0")
      ),
    도로명주소 =
      str_c(
        시도, " ", 시군구, " ",
        ifelse(is.na(읍면), "", str_c(읍면, " ")), 
        도로명, " ", 본번,
        ifelse(부번 == 0 | is.na(부번), "", str_c("-", 부번))
      )
  ) %>%
  # 도로명매칭코드가 여러개인 경우 하나만 남기고 정리
  distinct(도로명매칭코드, .keep_all = TRUE)


### ----- 지번 기준 자료 정리 --------------------------------------------------
## 도로명주소 전자지도 지번정보 DB 불러오기
juso_db_jibun <- read.table("주소 검토/match_jibun_gangwon.txt",
                            header = F, sep = "|",
                            encoding = "UTF-8", fileEncoding = "EUC-KR"
)

## 도로명주소 전자지도 지번주소 매칭코드 생성
juso_code <- juso_db_jibun %>%
  select(1:12) %>%
  set_names(c(
    "법정동코드", "시도", "시군구", "읍면동", "리", "산", "번지", "호",
    "도로명코드", "지하여부", "본번", "부번"
  )) %>%
  mutate(
    도로명매칭코드 =
      str_c(
        "D",
        도로명코드,
        # str_sub(법정동코드, 6, 8),
        지하여부,
        str_pad(str_replace_na(본번, "0"), width = 5, side = "left", pad = "0"),
        str_pad(str_replace_na(부번, "0"), width = 5, side = "left", pad = "0")
      ),
    지번매칭코드 =
      str_c(
        "J",
        법정동코드,
        산,
        str_pad(번지, width = 4, side = "left", pad = "0"),
        str_pad(str_replace_na(호, "0"), width = 4, side = "left", pad = "0")
      ),
    지번주소 =
      str_c(
        시도, " ", 시군구, " ", 읍면동,
        ifelse(is.na(리) | 리 == "", "", str_c(" ", 리)),
        ifelse(산 == 1, " 산 ", " "), 번지,
        ifelse(호 == 0 | is.na(호), "", str_c("-", 호))
      )
  )

## 중복자료 확인
juso_code2_duplicate_check <- juso_code %>% 
  group_by(지번매칭코드) %>% mutate(지번중복 = length(지번매칭코드)) %>% 
  group_by(도로명매칭코드) %>% mutate(도로명중복 = length(도로명매칭코드))

## 지번코드 중복 삭제
juso_code_jibun <- juso_code %>% 
  distinct(지번매칭코드, .keep_all = TRUE) %>% 
  left_join(juso_code_doro %>% 
              select(도로명매칭코드, 도로명주소, 건물중심점_x좌표, 건물중심점_y좌표), 
            by = "도로명매칭코드") %>% 
  select(-c(산:부번)) %>% 
  select(1:6, 9, everything())


## 주소코드 최종 정리(도로명 기준)
final_juso_code_doro <- juso_code %>% 
  # 지하인 경우 삭제
  filter(지하여부 != 1) %>% 
  # 도로명매칭코드 중복 삭제
  distinct(도로명매칭코드, .keep_all = TRUE) %>% 
  left_join(juso_code_doro %>% 
              select(도로명매칭코드, 도로명주소, 건물중심점_x좌표, 건물중심점_y좌표), 
            by = "도로명매칭코드") %>% 
  select(-c(산:부번)) %>% 
  select(1:6, 9, everything())



##########  상수도요금자료 주소 정리  ###############################################################

## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/생활계 물사용량 주소 검증",
  pattern = "*.xls", full.names = T
)

# 경로지정된 생활계 파일 합치기
ws_address <- files %>%
  # map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
  map_dfr(read_excel) 
 
## *****************************************************************************


ws_address1 <- ws_address %>%
  mutate(
    시도 = "강원도",
    # 주소 1차 수정 : 
    # 괄호, 쉼표, 문장부호로 시작하는 경우 삭제
    주소수정 = str_replace(주소, '(\\[|\\(|\\,|\\")[:print:]{2,}', "") %>%
      # '00번길' 또는 '00길'앞에 띄어쓰기가 되어 있는 경우 공백 제거 
      str_replace(., " 번길", "번길") %>% 
      str_replace(., "[\\s][0-9]{1,}(번길)", str_extract(주소, "[0-9]{1,}(번길)")) %>%
      str_replace(., "[\\s][0-9]{1,}(길)", str_extract(주소, "[0-9]{1,}(길)")) %>%
      # 리 앞에 숫자가 있는 경우('00리') 숫자 삭제 
      str_replace(., "[0-9]{1,}(리)", "리") %>% 
      str_trim(),
    도로명 = str_extract(주소수정, "[가-힣A-Za-z0-9]{2,}(로|길)"),
    건물본번 = str_split_i(주소수정, 도로명, 2) %>% 
      str_extract(., "[0-9]{1,}"), 
    건물부번 = str_split_i(주소수정, 도로명, 2) %>% 
      str_extract(., "[0-9\\-]{1,}") %>%
      str_extract(., "(?<=\\-)[0-9]{1,}"),
    읍면 = str_extract(주소수정, "[가-힣0-9]{1,}(읍|면)") %>% str_remove(., '[0-9]{1,}'),
    # 읍면 기준으로 주소 분리(읍면이 없는 경우 무시)
    동리 = ifelse(is.na(읍면), 주소수정, str_split_i(주소수정, 읍면, 2)),
    동리 = ifelse(str_sub(str_extract(동리, "[가-힣0-9]{1,}(동|가|리)"), -1, -1) == "가", 
             str_extract(동리, "[가-힣0-9]{1,}(동|가|리)"),
             str_extract(동리, "[가-힣0-9]{1,}(동|가|리)") %>% 
             str_remove_all(., "[0-9]{1,}")),
    읍면동 = ifelse(str_sub(str_extract(주소수정, "[가-힣0-9]{1,}(읍|면|동|가)"), -1, -1) == "가", 
                 str_extract(주소수정, "[가-힣0-9]{1,}(읍|면|동|가)"),
                 str_extract(주소수정, "[가-힣0-9]{1,}(읍|면|동|가)") %>% 
                   str_remove(., '[0-9]')),
    # 읍면동 기준으로 주소 분리(읍면동이 없는 경우 무시)
    리 = ifelse(is.na(읍면동), 주소수정, str_split_i(주소수정, 읍면동, 2)),
    리 = str_extract(리, "[가-힣0-9]{1,}(리)") %>% 
      str_remove_all(., "[0-9]{1,}"),
    산 = str_extract(주소수정, "(?<=동 |리 |가 )(산)"),
    본번 = str_extract(주소수정, "(?<=동 |동|리 |리|가 |가|산 |산)[0-9]{1,}(?!로|길)"),
    부번 = str_extract(주소수정, "(?<=동 |동|리 |리|가 |가|산 |산)[0-9\\-]{1,}") %>%
      str_extract(., "(?<=\\-)[0-9]{1,}"),
    # 주소 합치기
    도로명주소 =
      str_c(
        시도, " ", 시군, " ", 
        ifelse(is.na(읍면), "", str_c(읍면, " ")), 
        도로명, " ", 건물본번,
        ifelse(건물부번 == 0 | is.na(건물부번), "", str_c("-", 건물부번))
      ),
    지번주소 =
      str_c(
        시도, " ", 시군, " ", 읍면동, " ", 
        ifelse(is.na(리), "", str_c(리, " ")),
        ifelse(is.na(산), "", str_c(산, " ")), 
        본번,
        ifelse(부번 == 0 | is.na(부번), "", str_c("-", 부번))
      )
  ) %>% 
  # 도로명주소 기준 주소매칭코드 결합
  left_join(final_juso_code_doro %>% select(도로명주소, 도로명매칭코드, 법정동코드), by = "도로명주소") %>% 
  # 지번주소 기준 주소매칭코드 결합
  left_join(juso_code_jibun %>% select(지번주소, 지번매칭코드, 법정동코드), by = "지번주소") %>% 
  left_join(dongcode, by = c("시도", "시군", "읍면동", "리", "읍면", "동리")) %>%
  # 코드 정리
  mutate(법정동코드 = ifelse(is.na(법정동코드.x), 
                        ifelse(is.na(법정동코드.y), 법정동코드, 법정동코드.y),
                        법정동코드.x)) %>% 
  # 법정동리 추가
  left_join(dongcode %>% 
              select(-c("시도", "시군", "읍면동", "리", "읍면")) %>% 
              rename(법정동리 = 동리), 
            by = "법정동코드") %>% 
  mutate(주소확인 = ifelse(is.na(도로명매칭코드) & is.na(지번매칭코드), "확인 필요", "")) %>% 
  relocate(c(법정동코드, 법정동리), .after = 부번)


## 수용가번호, 주소 중복확인
ws_address2 <- ws_address1 %>% 
  mutate(코드 = str_c(주소, 수용가번호)) %>%
  group_by(코드) %>% 
  mutate(중복자료 = length(코드),
         중복자료 = ifelse(중복자료 > 1, "중복", "")) %>% 
  ungroup()


###  파일 내보내기  ############################################################
ws_address2 <- ws_address2 %>% 
  select(-c(시도, 주소수정, 읍면, 동리, 도로명매칭코드:법정동코드.y, 코드))


write_xlsx(ws_address2, path = "전국오염원조사/생활계 물사용량 주소 검증/Output/상수도요금자료 주소 검토.xlsx")


  
  
################################################################################  
################################################################################
################################################################################
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
      )) %>% 
  
###########################################  
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
         코드 = str_c(주소, 수용가번호), 
         # 주소 확인 가능 여부 체크
         주소확인 = ifelse(is.na(도로명매칭코드) & is.na(지번매칭코드), "확인 필요", "")) %>% 
  # 중복 자료 확인 및 제거
  distinct(코드, .keep_all = TRUE)


