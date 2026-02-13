#####  라이브러리 로드  ########################################################
library(tidyverse)
library(data.table)
library(readxl)
library(writexl)
library(lubridate)

## 반올림 사용자 정의 함수 로드
source("Script/Function/round2func.R")
################################################################################



##**************************************************************************** ##
###################################  생활계  ###################################
##**************************************************************************** ##



##########  도로명주소 전자지도 기준 주소 DB 구축  ###############################################################

### ----- 도로명 기준 자료 정리 ------------------------------------------------
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


## 도로명주소 전자지도 건물정보 DB 불러오기
juso_db_doro <- read.table("Data/생활계 부하량 산정/match_build_gangwon.txt",
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
        시도, " ", 시군구, " ", 도로명, " ", 본번,
        ifelse(부번 == 0 | is.na(부번), "", str_c("-", 부번))
      )
  ) %>%
  # 도로명매칭코드가 여러개인 경우 하나만 남기고 정리
  distinct(도로명매칭코드, .keep_all = TRUE)


### ----- 지번 기준 자료 정리 --------------------------------------------------
## 도로명주소 전자지도 지번정보 DB 불러오기
juso_db_jibun <- read.table("Data/생활계 부하량 산정/match_jibun_gangwon.txt",
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
  distinct(도로명매칭코드, .keep_all = TRUE) %>% 
  left_join(juso_code_doro %>% 
            select(도로명매칭코드, 도로명주소, 건물중심점_x좌표, 건물중심점_y좌표), 
            by = "도로명매칭코드") %>% 
  select(-c(산:부번)) %>% 
  select(1:6, 9, everything())



##########  행안부 인구 자료 정리  ###############################################################################

### ----- 행안부 인구 도로명 자료 정리 -----------------------------------------
## 행안부 인구 도로명자료 불러오기
population_doro <- read.csv("Data/생활계 부하량 산정/me_drnmcd_42.csv",
  header = F, sep = "|", encoding = "UTF-8", fileEncoding = "EUC-KR"
) %>% set_names(c(
  "도로명코드", "법정동코드", "지하여부", "본번", "부번",
  "동", "층", "호", "세대수", "인구수"
))

# 도로명주소 매칭코드 생성 및 도로명주소 기준 세대수, 인구 합계 산정
population_doro1 <- population_doro %>%
  mutate(
    도로명매칭코드 =
      str_c(
        "D",
        도로명코드,
        # str_sub(법정동코드, 6, 8),
        지하여부,
        str_pad(str_replace_na(본번, "0"), width = 5, side = "left", pad = "0"),
        str_pad(str_replace_na(부번, "0"), width = 5, side = "left", pad = "0")
      )
  ) %>%
  group_by(도로명매칭코드) %>%
  summarise_at(vars(세대수, 인구수), ~ sum(.))

# 도로명주소 전자지도와 행안부 도로명주소자료 코드 매칭
population_doro2 <- population_doro1 %>%
  left_join(juso_code_doro %>% select(도로명매칭코드, 도로명주소, 지번매칭코드, 지번주소), by = "도로명매칭코드") %>%
  # 도로명매칭코드 중복자료 제거
  distinct(도로명매칭코드, .keep_all = TRUE) %>% 
  select(도로명매칭코드, 도로명주소, 지번매칭코드, 지번주소, 세대수, 인구수) %>% 
  mutate(기준자료 = "도로명")


### ----- 행안부 인구 지번 자료 정리 -------------------------------------------
## 행안부 인구 지번자료 불러오기
population_jibun <- read.csv("Data/생활계 부하량 산정/me_pnu_42.csv",
  header = F, sep = "|"
) %>%
  select(-1, -6, -7) %>%
  set_names(c(
    "법정동코드", "산", "번지", "호", "세대수", "인구수"
  ))

## 지번주소 매칭코드 생성 및 지번주소 기준 세대수, 인구 합계 산정
population_jibun1 <- population_jibun %>%
  mutate(
    지번매칭코드 =
      str_c(
        "J",
        법정동코드,
        str_replace_na(ifelse(산 == "0", "1", "0"), "0"),
        str_pad(str_replace_na(번지, "0"), width = 4, side = "left", pad = "0"),
        str_pad(str_replace_na(호, "0"), width = 4, side = "left", pad = "0")
      )
  ) %>%
  group_by(지번매칭코드) %>%
  summarise_at(vars(세대수, 인구수), ~ sum(.))

## 도로명주소 전자지도와 행안부 지번자료 코드 매칭
population_jibun2 <- population_jibun1 %>%
  left_join(juso_code_jibun %>% select(도로명매칭코드, 도로명주소, 지번매칭코드, 지번주소), by = "지번매칭코드") %>%
  # 지번매칭코드 중복자료 제거
  distinct(지번매칭코드, .keep_all = TRUE) %>% 
  select(도로명매칭코드, 도로명주소, 지번매칭코드, 지번주소, 세대수, 인구수) %>% 
  mutate(기준자료 = "지번")


### ----- 행안부 인구 도로명 및 지번 자료 합치기 -------------------------------
population <- rbind(population_doro2, population_jibun2)

## 중복확인
population_check <- population %>%
  group_by(지번매칭코드) %>% mutate(지번중복 = length(지번매칭코드)) %>% 
  group_by(도로명매칭코드) %>% mutate(도로명중복 = length(도로명매칭코드))

## 중복자료 합치기
final_population <- population %>%
  group_by(도로명매칭코드, 지번매칭코드) %>% 
  summarise_at(vars(세대수, 인구수), ~ sum(.))

## 인구수 총계 확인
sum(population_doro2$인구수)
sum(population_jibun2$인구수)
sum(population_doro2$인구수) + sum(population_jibun2$인구수)
sum(final_population$인구수)

## 세대수 총계 확인
sum(population_doro2$세대수)
sum(population_jibun2$세대수)
sum(population_doro2$세대수) + sum(population_jibun2$세대수)
sum(final_population$세대수)
  
##########  건축물대장 정리  #####################################################################################

### ----- 건축물대장으로 개인하수처리시설 정리 ---------------------------------
## 건축물대장 오수정화시설 자료 불러오기
building_register_o <- read.table("Data/생활계 부하량 산정/mart_djy_07.txt",
  header = F, quote = "", sep = "|", fill = T,
  encoding = "UTF-8", fileEncoding = "EUC-KR"
) %>%
  select(1, 6:13, 17:22, 27, 28) %>%
  # 강원도 자료만 필터(시도코드 42)
  filter(word(V6, 1) == "강원도")

names(building_register_o) <- c(
  "관리_건축물대장_PK", "대지_위치", "도로명_대지_위치", "건물명", "시군구_코드",
  "법정동_코드", "대지_구분_코드", "번", "지", "새주소_도로_코드",
  "새주소_법정동_코드", "새주소_지상지하_코드", "새주소_본_번", "새주소_부_번",
  "형식_코드", "용량_인용", "용량_루베"
)

## 개인하수처리시설 관련 자료 정리
individual_STP <- building_register_o %>%
  mutate(
    지번매칭코드 =
      str_c(
        "J",
        시군구_코드,
        법정동_코드,
        대지_구분_코드,
        str_pad(str_replace_na(번, "0"), width = 4, side = "left", pad = "0"),
        str_pad(str_replace_na(지, "0"), width = 4, side = "left", pad = "0")
      ),
    도로명매칭코드 =
      str_c(
        "D",
        새주소_도로_코드,
        # str_sub(법정동_코드, 1, 3),
        새주소_지상지하_코드,
        str_pad(str_replace_na(새주소_본_번, "0"), width = 5, side = "left", pad = "0"),
        str_pad(str_replace_na(새주소_부_번, "0"), width = 5, side = "left", pad = "0")
      ),
    지번주소 = str_remove(대지_위치, "번지"),
    도로명주소 = str_trim(도로명_대지_위치),
    처리형태 = case_when(
      str_sub(형식_코드, 1, 1) == 1 ~ "오수",
      str_sub(형식_코드, 1, 1) == 2 ~ "정화",
      str_sub(형식_코드, 1, 1) == 3 ~ "공공",
      str_sub(형식_코드, 1, 1) == 4 ~ "마을",
      str_sub(형식_코드, 1, 1) == 5 | str_sub(형식_코드, 1, 1) == 6 |
        str_sub(형식_코드, 1, 1) == 7 | str_sub(형식_코드, 1, 1) == 8 ~ "공란",
      str_sub(형식_코드, 1, 1) == 9 ~ "수거",
      TRUE ~ "공란"
    ),
    용량_인원 = case_when(
      용량_인용 == 0 & 용량_루베 > 1 ~ ceiling((용량_루베 - 1) / 0.1),
      용량_인용 == 0 & 용량_루베 <= 1 ~ 0,
      TRUE ~ 용량_인용
    ),
    용량_m3 = case_when(
      용량_루베 != 0 ~ 용량_루베,
      용량_루베 == 0 & 용량_인용 == 0 ~ 0,
      TRUE ~ 용량_인원 * 0.1 + 1
    )
  ) %>%
  select(지번매칭코드:용량_m3)

## 도로명주소 전자지도 지번코드와 매칭해서 도로명코드 확인
individual_STP1 <- individual_STP %>%
  left_join(juso_code_doro %>% select(도로명주소, 도로명매칭코드), by = "도로명주소") %>%
  left_join(juso_code_jibun %>% select(지번주소, 지번매칭코드), by = "지번주소")
  mutate(
    도로명매칭코드.x = ifelse(is.na(도로명매칭코드.x), 도로명매칭코드.y, 도로명매칭코드.x),
    주소코드확인 = ifelse(is.na(도로명매칭코드.x) & is.na(지번매칭코드), 1, "")
  ) %>% 
  rename(도로명매칭코드 = 도로명매칭코드.x) %>% 
  select(-도로명매칭코드.y)

## 처리형태 정리
# 처리형태코드 파일 불러오기
treat_code <- read_excel("Data/생활계 부하량 산정/처리형태별코드.xlsx")

# 처리형태에 따라 처리코드 정리
treat_code1 <- individual_STP1 %>%
  select(지번매칭코드, 도로명매칭코드, 처리형태) %>%
  mutate(개수 = 1) %>%
  pivot_wider(names_from = 처리형태, values_from = 개수, values_fn = sum) %>%
  mutate(
    공공a = ifelse(is.na(공공), "", "공공"),
    마을a = ifelse(is.na(마을), "", "마을"),
    오수a = ifelse(is.na(오수), "", "오수"),
    정화a = ifelse(is.na(정화), "", "정화"),
    수거a = ifelse(is.na(수거), "", "수거"),
    공란a = ifelse(is.na(공란), "", "공란")
  ) %>%
  mutate(처리형태 = str_c(공공a, 마을a, 오수a, 정화a, 수거a, 공란a)) %>%
  left_join(treat_code, by = "처리형태")

treat_doro <- individual_STP1 %>%
  select(도로명매칭코드, 처리형태) %>%
  filter(!is.na(도로명매칭코드)) %>%
  mutate(개수 = 1) %>%
  pivot_wider(names_from = 처리형태, values_from = 개수, values_fn = sum) %>%
  mutate(
    공공a = ifelse(is.na(공공), "", "공공"),
    마을a = ifelse(is.na(마을), "", "마을"),
    오수a = ifelse(is.na(오수), "", "오수"),
    정화a = ifelse(is.na(정화), "", "정화"),
    수거a = ifelse(is.na(수거), "", "수거"),
    공란a = ifelse(is.na(공란), "", "공란")
  ) %>%
  mutate(처리형태 = str_c(공공a, 마을a, 오수a, 정화a, 수거a, 공란a)) %>%
  left_join(treat_code, by = "처리형태")

treat_jibun <- individual_STP1 %>%
  select(지번매칭코드, 처리형태) %>%
  filter(!is.na(지번매칭코드)) %>%
  mutate(개수 = 1) %>%
  pivot_wider(names_from = 처리형태, values_from = 개수, values_fn = sum) %>%
  mutate(
    공공a = ifelse(is.na(공공), "", "공공"),
    마을a = ifelse(is.na(마을), "", "마을"),
    오수a = ifelse(is.na(오수), "", "오수"),
    정화a = ifelse(is.na(정화), "", "정화"),
    수거a = ifelse(is.na(수거), "", "수거"),
    공란a = ifelse(is.na(공란), "", "공란")
  ) %>%
  mutate(처리형태 = str_c(공공a, 마을a, 오수a, 정화a, 수거a, 공란a)) %>%
  left_join(treat_code, by = "처리형태")


## 도로명 코드 기준 건축물대장 정리
individual_STP_doro <- individual_STP1 %>%
  group_by(도로명매칭코드, 도로명주소) %>%
  summarise_at(vars(용량_인원, 용량_m3), ~ max(.)) %>%
  left_join(treat_doro %>% select(도로명매칭코드, 처리방법코드), by = "도로명매칭코드")

## 지번주소 코드 기준 건축물대장 정리
individual_STP_jibun <- individual_STP1 %>%
  group_by(지번매칭코드, 지번주소) %>%
  summarise_at(vars(용량_인원, 용량_m3), ~ max(.)) %>%
  left_join(treat_jibun %>% select(지번매칭코드, 처리방법코드), by = "지번매칭코드")


### ----- 건축물대장으로 건축물 용도 정리 --------------------------------------
## 건축물대장 총괄표제부 자료 불러오기
building_register_cp <- read.table("Data/생활계 부하량 산정/mart_djy_02_강원.txt",
  header = F, quote = "", sep = "|", fill = T,
  encoding = "UTF-8", fileEncoding = "EUC-KR"
) %>%
  select(1, 8:15, 20:24, 31:35) %>%
  # 강원도 자료만 필터(시도코드 42)
  filter(word(V8, 1) == "강원도")

names(building_register_cp) <- c(
  "관리_건축물대장_PK", "대지_위치", "도로명_대지_위치", "건물명", "시군구_코드",
  "법정동_코드", "대지_구분_코드", "번", "지", "새주소_도로_코드",
  "새주소_법정동_코드", "새주소_지상지하_코드", "새주소_본_번", "새주소_부_번",
  "주_용도_코드", "주_용도_코드_명", "기타_용도", "세대_수(세대)", "가구_수(가구)"
)


## 건축물대장 표제부 자료 불러오기
building_register_p <- read.table("Data/생활계 부하량 산정/mart_djy_03_강원.txt",
  header = F, quote = "", sep = "|", fill = T,
  encoding = "UTF-8", fileEncoding = "EUC-KR"
) %>%
  select(1, 6:13, 18:23, 35:37, 41, 42) %>%
  # 강원도 자료만 필터(시도코드 42)
  filter(word(V6, 1) == "강원도")

names(building_register_p) <- c(
  "관리_건축물대장_PK", "대지_위치", "도로명_대지_위치", "건물명", "시군구_코드",
  "법정동_코드", "대지_구분_코드", "번", "지", "새주소_도로_코드",
  "새주소_법정동_코드", "새주소_지상지하_코드", "새주소_본_번", "새주소_부_번", "동명",
  "주_용도_코드", "주_용도_코드_명", "기타_용도", "세대_수(세대)", "가구_수(가구)"
)


##########  상하수도요금 자료 정리  ##############################################################################

## 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "Data/생활계 부하량 산정/생활계 물사용량 기초자료",
  pattern = "*.xlsx", full.names = T
)

## 상수도요금 자료 불러오기
## guess_max = Inf : 자료 형태를 인식하는 범위를 무한대로 설정
## guess_max 설정하지 않았을 때 결측값이 많은 경우 전체열을 결측값으로 판단
watersupply <- files %>%
  map_dfr(read_excel, skip = 6, col_names = F, guess_max = Inf)


## 상수도요금 자료 주소 정리 ---------------------------------------------------
watersupply_a <- watersupply %>%
  select(2:15) %>% # 필요없는 열 삭제
  set_names(c(
    "법정동코드", "수용가번호", "지번_시도", "지번_시군구", "지번_읍면동",
    "리", "본번", "부번", "도로명_시도", "도로명_시군구", "도로명_읍면동",
    "도로명", "건물본번", "건물부번"
  )) %>%
  mutate_all(~ replace(., is.na(.), "")) %>%
  mutate(
    # 형식에 맞지 않는 주소 정리(괄호로 작성된 부분 삭제)
    # 본번a = str_sub(본번, 1, str_locate(본번, '\\(')[,1] - 1) %>% str_trim(),
    # 부번a = str_sub(부번, 1, str_locate(부번, '\\(')[,1] - 1) %>% str_trim(),
    산 = ifelse(str_replace(본번, "(\\()[:graph:]{1,}", "") %>%
      str_replace(., "[:graph:]{1,}(리)", "") %>%
      str_trim(.) %>%
      str_sub(., 1, 1) == "산", "산 ", ""),
    본번a = str_replace(본번, "(\\()[:graph:]{1,}", "") %>%
      str_replace(., "[:graph:]{1,}(리)", "") %>%
      str_extract(., "[0-9]{1,}"),
    부번a = str_extract(본번, "(?<=\\-)[0-9]{1,}"),
    부번b = ifelse(str_sub(부번, 1, 1) == 0, "",
      str_replace(부번, "(\\()[:graph:]{1,}", "") %>%
        str_extract(., "[0-9]{1,}")
    ),
    부번c = ifelse(부번b == "" | is.na(부번b),
      ifelse(str_sub(부번a, 1, 1) == 0,
        str_replace(부번a, "0", ""), 부번a
      ),
      부번b
    ),
    도로명a = str_replace(도로명, "(\\()[:graph:]{1,}", "") %>%
      str_replace(., "(\\,)[:graph:]{1,}", "") %>%
      str_extract(., "[가-힣A-Za-z0-9]{2,}(로|길)"),
    건물본번a = ifelse(str_sub(건물본번, 1, 1) == 0, "",
      str_replace(건물본번, "(\\()[:graph:]{1,}", "") %>%
        str_extract(., "[0-9]{1,}")
    ),
    건물본번b = str_replace(도로명, "[가-힣A-Za-z0-9]{2,}(로|길)", "") %>%
      str_extract(., "[0-9\\-]{1,}"),
    건물본번c = ifelse(건물본번a == "" | is.na(건물본번a),
      ifelse(str_sub(건물본번b, 1, 1) == 0,
        str_replace(건물본번b, "0", ""), 건물본번b
      ),
      건물본번a
    ),
    건물부번a = str_extract(건물본번, "(?<=\\-)[0-9]{1,}"),
    건물부번b = ifelse(str_sub(건물부번, 1, 1) == 0, "",
      str_replace(건물부번, "(\\()[:graph:]{1,}", "") %>%
        str_extract(., "[0-9]{1,}")
    ),
    건물부번c = ifelse(건물부번b == "" | is.na(건물부번b),
      ifelse(str_sub(건물부번a, 1, 1) == 0,
        str_replace(건물부번a, "0", ""), 건물부번a
      ),
      건물부번b
    ),
    # 주소 합치기
    도로명주소 =
      str_c(
        도로명_시도, " ", 도로명_시군구, " ", 도로명a,
        ifelse(건물본번c == 0 | is.na(건물본번c) | 건물본번c == "", "", str_c(" ", 건물본번a)),
        ifelse(건물부번c == 0 | is.na(건물부번c) | 건물부번c == "", "", str_c("-", 건물부번c))
      ),
    지번주소 =
      str_c(
        지번_시도, " ", 지번_시군구, " ", 지번_읍면동,
        ifelse(리 == "", "", str_c(" ", 리)),
        " ", 산,
        본번a,
        ifelse(부번c == "" | is.na(부번c), "", str_c("-", 부번c))
      )
  ) %>%
  # 도로명코드 추가
  left_join(dorocode, by = "도로명") %>%
  # 매칭코드 추가
  mutate(
    도로명매칭코드 =
      str_c(
        "D",
        도로명코드,
        0,
        str_pad(str_replace_na(건물본번c, "0"), width = 5, side = "left", pad = "0"),
        str_pad(str_replace_na(건물부번c, "0"), width = 5, side = "left", pad = "0")
      ),
    지번매칭코드 =
      str_c(
        "J",
        법정동코드,
        str_replace_na(ifelse(산 == "산", "1", "0"), "0"),
        str_pad(본번a, width = 4, side = "left", pad = "0"),
        str_pad(str_replace_na(부번c, "0"), width = 4, side = "left", pad = "0")
      )) %>% 
  # 도로명주소 기준 주소매칭코드 결합
  left_join(final_juso_code_doro %>% select(도로명주소, 도로명매칭코드), by = "도로명주소") %>%
  # 지번주소 기준 주소매칭코드 결합
  left_join(juso_code_jibun %>% select(지번주소, 지번매칭코드), by = "지번주소") %>%
  # 도로명주소 기준 지번주소매칭코드 결합
  left_join(final_juso_code_doro %>% select(도로명주소, 지번주소, 지번매칭코드), by = "도로명주소") %>%
  # 도로명은 도로명주소 기준, 지번은 지번주소 기준 정리
  mutate(도로명매칭코드 = ifelse(is.na(도로명매칭코드.y), 도로명매칭코드.x, 도로명매칭코드.y),
         지번매칭코드 = ifelse(is.na(지번매칭코드), 
                         ifelse(is.na(지번매칭코드.y), 지번매칭코드.x, 지번매칭코드.y), 
                         지번매칭코드),
         지번주소 = ifelse(is.na(지번주소.y), 지번주소.x, 지번주소.y)) %>%
  # 주소 확인 가능 여부 체크(도로명 및 지번 매칭코드가 모두 없는 경우 확인)
  mutate(주소확인 = ifelse(is.na(도로명매칭코드) & is.na(지번매칭코드), 1, "")) %>%
  mutate(코드 = str_c(법정동코드, 수용가번호)) %>%
  distinct(코드, .keep_all = TRUE)

## 상수도요금 원본자료 주소와 비교해서 추가 정리 
watersupply_b <- watersupply_a %>%
  select(법정동코드:건물부번, 도로명매칭코드, 도로명주소, 지번매칭코드, 지번주소) %>%
  # 상수도요금 원본자료 주소 및 코드 합치기
  left_join(ws_address1 %>%
    select(
      수용가번호, 시군, 도로명주소, 지번주소,
      도로명매칭코드, 지번매칭코드
    ) %>%
    rename(지번_시군구 = 시군),
  by = c("수용가번호", "지번_시군구")
  ) %>%
  mutate(
    # 기존자료와 원본자료 코드 동일 여부 확인
    도로코드확인 = ifelse(도로명매칭코드.x == 도로명매칭코드.y, 1, 0),
    지번코드확인 = ifelse(지번매칭코드.x == 지번매칭코드.y, 1, 0),
    # 기존주소 및 코드가 있는 경우 기존 자료 활용, 없는 경우 원본자료 활용
    도로명주소 = ifelse(is.na(도로명주소.x), 도로명주소.y, 도로명주소.x),
    지번주소 = ifelse(is.na(지번주소.x), 지번주소.y, 지번주소.x),
    도로명매칭코드 = ifelse(is.na(도로명매칭코드.x), 도로명매칭코드.y, 도로명매칭코드.x), 
    지번매칭코드 = ifelse(is.na(지번매칭코드.x), 지번매칭코드.y, 지번매칭코드.x), 
    # 도로명 및 지번 매칭코드가 기존자료 및 원본자료 모두 없는 경우 확인
    주소확인b = ifelse(is.na(도로명매칭코드) & is.na(지번매칭코드), 1, "")
  ) %>%
  mutate_at(vars(지번_시군구), as.factor)


## 하수도요금 자료 불러오기 ----------------------------------------------------
sewage <- files %>%
  map_dfr(read_excel, sheet = 2, skip = 6, col_names = F) %>%
  # 연도 삭제
  select(-1) %>%
  set_names("수용가번호") %>%
  mutate(하수요금 = 1)


## 상하수도 요금 자료 정리 -----------------------------------------------------
watersupply1 <- watersupply %>%
  select(-1, -c(16:17)) %>% # 필요없는 열 삭제
  set_names(c(
    "법정동코드", "수용가번호", "지번_시도", "지번_시군구", "지번_읍면동",
    "리", "본번", "부번", "도로명_시도", "도로명_시군구", "도로명_읍면동",
    "도로명", "건물본번", "건물부번", "업종", "가구수",
    "1월", "2월", "3월", "4월", "5월", "6월",
    "7월", "8월", "9월", "10월", "11월", "12월"
  )) %>%
  # 월별 물사용량 숫자로 지정 및 결측값 0으로 변경
  mutate_at(vars(`1월`:`12월`), as.numeric) %>%
  mutate_at(vars(`1월`:`12월`), ~ replace(., is.na(.), 0)) %>%
  # 하수도요금 자료 유무 확인
  left_join(sewage, by = "수용가번호") %>%
  mutate(
    # 업종이 "가정용"이 아닌 경우 "영업용"으로 수정
    가정영업 = ifelse(업종 == "가정용", "가정용", "영업용"),
    # 월별 물사용량 합계로 일별 물사용량 계산
    물사용량 = round2(pmap_dbl(select(., 17:28), sum) / 365, 3)
  ) %>%
  # 주소 합치기
  left_join(watersupply_b %>% 
              select(법정동코드, 수용가번호, 도로명매칭코드, 도로명주소, 
                     지번매칭코드, 지번주소),
            by = c("법정동코드", "수용가번호")) %>% 
  mutate(코드 = str_c(법정동코드, 수용가번호)) %>%
  distinct(코드, .keep_all = TRUE)

## 상하수도 요금 자료 필수 항목만 선택
watersupply2 <- watersupply1 %>% 
  select(1:6, 도로명매칭코드, 도로명주소, 지번매칭코드, 지번주소,
         가정영업, 하수요금, 물사용량) %>% 
  mutate(분류 = "상수")



## 지하수기준 하수도요금 자료 불러오기
groundwater <- files %>%
  map_dfr(read_excel, sheet = 3, skip = 6, col_names = F)

## 지하수기준 하수도요금 자료 정리 ---------------------------------------------
groundwater1 <- groundwater %>%
  select(-1, -c(16:17)) %>% # 필요없는 열 삭제
  set_names(c(
    "법정동코드", "수용가번호", "지번_시도", "지번_시군구", "지번_읍면동",
    "리", "본번", "부번", "도로명_시도", "도로명_시군구", "도로명_읍면동",
    "도로명", "건물본번", "건물부번", "업종", "가구수",
    "1월", "2월", "3월", "4월", "5월", "6월",
    "7월", "8월", "9월", "10월", "11월", "12월"
  )) %>%
  # 월별 물사용량 숫자로 지정 및 결측값 0으로 변경
  mutate_at(vars(`1월`:`12월`), as.numeric) %>%
  mutate_at(vars(`1월`:`12월`), ~ replace(., is.na(.), 0)) %>%
  # 하수도요금 자료 유무 확인
  left_join(sewage, by = "수용가번호") %>%
  mutate(
    # 업종이 "가정용"이 아닌 경우 "영업용"으로 수정
    가정영업 = ifelse(업종 == "가정용", "가정용", "영업용"),
    # 월별 물사용량 합계로 일별 물사용량 계산
    물사용량 = round2(pmap_dbl(select(., 18:29), sum) / 365, 3),
    # 형식에 맞지 않는 주소 정리(괄호로 작성된 부분 삭제)
    본번a = str_sub(본번, 1, str_locate(본번, "\\(")[, 1] - 1) %>% str_trim(),
    부번a = str_sub(부번, 1, str_locate(부번, "\\(")[, 1] - 1) %>% str_trim(),
    본번a = str_sub(본번, 1, str_locate(본번, "\\(")[, 1] - 1) %>% str_extract(., "[0-9]{1,}"),
    부번a = str_sub(본번, 1, str_locate(본번, "\\(")[, 1] - 1) %>% str_extract(., "(?<=[:punct:])[0-9]{1,}"),
    # 주소 합치기
    도로명주소 =
      str_c(
        도로명_시도, " ", 도로명_시군구, " ", 도로명,
        ifelse(건물본번 == 0 | is.na(건물본번), "", str_c(" ", 건물본번)),
        ifelse(건물부번 == 0 | is.na(건물부번), "", str_c("-", 건물부번))
      ),
    지번주소 =
      str_c(
        지번_시도, " ", 지번_시군구, " ", 지번_읍면동,
        ifelse(is.na(리), "", str_c(" ", 리)),
        ifelse(str_sub(본번, 1, 1) == "산",
          str_c(" 산 ", str_replace(str_sub(본번), "산", "")),
          str_c(" ", 본번)
        ),
        ifelse(부번 == 0 | is.na(부번), "", str_c("-", 부번))
      )
  ) %>%
  # 도로명주소 기준 주소매칭코드 결합
  left_join(juso_code_doro %>% select(도로명주소, 도로명매칭코드), by = "도로명주소") %>%
  # 지번주소 기준 주소매칭코드 결합
  left_join(juso_code_jibun %>% select(지번주소, 지번매칭코드), by = "지번주소") %>%
  # 도로명은 도로명주소 기준, 지번은 지번주소 기준 정리
  # mutate(도로명매칭코드 = ifelse(is.na(도로명매칭코드.y), 도로명매칭코드.x, 도로명매칭코드.y),
  #               지번매칭코드 = ifelse(is.na(지번매칭코드.x), 지번매칭코드.y, 지번매칭코드.x)) %>%
  # 주소 확인 가능 여부 체크
  mutate(주소확인 = ifelse(is.na(도로명매칭코드) & is.na(지번매칭코드), 1, "")) %>% 
  mutate(코드 = str_c(법정동코드, 수용가번호)) %>%
  distinct(코드, .keep_all = TRUE)


## 지하수기준 하수도요금 자료 필수 항목만 선택
groundwater2 <- groundwater1 %>% 
  select(1:6, 도로명매칭코드, 도로명주소, 지번매칭코드, 지번주소,
         가정영업, 하수요금, 물사용량) %>% 
  mutate(분류 = "지하수")


final_watersupply <- rbind(watersupply2, groundwater2)

final_watersupply <- final_watersupply %>% 
  mutate(도로명매칭코드 = ifelse(is.na(도로명매칭코드), "x", 도로명매칭코드),
         지번매칭코드 = ifelse(is.na(지번매칭코드), "x", 지번매칭코드)) %>% 
  rename_with(~ str_remove(., "지번_")) %>% 
  mutate(코드확인 = ifelse(도로명매칭코드 == "x" & 지번매칭코드 == "x", 1, "")) %>% 
  left_join(final_juso_code_doro %>% 
            select(도로명매칭코드, 지번매칭코드, 지번주소), by = "도로명매칭코드") %>% 
  mutate(지번매칭코드 = ifelse(지번매칭코드.x == "x", 지번매칭코드.y, 지번매칭코드.x),
             지번주소 = ifelse(is.na(지번주소.x), 지번주소.y, 지번주소.x),
         지번매칭코드 = ifelse(is.na(지번매칭코드), "x", 지번매칭코드), 
            코드확인b = ifelse(도로명매칭코드 == "x" & 지번매칭코드 == "x", 1, ""))


## 물사용량 총계 확인(전체 및 주소 미확인 비교)
sum(final_watersupply$물사용량, na.rm = T)
sum(final_watersupply %>% filter(코드확인 == 1) %>% select(물사용량), na.rm = T)
