#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(readxl)
library(writexl)
library(nortest)

## 주소 좌표 변환 관련
library(httr)
library(jsonlite) # fromJSON()
library(progress)

## 반올림 사용자 정의 함수 로드
source("Script_R/Function/func_round2.R")


##****************************************************************************##
##############################  1. 시설 현황 확인  #############################
##****************************************************************************##


#####  1-1. 데이터 불러오기  ###################################################

## 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/기준배출수질/환경기초시설 데이터",
  pattern = "*.xls", full.names = T
)

## 경로지정된 파일 불러오기(파일이 여러개인 경우 병합)
시설현황_원본 <- files %>%
  # map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
  map_dfr(read_excel, sheet = "환경기초시설", skip = 3, col_names = F)

## 파일 불러오기
# 시설현황_원본 <- read_excel(
#   "전국오염원조사/환경기초시설/2023년기준_전국오염원_조사자료_환경기초시설.xlsx",
#   sheet = 1, skip = 3, col_names = F
# )

## 환경기초시설별 단위유역 현황 자료 불러오기
stp_유역 <- read_excel(
  "전국오염원조사/환경기초시설/환경기초시설 현황/환경기초시설_현황.xlsx"
) %>%
  select(2, 14)


#####  1-2. 데이터 정리  ###################################################

시설현황_정리 <- 시설현황_원본 %>%
  select(2:10, 26) %>%
  set_names(c(
    "처리시설명", "처리시설코드", "구분", "시도", "시군",
    "읍면동", "리", "본번", "부번", "가동개시"
  )) %>%
  # filter(!(시군 %in% c("동해시", "속초시", "양양군"))) %>% 
  mutate(
    주소 =
      str_c(
        "강원특별자치도", " ", 시군, " ", 읍면동, " ",
        ifelse(is.na(리), "", str_c(리, " ")),
        본번,
        ifelse(부번 == 0 | is.na(부번), "", str_c("-", 부번))
      ),
    동리 = ifelse(is.na(리), 읍면동, 리),
    동리코드 = str_c(시군, 읍면동, ifelse(is.na(리), "", 리), sep = " ") %>% 
      str_trim(),
    가동개시 = str_sub(가동개시, 1, 4) %>% as.numeric(.)
  ) %>%
  left_join(stp_유역, by = "처리시설코드") %>% 
  distinct()


#####  1-3. 단위유역 미확인 시설 유역 확인  ####################################

##### 1-3-1. 동리별 유역 현황과 매칭 -------------------------------------------
# 동리별 유역현황 자료 중 유역 점유율이 100%인 지역만 선택
동리별_유역현황 <- read_excel("전국오염원조사/동리별_유역현황.xlsx") %>%
  filter(점유율 == 100) %>% 
  select(동리코드, 동리별_유역 = 단위유역)

# 동리별 유역현황 자료와 매칭하여 유역 확인
시설현황_정리 %<>% left_join(동리별_유역현황, by = "동리코드") %>% 
  mutate(확인 = ifelse(단위유역 == 동리별_유역, 0, 1)) %>% 
  mutate(단위유역 = ifelse(is.na(단위유역), 동리별_유역, 단위유역))

# 유역 확인용 열 삭제
시설현황_정리 %<>% select(-c(동리별_유역, 확인))


##### 1-3-2. 주소 좌표 변환 ----------------------------------------------------
## 단위유역 미확인 시설 시설명 및 주소 추출 후 ID 부여
address_ind <- 시설현황_정리 %>%
  filter(is.na(단위유역)) %>%
  rowid_to_column(var = "ID")

## 주소 목록 list 생성
address_list <- address_ind$주소

## progress bar 설정
pb <- progress_bar$new(
  format = " Progress: [:bar] :current / :total (:percent), Estimated completion time::eta",
  total = nrow(address_ind), # 총 tick 개수 (default 100)
  clear = FALSE, # 종료 후에도 진행 경과 출력 결과 유지 (default TRUE)
  width = 80 # 진행 경과 막대 너비
)

## tibble 사전 생성
result <- tibble()
temp <- tibble()

## 주소 검색 및 데이터 취합
for (i in 1:nrow(address_ind)) {
  # 카카오맵 검색
  place_list <- GET(
    url = "https://dapi.kakao.com/v2/local/search/address.json",
    query = list(query = address_list[i]),
    add_headers(Authorization = "KakaoAK 9e9a85a9ec8362e009da2f7bc4b3a09c")
  ) %>%
    content(as = "text") %>%
    fromJSON()
  
  # 업소명 및 기존 주소 불러오기
  temp_addr <- address_ind %>% filter(ID == i)
  
  ## 주소
  temp <- bind_cols(temp_addr, place_list$documents)
  result <- bind_rows(result, temp)
  
  ## 진행상황 확인
  pb$tick()
}


## 주소 검색 결과 정리
# 중복 주소 정리
# 기존 주소에서 카카오맵 검색결과 읍면동이 일치하지 않는 경우 삭제
result %<>%
  mutate(확인 = ifelse(is.na(x),
                     TRUE,
                     str_detect(주소, address$region_3depth_name)
  )) %>%
  filter(확인 == TRUE)


#####  1-3-3. 유역 확인 --------------------------------------------------------

library(sf)

## 경위도좌표 정리
경위도 <- result %>% 
  mutate(across(c(x, y), as.numeric)) %>% 
  select(처리시설명:동리코드, x, y)

## 경위도 point 자료 생성(경위도 좌표계(WGS84) 적용 - EPSG:4326)
경위도_point <- 경위도 %>% filter(!is.na(x)) %>% 
  st_as_sf(coords = c('x', 'y'), crs = 4326)
  

## 좌표를 EPSG:5174로 변경(단위유역 shp 파일과 좌표계 통일)
경위도_point_5174 <- sf::st_transform(경위도_point, crs = 5174)


## 단위유역 shp 파일 불러오기(sf객체)
단위유역 <- sf::st_read("D:/GIS/유역/강원도_유역도_최종(이티워터)/총량단위유역_강원도_1985M_20200924.shp") %>% 
  select(SW_NAME, geometry) %>% 
  rename(단위유역 = SW_NAME)

## 단위유역 정리
stp_유역2 <- sf::st_join(경위도_point_5174, 단위유역) %>% 
  # select(연도, 시군, 업소명, 주소전체, 지번주소, 단위유역 = SW_NAME) %>% 
  mutate(단위유역 = ifelse(is.na(단위유역), "기타", 단위유역)) %>% 
  # 지리정보(geometry) 제거
  sf::st_drop_geometry()

## 유역 확인된 시설 기존 정리자료와 합치기
시설현황_정리 %<>% 
  filter(!is.na(단위유역)) %>% 
  bind_rows(stp_유역2) %>% 
  mutate(
    시군 = factor(시군, levels = c(
      "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군",
      "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군",
      "양구군", "인제군", "고성군", "동해시", "속초시", "양양군"
    ))) %>% 
  arrange(시군, 처리시설명)

#####  1-3-4. 유역 확인 정리자료 내보내기 --------------------------------------
write_xlsx(
  시설현황_정리, 
  "전국오염원조사/환경기초시설/환경기초시설 현황/환경기초시설_현황.xlsx"
  )


##****************************************************************************##
###############################  2. 정규성 검증  ###############################
##****************************************************************************##


#####  2-1. 데이터 불러오기  ###################################################

## 환경기초시설 방류량 자료 불러오기(파일이 여러개인 경우 병합)
data_원본 <- files %>%
  # map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
  map_dfr(~ {
    data <- read_excel(.x, sheet = "방류량", skip = 3, col_names = F) 
      # 데이터 형식이 달라서 합쳐지지 않는 문제 해결
      # mutate(across(everything(), as.character))
  })


## 데이터 정리
data_정리 <- data_원본 %>%
  select(1, 7, 10, 14) %>%
  set_names(c("처리시설명", "유량", "BOD", "TP")) %>%
  # 시군, 유역 추가
  left_join(시설현황_정리 %>% select(처리시설명, 시군, 단위유역),
    by = "처리시설명"
  ) %>%
  # 기타 수계 제외
  filter(단위유역 != "기타") %>%
  mutate(across(c(유량:TP), as.numeric))


#####  2-2. 정규성 검증  #######################################################

## 정규성 검증 함수 정의
run_normality_tests <- function(df, raw_col) {
  log_col    <- paste0("ln_", raw_col)
  result_col <- paste0(raw_col, "_정규성")
  
  df %>%
    # NA 및 0 값 제거, 로그 변환
    filter(!is.na(.data[[raw_col]]), .data[[raw_col]] != 0) %>%
    mutate(!!log_col := log(.data[[raw_col]])) %>%
    group_by(처리시설명) %>%
    # 정규성 검증
    # - Shapiro-wilk, Anderson-Darling, Kolmogorov-Smirnov 3가지 방법 중 
    #   1개 이상 정규성이면 정규성으로 판단
    # - ks.test는 모집단 평균(μ), 표준편차(σ)를 “사전 고정”된 모수로 가정하므로,
    #   표본에서 추정한 μ̂, σ̂를 사용할 경우 p-value가 과대·과소 평가될 수 있음
    # - 따라서 모수추정 보정을 포함한 Lilliefors 테스트(nortest::lillie.test)를 사용
    summarise(
      KS            = lillie.test(.data[[log_col]])$p.value,
      SW            = shapiro.test(.data[[log_col]])$p.value,
      AD            = ad.test(.data[[log_col]])$p.value,
      !!result_col  := if_else(KS > 0.05 | SW > 0.05 | AD > 0.05,
                               "정규성", "비정규성"),
      .groups = "drop"
    )
}

## 정규성 검증
BOD_정규성검증 <- run_normality_tests(data_정리, raw_col = "BOD")
TP_정규성검증  <- run_normality_tests(data_정리, raw_col = "TP")



##****************************************************************************##
############################  3. 기준배출수질 산정  ############################
##****************************************************************************##


#####  3-1. 기준배출수질(가)  ##############################################

## 시행규칙 별표3 가 방식(로그정규분포) 기준배출수질 함수
calc_parametric_limit <- function(df, raw_col) {
  log_col    <- paste0("ln_", raw_col)
  mean_col   <- paste0(raw_col, "_mean")
  sd_col     <- paste0(raw_col, "_sd")
  result_col <- paste0(raw_col, "_가")
  
  df %>%
    # NA 및 0 값 제거, 로그 변환
    filter(!is.na(.data[[raw_col]]), .data[[raw_col]] != 0) %>%
    mutate(!!log_col := log(.data[[raw_col]])) %>%
    group_by(처리시설명) %>%
    summarise(
      !!mean_col   := mean(.data[[log_col]], na.rm = TRUE),
      !!sd_col     := sd(.data[[log_col]],   na.rm = TRUE),
      !!result_col := exp(
        mean(.data[[log_col]], na.rm = TRUE) +
          1.645 * sd(.data[[log_col]], na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

## 기준배출수질(가)
BOD_기준배출수질_가 <- calc_parametric_limit(data_정리, "BOD")
TP_기준배출수질_가  <- calc_parametric_limit(data_정리,  "TP")


#####  3-2. 기준배출수질(나)  ##############################################

# 시행규칙 별표3 나 방식(비정규분포) 기준배출수질 계산 함수
calc_nonparametric_limit <- function(df, raw_col) {
  result_col <- paste0(raw_col, "_나")
  count_col  <- paste0(raw_col, "_개수")
  max_col    <- paste0(raw_col, "_최대")
  
  df %>%
    # NA 및 0 값 제거
    filter(!is.na(.data[[raw_col]]), .data[[raw_col]] != 0) %>%
    group_by(처리시설명) %>%
    arrange(.data[[raw_col]], .by_group = TRUE) %>%
    summarise(
      n     = n(),                          # 데이터 갯수
      a     = floor(1 + 0.95 * (n - 1)),    # 1+0.95×(측정횟수-1)의 정수부
      b     = (1 + 0.95 * (n - 1)) - a,     # 1+0.95×(측정횟수-1)의 소수부분
      Xa    = nth(.data[[raw_col]], a),     # nth(): 변수의 a번째 행 출력
      Xa1   = nth(.data[[raw_col]], a + 1), # nth(): 변수의 a+1번째 행 출력
      result = (1 - b) * Xa + b * Xa1,      # 기준배출수질 = (1-b) × Xa + b × X(a+1)
      !!max_col     := max(.data[[raw_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(
      !!count_col  := n,
      !!result_col := result
    )
}

# 사용 예시: raw_col 에 "BOD" 혹은 "TP" 만 넘기면 됩니다.
BOD_기준배출수질_나 <- calc_nonparametric_limit(data_정리, "BOD")
TP_기준배출수질_나  <- calc_nonparametric_limit(data_정리,  "TP")


#####  3-3. 평균유량 산정  #####################################################
평균유량 <- data_정리 %>%
  select(시군, 단위유역, 처리시설명, 유량) %>%
  group_by(시군, 단위유역, 처리시설명) %>%
  reframe(평균유량 = round2(mean(유량), 1))



##****************************************************************************##
################################  4. 최종 정리  ################################
##****************************************************************************##

## 2030년 계획 자료 불러오기
최종년도계획 <- 
  read_excel("전국오염원조사/기준배출수질/환경기초시설_2030년_계획.xlsx") %>% 
  select(-c(준공연도, 처리시설코드))

## 기준배출 수질 확인 및 정리
기준배출수질_최종 <- 최종년도계획 %>%
  full_join(평균유량, by = c("시군", "단위유역","처리시설명")) %>% 
  left_join(BOD_정규성검증 %>% select(처리시설명, BOD_정규성), by = "처리시설명") %>%
  left_join(BOD_기준배출수질_가 %>% select(처리시설명, BOD_가), by = "처리시설명") %>%
  left_join(BOD_기준배출수질_나 %>% select(처리시설명, BOD_나, BOD_최대, BOD_개수), by = "처리시설명") %>%
  left_join(TP_정규성검증 %>% select(처리시설명, TP_정규성), by = "처리시설명") %>%
  left_join(TP_기준배출수질_가 %>% select(처리시설명, TP_가), by = "처리시설명") %>%
  left_join(TP_기준배출수질_나 %>% select(처리시설명, TP_나, TP_최대, TP_개수), by = "처리시설명") %>%
  left_join(시설현황_정리 %>% select(처리시설명, 가동개시), by = "처리시설명") %>% 
  mutate(
    시군 = factor(시군, levels = c(
      "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군",
      "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군",
      "양구군", "인제군", "고성군", "동해시", "속초시", "양양군"
    )),
    단위유역 = factor(단위유역, levels = c(
      "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A",
      "섬강A", "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
      "홍천A", "한탄A", "한강B", "제천A", "한강D", "북한D", "한탄B", "임진A",
      "낙본A"
    )),
    BOD_정규성 = case_when(
      BOD_개수 < 30 ~ "n<30",
      BOD_개수 >= 347 ~ "n>=347",
      .default = BOD_정규성
    ),
    TP_정규성 = case_when(
      TP_개수 < 30 ~ "n<30",
      TP_개수 >= 347 ~ "n>=347",
      .default = TP_정규성
    ),
    # 기준배출수질 결정
    # - 가 방식 : 정규성인 경우 
    # - 나 방식 : 비정규성 및 측정횟수 347회 이상 
    # - 최대값 : 측정횟수 30회 미만 
    BOD_기준 = case_when(
      BOD_정규성 == "정규성" ~ BOD_가,
      BOD_정규성 == "비정규성" ~ BOD_나,
      BOD_정규성 == "n>=347" ~ BOD_나,
      BOD_정규성 == "n<30" ~ BOD_최대
    ) %>% round2(., 1),
    TP_기준 = case_when(
      TP_정규성 == "정규성" ~ TP_가,
      TP_정규성 == "비정규성" ~ TP_나,
      TP_정규성 == "n>=347" ~ TP_나,
      TP_정규성 == "n<30" ~ TP_최대
    ) %>% round2(., 3),
    구분 = ifelse(is.na(구분), "신규", 구분)
  ) %>%
  relocate(c(평균유량, BOD_기준, TP_기준), .after = TP_30년) %>%
  relocate(가동개시, .after = 구분) %>% 
  arrange(시군, 단위유역, 구분, 가동개시, 처리시설명) %>% 
  filter(!단위유역 %in% c("북한D", "임진A"), 
         구분 != "미준공")


### 최종 정리 자료 내보내기
sheets <- list(
  "기준배출수질_최종" = 기준배출수질_최종, 
  "BOD_정규성검증" = BOD_정규성검증,
  "BOD_기준배출수질_가" = BOD_기준배출수질_가,
  "BOD_기준배출수질_나" = BOD_기준배출수질_나, 
  "TP_정규성검증" = TP_정규성검증, 
  "TP_기준배출수질_가" = TP_기준배출수질_가,
  "TP_기준배출수질_나" = TP_기준배출수질_나
)

write_xlsx(sheets,
  path = "전국오염원조사/기준배출수질/Output/기준배출수질_2023년기준_2.xlsx"
)


