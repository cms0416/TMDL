#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(janitor)
library(readxl)
library(writexl)

## 그래프 관련
library(scales)
library(ggthemes)
library(showtext)

## 그래프용 Noto Sans KR 폰트 추가
font_add_google("Noto Sans KR", "notosanskr")
showtext_auto()


#####  기준년도 정의  ##########################################################
기준년도 <- 2024


#####  지역개발부하량 및 기승인 기초자료(기본계획) 정리  #######################

## 기초자료 파일 불러오기기
base <- read_excel("지역개발부하량 관리/지역개발부하량.xlsx") %>%
  select(-항목정렬)

## 추진실적 검토용 시행계획 수립지역 정리
# 시행계획 수립 유역 및 해당 시군 소계 삭제
base_추진실적 <- base %>%
  filter(
    !단위유역 %in% c("북한D", "임진A"),
    시군 != "강원도"
  ) %>%
  filter(!(시군 %in% c("춘천시", "철원군") & 단위유역 == "소계"))

# 시행계획 수립 시군(춘천시, 철원군) 소계 재산정
base_추진실적 %<>%
  bind_rows(base_추진실적 %>%
    filter(시군 %in% c("춘천시", "철원군")) %>%
    group_by(대상물질, 시군) %>%
    summarise_at(vars(지역개발_점:기승인_비점), sum) %>%
    mutate(단위유역 = "소계") %>%
    relocate(단위유역, .after = 시군) %>%
    left_join(base %>% select(시군정렬:단위유역),
      by = c("대상물질", "시군", "단위유역")
    )) %>%
  arrange(시군정렬) %>%
  select(-c(시군정렬, 기승인_점, 기승인_비점))


#####  지역개발 연차별 관리계획  ###############################################
연차별관리계획 <- read_excel("지역개발부하량 관리/지역개발부하량.xlsx",
                      sheet = "지역개발 연차별 관리계획") %>%
  select(시군:대상물질, str_c(기준년도, "_점"), str_c(기준년도, "_비점"))


#####  누적관리대장 정리  ######################################################

## 누적관리대장 파일 불러오기
# 데이터 경로지정
files <- list.files(
  path = "지역개발부하량 관리/", pattern = "누적관리대장*", full.names = T
)

# 경로지정된 파일 합치기 ← 사전에 기간외소진 사업 협의일자 및 사업종류 수정
data <- files %>%
  map_dfr(read_excel, skip = 3, col_names = F)


#####  누적관리대장 시군별 개별 파일인 경우 파일 합치기  #######################
## 파일 취합 할 데이터 프레임 사전 생성
# data <- tibble()
#
# ## 모든 누적관리대장 파일에서 "협의"가 포함안된 시트 자료 합치기
# for (i in 1:length(files)) {
#   path <- files[i]
#
#   temp <-
#     # str_detect : 특정 문자열(협의) 포함여부 확인
#     excel_sheets(path)[str_detect(excel_sheets(path), "협의", negate = T)] %>%
#     # map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
#     map_dfr(read_excel, path = path, skip = 3, col_names = F)
#
#   data <- rbind(data, temp)
# }
# ******************************************************************************


## 내부검토용(공통) : 삭감방법 열 추가 / 필요 없는 열 제거 / 열 이름 설정 / 협의연도 추가
data_내부검토 <- data %>%
  # 삭감방법 관련 열 값이 NA인 경우 대체값 입력
  # 처음열은 빈칸, 두번째 열 부터는 "/"로 대체
  mutate(across(c(13, 38), ~ replace(., is.na(.), ""))) %>%
  mutate(across(c(18, 23, 28, 33, 43, 48, 53, 58), ~ replace(., is.na(.), "/"))) %>%
  # 삭감량 데이터 합산 위해 숫자로 변환하고 NA는 0으로 변환
  mutate(across(c(...14, ...19, ...24, ...29, ...34, 
                  ...41, ...46, ...51, ...56, ...61,
                  ...90, ...95, ...100, ...105, ...110,
                  ...117, ...122, ...127, ...132, ...137), 
                ~replace(as.numeric(.), is.na(as.numeric(.)), 0))) %>%
  mutate(
    # 삭감방법 열 모두 합치고, NA 대체값 삭제
    # (BOD 삭감방법만 정리 : BOD, T-P 삭감방법 동일)
    삭감방법_점 =
      str_c(...13, ...18, ...23, ...28, ...33, sep = ", ") %>% 
      str_remove_all(., ", /") %>%
      # 합친 삭감방법 문자열 시작이 ","인 경우 삭제
      ifelse(str_sub(., 1, 1) == ",", str_remove(., ", "), .),
    삭감방법_비점 =
      str_c(...38, ...43, ...48, ...53, ...58, sep = ", ") %>%
      str_remove_all(., ", /") %>%
      # 합친 삭감방법 문자열 시작이 ","인 경우 삭제
      ifelse(str_sub(., 1, 1) == ",", str_remove(., ", "), .),
    # 삭감량 계산
    BOD.삭감량_점 = ...14 + ...19 + ...24 + ...29 + ...34,
    BOD.삭감량_비점 = ...41 + ...46 + ...51 + ...56 + ...61,
    TP.삭감량_점 = ...90 + ...95 + ...100 + ...105 + ...110,
    TP.삭감량_비점 = ...117 + ...122 + ...127 + ...132 + ...137
  ) %>%
  # 필요한 열 선택
  select(
    1:6, 9:12, 64, 65, 70, 71, 88, 140, 141, 146, 147,
    167, 168, 170:173, 삭감방법_점, 삭감방법_비점,
    BOD.삭감량_점, TP.삭감량_점, BOD.삭감량_비점, TP.삭감량_비점
  ) %>%
  # 열 이름 설정
  set_names(c(
    "사업구분", "시군", "단위유역", "할당일자", "사업명",  "사업위치", "착공연도",
    "준공예정연도", "준공여부", "BOD.삭감량_합계", "BOD.지역개발_점",
    "BOD.지역개발_비점", "BOD.소진_점", "BOD.소진_비점", "TP.삭감량_합계",
    "TP.지역개발_점", "TP.지역개발_비점", "TP.소진_점", "TP.소진_비점",
    "협의일자", "협의상태", "주관부서", "담당자명", "부서연락처",
    "사업종류", "삭감방법_점", "삭감방법_비점", "BOD.삭감량_점", "TP.삭감량_점",
    "BOD.삭감량_비점", "TP.삭감량_비점"
  )) %>%
  # 협의일자가 누락된 경우 할당일자로 설정
  mutate(협의일자 = ifelse(is.na(협의일자), 할당일자, 협의일자)) %>%
  # 날짜 서식 변경
  mutate(across(c(할당일자, 협의일자), ~ str_replace_all(., "\\.", "-"))) %>%
  # 날짜 및 연도 데이터 형식 변경(문자 → 날짜 및 숫자),
  mutate(across(c(할당일자, 협의일자), as.Date)) %>%
  mutate(across(c(착공연도, 준공예정연도, BOD.삭감량_합계:TP.소진_비점), as.numeric)) %>%
  # 협의연도 추가(협의일자가 없는 경우 할당연도로 추가), 시군명만 추출
  mutate(
    할당연도 = year(할당일자),
    협의연도 = year(협의일자),
    # 최초 및 기승인 개발은 협의연도를 2020년으로 설정
    # 기간외 소진 사업 현황 확인
    협의연도 = case_when(
      사업구분 == "기본계획_최초개발" | 사업구분 == "기본계획_기승인" ~ 2020,
      TRUE ~ 협의연도
    ),
    시군 = str_remove(시군, "강원특별자치도 "),
    # 태백시 수계 구분
    시군 = case_when(
      시군 == "태백시" & 단위유역 == "낙본A" ~ "태백시(낙동강)",
      시군 == "태백시" & 단위유역 != "낙본A" ~ "태백시(한강)",
      TRUE ~ 시군
    )
  ) %>%
  # 미협의 할당 및 보완 사업 제외
  filter(!협의상태 %in% c("할당", "보완"))


## 추진실적용 : 시행계획 수립 유역(북한D, 임진A) 제외
data_추진실적 <- data_내부검토 %>%
  filter(!단위유역 %in% c("북한D", "임진A"))


#####  함수 정의  ##############################################################

##### 1. 부하량 자료 정리 함수  ------------------------------------------------
load_cal <- function(data) {
  data_cal <- data %>%
    group_by(시군, 단위유역) %>%
    summarise(across(
      c(BOD.소진_점, BOD.소진_비점, TP.소진_점, TP.소진_비점),
      ~ sum(.)
    ), .groups = "drop") %>%
    pivot_longer(
      cols = BOD.소진_점:TP.소진_비점,
      names_to = c("대상물질", "항목"),
      names_sep = "\\."
    ) %>%
    pivot_wider(
      names_from = 항목,
      values_from = value
    ) %>%
    # 시군 및 유역별 소계 계산
    group_by(시군, 대상물질) %>%
    group_modify(~ .x %>%
                   adorn_totals(
                     where = "row", fill = "소계",
                     na.rm = TRUE, name = "소계"
                   )) %>% 
    ungroup()
}


##### 2. 시군 유역 순서 지정 함수  ---------------------------------------------
## 내부검토용(합계/소계를 제일 앞으로)
order_func <- function(data) {
  data %>%
    mutate(
      시군 = factor(시군, levels = c(
        "강원도", "강원도(한강)", "춘천시", "원주시", "강릉시", "태백시(낙동강)",
        "태백시(한강)", "삼척시", "홍천군", "횡성군", "영월군", "평창군",
        "정선군", "철원군", "화천군", "양구군", "인제군", "고성군"
      )),
      단위유역 = factor(단위유역, levels = c(
        "합계", "소계", "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A",
        "섬강A", "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
        "홍천A", "한탄A", "제천A", "한강B", "한강D", "북한D", "임진A", "한탄B",
        "낙본A"
      ))
    )
}

## 추진실적용(소계를 제일 뒤로)
order_func2 <- function(data) {
  data %>%
    mutate(
      시군 = factor(시군, levels = c(
        "강원도", "강원도(한강)", "춘천시", "원주시", "강릉시", "태백시(낙동강)",
        "태백시(한강)", "삼척시", "홍천군", "횡성군", "영월군", "평창군",
        "정선군", "철원군", "화천군", "양구군", "인제군", "고성군"
      )),
      단위유역 = factor(단위유역, levels = c(
        "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A", "섬강B",
        "북한A", "북한B", "소양A", "인북A", "소양B", "북한C", "홍천A", "한탄A",
        "제천A", "한강B", "한강D", "북한D", "임진A", "한탄B", "낙본A",
        "소계", "합계"
      ))
    )
}


#####  * 추진실적용 정리  #######################################################

##### 지역개발부하량 소진현황 정리  --------------------------------------------

## 협의 부하량(누적) 정리 
추진실적_협의 <- data_추진실적 %>%
  # 당해연도 이전까지 협의된 사업 필터
  filter(협의연도 < 기준년도) %>%
  load_cal(.) %>%
  rename(협의부하량_점 = 소진_점, 협의부하량_비점 = 소진_비점)

## 사용부하량 정리  
추진실적_사용 <- data_추진실적 %>%
  # 당해연도 협의된 사업 필터
  filter(협의연도 == 기준년도) %>%
  load_cal(.) %>%
  rename(사용부하량_점 = 소진_점, 사용부하량_비점 = 소진_비점)

## 소진현황 최종 정리
추진실적_소진현황 <- base_추진실적 %>%
  # 협의 부하량 합치기
  left_join(추진실적_협의, by = c("시군", "대상물질", "단위유역")) %>%
  # na를 0으로 대체(협의 가능량 계산 전 변환)
  mutate_all(~ replace(., is.na(.), 0)) %>%
  mutate(
    협의가능량_점 = 지역개발_점 - 협의부하량_점,
    협의가능량_비점 = 지역개발_비점 - 협의부하량_비점
  ) %>%
  # 사용 부하량 합치기
  left_join(추진실적_사용, by = c("시군", "대상물질", "단위유역")) %>%
  # na를 0으로 대체(잔여 및 누적 부하량 계산 전 변환)
  mutate_all(~ replace(., is.na(.), 0)) %>%
  mutate(
    # 잔여부하량 추가
    잔여부하량_점 = 협의가능량_점 - 사용부하량_점,
    잔여부하량_비점 = 협의가능량_비점 - 사용부하량_비점,
    # 누적부하량 추가(참고용)
    누적부하량_점 = 협의부하량_점 + 사용부하량_점,
    누적부하량_비점 = 협의부하량_비점 + 사용부하량_비점
  ) %>%
  # "시군"을 첫번째 열로 이동
  relocate(시군)


##### 협의 현황(사업 목록) 정리  -----------------------------------------------
추진실적_사업목록 <- data_추진실적 %>%
  select(
    시군, 단위유역, 사업구분, 사업명, 협의일자, 준공예정연도,
    BOD.소진_점, BOD.소진_비점, TP.소진_점, TP.소진_비점,
    삭감방법_비점, BOD.삭감량_비점, TP.삭감량_비점, 
    삭감방법_점, BOD.삭감량_점, TP.삭감량_점, BOD.삭감량_합계, TP.삭감량_합계,
    할당일자, 착공연도, 준공여부, 협의상태, 협의연도
  ) %>%
  add_count(시군, 단위유역, 사업명, name = "협의건수") %>%
  mutate(
    비고 = case_when(
      사업구분 == "기본계획_최초개발" & 협의건수 >= 2 ~ "협의 부하량",
      사업구분 == "기본계획_최초개발" ~ "제외",
      협의연도 < 기준년도 ~ "협의 부하량",
      협의연도 == 기준년도 ~ "사용 부하량",
      TRUE ~ "제외"
    ),
    # 연도는 문자형으로
    across(c(준공예정연도, 착공연도, 협의연도), as.character),
    # 사업취소일 경우 음수, 재협의일 경우 0으로
    BOD.삭감량_점 = case_when(
      사업구분 == "사업취소" ~ -1 * BOD.삭감량_점,
      사업구분 == "재협의" ~ 0,
      TRUE ~ BOD.삭감량_점
    ),
    TP.삭감량_점 = case_when(
      사업구분 == "사업취소" ~ -1 * TP.삭감량_점,
      사업구분 == "재협의" ~ 0,
      TRUE ~ TP.삭감량_점
    ),
    BOD.삭감량_비점 = case_when(
      사업구분 == "사업취소" ~ -1 * BOD.삭감량_비점,
      사업구분 == "재협의" ~ 0,
      TRUE ~ BOD.삭감량_비점
    ),
    TP.삭감량_비점 = case_when(
      사업구분 == "사업취소" ~ -1 * TP.삭감량_비점,
      사업구분 == "재협의" ~ 0,
      TRUE ~ TP.삭감량_비점
    ),
    BOD.삭감량_합계 = case_when(
      사업구분 == "사업취소" ~ -1 * BOD.삭감량_합계,
      사업구분 == "재협의" ~ 0,
      TRUE ~ BOD.삭감량_합계
    ),
    TP.삭감량_합계 = case_when(
      사업구분 == "사업취소" ~ -1 * TP.삭감량_합계,
      사업구분 == "재협의" ~ 0,
      TRUE ~ TP.삭감량_합계
    )
  ) %>%
  # 행순서 조정
  # 사업명이 동일한 경우 하나의 그룹으로 묶고
  # 그룹의 마지막 협의 일자를 기준으로 오름차순 정렬
  # 그룹 내부에서는 협의 일자 오름차순 정렬
  group_by(시군, 단위유역, 사업명) %>%
  mutate(group_key = max(협의일자)) %>%
  ungroup() %>%
  order_func2() %>%
  arrange(시군, 단위유역, group_key, 사업명, 협의일자) %>%
  select(-group_key) %>% 
  # 시군 및 유역별 소계 계산
  group_by(시군, 단위유역) %>%
  group_modify(~ .x %>%
                 adorn_totals(
                   where = "row", fill = "소계",
                   na.rm = TRUE, name = "소계"
                 )) %>%
  relocate(협의건수, .after = last_col())


#####  준공현황 정리  ----------------------------------------------------------

## 준공사업 배출부하량 정리
추진실적_준공 <- data_추진실적 %>%
  # 당해연도 까지 누적 준공 사업 필터
  filter(준공여부 <= 기준년도) %>%
  load_cal(.) %>%
  rename(준공_점 = 소진_점, 준공_비점 = 소진_비점) 

## 지역개발 연차별 관리계획 정리
연차별관리계획_추진실적 <- 연차별관리계획 %>%
  rename(당해연도계획_점 = str_c(기준년도, "_점"), 당해연도계획_비점 = str_c(기준년도, "_비점")) %>%
  filter(
    !단위유역 %in% c("북한D", "임진A"),
    시군 != "강원도"
  ) %>%
  filter(!(시군 %in% c("춘천시", "철원군") & 단위유역 == "소계")) %>%
  mutate(
    `당해연도계획_점` = ifelse(단위유역 == "한탄B" & 대상물질 == "BOD", 0, `당해연도계획_점`),
    `당해연도계획_비점` = ifelse(단위유역 == "한탄B" & 대상물질 == "BOD", 0, `당해연도계획_비점`)
  )

# 시행계획 수립 시군(춘천시, 철원군) 소계 재산정
연차별관리계획_추진실적 %<>%
  bind_rows(연차별관리계획_추진실적 %>%
              filter(시군 %in% c("춘천시", "철원군")) %>%
              group_by(대상물질, 시군) %>%
              summarise_at(vars(`당해연도계획_점`, `당해연도계획_비점`), sum) %>%
              mutate(단위유역 = "소계") %>%
              relocate(단위유역, .after = 시군))

## 준공현황 및 연차별 관리계획 합쳐서 정리
추진실적_준공현황 <- 연차별관리계획_추진실적 %>%
  left_join(추진실적_준공, by = c("시군", "단위유역", "대상물질")) %>%
  mutate_all(~ replace(., is.na(.), 0)) %>%
  order_func2() %>%
  arrange(시군, 대상물질, 단위유역)


##### 준공 사업 내역 정리  -----------------------------------------------------
추진실적_준공사업내역 <- data_추진실적 %>%
  select(
    시군, 단위유역, 사업구분, 사업명, 협의일자, 준공여부,
    BOD.소진_점, BOD.소진_비점, TP.소진_점, TP.소진_비점, 사업위치,
    BOD.삭감량_합계, TP.삭감량_합계, 삭감방법_점, 삭감방법_비점
  ) %>%
  filter(준공여부 <= 기준년도, 사업구분 != "기본계획_최초개발")  %>%
  # 행순서 조정
  order_func2() %>%
  arrange(시군, 단위유역, 준공여부, 사업명) %>%
  # 시군 및 유역별 소계 계산
  group_by(시군, 단위유역) %>%
  group_modify(~ .x %>%
                 adorn_totals(
                   where = "row", fill = "소계",
                   na.rm = TRUE, name = "소계"
                 ))



##### 삭감 목표 정리  ----------------------------------------------------------
삭감목표 <-
  read_excel("지역개발부하량 관리/지역개발부하량.xlsx",
             sheet = 3
  ) %>%
  select(시군:삭감_비점, str_c(기준년도, "_점"), str_c(기준년도, "_비점")) %>%
  filter(!단위유역 %in% c("북한D", "임진A", "소계", "합계"))


##### 추진실적 자료 파일 내보내기 ----------------------------------------------
write_xlsx(
  list(
    "5-가. 개발사업 소진현황" = 추진실적_소진현황,
    "5-나. 개발사업 협의현황" = 추진실적_사업목록,
    "5-다. 개발사업 준공현황" = 추진실적_준공현황,
    "5-라. 준공된 개발사업 내역" = 추진실적_준공사업내역,
    "6. 삭감계획 이행실적" = 삭감목표
  ),
  path = "지역개발부하량 관리/Output/누적관리대장_정리(추진실적).xlsx"
)



#####  * 내부 검토용 정리  #####################################################

##### 소진 부하량 정리  --------------------------------------------------------
내부_소진부하량 <- data_내부검토 %>%
  load_cal(.) %>%
  relocate(대상물질) %>%
  rename(소진량_점 = 소진_점, 소진량_비점 = 소진_비점) %>%
  select(-삭감량)

##### 기승인 소진량 정리  ------------------------------------------------------
내부_기승인 <- data_내부검토 %>%
  filter(사업구분 == "기본계획_기승인") %>%
  load_cal(.) %>%
  relocate(대상물질) %>%
  rename(기승인_점 = 소진_점, 기승인_비점 = 소진_비점) %>%
  select(-삭감량)

##### 전년도까지 소진량 정리  --------------------------------------------------
내부_전년도까지 <- data_내부검토 %>%
  filter(사업구분 != "기본계획_기승인", 협의연도 < 기준년도) %>%
  load_cal(.) %>%
  relocate(대상물질) %>%
  rename(이전_점 = 소진_점, 이전_비점 = 소진_비점) %>%
  select(-삭감량)

##### 당해연도 소진량 정리  ----------------------------------------------------
내부_당해연도 <- data_내부검토 %>%
  filter(사업구분 != "기본계획_기승인", 협의연도 == 기준년도) %>%
  load_cal(.) %>%
  relocate(대상물질) %>%
  rename(올해_점 = 소진_점, 올해_비점 = 소진_비점) %>%
  select(-삭감량)


## 기승인 확인
기승인확인 <- base %>%
  select(-시군정렬) %>%
  # 소진 부하량 자료 합치기
  left_join(내부_소진부하량, by = c("대상물질", "시군", "단위유역")) %>%
  # 기승인 부하량 자료 합치기
  left_join(내부_기승인, by = c("대상물질", "시군", "단위유역")) %>%
  # na를 0으로 대체
  mutate_all(~ replace(., is.na(.), 0)) %>%
  # 신규, 잔여량
  mutate(
    기승인확인_점 = 기승인_점.x - 기승인_점.y,
    기승인확인_비점 = 기승인_비점.x - 기승인_비점.y
  )

##### 내부검토용 자료 최종 정리  -----------------------------------------------

### 기본계획 지역개발부하량 자료 및 누적관리대장 자료 합치기
내부_결과 <- base %>%
  select(-c(시군정렬, 기승인_점, 기승인_비점)) %>%
  filter(시군 != "강원도") %>%
  # 부하량 자료 합치기
  left_join(내부_소진부하량, by = c("대상물질", "시군", "단위유역")) %>%
  left_join(내부_기승인, by = c("대상물질", "시군", "단위유역")) %>%
  left_join(내부_전년도까지, by = c("대상물질", "시군", "단위유역")) %>%
  left_join(내부_당해연도, by = c("대상물질", "시군", "단위유역")) %>%
  # left_join(연차별관리계획, by = c("대상물질", "시군", "단위유역")) %>%
  # na를 0으로 대체
  mutate_all(~ replace(., is.na(.), 0)) %>%
  # 신규, 잔여량
  mutate(
    신규_점 = 소진량_점 - 기승인_점,
    신규_비점 = 소진량_비점 - 기승인_비점,
    잔여량_점 = 지역개발_점 - 소진량_점,
    잔여량_비점 = 지역개발_비점 - 소진량_비점
  ) %>%
  # 열 순서 조정
  relocate(c(잔여량_점, 잔여량_비점), .after = 지역개발_비점)



### 강원도 전체 합계 계산
내부_결과 %<>%
  # 강원도 전체 합계행 추가
  bind_rows(내부_결과 %>%
    filter(단위유역 == "소계") %>%
    group_by(대상물질, 단위유역) %>%
    summarise(across(
      c(지역개발_점:신규_비점),
      ~ sum(.)
    ), .groups = "drop") %>%
    mutate(시군 = "강원도", .after = "대상물질")) %>%
  # 강원도 한강수계 합계행 추가
  bind_rows(내부_결과 %>%
    filter(시군 != "태백시(낙동강)", 단위유역 == "소계") %>%
    group_by(대상물질, 단위유역) %>%
    summarise(across(
      c(지역개발_점:신규_비점),
      ~ sum(.)
    ), .groups = "drop") %>%
    mutate(시군 = "강원도(한강)", .after = "대상물질")) %>%
  # 소진율 및 잔여율 추가
  mutate(
    # 연차별계획비교_점 = `2024_점` - 소진량_점,
    # 연차별계획비교_비점 = `2024_비점` - 소진량_비점,
    소진율.전체_점 = round(소진량_점 / 지역개발_점, 4),
    소진율.전체_비점 = round(소진량_비점 / 지역개발_비점, 4),
    잔여율_점 = 1 - 소진율.전체_점,
    잔여율_비점 = 1 - 소진율.전체_비점,
    소진율.신규_점 = round(신규_점 / (지역개발_점 - 기승인_점), 4),
    소진율.신규_비점 = round(신규_비점 / (지역개발_비점 - 기승인_비점), 4)
  ) %>%
  relocate(c(소진율.전체_점:잔여율_비점), .after = 올해_비점) %>%
  order_func(.) %>%
  arrange(대상물질, 시군, 단위유역)


# 소진율.전체_점 = str_c(sprintf("%.1f", 소진율.전체_점), "%"),
# 소진율.전체_비점 = str_c(sprintf("%.1f", 소진율.전체_비점), "%"),
# 소진율.신규_점 = str_c(sprintf("%.1f", 소진율.신규_점), "%"),
# 소진율.신규_비점 = str_c(sprintf("%.1f", 소진율.신규_비점), "%")



### 점/비점 행 기준으로 전환
내부_결과_행기준 <- 내부_결과 %>%
  pivot_longer(
    cols = 지역개발_점:소진율.신규_비점,
    names_to = c("구분", "점비점"),
    names_sep = "_",
  ) %>%
  pivot_wider(
    names_from = 구분,
    values_from = value
  )

##### 협의 현황 정리  ----------------------------------------------------------
## 데이터 정리
협의사업목록 <- data_내부검토 %>%
  select(
    사업구분:단위유역, 사업명, 준공예정연도, 준공여부, 협의상태, 협의연도,
    협의일자, 사업종류
  ) %>%
  # filter(!(사업구분 != "사업취소" & 협의상태 == "사업 취소")) %>%
  mutate(
    사업구분 = case_when(
      사업구분 == "기본계획_기승인" ~ "기승인",
      사업구분 == "기본계획_최초개발" ~ "최초개발",
      사업구분 == "시행계획_최초개발" ~ "기승인",
      TRUE ~ 사업구분
    ),
    환경영향평가 = ifelse(str_detect(사업종류, "환경영향평가"), "O", "X")
  )

## 협의 건수 정리 함수 정의
협의건수_func <- function(data) {
  result <- data %>%
    group_by(사업구분, 시군, 단위유역) %>%
    summarise(개수 = length(사업구분), .groups = "drop") %>%
    pivot_wider(
      names_from = 사업구분,
      values_from = 개수,
      values_fill = 0
    ) %>%
    # 최초개발은 협의 현황에서 제외
    select(
      시군, 단위유역, 기승인, 신규,
      기간외소진, 재협의, 사업취소
    ) %>%
    # 재협의는 중복되므로 총사업건수에서 제외
    mutate(
      총사업건수 = 기승인 + 신규 + 기간외소진 - 사업취소,
      .after = 단위유역
    ) %>%
    # 강원도 총계 계산
    adorn_totals(where = "row", fill = "합계", name = "강원도") %>%
    # 시군별 소계 계산
    group_by(시군) %>%
    group_modify(~ .x %>% adorn_totals(where = "row", name = "소계"))

  ## 시군 및 유역별 순서 정리
  result <- base %>%
    filter(대상물질 == "BOD") %>%
    select(시군, 단위유역) %>%
    left_join(result, by = c("시군", "단위유역")) %>%
    mutate_all(~ replace(., is.na(.), 0))
}

## 협의 건수
협의현황 <- 협의사업목록 %>%
  협의건수_func() %>%
  order_func() %>%
  arrange(시군, 단위유역)

협의현황_영향평가 <- 협의사업목록 %>%
  filter(환경영향평가 == "O") %>%
  협의건수_func()


##### 준공 건수 정리  ----------------------------------------------------------
내부_준공건수 <- data_내부검토 %>%
  select(시군, 단위유역, 사업명, 준공여부) %>%
  filter(준공여부 != "미준공", 준공여부 > 2020) %>%
  group_by(시군, 단위유역, 준공여부) %>%
  summarise(개수 = length(준공여부), .groups = "drop") %>%
  pivot_wider(
    names_from = 준공여부,
    names_sort = TRUE,
    values_from = 개수,
    values_fill = 0
  ) %>%
  # 연도별 합계 계산
  adorn_totals(where = "col", name = "합계") %>%
  relocate(합계, .after = 단위유역) %>%
  # 강원도 총계 계산
  adorn_totals(where = "row", fill = "합계", name = "강원도") %>%
  # 시군별 소계 계산
  group_by(시군) %>%
  group_modify(~ .x %>% adorn_totals(where = "row", name = "소계"))


## 시군 및 유역별 순서 정리
내부_준공건수 <- base %>%
  filter(대상물질 == "BOD") %>%
  select(시군, 단위유역) %>%
  left_join(내부_준공건수, by = c("시군", "단위유역")) %>%
  mutate_all(~ replace(., is.na(.), 0))
# arrange(시군, 단위유역)



##### 내부검토용 자료 파일 내보내기  -------------------------------------------
시군별소진현황 <- 내부_결과 %>%
  select(
    대상물질:소진량_비점,
    소진율.전체_점:잔여율_비점
  ) %>%
  filter(시군 != "강원도(한강)", 단위유역 == "소계") %>%
  select(-단위유역)

시군별소진현황_행기준 <- 내부_결과_행기준 %>%
  select(
    대상물질:소진량,
    소진율.전체:잔여율
  ) %>%
  filter(시군 != "강원도(한강)", 단위유역 == "소계") %>%
  select(-단위유역)

write_xlsx(
  list(
    "시군별소진현황" = 시군별소진현황,
    "시군별소진현황_점비점행기준" = 시군별소진현황_행기준,
    "협의현황" = 협의현황,
    "지역개발부하량" = 내부_결과,
    "지역개발부하량_점비점행기준" = 내부_결과_행기준,
    "협의사업목록" = 협의사업목록,
    "협의현황_영향평가" = 협의현황_영향평가,
    "준공건수" = 내부_준공건수
  ),
  path = "지역개발부하량 관리/Output/누적관리대장_정리(내부검토).xlsx"
)


##### 미준공 사업 현황  --------------------------------------------------------
미준공사업내역 <- data_내부검토 %>%
  select(
    사업구분:단위유역, 사업명, 준공예정연도, 준공여부, 협의상태, 협의연도,
    협의일자, 주관부서:삭감방법
  ) %>%
  filter(협의상태 == "협의", 준공여부 == "미준공", 준공예정연도 <= 기준년도) %>%
  order_func() %>%
  arrange(시군, 단위유역, 준공예정연도)

미준공사업현황 <- 협의사업목록 %>%
  filter(협의상태 == "협의", 준공여부 == "미준공", 준공예정연도 <= 기준년도) %>%
  group_by(시군, 단위유역) %>%
  summarise(개수 = n(), .groups = "drop") %>%
  # 시군별 소계 계산
  group_by(시군) %>%
  group_modify(~ .x %>% adorn_totals(where = "row", name = "소계")) %>%
  order_func() %>%
  arrange(시군, 단위유역)


## 미준공 사업 현황 내보내기
write_xlsx(
  list(
    "미준공 사업 현황" = 미준공사업현황,
    "미준공 사업 내역" = 미준공사업내역
  ),
  path = "지역개발부하량 관리/Output/미준공_사업_현황.xlsx"
)


#####  그래프  #################################################################

## 그래프용 데이터 정리
내부_결과_그래프 <- 내부_결과_행기준 %>%
  filter(!(시군 %in% c("강원도", "강원도(한강)")), 단위유역 == "소계") %>%
  select(대상물질, 시군, 점비점:소진량, 소진율.전체) %>%
  mutate(
    소진율 = 소진율.전체 * 100,
    잔여율 = 100 - 소진율,
    잔여량_1 = 잔여량,
    시군 = 시군 %>%
      str_remove("(시|군)") %>% # "시", "군" 삭제
      str_replace("낙동강", "낙") %>% # "낙동강" → "낙" 수정
      str_replace("한강", "한")
  ) %>% # "한강" → "한" 수정
  pivot_longer(
    cols = 잔여량:소진량,
    names_to = "구분",
    values_to = "부하량"
  ) %>%
  mutate(
    비율 = case_when(
      구분 == "잔여량" ~ 잔여율,
      구분 == "소진량" ~ 소진율
    )
  ) %>%
  select(-c(소진율.전체:잔여율)) %>%
  rename(잔여량 = 잔여량_1) %>%
  mutate(
    라벨위치 = ifelse(구분 == "잔여량",
      부하량 - 부하량 * 0.5,
      잔여량 + 부하량 * 0.5
    )
  )

### 그래프 pdf파일 생성 경로 및 사이즈 설정
pdf("E:/Coding/TMDL/지역개발부하량 관리/Output/Plot/지역개발부하량_소진현황_그래프(8.4x4.75).pdf",
  width = 8.4, height = 4.75
)

## 그래프_BOD 점 소진 및 잔여량 현황 -----
내부_결과_그래프 %>%
  # 연도 선택 및 도전체 합계 삭제
  filter(대상물질 == "BOD", 점비점 == "점") %>%
  ggplot(aes(x = 시군, y = 부하량, fill = 구분)) +
  geom_bar(stat = "identity") +
  # geom_text(aes(y = 라벨위치, label = round(부하량)),
  #           size = 4, check_overlap = TRUE) +
  # geom_text(aes(y = 라벨위치, label = str_c("(", round(비율), "%)")),
  #           vjust = 2, size = 3.5, check_overlap = TRUE) +
  geom_text(aes(y = 지역개발, label = round(지역개발)),
    vjust = -1.5, size = 4.3, color = "blue"
  ) +
  geom_text(aes(y = 지역개발, label = str_c("(", round(비율), "%)")),
    vjust = -0.5, size = 3.5, color = "blue", check_overlap = TRUE
  ) +
  scale_x_discrete(limits = c(
    "춘천", "원주", "강릉", "태백(낙)", "태백(한)",
    "삼척", "홍천", "횡성", "영월", "평창", "정선",
    "철원", "화천", "양구", "인제", "고성"
  )) +
  scale_y_continuous(
    name = "BOD 점 지역개발부하량(kg/일)",
    breaks = seq(0, 400, by = 50), limits = c(0, 420)
  ) +
  theme_few(base_family = "notosanskr") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black", angle = 30, hjust = 1),
    axis.text.y = element_text(size = 11, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "inside",
    legend.position.inside = c(0.80, 0.80)
  )


## 그래프_BOD 비점 소진 및 잔여량 현황 -----
내부_결과_그래프 %>%
  # 연도 선택 및 도전체 합계 삭제
  filter(대상물질 == "BOD", 점비점 == "비점") %>%
  ggplot(aes(x = 시군, y = 부하량, fill = 구분)) +
  geom_bar(stat = "identity") +
  # geom_text(aes(y = 라벨위치, label = round(부하량)),
  #           size = 4, check_overlap = TRUE) +
  # geom_text(aes(y = 라벨위치, label = str_c("(", round(비율), "%)")),
  #           vjust = 2, size = 3.5, check_overlap = TRUE) +
  geom_text(aes(y = 지역개발, label = round(지역개발)),
    vjust = -1.5, size = 4.3, color = "blue"
  ) +
  geom_text(aes(y = 지역개발, label = str_c("(", round(비율), "%)")),
    vjust = -0.5, size = 3.5, color = "blue", check_overlap = TRUE
  ) +
  scale_x_discrete(limits = c(
    "춘천", "원주", "강릉", "태백(낙)", "태백(한)",
    "삼척", "홍천", "횡성", "영월", "평창", "정선",
    "철원", "화천", "양구", "인제", "고성"
  )) +
  scale_y_continuous(
    name = "BOD 비점 지역개발부하량(kg/일)",
    breaks = seq(0, 1200, by = 200), limits = c(0, 1200)
  ) +
  theme_few(base_family = "notosanskr") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black", angle = 30, hjust = 1),
    axis.text.y = element_text(size = 11, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "inside",
    legend.position.inside = c(0.90, 0.80)
  )

## 그래프_TP 점 소진 및 잔여량 현황 -----
내부_결과_그래프 %>%
  # 연도 선택 및 도전체 합계 삭제
  filter(대상물질 == "TP", 점비점 == "점") %>%
  ggplot(aes(x = 시군, y = 부하량, fill = 구분)) +
  geom_bar(stat = "identity") +
  # geom_text(aes(y = 라벨위치, label = round(부하량)),
  #           size = 4, check_overlap = TRUE) +
  # geom_text(aes(y = 라벨위치, label = str_c("(", round(비율), "%)")),
  #           vjust = 2, size = 3.5, check_overlap = TRUE) +
  geom_text(aes(y = 지역개발, label = round(지역개발)),
    vjust = -1.5, size = 4.3, color = "blue"
  ) +
  geom_text(aes(y = 지역개발, label = str_c("(", round(비율), "%)")),
    vjust = -0.5, size = 3.5, color = "blue", check_overlap = TRUE
  ) +
  scale_x_discrete(limits = c(
    "춘천", "원주", "강릉", "태백(낙)", "태백(한)",
    "삼척", "홍천", "횡성", "영월", "평창", "정선",
    "철원", "화천", "양구", "인제", "고성"
  )) +
  scale_y_continuous(
    name = "T-P 점 지역개발부하량(kg/일)",
    breaks = seq(0, 25, by = 5), limits = c(0, 25)
  ) +
  theme_few(base_family = "notosanskr") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black", angle = 30, hjust = 1),
    axis.text.y = element_text(size = 11, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "inside",
    legend.position.inside = c(0.80, 0.80)
  )


## 그래프_TP 비점 소진 및 잔여량 현황 -----
내부_결과_그래프 %>%
  # 연도 선택 및 도전체 합계 삭제
  filter(대상물질 == "TP", 점비점 == "비점") %>%
  ggplot(aes(x = 시군, y = 부하량, fill = 구분)) +
  geom_bar(stat = "identity") +
  # geom_text(aes(y = 라벨위치, label = round(부하량)),
  #           size = 4, check_overlap = TRUE) +
  # geom_text(aes(y = 라벨위치, label = str_c("(", round(비율), "%)")),
  #           vjust = 2, size = 3.5, check_overlap = TRUE) +
  geom_text(aes(y = 지역개발, label = round(지역개발)),
    vjust = -1.5, size = 4.3, color = "blue"
  ) +
  geom_text(aes(y = 지역개발, label = str_c("(", round(비율), "%)")),
    vjust = -0.5, size = 3.5, color = "blue", check_overlap = TRUE
  ) +
  scale_x_discrete(limits = c(
    "춘천", "원주", "강릉", "태백(낙)", "태백(한)",
    "삼척", "홍천", "횡성", "영월", "평창", "정선",
    "철원", "화천", "양구", "인제", "고성"
  )) +
  scale_y_continuous(
    name = "T-P 비점 지역개발부하량(kg/일)",
    breaks = seq(0, 100, by = 20), limits = c(0, 100)
  ) +
  theme_few(base_family = "notosanskr") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black", angle = 30, hjust = 1),
    axis.text.y = element_text(size = 11, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "inside",
    legend.position.inside = c(0.90, 0.80)
  )

dev.off()


# ______________________________________________________________________________
### 점전환 -----
점전환기준 <- tibble(
  단위유역 = c(
    "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A",
    "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C", "홍천A",
    "한탄A", "제천A", "한강B", "한강D", "북한D", "임진A", "한탄B", "낙본A"
  ),
  # 유량구간 = c(
  #   "저수기", "저수기", "평수기", "평수기", "평수기", "저수기",
  #   "평수기", "평수기", "저수기", "저수기", "평수기", "저수기", "저수기",
  #   "평수기", "평수기", "평수기", "평수기", "저수기", "평수기", "평수기",
  #   "저수기", "저수기", "저수기"
  # ),
  전환기준 = c(
    "0.15", "0.15", "0.50", "0.50", "0.50", "0.15", "0.50", "0.50",
    "0.15", "0.15", "0.50", "0.15", "0.15", "0.50", "0.50", "0.50",
    "0.50", "0.15", "0.50", "0.50", "0.15", "0.15", "0.15"
  )
) %>% mutate(전환기준 = as.numeric(전환기준))


점전환 <- 내부_결과 %>%
  select(대상물질:소진량_비점) %>%
  filter(단위유역 != "소계") %>%
  # 점비점 전환
  left_join(점전환기준, by = "단위유역") %>%
  mutate(
    지역개발_점전환 = 지역개발_점 + 지역개발_비점 * ifelse(대상물질 == "BOD", 0.15, 전환기준),
    잔여량_점전환 = 잔여량_점 + 잔여량_비점 * ifelse(대상물질 == "BOD", 0.15, 전환기준),
    소진량_점전환 = 소진량_점 + 소진량_비점 * ifelse(대상물질 == "BOD", 0.15, 전환기준)
  ) %>%
  # 강원도 총계 계산
  group_by(대상물질) %>%
  group_modify(~ .x %>% adorn_totals(where = "row", fill = "합계", name = "강원도전체")) %>%
  ungroup() %>%
  mutate(소진율 = round(소진량_점전환 / 지역개발_점전환, 4))
