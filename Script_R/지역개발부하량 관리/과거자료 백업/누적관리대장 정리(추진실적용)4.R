#####  라이브러리 로드  #########################################################
library(tidyverse)
library(magrittr)
library(data.table)
library(readxl)
library(writexl)



#####  지역개발부하량 및 기승인 기초자료(기본계획) 정리  ########################

## 파일 불러오기기
base <- read_excel("지역개발부하량 관리/지역개발부하량.xlsx") %>%
  rowid_to_column(var = "ID")

## 시행계획 수립 유역 및 해당 시군 합계 삭제
base1 <- base %>%
  filter(!단위유역 %in% c("북한D", "임진A")) %>%
  filter(!(시군 %in% c("춘천시", "철원군") & 단위유역 == "합계"))

## 시행계획 수립 시군(춘천시, 철원군) 합계 재산정
base1 <- base1 %>%
  bind_rows(base1 %>%
    filter(시군 %in% c("춘천시", "철원군")) %>%
    group_by(대상물질, 시군) %>%
    summarise_at(vars(지역개발_점:기승인_비점), sum) %>%
    mutate(단위유역 = "합계") %>%
    relocate(단위유역, .after = 시군) %>%
    left_join(base %>% select(ID:단위유역), 
              by = c("대상물질", "시군", "단위유역")
    )) %>%
  arrange(ID) %>%
  select(-c(ID, 기승인_점, 기승인_비점))



#####  누적관리대장 정리  #######################################################

## 누적관리대장 파일 불러오기
file <- list.files(
  path = "지역개발부하량 관리/", pattern = "누적관리대장*", full.names = T
)

data <- read_excel(file, sheet = 2, skip = 3, col_names = F)

## 삭감방법 열 추가 / 필요 없는 열 제거 / 열 이름 설정
data1 <- data %>%
  # 삭감방법 관련 열 값이 NA인 경우 대체값 입력
  mutate(across(13, ~ replace(., is.na(.), ""))) %>%
  mutate(across(c(13, 17, 21, 25, 29, 33, 37, 41, 45, 49), ~ replace(., is.na(.), "/"))) %>%
  # 삭감방법 열 모두 합치고, NA 대체값 삭제
  mutate(
    삭감방법 =
      str_c(...13, ...17, ...21, ...25, ...29,
        ...33, ...37, ...41, ...45, ...49,
        sep = ", "
      ) %>%
        str_remove_all(., ", /") %>%
        # 합친 삭감방법 문자열 시작이 ","인 경우 삭제
        ifelse(str_sub(., 1, 1) == ",", str_remove(., ", "), .)
  ) %>%
  # 필요한 열 선택
  select(
    1:5, 9:12, 54, 55, 60, 61, 78, 120, 121, 126, 127, 147, 148, 삭감방법
  ) %>%
  # 열 이름 설정
  set_names(c(
    "관리자번호", "시군", "단위유역", "할당일자", "사업명", "착공연도",
    "완공연도", "준공여부", "BOD_삭감량", "BOD_지역개발점", "BOD_지역개발비점",
    "BOD_소진점", "BOD_소진비점", "TP_삭감량", "TP_지역개발점", "TP_지역개발비점",
    "TP_소진점", "TP_소진비점", "협의일자", "협의상태", "삭감방법"
  ))

## 날짜 서식 변경
data1$할당일자 <- gsub("\\.", "-", data1$할당일자)
data1$협의일자 <- gsub("\\.", "-", data1$협의일자)

## 데이터 형식 변경(문자 → 날짜 및 숫자), 협의연도 추가, 시군명만 추출
data2 <- data1 %>%
  mutate_at(vars(할당일자, 협의일자), as.Date) %>%
  mutate_at(vars(착공연도, 완공연도, BOD_삭감량:TP_소진비점), as.numeric) %>%
  # 협의연도 추가(협의일자가 없는 경우 할당연도로 추가)
  mutate(
    할당연도 = year(할당일자),
    협의연도 = year(협의일자),
    협의연도 = ifelse(is.na(협의일자), 할당연도, year(협의일자)),
    시군 = str_remove(시군, "강원도 ")
  ) %>%
  # 시행계획 수립 유역(북한D, 임진A) 제외
  filter(!단위유역 %in% c("임진A"))



#####  추진실적 정리  #########################################################

##### 협의 부하량(누적) 정리 ##### ---------------------------------------------
load_old <- data2 %>%
  # 당해연도(2022년) 이전까지 협의된 사업 필터
  filter(협의연도 <= 2021, 협의상태 == "협의") %>%
  group_by(시군, 단위유역) %>%
  summarise_at(vars(
    BOD_소진점, BOD_소진비점,
    TP_소진점, TP_소진비점
  ), sum)

## 대상물질별 정리
load_old %<>%
  pivot_longer(
    cols = BOD_소진점:TP_소진비점,
    names_to = c("대상물질", "항목"),
    names_sep = "_",
    values_to = "수치"
  ) %>%
  pivot_wider(
    names_from = 항목,
    values_from = 수치
  ) %>%
  rename(협의부하량_점 = 소진점, 협의부하량_비점 = 소진비점)

## 합계 계산
load_old %<>%
  bind_rows(load_old %>%
    group_by(대상물질, 시군) %>%
    summarise_at(vars(협의부하량_점, 협의부하량_비점),
      ~ sum(.),
      .groups = "drop"
    ) %>%
    mutate(단위유역 = "합계"))


#####  사용부하량 정리  ###### -------------------------------------------------
load_new <- data2 %>%
  # 당해연도(2022년)에 협의된 사업 필터
  filter(협의연도 == 2022, 협의상태 == "협의") %>%
  group_by(시군, 단위유역) %>%
  summarise_at(vars(
    BOD_소진점, BOD_소진비점,
    TP_소진점, TP_소진비점
  ), sum)

## 대상물질별 정리
load_new %<>%
  pivot_longer(
    cols = BOD_소진점:TP_소진비점,
    names_to = c("대상물질", "항목"),
    names_sep = "_",
    values_to = "수치"
  ) %>%
  pivot_wider(
    names_from = 항목,
    values_from = 수치
  ) %>%
  rename(사용부하량_점 = 소진점, 사용부하량_비점 = 소진비점)

## 합계 계산
load_new %<>%
  bind_rows(load_new %>%
    group_by(대상물질, 시군) %>%
    summarise_at(vars(사용부하량_점, 사용부하량_비점),
      ~ sum(.),
      .groups = "drop"
    ) %>%
    mutate(단위유역 = "합계"))


#####  당해 연도 준공현황 정리  ###### -----------------------------------------
complete <- data2 %>%
  # 당해연도(2022년) 까지 누적 준공 현황 필터
  filter(준공여부 <= 2022) %>%
  group_by(시군, 단위유역) %>%
  summarise_at(vars(
    BOD_소진점, BOD_소진비점, BOD_삭감량,
    TP_소진점, TP_소진비점, TP_삭감량
  ), sum)

## 대상물질별 정리
complete %<>%
  pivot_longer(
    cols = BOD_소진점:TP_삭감량,
    names_to = c("대상물질", "항목"),
    names_sep = "_",
    values_to = "수치"
  ) %>%
  pivot_wider(
    names_from = 항목,
    values_from = 수치
  ) %>%
  rename(준공_점 = 소진점, 준공_비점 = 소진비점)

## 합계 계산
complete %<>%
  bind_rows(complete %>%
    group_by(대상물질, 시군) %>%
    summarise_at(vars(준공_점, 준공_비점, 삭감량),
      ~ sum(.),
      .groups = "drop"
    ) %>%
    mutate(단위유역 = "합계"))


##### 기본계획 자료와 합치기 ##### ---------------------------------------------
result <- base1 %>%
  # 협의 부하량 합치기
  left_join(load_old, by = c("시군", "대상물질", "단위유역")) %>%
  mutate_all(~ replace(., is.na(.), 0)) %>% # na를 0으로 대체
  mutate(
    협의가능량_점 = 지역개발_점 - 협의부하량_점,
    협의가능량_비점 = 지역개발_비점 - 협의부하량_비점
  ) %>%
  # 사용 부하량 합치기
  left_join(load_new, by = c("시군", "대상물질", "단위유역")) %>%
  mutate_all(~ replace(., is.na(.), 0)) %>% # na를 0으로 대체
  mutate(
    잔여부하량_점 = 협의가능량_점 - 사용부하량_점,
    잔여부하량_비점 = 협의가능량_비점 - 사용부하량_비점,
    누적부하량_점 = 협의부하량_점 + 사용부하량_점,
    누적부하량_비점 = 협의부하량_비점 + 사용부하량_비점
  ) %>%
  # 당해 연도 준공현황 합치기
  left_join(complete, by = c("시군", "대상물질", "단위유역")) %>%
  # na를 0으로 대체
  mutate_all(~ replace(., is.na(.), 0)) %>%
  # 시군 순서로 조정
  mutate(
    시군 = factor(시군, levels = c(
      "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군",
      "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군",
      "양구군", "인제군", "고성군"
    ))
  ) %>%
  arrange(시군) %>%
  # 시군을 제일 앞으로
  relocate(시군)


##### 사업 목록 정리 ##### ------------------------------------------------------
project_list <- data2 %>%
  select(
    관리자번호:단위유역, 사업명, 협의일자, 완공연도,
    BOD_소진점, BOD_소진비점, TP_소진점, TP_소진비점,
    삭감방법, BOD_삭감량, TP_삭감량,
    할당일자, 착공연도, 준공여부, 협의상태, 협의연도
  ) %>%
  mutate(구분 = case_when(
    협의연도 > 2022 | 관리자번호 == "기본계획_최초개발" ~ "제외",
    협의연도 < 2022 ~ "협의 부하량",
    협의연도 == 2022 ~ "사용 부하량"
  ))


##### 파일 내보내기 ##### ------------------------------------------------------
write_xlsx(result, path = "지역개발부하량 관리/Output/누적관리대장 정리(추진실적).csv")
write_xlsx(project_list, path = "지역개발부하량 관리/Output/지역개발사업 목록(추진실적).csv")

