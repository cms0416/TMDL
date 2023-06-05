################################################################################
## 라이브러리 로드
library(tidyverse)
library(data.table)
library(readxl)
library(writexl)
################################################################################


### 지역개발부하량 및 기승인 기초자료(기본계획) 정리  --------------------------
## 파일 불러오기기
base <- read_excel("지역개발부하량 관리/지역개발부하량.xlsx") %>%
  rowid_to_column(var = "ID")

## 시행계획 수립 유역 및 해당 시군 소계 삭제
base1 <- base %>%
  filter(!단위유역 %in% c("북한D", "임진A")) %>%
  filter(!(시군 %in% c("춘천시", "철원군") & 단위유역 == "소계"))

## 시행계획 수립 시군(춘천시, 철원군) 소계 재산정
base1 <- base1 %>%
  bind_rows(base1 %>%
    filter(시군 %in% c("춘천시", "철원군")) %>%
    group_by(대상물질, 시군) %>%
    summarise_at(vars(지역개발_점:기승인_비점), sum) %>%
    mutate(단위유역 = "소계") %>%
    relocate(단위유역, .after = 시군) %>%
    left_join(base %>% select(ID:단위유역),
      by = c("대상물질", "시군", "단위유역")
    )) %>%
  arrange(ID) %>%
  select(-ID)

### ----------------------------------------------------------------------------


## 누적관리대장 파일 불러오기
data <- read_excel("지역개발부하량 관리/누적관리대장_강원도(230510).xls",
  skip = 3, col_names = F
)

## 필요 없는 열 제거 및 열 이름 설정
data1 <- data %>%
  select(1:5, 9:12, 54, 55, 60, 61, 78, 120, 121, 126, 127, 147, 148) %>%
  set_names(c(
    "관리자번호", "시군", "단위유역", "할당일자", "사업명", "착공연도", "완공연도",
    "준공여부", "BOD_삭감량", "BOD_지역개발점", "BOD_지역개발비점", "BOD_소진점",
    "BOD_소진비점", "TP_삭감량", "TP_지역개발점", "TP_지역개발비점", "TP_소진점",
    "TP_소진비점", "협의일자", "협의상태"
  ))

## 날짜 서식 변경
data1$할당일자 <- gsub("\\.", "-", data1$할당일자)
data1$협의일자 <- gsub("\\.", "-", data1$협의일자)

## 데이터 형식 변경(문자 → 날짜 및 숫자), 협의연도 추가, 시군명만 추출
data2 <- data1 %>%
  mutate_at(vars(할당일자, 협의일자), as.Date) %>%
  mutate_at(vars(착공연도, 완공연도, BOD_삭감량:TP_소진비점), as.numeric) %>%
  # 협의연도 추가(협의일자가 없는 경우 할당연도로 추가)
  mutate(할당연도 = year(할당일자), 
         협의연도 = year(협의일자),
         계산연도 = case_when(
           관리자번호 == "기본계획_최초개발" ~ 2019,
           관리자번호 == "기본계획_기승인" ~ 2020,
           is.na(협의일자) ~ 할당연도,
           TRUE ~ 협의연도
         ),
         협의상태 = ifelse(관리자번호 == "기본계획_기승인" & 협의상태 == "할당", "협의", 협의상태)
    ) %>%
  mutate(시군 = str_remove(시군, "강원도 ")) %>% # 시군명만 추출
  # 시행계획 수립 유역(북한D, 임진A) 제외
  filter(!단위유역 %in% c("임진A"))


##### 취소된 사업 정리(확인용) #####
canceled <- data2 %>%
  filter(협의상태 == "사업 취소")


##### 협의 부하량(누적) 정리 ##### ---------------------------------------------
load_old <- data2 %>%
  filter(계산연도 <= 2021, 협의상태 != "할당") %>%
  group_by(시군, 단위유역) %>%
  summarise_at(vars(
    BOD_소진점, BOD_소진비점,
    TP_소진점, TP_소진비점
  ), sum)

## 대상물질별 정리
load_old <- load_old %>%
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

## 소계 계산
load_old <- load_old %>%
  bind_rows(load_old %>%
    group_by(대상물질, 시군) %>%
    summarise_at(vars(협의부하량_점, 협의부하량_비점),
      ~ sum(.),
      .groups = "drop"
    ) %>%
    mutate(단위유역 = "소계"))


#####  사용부하량 정리  ###### -------------------------------------------------
load_new <- data2 %>%
  filter(계산연도 == 2022, 협의상태 != "할당") %>%
  group_by(시군, 단위유역) %>%
  summarise_at(vars(
    BOD_소진점, BOD_소진비점,
    TP_소진점, TP_소진비점
  ), sum)

## 대상물질별 정리
load_new <- load_new %>%
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

## 소계 계산
load_new <- load_new %>%
  bind_rows(load_new %>%
              group_by(대상물질, 시군) %>%
              summarise_at(vars(사용부하량_점, 사용부하량_비점),
                           ~ sum(.),
                           .groups = "drop"
              ) %>%
              mutate(단위유역 = "소계"))


#####  당해년도 준공현황 정리  ###### ------------------------------------------
complete <- data2 %>%
  filter(준공여부 == 2022) %>%
  group_by(시군, 단위유역) %>%
  summarise_at(vars(
    BOD_소진점, BOD_소진비점, BOD_삭감량,
    TP_소진점, TP_소진비점, TP_삭감량
  ), sum) 

## 대상물질별 정리
complete <- complete %>%
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

## 소계 계산
complete <- complete %>%
  bind_rows(complete %>%
              group_by(대상물질, 시군) %>%
              summarise_at(vars(준공_점, 준공_비점, 삭감량),
                           ~ sum(.),
                           .groups = "drop"
              ) %>%
              mutate(단위유역 = "소계"))


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
  # 당해년도 준공현황 합치기
  left_join(complete, by = c("시군", "대상물질", "단위유역")) %>%
  mutate_all(~ replace(., is.na(.), 0)) # na를 0으로 대체



##### 파일 내보내기 ##### ------------------------------------------------------
write_xlsx(result, path = "지역개발부하량 관리/Output/누적관리대장 정리(추진실적).csv")
