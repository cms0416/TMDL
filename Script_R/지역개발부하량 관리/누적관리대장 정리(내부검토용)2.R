#####  라이브러리 로드  ########################################################
library(tidyverse)
library(data.table)
library(readxl)
library(writexl)
################################################################################

## 지역개발부하량 및 기승인 기초자료(기본계획) 불러오기
base <- read_excel("Data/지역개발부하량.xlsx")

## 파일 경로 목록 만들기
files <- list.files(
  path = "Data/누적관리대장/",
  pattern = "*.xls", full.names = T
)

## 파일 취합 할 데이터 프레임 사전 생성
data <- data.frame()

## 모든 누적관리대장 파일에서 "협의"가 포함안된 시트 자료 합치기
for (i in 1:length(files)) {
  path <- files[i]

  temp <- excel_sheets(path)[str_detect(excel_sheets(path), "협의", negate = T)] %>% # str_detect : 특정 문자열(협의) 포함여부 확인
    map_dfr(read_excel, path = path, skip = 3, col_names = F) # map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환

  data <- rbind(data, temp)
}

## 필요 없는 열 제거 및 열 이름 설정
data1 <- data %>% 
  select(1:5, 9:12, 54, 55, 60, 61, 78, 120, 121, 126, 127, 147, 148) %>% 
  set_names(c(
    "관리자번호", "시군", "단위유역", "할당일자", "사업명", "착공연도", "완공연도",
    "준공여부", "삭감량_BOD", "BOD지역개발_점", "BOD지역개발_비점", "BOD소진_점",
    "BOD소진_비점", "삭감량_TP", "TP지역개발_점", "TP지역개발_비점", "TP소진_점",
    "TP소진_비점", "협의일자", "협의상태"
  ))
  
  

## 날짜 서식 변경
data1$할당일자 <- gsub("\\.", "-", data1$할당일자)
data1$협의일자 <- gsub("\\.", "-", data1$협의일자)

## 데이터 형식 변경(문자 → 날짜 및 숫자), 협의연도 추가, 시군명만 추출
data2 <- data1 %>%
  mutate_at(vars(할당일자, 협의일자), as.Date) %>%
  mutate_at(vars(착공연도, 완공연도, 삭감량_BOD:TP소진_비점), as.numeric) %>%
  mutate(할당연도 = year(할당일자)) %>% # 협의연도 추가(협의일자가 없는 경우 할당연도로 추가)
  mutate(협의연도 = year(협의일자)) %>%
  mutate(계산연도 = ifelse(관리자번호 == "기본계획_최초개발", 2019,
    ifelse(관리자번호 == "기본계획_기승인", 2020,
      ifelse(is.na(협의일자), 할당연도, 협의연도)
    )
  )) %>%
  mutate(시군 = substr(시군, 5, 7)) # 시군명만 추출


##### 부하량 정리 #####

## 소진 부하량 정리
load <- data2 %>%
  # filter(계산연도 < 2023) %>%
  group_by(시군, 단위유역) %>%
  summarise_at(vars(
    BOD소진_점, BOD소진_비점,
    TP소진_점, TP소진_비점
  ), sum)

## BOD
load_bod <- load %>%
  select(-TP소진_점, -TP소진_비점) %>%
  mutate(대상물질 = "BOD") %>%
  select(시군, 대상물질, 단위유역, everything()) %>%
  rename(소진량_점 = BOD소진_점, 소진량_비점 = BOD소진_비점)

load_bod_subtotal <- load_bod %>%
  group_by(시군, 대상물질) %>%
  summarise_at(vars(소진량_점, 소진량_비점), sum) %>%
  mutate(단위유역 = "소계") %>%
  select(시군, 대상물질, 단위유역, everything())

load_bod <- rbind(load_bod, load_bod_subtotal)

## T-P
load_tp <- load %>%
  select(-BOD소진_점, -BOD소진_비점) %>%
  mutate(대상물질 = "T-P") %>%
  select(시군, 대상물질, 단위유역, everything()) %>%
  rename(소진량_점 = TP소진_점, 소진량_비점 = TP소진_비점)

load_tp_subtotal <- load_tp %>%
  group_by(시군, 대상물질) %>%
  summarise_at(vars(소진량_점, 소진량_비점), sum) %>%
  mutate(단위유역 = "소계") %>%
  select(시군, 대상물질, 단위유역, everything())

load_tp <- rbind(load_tp, load_tp_subtotal)


## 합치기
load_all <- rbind(load_bod, load_tp)

## 할당총괄표 지역개발부하량 및 기승인 자료 기준 데이터 정리
base1 <- base %>%
  # 소진 부하량 자료 합치기
  left_join(load_all, by = c("시군", "대상물질", "단위유역")) %>%
  # na를 0으로 대체
  mutate_all(~ replace(., is.na(.), 0)) %>%
  # 신규, 잔여량, 소진율 추가
  mutate(
    신규_점 = 소진량_점 - 기승인_점, 
    신규_비점 = 소진량_비점 - 기승인_비점, 
    잔여량_점 = 지역개발_점 - 소진량_점,
    잔여량_비점 = 지역개발_비점 - 소진량_비점, 
    소진율_점 = round(소진량_점 / 지역개발_점, 3),
    소진율_비점 = round(소진량_비점 / 지역개발_비점, 3)
  ) %>%
  # 열 순서 조정
  relocate(c(신규_점, 신규_비점), .after = 기승인_비점) %>% 
  # 시군 및 단위유역 순서 정리
  mutate_at(vars(시군, 단위유역), as.factor) %>%
  mutate(시군 = factor(시군, levels = c(
    "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군",
    "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군",
    "양구군", "인제군", "고성군"
  ))) %>%
  mutate(단위유역 = factor(단위유역, levels = c(
    "소계", "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A",
    "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C", "홍천A",
    "한탄A", "제천A", "한강B", "한강D", "북한D", "임진A", "한탄B", "낙본A"
  ))) %>% 
  arrange(대상물질, 시군, 단위유역) %>% 
  rowid_to_column(var = "ID")

### 파일 내보내기
write_xlsx(base1, path = "Output/Data/누적관리대장 내부검토.xlsx")



################  기승인 소진량 검토  ##########################################

## 기승인 소진 부하량 정리
load_approved <- data2 %>%
  filter(관리자번호 == "기본계획_기승인") %>%
  group_by(시군, 단위유역) %>%
  summarise_at(vars(
    BOD소진_점, BOD소진_비점,
    TP소진_점, TP소진_비점
  ), sum) %>% 
  rename(기승인소진_점.BOD = BOD소진_점, 기승인소진_비점.BOD = BOD소진_비점,
         기승인소진_점.TP = TP소진_점, 기승인소진_비점.TP = TP소진_비점) %>% 
  pivot_longer(
    cols = 기승인소진_점.BOD:기승인소진_비점.TP,
    names_to = c("name", "대상물질"),
    names_sep = '\\.'
  ) %>% 
  pivot_wider(
    names_from = name,
    values_from = value
  ) %>% 
  mutate(대상물질 = case_when(
    대상물질 == "BOD" ~ "BOD",
    대상물질 == "TP" ~ "T-P"
  ))

load_approved <- load_approved %>%   
  rbind(load_approved %>%
          group_by(시군, 대상물질) %>%
          summarise_at(vars(기승인소진_점, 기승인소진_비점), sum) %>%
          mutate(단위유역 = "소계") %>%
          select(시군, 대상물질, 단위유역, everything()))



base2 <- base1 %>% 
  left_join(load_approved, by = c("시군", "대상물질", "단위유역")) %>% 
  # na를 0으로 대체
  mutate_all(~ replace(., is.na(.), 0)) %>% 
  mutate(기승인점검_점 = round(기승인소진_점 - 기승인_점, 3),
         기승인점검_비점 = round(기승인소진_비점 - 기승인_비점, 3)) %>% 
  select(-c(신규_점:소진율_비점))

