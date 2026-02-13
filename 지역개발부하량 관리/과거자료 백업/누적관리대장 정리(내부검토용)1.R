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


##### 협의 부하량 정리 #####

load_old <- data2 %>%
  filter(계산연도 < 2022) %>%
  group_by(시군, 단위유역) %>%
  summarise_at(vars(
    BOD소진_점, BOD소진_비점,
    TP소진_점, TP소진_비점
  ), sum)
# %>% arrange(시군, 단위유역)

# BOD
load_old_bod <- load_old %>%
  select(-TP소진_점, -TP소진_비점) %>%
  mutate(대상물질 = "BOD") %>%
  select(시군, 대상물질, 단위유역, everything()) %>%
  rename(협의부하량_점 = BOD소진_점, 협의부하량_비점 = BOD소진_비점)

load_old_bod_subtotal <- load_old_bod %>%
  group_by(시군, 대상물질) %>%
  summarise_at(vars(협의부하량_점, 협의부하량_비점), sum) %>%
  mutate(단위유역 = "소계") %>%
  select(시군, 대상물질, 단위유역, everything())

load_old_bod <- rbind(load_old_bod, load_old_bod_subtotal)

# load_old_bod <- load_old_bod %>% arrange(시군, 단위유역)

# T-P
load_old_tp <- load_old %>%
  select(-BOD소진_점, -BOD소진_비점) %>%
  mutate(대상물질 = "T-P") %>%
  select(시군, 대상물질, 단위유역, everything()) %>%
  rename(협의부하량_점 = TP소진_점, 협의부하량_비점 = TP소진_비점)

load_old_tp_subtotal <- load_old_tp %>%
  group_by(시군, 대상물질) %>%
  summarise_at(vars(협의부하량_점, 협의부하량_비점), sum) %>%
  mutate(단위유역 = "소계") %>%
  select(시군, 대상물질, 단위유역, everything())

load_old_tp <- rbind(load_old_tp, load_old_tp_subtotal)


# 합치기
load_old_all <- rbind(load_old_bod, load_old_tp)
base1 <- base %>%
  left_join(load_old_all, by = c("시군", "대상물질", "단위유역")) %>%
  mutate_all(~ replace(., is.na(.), 0)) %>%
  # na를 0으로 대체
  mutate(
    협의가능량_점 = 지역개발_점 - 협의부하량_점,
    협의가능량_비점 = 지역개발_비점 - 협의부하량_비점
  )



#####  사용부하량 정리  ######

load_new <- data2 %>%
  filter(계산연도 == 2022) %>%
  group_by(시군, 단위유역) %>%
  summarise_at(vars(
    BOD소진_점, BOD소진_비점,
    TP소진_점, TP소진_비점
  ), sum)
# %>% arrange(시군, 단위유역)

# BOD
load_new_bod <- load_new %>%
  select(-TP소진_점, -TP소진_비점) %>%
  mutate(대상물질 = "BOD") %>%
  select(시군, 대상물질, 단위유역, everything()) %>%
  rename(사용부하량_점 = BOD소진_점, 사용부하량_비점 = BOD소진_비점)

load_new_bod_subtotal <- load_new_bod %>%
  group_by(시군, 대상물질) %>%
  summarise_at(vars(사용부하량_점, 사용부하량_비점), sum) %>%
  mutate(단위유역 = "소계") %>%
  select(시군, 대상물질, 단위유역, everything())

load_new_bod <- rbind(load_new_bod, load_new_bod_subtotal)

# T-P
load_new_tp <- load_new %>%
  select(-BOD소진_점, -BOD소진_비점) %>%
  mutate(대상물질 = "T-P") %>%
  select(시군, 대상물질, 단위유역, everything()) %>%
  rename(사용부하량_점 = TP소진_점, 사용부하량_비점 = TP소진_비점)

load_new_tp_subtotal <- load_new_tp %>%
  group_by(시군, 대상물질) %>%
  summarise_at(vars(사용부하량_점, 사용부하량_비점), sum) %>%
  mutate(단위유역 = "소계") %>%
  select(시군, 대상물질, 단위유역, everything())

load_new_tp <- rbind(load_new_tp, load_new_tp_subtotal)

# 합치기
load_new_all <- rbind(load_new_bod, load_new_tp)
base2 <- base1 %>%
  left_join(load_new_all, by = c("시군", "대상물질", "단위유역")) %>%
  mutate_all(~ replace(., is.na(.), 0)) %>%
  # na를 0으로 대체
  mutate(
    잔여부하량_점 = 협의가능량_점 - 사용부하량_점,
    잔여부하량_비점 = 협의가능량_비점 - 사용부하량_비점
  )



#####  당해년도 준공현황 정리  ######

complete_new <- data2 %>%
  filter(준공여부 == 2022) %>%
  group_by(시군, 단위유역) %>%
  summarise_at(vars(
    BOD소진_점, BOD소진_비점, 삭감량_BOD,
    TP소진_점, TP소진_비점, 삭감량_TP
  ), sum)

# BOD
complete_new_bod <- complete_new %>%
  select(-TP소진_점, -TP소진_비점, -삭감량_TP) %>%
  mutate(대상물질 = "BOD") %>%
  select(시군, 대상물질, 단위유역, everything()) %>%
  rename(준공_점 = BOD소진_점, 준공_비점 = BOD소진_비점, 삭감량 = 삭감량_BOD)

complete_new_bod_subtotal <- complete_new_bod %>%
  group_by(시군, 대상물질) %>%
  summarise_at(vars(준공_점, 준공_비점, 삭감량), sum) %>%
  mutate(단위유역 = "소계") %>%
  select(시군, 대상물질, 단위유역, everything())

complete_new_bod <- rbind(complete_new_bod, complete_new_bod_subtotal)

# T-P
complete_new_tp <- complete_new %>%
  select(-BOD소진_점, -BOD소진_비점, -삭감량_BOD) %>%
  mutate(대상물질 = "T-P") %>%
  select(시군, 대상물질, 단위유역, everything()) %>%
  rename(준공_점 = TP소진_점, 준공_비점 = TP소진_비점, 삭감량 = 삭감량_TP)

complete_new_tp_subtotal <- complete_new_tp %>%
  group_by(시군, 대상물질) %>%
  summarise_at(vars(준공_점, 준공_비점, 삭감량), sum) %>%
  mutate(단위유역 = "소계") %>%
  select(시군, 대상물질, 단위유역, everything())

complete_new_tp <- rbind(complete_new_tp, complete_new_tp_subtotal)

# 합치기
complete_new_all <- rbind(complete_new_bod, complete_new_tp)

base3 <- base2 %>%
  mutate(
    누적부하량_점 = 협의부하량_점 + 사용부하량_점,
    누적부하량_비점 = 협의부하량_비점 + 사용부하량_비점
  ) %>%
  left_join(complete_new_all, by = c("시군", "대상물질", "단위유역")) %>%
  mutate_all(~ replace(., is.na(.), 0)) %>% 
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
  arrange(대상물질, 시군, 단위유역)

### 파일 내보내기
write_xlsx(base3, path = "Output/Data/누적관리대장 내부검토.xlsx")
