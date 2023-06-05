###  라이브러리 로드
library(tidyverse)
library(magrittr)

#-----------  중복자료 관련  ---------------------------------------------------
### 중복자료 확인
data %>% mutate(중복자료 = ifelse(duplicated(코드), "중복", ""))

### 중복자료 개수 확인
data %>% 
  group_by(코드) %>% 
  mutate(중복자료 = length(코드)) %>% 
  ungroup()

### 중복자료 제거
### .keep_all = TRUE 이면 모든 열 유지, FALSE(기본값)인 경우 선택 열만 유지
data %>% distinct(코드, .keep_all = TRUE)
distinct_all()


#-----------  결측치(NA) 관련  -------------------------------------------------
### 결측치가 없는 열만 필터
data %>% filter(!is.na(코드))


