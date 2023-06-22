###  라이브러리 로드
library(tidyverse)
library(magrittr)

#-----------  중복자료 관련  ---------------------------------------------------
### 중복자료 확인
df %>% mutate(중복자료 = ifelse(duplicated(코드), "중복", ""))

### 중복자료 개수 확인
df %>% 
  group_by(코드) %>% 
  mutate(중복자료 = length(코드)) %>% 
  ungroup()

### 중복자료 제거
### .keep_all = TRUE 이면 모든 열 유지, FALSE(기본값)인 경우 선택 열만 유지
df %>% distinct(코드, .keep_all = TRUE)
distinct_all()


#-----------  결측치(NA) 관련  -------------------------------------------------
### 결측치가 없는 열만 필터
df %>% filter(!is.na(코드))

### 데이터프레임 전체 결측치 0으로 변경
df %>% mutate_all(~replace(., is.na(.), 0))

### 특정열 결측치 0으로 변경
df %>% mutate(across(c(var1, var2), ~replace(., is.na(.), 0)))


#-----------  filter 함수 관련  ------------------------------------------------
### 해당 조건이 모두 아닌 경우 필터
df %>% filter(test != "조건1" & test != "조건2" & test != "조건3")

df %>% filter(!(test != "조건1" | test != "조건2" | test != "조건3"))

df %>% filter(!(test %in% c("조건1", "조건2", "조건3")))


#-----------  열 이름(변수명) 변경  --------------------------------------------
### select 함수
df %>% select(new_name = old_name) # 변수명이 변경되고, 해당변수만 반환된다.
df %>% select(new_name = old_name, everything()) # 나머지 변수도 모두 반환

### set_names() 함수 : 변수명 일괄 변경
df %>% set_names(c("x1", "x2", "x3"))
head(mtcars) %>% set_names(1:3, c("x1", "x2", "x3"))
head(mtcars) %>% set_names(paste0, "_foo")

### rename() 함수
df %>% rename(new_name = old_name)

### rename_with() 함수
# .fn : 변수명 변경 시 적용하고자 하는 함수
# .cols : 특정 열(변수) 선택 가능(기본은 모든 변수 적용)
rename_with(.data, .fn, .cols = everything(), ...)
df %>% rename_with(toupper) # 모든 변수명을 대문자로 변환

# Examples
df %>% rename_with(toupper) 
df %>% rename_with(toupper, starts_with("Petal")) 
df %>% rename_with(toupper, contains("a"))
df %>% rename_with(~ tolower(gsub(".", "_", .x, fixed = TRUE)))
df %>% rename_with(~ str_remove(., "a_"))
df %>% rename_with(~ str_replace(., "a_", "b_"))

### names() 함수(변수명 일괄 변경)
names(df) <- c("x4", "x2", "x5")


#-----------  열 위치 변경  --------------------------------------------
### relocate() 함수
df %>% relocate(var5)                          # var5를 맨 앞으로
df %>% relocate(starts_with("a"), .after=var2) # 이름에 a가 포함되는 변수를 var2 뒤로 이동


