#  라이브러리 로드
library(tidyverse)
library(magrittr)
library(readxl)
library(writexl)

# ========== ▶ 파일 불러오기 ◀ =================================================
# └ read_excel : 엑셀파일 불러오기 ---------------------------------------------
# sheet : 불러올 엑셀 파일 시트번호 지정
# skip : 지정된 개수의 행 스킵, col_names : 첫햇을 제목행으로 지정할지 여부 결정
# guess_max : 열의 자료 형태를 결정하는 최대 행 개수(inf : 범위를 무한대로 설정)
# guess_max 미설정 시 초반 행 1000개 이상이 결측값인 경우 전체열을 결측으로 판단
df <- read_excel("AAA/BBB/data.xlsx", sheet = 2, skip = 5, col_names = F, 
                 guess_max = Inf)

# 특정 행 또는 열만 불러오기
df <- read_excel("AAA/BBB/data.xlsx", range = cell_rows(102:151), col_names = FALSE)
df <- read_excel("AAA/BBB/data.xlsx", range = cell_cols("B:D"))

# └ map_dfr : 여러 파일 불러와서 합치기 1 -------------------------------------
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "AAA/data",
  pattern = "*.xls", full.names = T
)

# 경로지정된 파일 합치기
df <- files %>%
  # map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
  map_dfr(read_excel, skip = 4, col_names = F)


# └ for : 여러 파일 불러와서 합치기 2 -----------------------------------------
# 데이터 경로지정 및 데이터 목록
dir <- ("AAA/data")
files <- list.files(dir)

# 데이터 불러오기 및 합치기
df <- tibble()

for (file in files) {
  print(file)
  temp <- read_excel(file.path(dir, file), skip = 6, col_names = F) %>%
    select(11:13, 16, 17) %>%
    # str_sub(문자열, 시작위치, 끝위치) : 파일 이름에서 연도 추출
    mutate(연도 = str_sub(file, 1, 4))
  df <- bind_rows(df, temp)
}

# └ read.table() : txt 파일 불러오기 -------------------------------------------
df <- read.table("AAA/BBB/data.txt",
                 header = F, quote = "", sep = "|", fill = T,
                 encoding = "UTF-8", fileEncoding = "EUC-KR"
)


# ========== ▶ 파일 내보내기 ◀ =================================================
# └ write.csv : csv 파일 내보내기 ----------------------------------------------
write.csv(df, "AAA/BBB/Output/df.csv")
write_xlsx(df, path = "AAA/BBB/Output/df.csv")  # write_xlsx도 csv 내보내기 가능

# └ write_xlsx : 엑셀파일 내보내기 ---------------------------------------------
write_xlsx(df, path = "AAA/BBB/Output/df.xlsx")

## 여러 데이터프레임을 시트로 나눠서 엑셀파일 하나로 내보내기 1
write_xlsx(list(
  "aaa" = df1,
  "bbb" = df2 %>% filter(var1 != "x"),
  "ccc" = df3
), path = "AAA/BBB/Output/df.xlsx")


## 여러 데이터프레임을 시트로 나눠서 엑셀파일 하나로 내보내기 2
sheets <- list(
  "aaa" = df1,
  "bbb" = df2 %>% filter(var1 != "x"),
  "ccc" = df3,
  "aaa2" = df4,
  "bbb2" = df5 %>% filter(var1 != "x"),
  "ccc2" = df6
)

write_xlsx(sheets, path = "AAA/BBB/Output/df.xlsx")



# ========== ▶ dplyr_데이터 변환 ◀ =============================================
# 1. 행을 작업 대상으로 하는 함수 ----------------------------------------------
# └ 1.1 filter() : 조건에 의한 행 선택 -----------------------------------------
df %>% filter(var %in% c(a, b, c, ...))
df %>% filter(var1 == 1, var2 != 2, var3 > 3)  # ,과 &는 같은 의미
df %>% filter(var1 == 1 | var2 != 2)

# 해당 조건이 모두 아닌 경우 필터
df %>% filter(test != "조건1" & test != "조건2" & test != "조건3")
df %>% filter(!(test != "조건1" | test != "조건2" | test != "조건3"))
df %>% filter(!(test %in% c("조건1", "조건2", "조건3")))


# └ 1.2 slice() : 인덱스에 의한 행 선택 ----------------------------------------
# slice()
df %>% slice(5:10)     # 5~10번째 행 선택
df %>% slice(-(5:10))  # 5~10번째 행 제거
df %>% slice(n())      # 마지막 행 선택, n()은 데이터 프레임의 행의 개수를 반환

# slice_head(), slice_tail() : 데이터 프레임의 처음 또는 마지막 몇 행을 선택
df %>% slice_head(n=10)       # 앞에서부터 10개의 행을 선택
df %>% slice_tail(prop=0.3)   # 뒤에서 비율 0.3만큼 행을 선택

# slice_sample() : 랜덤으로 행 선택
# n : 행의 개수, prop : 비율, replace=TRUE : 복원추출(기본적으로 비복원추출)
df %>% slice_sample(n=3)
df %>% slice_sample(prop=0.2, replace=TRUE)

# slice_max(), slice_min() : 특정 변수가 가장 크거나 가장 작은 행을 선택
df %>% slice_max(var, n=10)     # var이 가장 큰 10개의 행 선택
df %>% slice_min(var, prop=0.1) # var이 작은 순서대로 전체 행의 10%를 선택


# └ 1.3 arrange() : 행의 정렬 --------------------------------------------------
df %>% arrange(var1)             # var1으로 오름차순
df %>% arrange(var1, desc(var2)) # var1으로 오름차순, var1이 같을 경우 var2로 내림차순

# └ 1.3.1 fct_reorder() : 다른 행을 기준으로 범주형 데이터(factor) 정렬 --------
# var1을 var2 기준으로 정렬(내림차순 정렬 : .desc 를 TRUE 로 지정)
df %>% mutate(var1 = fct_reorder(var1, var2, .desc = T))

# └ 1.4 distinct() : 중복된 행 제거 --------------------------------------------
# 변수가 없을 경우 모든 열이 같을 때에만 중복된 것으로 결정
# .keep_all=TRUE를 지정하면 데이터프레임의 모든 열을 가져올 수 있다.
df %>% distinct(var1, .keep_all = TRUE)

# 참고 : 중복자료 개수 확인
df %>% group_by(var1) %>%
  mutate(중복 = length(var1))

# 2. 열을 작업 대상으로 하는 함수 ----------------------------------------------
# └ 2.1 select() : 열 선택 -----------------------------------------------------
# 열 번호 또는 열 이름으로 선택
df %>% select(1, 2, 3)
df %>% select(1:3)
df %>% select(var1:var3, var10)
df %>% select(!c(1, 4, 10)) # 1, 4, 10번째 변수를 제외
df %>% select(-c(1, 4, 10)) # 위와 같음
df %>% select(1:4, -1)      # 1~4번 변수 중 1번 변수를 제외 = 1~3번 변수
df %>% select(1:4, !1)      # 1~4번 변수 + (1번 변수 제외 나머지) = 전체

# 특정 타입의 변수만 선택
df %>% select(where(is.numeric))                       # 수(int, dbl)만 선택
df %>% select(where(is.numeric) | where(is.character)) # 수 또는 문자형 변수를 선택

# ++ 열이름순으로 열순서 정렬 -----
df %>% select(sort(names(.)))
df %>% select(var1:var3, sort(names(.)))  # 특정 열은 먼저 선택 후 나머지 열은 이름순 정렬

# select()에서 사용할 수 있는 함수
# - everything(): 모든 변수를 선택한다.
# - last_col(): 마지막 변수를 선택한다.
# - starts_with("x"): 이름이 x로 시작하는 변수를 선택한다.
# - ends_with("x"): 이름이 x로 끝나는 변수를 선택한다.
# - contains("x"): 이름에 x가 들어가는 변수를 선택한다.
# - num_range("x", 1:10): c("x1", "x2", ...)와 같음

# ++ pull() : 변수를 데이터프레임이 아닌 벡터의 형태로 선택 -----
df %>% pull(var=1)    # 가져올 변수의 위치(첫번째 열) 지정
df %>% pull(var=var2) # 가져올 변수의 이름(var2) 지정


# └ 2.2 열 이름 변경 -----------------------------------------------------------
# ++ select() : 선택된 열 이름 변경 -----
df %>% select(new_name = old_name) # 변수명이 변경되고, 해당변수만 반환된다.
df %>% select(new_name = old_name, everything()) # 나머지 변수도 모두 반환

# ++ rename() : 이름을 바꾸고, df 전체가 반환됨 -----
df %>% rename(new_name = old_name)

# ++ rename_with() : 이름 변경 시 함수 적용 -----
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
df %>% rename_with(~ str_c(i - 2002, "~", i - 2000), var1)

# ++ set_names() : 변수명 일괄 변경 -----
df %>% set_names(c("x1", "x2", "x3"))
head(mtcars) %>% set_names(1:3, c("x1", "x2", "x3"))
head(mtcars) %>% set_names(paste0, "_foo")

# ++ names() : 변수명 일괄 변경 -----
names(df) <- c("x4", "x2", "x5")


# └ 2.3 relocate() : 열의 위치(순서) 변경 --------------------------------------
df %>% relocate(var5)                             # var5를 맨 앞으로 이동
df %>% relocate(var5, .after = var2)              # var5를 var2 뒤로 이동
df %>% relocate(var5, .before = var3)             # var5를 var3 앞으로 이동
df %>% relocate(starts_with("a"), .after = var2)  # 이름에 a가 포함되는 변수를 var2 뒤로 이동


# └ 2.4 mutate(), transmute() : 열 추가 ----------------------------------------
# mutate()를 사용하면 데이터프레임에 변수가 추가되어 반환되고, 
# transmute()를 사용하면 추가한 변수만 얻을 수 있다.
# ++ mutate() -----
df %>% mutate(z = x + y)
df %>% mutate(z = x + y, .before = 1)  # 계산 결과 z열을 맨 앞열에 추가
df %>% mutate(z = x + y, .after = x)   # 계산 결과 z열을 X 뒷열에 추가
df %>% mutate(speed = dist / time) %>% relocate(speed) # speed를 계산하고 맨 앞으로 옮김

# 참고
# ++ if_else() -----
if_else(condition, val1, val2)

# ++ case_when() -----
case_when(
  condition1 ~ value_1,
  condition2 ~ value_2,
  ...,
  .default =  ~ value_n)

# 예시
df %>% mutate(speed_tag = case_when(speed < 5 ~ "SLOW",
                                    speed < 10 ~ "MIDDLE",
                                    TRUE ~ "FAST"))


# 3. 행 자료의 요약 ----------------------------------------------------------
# ++ summarise() -----
df %>% sumamrise(total_num = n(), unique_num = n_distinct()) # 중복되지 않는 열의 개수


# 4. 데이터프레임 그룹화 -------------------------------------------------------
# └ 4.1 그룹 데이터프레임 생성: group_by() -------------------------------------
# group_by()
df %>% group_by(var1)          # var1이 같은 행끼리 그룹화
df %>% group_by(var1, var2)    # var1, var2가 모두 같은 행끼리 그룹화

# ++ tally() : 각 그룹에 속하는 행의 개수 -----
df %>% group_by(var1) %>% tally(name = "개수") # var1로 구분된 각 그룹의 데이터의 수를 반환

# 변수 추가해서 그룹 세분화 : .add=TRUE를 지정하지 않은 경우 그룹 변수가 새로운 변수로 대체
df %>% group_by(var1) %>% group_by(var2, .add = TRUE)

# ++ ungroup() : 그룹 해제 -----
df %>% group_by(var1) %>% sumamrise(total_num = n()) %>% ungroup()


# └ 4.2 그룹 데이터 프레임(group_by())에 dplyr 함수 적용하기 -------------------
# summarise()
# var1 그룹별로 var2 평균
df %>% group_by(var1) %>% summarise(var2_mean = mean(var2, na.rm=TRUE), .groups = "drop")
# var1 그룹별로 var2가 결측값인 행의 개수
df %>% group(var1) %>% summarise(na_count = sum(var2, na.rm=TRUE), .groups = "drop")

# select() : 그룹을 구성하는 변수는 선택 대상이 아니어도 자동으로 선택된다.

# arrange()
# .by_group = TRUE 매개변수를 주면, 그룹 변수를 첫 번째 정렬 변수로 사용
# var1로 정렬, var1이 같을 경우 var2 기준으로 정렬
df %>% group_by(var1) %>% arrange(var2, .by_group = TRUE) 

# mutate(), transmute()
# 그룹화된 데이터프레임에 mean(), max() 등을 적용하면 각 그룹별로 결과를 계산
# 그룹화되지 않았을 때와 다른 결과 산출

# filter()
# 조건을 지정할 때 요약통계량 함수(mean(), max() 등)를 사용하면 각 그룹별로 다른 값을 이용하여 행을 선택

# slice()
# var1의 그룹별로 var2가 가장 큰 열을 선택
df %>% group_by(var1) %>% slice_max(var2, n = 1)

# n()
# var1의 각 그룹별 데이터의 개수
df %>% group_by(var1) %>% summarise(group_n = n()) 


# 5. 여러 열을 대상으로 작업 수행 --------------------------------------------
# ++ across() -----
# across(.cols = everything(), .fns = NULL, ..., .names = NULL)
# - .cols: 작업을 수행할 열. tidy-select 방식으로 열을 선택할 수 있다.
# - .fns: 수행할 함수. 다음의 세 가지 방법으로 지정할 수 있다.
# - 함수 이름: ex) mean
# - purrr 방식으로 정의: ex) ~ mean(.x, na.rm=TRUE). .x는 각 열을 의미한다.
# - 여러 함수의 리스트: ex) list(Mean=mean, SD=sd)
# - ...: .fns에 넘겨줄 추가 매개변수이다. 보통 na.rm=TRUE 같은 걸 많이 쓴다.
# - .names: 결과물로 생성될 열의 이름을 지정할 수 있다. 선택된 열의 이름을 {col}, 함수의 이름을 {fn}으로 지정하여 문자열을 만들 수 있다. ex) .names="{col}_{fn}"

# summarise()와 함께 사용
# var1, var2 그룹별로 var3, var4 합계 계산 후 그룹화 해제
df %>% group_by(var1, var2) %>%
  summarise(across(c(var3, var4), ~ sum(.)), .groups = "drop")

# 모든 변수의 평균값을 계산
df %>% summarise(across(everything(), mean))
df %>% summarise_all(list(mean))

# 모든 숫자형 변수의 평균값을 계산
df %>% summarise(across(where(is.numeric), mean))

# 숫자형 변수의 평균을, 범주형 변수의 level의 개수
df %>% summarise(across(where(is.numeric), mean),
                 across(where(is.factor), nlevels))

# 이름이 Num으로 시작하는 변수의 평균과 표준편차 산정
df %>% summarise(across(starts_with("Num"), list(M = mean, SD = sd)))

# 각 숫자형 변수의 결측값의 개수
df %>% summarise(across(where(is.numeric), ~ sum(is.na(.x))))

# 모든 숫자형 변수에 대해 중복되지 않는 값의 개수
df %>% summarise(across(where(is.numeric), ~ length(unique(.x))))

# 특정 열만 지정
df %>% summarise(across(c(var1, var2), mean))

# 다른 함수(mutate, filter)와 함께 사용
# mutate()
# 자료 형태 변경
df %>% mutate(across(c(var1, var2), as.factor))
df %>% mutate(across(c(var1, var2), as.numeric)) # 실수(소수점 이하 포함)로 전환
df %>% mutate(across(c(var1, var2), as.integer)) # 정수(소수점 이하 미포함)로 전환
df %>% mutate(across(c(var1, var2), as.Date))
df %>% mutate(across(c(var1, var2), as.character))
df %>% mutate(across(where(is.numeric), ~ as.character(.x)))

# 날짜 서식 변경 0000.00.00 → 0000-00-00
df %>% mutate(across(c(var1, var2), ~ str_replace_all(., "\\.", "-")))
df %>% mutate(날짜 = str_replace_all(날짜, "\\.", "-"))

# filter()
# var로 시작하는 변수의 값이 모두 5 이상인 행을 선택
df %>% filter(across(starts_with("var"), ~ .x >= 5))

# 적어도 하나의 결측값을 갖는 행을 모두 제거 
# 모든 행을 대상으로 하는 것이므로 .cols는 생략 가능
df %>% filter(.fns = !is.na(.x))


# 6. 행 단위 작업 --------------------------------------------------------------
# ++ rowwise() -----
df1 = tibble(x = 1:2, y = 3:4, z = 5:6)
# A tibble: 2 x 3
#       x     y     z
#   <int> <int> <int>
# 1     1     3     5
# 2     2     4     6

# 각 행의 합 계산
df1 %>% rowwise() %>% mutate(total = sum(c(x, y, z)))
# A tibble: 2 x 4
# Rowwise: 
#       x     y     z total
#   <int> <int> <int> <int>
# 1     1     3     5     9
# 2     2     4     6    12

# rowwise() 없이 실행하면 모든 변수의 합이 계산
df1 %>% mutate(total = sum(c(x, y, z)))
# A tibble: 2 x 4
#       x     y     z total
#   <int> <int> <int> <int>
# 1     1     3     5    21
# 2     2     4     6    21

# 그 외 다른 행 기준 계산 함수
# ++ pmap_dbl() -----
df %>% mutate(합계 = pmap_dbl(select(., 1:10), sum))
df %>% mutate(t_sum = pmap_dbl(list(t1, t2, t3), sum)) 
df %>% mutate(t_sum = pmap_dbl(select(., starts_with("t")), sum))
df %>% mutate(t_avg = pmap_dbl(list(t1, t2, t3), lift_vd(mean)))

# ++ rowSums(), rowMeans() : 행렬에서 행 별로 합 및 평균을 구하는 함수 -----
df %>% mutate(t_sum = rowSums(select_if(., is.numeric))) 
df %>% mutate(t_avg = rowMeans(select(., -name)))


# 7. 데이터 합치기 -------------------------------------------------------------
# └ 7.1 다수의 데이터프레임 행 및 열 기준 단순 결합 ----------------------------
# ++ bind_rows() : 행 기준 결합 -----
bind_rows(df1, df2)
df1 %>% bind_rows(df2)

# 예시 : '.id' 매개변수를 사용해 합쳐지기 전 데이터프레임의 원천을 알 수 있음
df1 <- data.frame(x = 1:3, y = 1:3)
df2 <- data.frame(x = 4:6, y = 4:6)
df3 <- data.frame(x = 7:9, z = 7:9)

bind_rows(list(a = df1, b = df2, c = df3), .id = "id")

#     id  x  y  z
# 1    a  1  1 NA
# 2    a  2  2 NA
# 3    a  3  3 NA
# 4    b  4  4 NA
# 5    b  5  5 NA
# 6    b  6  6 NA
# 7    c  7 NA  7
# 8    c  8 NA  8
# 9    c  9 NA  9

# ++ bind_cols() : 열 기준 결합 -----
# 행의 개수가 같은 경우만 가능
bind_cols(df1, df2)
df1 %>% bind_cols(df2)


# └ 7.2 두개의 데이터프레임 기준 변수(열)에 따른 결합 --------------------------
## └ 7.2.1 열을 추가 하는 결합(Mutating join) ----------------------------------
# ++ inner_join() : 양쪽 데이터 모두에 포함된 공통 데이터만 출력(교집합) -----
inner_join(df1, df2, by = "결합 기준 변수")
df1 %>% inner_join(df2, by = "결합 기준 변수")

# ++ left_join(), right_join() : 한쪽을 기준으로 결합 -----
# df1 데이터를 기준으로 df1의 모든데이터와 df2는 공통 데이터만 출력
# df2 데이터에 값이 없는 경우 NA로 표시되며, df2에만 있는 경우 제외
left_join(df1, df2, by = "결합 기준 변수")
right_join(df2, df1, by = "결합 기준 변수")

df1 %>% left_join(df2, by = "결합 기준 변수")
df2 %>% right_join(df1, by = "결합 기준 변수")

# ++ full_join() : 양쪽 데이터에 포함된 모든값을 결합(합집합, UNION) -----
# 해당 변수에 값이 없는 경우 NA로 표시
full_join(df1, df2, by = "결합 기준 변수")
df1 %>% full_join(df2, by = "결합 기준 변수")


## └ 7.2.2 행을 필터링하는 결합(Filtering Join) --------------------------------
# ++ semi_join() : 한쪽기준으로 겹치는 데이터 출력(교집합) -----
# df1 데이터 중 df2의 데이터와 겹치는 데이터만 출력
semi_join(df1, df2, by = "결합 기준 변수")
df1 %>% semi_join(df2, by = "결합 기준 변수")

# ++ anti_join() : 한쪽에만 있는 데이터 출력(차집합) -----
# df1 데이터 중 df2의 데이터와 겹치는 데이터는 제외하고 나머지만 출력
anti_join(df1, df2, by = "결합 기준 변수")
df1 %>% anti_join(df2, by = "결합 기준 변수")



# ========== ▶ tidyr_데이터 정리 ◀ =============================================
# 1. 자료 재구조화 -------------------------------------------------------------
# └ 1.1 pivot_longer(): 데이터를 long format 으로 변경 -------------------------
df %>% 
  pivot_longer(
    cols = var1,         # long format 으로 재구조화할 칼럼,
    names_to = "name",   # 재구조화된 항목의 칼럼명
    values_to = "value", # 재구조화된 수치의 칼럼명
    names_sep = NULL,    # names_to 에 여러 정보를 포함할 경우, 구분자를 기준으로 칼럼 이름을 분할하는 인자
    values_drop_na = FALSE  # 결측치 존재시 포함여부
  ) 

# 구분자 분할 예시
# names_sep 인자에 .(dot) 을 기준으로 분할
# .(dot)을 인식시키기 위해서 앞에 \\ 을 입력함
# names_to 인자에 분할될 변수명을 정함
pivot_longer(data = iris,
             cols = Sepal.Length:Petal.Width,
             names_to = c("name","name1"),
             names_sep = '\\.')

# └ 1.2 pivot_wider() : 데이터를 wide format 으로 변경 -------------------------
df %>%
  pivot_wider(
    names_from = name,    # 현재 데이터에서 함수를 적용했을 때 칼럼명으로 갈 칼럼
    values_from = value,  # 현재 데이터에서 함수를 적용했을 때 값으로 들어갈 칼럼
    values_fill = NULL,   # 결측값이 있을 때 대체할 값
    values_fn = NULL      # 값에 적용할 함수
  )

# 예시 
df %>% pivot_wider(
  names_from = 단위유역, 
  names_glue = "{단위유역}_{.value}",
  names_sort = TRUE,
  names_vary = "slowest",
  values_from = c("젖소":"합계")
  )





# ========== ▶ 중복자료 관련 ◀ =================================================
# └ 중복자료 확인 -----
df %>% mutate(중복자료 = ifelse(duplicated(코드), "중복", ""))

# └ 중복자료 개수 확인 -----
df %>% 
  group_by(코드) %>% 
  mutate(중복자료 = length(코드)) %>% 
  ungroup()

# └ 중복자료 제거 -----
# .keep_all = TRUE 이면 모든 열 유지, FALSE(기본값)인 경우 선택 열만 유지
df %>% distinct(코드, .keep_all = TRUE)
distinct_all()



# ========== ▶ 결측치(NA) 관련 ◀ ===============================================
# └ 결측치가 없는 열만 필터 -----
df %>% filter(!is.na(코드))

# └ 데이터프레임 전체 결측치 0으로 변경 -----
df %>% mutate_all(~replace(., is.na(.), 0))

# └ 특정열 결측치 0으로 변경 -----
df %>% mutate(across(c(var1, var2), ~replace(., is.na(.), 0)))
df %>% mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))

# └ 특정열 0 및 빈칸을 결측치(NA)로 변경 -----
df %>% mutate(across(c(var1, var2), ~replace(., . == 0, NA)))
df %>% mutate(across(c(var1, var2), ~replace(., . == "", NA)))
df %>% mutate(across(c(var1, var2), ~replace(., . == "" | . == 0, NA)))
df %>% mutate(across(c(var1, var2), ~replace(., . %in% c("", 0), NA)))

# 참고(음수 값을 NA로 변경)
df %>% mutate(across(c(var1, var2), ~replace(., . < 0, NA)))

# ========== ▶ stringr_텍스트 관련 ◀ ===========================================
# 1. Detect Matches ----------------------------------------------------
# ++ str_detect()  -----
str_detect(string, pattern, negate = FALSE)

# ++ str_starts() -----
str_starts(string, pattern, negate = FALSE)

# ++ str_which() -----
str_which(string, pattern, negate = FALSE)

# ++ str_locate() -----
str_locate(string, pattern)

# ++ str_count() -----
str_count(string, pattern)


# 2. Subset Strings ----------------------------------------------------
# ++ str_sub() -----
str_sub(string, start = 1L, end = -1L)

# ++ str_subset() -----
str_subset(string, pattern, negate = FALSE)

# ++ str_extract() Return the first pattern match found in each string, as a vector. -----
# Also str_extract_all() to return every pattern
str_extract(string, pattern)
str_extract_all(string, pattern)

# ++ str_match() -----
str_match(string, pattern)


# 3. Join and Split ----------------------------------------------------
# ++ str_c() -----
str_c(..., sep = "", collapse = NULL) 
# sep : 각각의 원소들을 이어줄 때 사용 
# collapse : 배열 원소들을 이어줄 때 사용
str_c("prefix", c("a0", "a1", "a2"), "suffix")
str_c("prefix-", c("a0", "a1", "a2"), "-suffix")
str_c("prefix", c("a0", "a1", "a2"), "suffix", sep = "-", collapse = ", ")

str_c(c("a0", "a1", "a2"))
str_c(c("a0", "a1", "a2"), sep = ", ")
str_c(c("a0", "a1", "a2"), collapse = ", ")

str_c("a0", "a1", "a2")
str_c("a0", "a1", "a2", sep = ", ")
str_c("a0", "a1", "a2", collapse = ", ")

# ++ str_flatten()
str_flatten(string, collapse = "") 

# ++ str_dup()
str_dup(string, times) 

# ++ str_split_fixed()
str_split_fixed(string, pattern, n) 

# ++ str_glue()
str_glue(..., .sep = "", .envir = parent.frame())

# ++ str_glue_data()
str_glue_data(.x, ..., .sep = "", .envir =
                parent.frame(), .na = "NA")



# ========== ▶ 그래프 관련 ◀ ===================================================
# └ 그래프용 Noto Sans KR 폰트 추가 -----
library(showtext)

font_add_google('Noto Sans KR', 'notosanskr')
showtext_auto()


# ========== ▶ 기타 ◀ ==========================================================
# └ 행별 ID 부여 -----
df %>% rowid_to_column(var = "ID")

# └ e 없이 숫자 표시 -----
options(scipen=999)  # 지수 표기에서 숫자 표기로 변경
options(scipen=0)    # 다시 기본설정으로 변경

# └ 시군, 단위유역 순서 별 정렬 -----
df %>%
  mutate(
    단위유역 = factor(단위유역, levels = c(
      "합계", "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A",
      "섬강A", "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
      "홍천A", "한탄A", "한강B", "제천A", "한강D", "북한D", "한탄B", "임진A",
      "낙본A", "기타"
    )),
    시군 = factor(시군, levels = c(
      "합계", "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군",
      "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군",
      "양구군", "인제군", "고성군", "동해시", "속초시", "양양군"
    ))) %>% 
  arrange(단위유역, 시군)

# └ 소계 계산(janitor::adorn_totals) -----
library(janitor)

df %>% 
  group_by(시군) %>% 
  group_modify(~ .x %>% adorn_totals(where = "row", name = "소계"))

df %>% 
  # 강원도 총계 계산
  adorn_totals(where = "row", fill = "합계", name = "강원도") %>% 
  # 시군별 소계 계산
  group_by(시군) %>% 
  group_modify(~ .x %>% adorn_totals(where = "row", name = "소계"))

