#####  안전 숫자 변환 함수  ####################################################

# - 문자, 논리형, 숫자형 등 다양한 타입을 받아 숫자로 변환
# - 빈 문자열("")과 "/"는 NA로 간주
# - 숫자 패턴이 아닌 값은 0으로 처리
# - 숫자 내 콤마 제거

to_num <- function(x) {
  # 1. 숫자형(numeric)인 경우: 그대로 두되, NA는 0으로 대체
  if (is.numeric(x)) return(replace_na(x, 0))
  # 2. 논리형(logical)인 경우: TRUE=1, FALSE=0으로 변환, NA는 0
  if (is.logical(x)) return(replace_na(as.numeric(x), 0))
  # 3. 나머지는 문자형으로 변환
  x <- as.character(x)
  # 4. 빈 문자열("")이나 "/"는 NA로 변환
  x[x %in% c("", "/")] <- NA
  # 5. 숫자 패턴(부호, 콤마, 소수점 포함 가능) 여부 확인
  looks_num <- str_detect(
    str_trim(x),
    "^[-+]?\\d{1,3}(?:,\\d{3})*(?:\\.\\d+)?$|^[-+]?\\d+(?:\\.\\d+)?$"
  )
  # 6. 숫자 패턴이면 콤마 제거 후 숫자로 변환, 아니면 NA
  out <- ifelse(looks_num, as.numeric(str_replace_all(x, ",", "")), NA_real_)
  # 7. 변환 결과 NA는 0으로 대체
  replace_na(out, 0)
}