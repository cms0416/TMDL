#####  원별 계절 정의 함수  ####################################################
date_format <- function(data, date_column) {
  data %>%
    mutate(
      {{ date_column }} := str_replace_all({{ date_column }}, "\\.", "-") %>% as.Date(),
      연도 = year({{ date_column }}),
      월 = month({{ date_column }})
    )
}

# {{ }} 연산자 사용: {{ date_column }}를 사용함으로써, 
# 함수에 전달된 열 이름(일자)을 동적으로 참조할 수 있음
# 이는 dplyr의 tidy evaluation 기능을 활용한 것으로, 
# 함수 내부에서 열 이름을 올바르게 처리할 수 있게 해준다.

# :=는 dplyr의 tidy evaluation 환경에서 동적으로 열 이름을 할당하거나 수정할 때 사용
# {{ }} 연산자와 함께 사용되어 인자로 전달된 변수를 열 이름으로 평가