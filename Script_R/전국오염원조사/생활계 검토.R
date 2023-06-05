## 관련 패키지 로드
library(tidyverse)
library(data.table)
library(readxl)
library(writexl)


## 공통파일(단위유역별 점유율) 불러오기
share <- read_excel("Data/전국오염원조사/단위유역별 점유율.xlsx")

####################################################################################################################################
##                                  생활계 - 인구
####################################################################################################################################


## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "Data/전국오염원조사/생활계/",
  pattern = "*.xls", full.names = T
)

# map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
population <- files %>%
  map_dfr(read_excel, skip = 4, col_names = F) %>%
  select(-c(7:18)) # 필요없는 열 삭제
## *****************************************************************************


# 수치데이터 및 연도 숫자로 지정
cname <- c(1, 7:23)
population[, cname] <- map(population[, cname], as.numeric)

# 변수명 지정
names(population) <- c(
  "연도", "행정구역코드", "시도", "시군구", "법정동", "법정리", "가정인구합계",
  "시가지역인구합계", "시가인구_하수처리구역_소계", "시가인구_하수처리구역_분류식",
  "시가인구_하수처리구역_합류식", "시가인구_하수미처리구역_소계",
  "시가인구_하수미처리구역_오수처리", "시가인구_하수미처리구역_정화조",
  "시가인구_하수미처리구역_수거식", "비시가지역인구합계",
  "비시가인구_하수처리구역_소계", "비시가인구_하수처리구역_분류식",
  "비시가인구_하수처리구역_합류식", "비시가인구_하수미처리구역_소계",
  "비시가인구_하수미처리구역_오수처리", "비시가인구_하수미처리구역_정화조",
  "비시가인구_하수미처리구역_수거식"
)

# 주소코드 추가("리"가 없는 "동"의 경우 "법정리"칸에 "동"으로 추가)
population <- population %>%
  mutate(주소 = paste(시군구, 법정동, ifelse(is.na(population$법정리), 법정동, 법정리)))

## *****************************************************************************
# 데이터 정리
population1 <- population %>%
  group_by(연도, 시군구) %>%
  summarise(
    가정인구합계 = sum(가정인구합계), 하수처리인구 = sum(시가인구_하수처리구역_소계) + sum(비시가인구_하수처리구역_소계),
    하수미처리인구 = sum(시가인구_하수미처리구역_소계) + sum(비시가인구_하수미처리구역_소계)
  ) %>%
  mutate(처리율 = 하수처리인구 / 가정인구합계 * 100)

## *****************************************************************************


# 연도 및 동리 별 가정인구합계 정리
population_total <- population %>%
  group_by(연도, 주소) %>%
  summarise(가정인구합계 = sum(가정인구합계))

# 단위유역별 점유율 합계 계산
share_population <- share %>%
  group_by(주소, 단위유역, 시군구) %>%
  summarise(생활계 = sum(생활계) / 100)

# 유역/시군 기준 인구 합계 연도별 정리
population_sum <- data.frame()

for (i in 2014:2020) {
  temp <- share_population %>%
    mutate(연도 = i) %>%
    left_join(population_total, by = c("주소", "연도")) %>%
    mutate(총인구 = round(생활계 * 가정인구합계)) %>%
    group_by(연도, 단위유역, 시군구) %>%
    summarise(총인구 = sum(총인구, na.rm = T))

  population_sum <- rbind(population_sum, temp)
}

## *****  소계 계산  ***********************************************************
# 유역별 합계
population_subtotal_1 <- population_sum %>%
  group_by(연도, 단위유역) %>%
  summarise(총인구 = sum(총인구)) %>%
  mutate(시군구 = "합계") %>%
  select(연도, 단위유역, 시군구, 총인구)

# 유역별 합계 합치기
population_sum <- rbind(population_sum, population_subtotal_1)

# 시군별 합계
population_subtotal_2 <- population_sum %>%
  group_by(연도, 시군구) %>%
  summarise(총인구 = sum(총인구)) %>%
  mutate(단위유역 = "합계") %>%
  select(연도, 단위유역, 시군구, 총인구)

# 시군별 합계 합치기
population_sum <- rbind(population_sum, population_subtotal_2)

## *****************************************************************************


# 권역 추가 후 단위유역, 시군구, 권역 factor로 지정
population_sum <- population_sum %>%
  mutate(권역 = ifelse(단위유역 == "골지A" | 단위유역 == "오대A" | 단위유역 == "주천A" |
    단위유역 == "평창A" | 단위유역 == "옥동A" | 단위유역 == "한강A", "남한강",
  ifelse(단위유역 == "섬강A" | 단위유역 == "섬강B", "섬강",
    ifelse(단위유역 == "북한A" | 단위유역 == "북한B" | 단위유역 == "소양A" |
      단위유역 == "인북A" | 단위유역 == "소양B" | 단위유역 == "북한C", "북한강",
    ifelse(단위유역 == "홍천A", "홍천강",
      ifelse(단위유역 == "한탄A", "한탄강",
        ifelse(단위유역 == "한강B" | 단위유역 == "제천A" | 단위유역 == "한강D", "충청북도",
          ifelse(단위유역 == "북한D" | 단위유역 == "한탄B" | 단위유역 == "임진A", "경기도",
            ifelse(단위유역 == "낙본A", "낙동강", "기타")
          )
        )
      )
    )
    )
  )
  )) %>%
  select(권역, everything()) %>%
  mutate_at(vars(단위유역, 시군구, 권역), as.factor)

# 단위유역, 시군구 순서 설정 및 각 연도를 열로 변경(wide 포맷)
population_sum <- population_sum %>%
  mutate(단위유역 = factor(단위유역, levels = c(
    "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A", "섬강B",
    "북한A", "북한B", "소양A", "인북A", "소양B", "북한C", "홍천A", "한탄A",
    "한강B", "제천A", "한강D", "북한D", "한탄B", "임진A", "낙본A", "기타", "합계"
  ))) %>%
  mutate(시군구 = factor(시군구, levels = c(
    "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군", 
    "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군", 
    "양구군", "인제군", "고성군", "동해시", "속초시", "양양군", "합계"
  ))) %>%
  mutate(권역 = ifelse(단위유역 == "골지A" | 단위유역 == "오대A" | 단위유역 == "주천A" |
    단위유역 == "평창A" | 단위유역 == "옥동A" | 단위유역 == "한강A", "남한강",
  ifelse(단위유역 == "섬강A" | 단위유역 == "섬강B", "섬강",
    ifelse(단위유역 == "북한A" | 단위유역 == "북한B" | 단위유역 == "소양A" |
      단위유역 == "인북A" | 단위유역 == "소양B" | 단위유역 == "북한C", "북한강",
    ifelse(단위유역 == "홍천A", "홍천강",
      ifelse(단위유역 == "한탄A", "한탄강",
        ifelse(단위유역 == "한강B" | 단위유역 == "제천A" | 단위유역 == "한강D", "충청북도",
          ifelse(단위유역 == "북한D" | 단위유역 == "한탄B" | 단위유역 == "임진A", "경기도",
            ifelse(단위유역 == "낙본A", "낙동강", "기타")
          )
        )
      )
    )
    )
  )
  )) %>%
  select(권역, everything()) %>%
  pivot_wider(names_from = 연도, values_from = 총인구) %>%
  arrange(단위유역, 시군구)

# 시군 기준 정리
population_sum_s <- population_sum %>%
  arrange(시군구, 단위유역) %>%
  select(시군구, everything(), -권역) %>%
  filter(시군구 != "합계")
population_sum <- population_sum %>% filter(단위유역 != "합계")

### 권역별 정리
population_sum_a <- population_sum %>%
  filter(시군구 == "합계") %>%
  group_by(권역) %>%
  summarise_at(vars(c("2014":"2020")), funs(sum)) %>%
  mutate(권역 = factor(권역, levels = c(
    "남한강", "섬강", "북한강", "홍천강", "한탄강", "충청북도", "경기도", "낙동강", "기타"
  ))) %>%
  arrange(권역)


#####  엑셀 파일 내보내기_writexl
write_xlsx(list("유역별정리" = population_sum, "시군별정리" = population_sum_s, "권역별정리" = population_sum_a), path = "Output/Data/전국오염원조사/생활계_인구.xlsx")



## *****  데이터 정리  ******************************
# population1 <- population %>%
#  group_by(연도, 시군구) %>%
#  summarise(
#    가정인구합계 = sum(가정인구합계), 하수처리인구 = sum(시가인구_하수처리구역_소계) + sum(비시가인구_하수처리구역_소계),
#    하수미처리인구 = sum(시가인구_하수미처리구역_소계) + sum(비시가인구_하수미처리구역_소계)
#  ) %>%
#  mutate(처리율 = 하수처리인구 / 가정인구합계 * 100)
# ***************************************************



####################################################################################################################################
##                                  생활계 - 물사용량
####################################################################################################################################


## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "Data/전국오염원조사/생활계/",
  pattern = "*.xls", full.names = T
)

# map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
waterusage <- files %>%
  map_dfr(read_excel, sheet = 2, skip = 4, col_names = F) %>%
  select(1:6, 19, 36) # 필요없는 열 삭제
## *****************************************************************************


# 변수명 지정
names(waterusage) <- c(
  "연도", "행정구역코드", "시도", "시군구", "법정동", "법정리", "가정용물사용합계", "영업용물사용합계"
)

# 수치데이터 숫자로 지정, 물사용량 합계 계산, 주소코드 추가
waterusage <- waterusage %>% mutate_at(vars(연도, 가정용물사용합계, 영업용물사용합계), as.numeric) %>% # 수치데이터 및 연도 숫자로 지정
  mutate(
    물사용량 = 가정용물사용합계 + 영업용물사용합계, # 물사용량 합계 계산
    주소 = paste(시군구, 법정동, ifelse(is.na(waterusage$법정리), 법정동, 법정리))
  ) # 주소코드 추가("리"가 없는 "동"의 경우 "법정리"칸에 "동"으로 추가)

# 연도 및 동리 별 가정인구합계 정리
waterusage_total <- waterusage %>%
  group_by(연도, 주소) %>%
  summarise(물사용량 = sum(물사용량))

# 단위유역별 점유율 합계 계산
share_waterusage <- share %>%
  group_by(주소, 단위유역, 시군구) %>%
  summarise(생활계 = sum(생활계) / 100)

# 유역/시군 기준 인구 합계 연도별 정리
waterusage_sum <- data.frame()

for (i in 2014:2020) {
  temp <- share_waterusage %>%
    mutate(연도 = i) %>%
    left_join(waterusage_total, by = c("주소", "연도")) %>%
    mutate(물사용량 = round(생활계 * 물사용량)) %>%
    group_by(연도, 단위유역, 시군구) %>%
    summarise(물사용량 = sum(물사용량, na.rm = T))

  waterusage_sum <- rbind(waterusage_sum, temp)
}

## *****  소계 계산  ***********************************************************
# 유역별 합계
waterusage_subtotal_1 <- waterusage_sum %>%
  group_by(연도, 단위유역) %>%
  summarise(물사용량 = sum(물사용량)) %>%
  mutate(시군구 = "합계") %>%
  select(연도, 단위유역, 시군구, 물사용량)

# 유역별 합계 합치기
waterusage_sum <- rbind(waterusage_sum, waterusage_subtotal_1)

# 시군별 합계
waterusage_subtotal_2 <- waterusage_sum %>%
  group_by(연도, 시군구) %>%
  summarise(물사용량 = sum(물사용량)) %>%
  mutate(단위유역 = "합계") %>%
  select(연도, 단위유역, 시군구, 물사용량)

# 시군별 합계 합치기
waterusage_sum <- rbind(waterusage_sum, waterusage_subtotal_2)

## *****************************************************************************


# 권역 추가 후 단위유역, 시군구, 권역 factor로 지정
waterusage_sum <- waterusage_sum %>%
  mutate(권역 = ifelse(단위유역 == "골지A" | 단위유역 == "오대A" | 단위유역 == "주천A" |
    단위유역 == "평창A" | 단위유역 == "옥동A" | 단위유역 == "한강A", "남한강",
  ifelse(단위유역 == "섬강A" | 단위유역 == "섬강B", "섬강",
    ifelse(단위유역 == "북한A" | 단위유역 == "북한B" | 단위유역 == "소양A" |
      단위유역 == "인북A" | 단위유역 == "소양B" | 단위유역 == "북한C", "북한강",
    ifelse(단위유역 == "홍천A", "홍천강",
      ifelse(단위유역 == "한탄A", "한탄강",
        ifelse(단위유역 == "한강B" | 단위유역 == "제천A" | 단위유역 == "한강D", "충청북도",
          ifelse(단위유역 == "북한D" | 단위유역 == "한탄B" | 단위유역 == "임진A", "경기도",
            ifelse(단위유역 == "낙본A", "낙동강", "기타")
          )
        )
      )
    )
    )
  )
  )) %>%
  select(권역, everything()) %>%
  mutate_at(vars(단위유역, 시군구, 권역), as.factor)

# 단위유역, 시군구 순서 설정 및 각 연도를 열로 변경(wide 포맷)
waterusage_sum <- waterusage_sum %>%
  mutate(단위유역 = factor(단위유역, levels = c(
    "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A", "섬강B",
    "북한A", "북한B", "소양A", "인북A", "소양B", "북한C", "홍천A", "한탄A",
    "한강B", "제천A", "한강D", "북한D", "한탄B", "임진A", "낙본A", "기타", "합계"
  ))) %>%
  mutate(시군구 = factor(시군구, levels = c(
    "춘천시", "원주시", "강릉시", "태백시", "삼척시", "홍천군", 
    "횡성군", "영월군", "평창군", "정선군", "철원군", "화천군", 
    "양구군", "인제군", "고성군", "동해시", "속초시", "양양군", "합계"
  ))) %>%
  mutate(권역 = ifelse(단위유역 == "골지A" | 단위유역 == "오대A" | 단위유역 == "주천A" |
    단위유역 == "평창A" | 단위유역 == "옥동A" | 단위유역 == "한강A", "남한강",
  ifelse(단위유역 == "섬강A" | 단위유역 == "섬강B", "섬강",
    ifelse(단위유역 == "북한A" | 단위유역 == "북한B" | 단위유역 == "소양A" |
      단위유역 == "인북A" | 단위유역 == "소양B" | 단위유역 == "북한C", "북한강",
    ifelse(단위유역 == "홍천A", "홍천강",
      ifelse(단위유역 == "한탄A", "한탄강",
        ifelse(단위유역 == "한강B" | 단위유역 == "제천A" | 단위유역 == "한강D", "충청북도",
          ifelse(단위유역 == "북한D" | 단위유역 == "한탄B" | 단위유역 == "임진A", "경기도",
            ifelse(단위유역 == "낙본A", "낙동강", "기타")
          )
        )
      )
    )
    )
  )
  )) %>%
  select(권역, everything()) %>%
  pivot_wider(names_from = 연도, values_from = 물사용량) %>%
  arrange(단위유역, 시군구)

# 시군 기준 정리
waterusage_sum_s <- waterusage_sum %>%
  arrange(시군구, 단위유역) %>%
  select(시군구, everything(), -권역) %>%
  filter(시군구 != "합계")
waterusage_sum <- waterusage_sum %>% filter(단위유역 != "합계")

### 권역별 정리
waterusage_sum_a <- waterusage_sum %>%
  filter(시군구 == "합계") %>%
  group_by(권역) %>%
  summarise_at(vars(c("2014":"2020")), funs(sum)) %>%
  mutate(권역 = factor(권역, levels = c(
    "남한강", "섬강", "북한강", "홍천강", "한탄강", "충청북도", "경기도", "낙동강", "기타"
  ))) %>%
  arrange(권역)


#####  엑셀 파일 내보내기_writexl
write_xlsx(list(
  "유역별정리" = waterusage_sum, "시군별정리" = waterusage_sum_s,
  "권역별정리" = waterusage_sum_a
), path = "Output/Data/전국오염원조사/생활계_물사용량.xlsx")


#####  생활계 전체 엑셀 파일 내보내기_writexl
sheets <- list(
  "생활계_인구_유역별" = population_sum, "생활계_인구_시군별" = population_sum_s, "생활계_인구_권역별" = population_sum_a,
  "생활계_물사용량_유역별" = waterusage_sum, "생활계_물사용량_시군별" = waterusage_sum_s, "생활계_물사용량_권역별" = waterusage_sum_a
)

write_xlsx(sheets, path = "Output/Data/전국오염원조사/생활계.xlsx")


###################################   END   #######################################################################################
