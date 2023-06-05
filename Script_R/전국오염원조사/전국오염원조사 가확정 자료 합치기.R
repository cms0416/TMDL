#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(data.table)
library(readxl)
library(writexl)
################################################################################


########  축산계  ##############################################################
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/가확정/축산계/",
  pattern = "*.xls", full.names = T
)

# map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
livestock <- files %>%
  map_dfr(read_excel, skip = 6, col_names = F)

# 제목 행 추가(기존 자료와 합치기 위해 동일한 갯수로 추가)
livestock %<>% add_row(`...1` = rep(NA, 5), .before = 1)

# "검증상태" 열 삭제
livestock %<>% select(-1)

# 파일 내보내기
write_xlsx(livestock, 
           path = "전국오염원조사/가확정/2022년기준_전국오염원_조사자료_축산계_가축분뇨현황_(가확정).xlsx")



########  산업계  ##############################################################
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/가확정/산업계/",
  pattern = "*.xls", full.names = T
)

# map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
industry <- files %>%
  map_dfr(read_excel, skip = 5, col_names = F) 

# 제목 행 추가(기존 자료와 합치기 위해 동일한 갯수로 추가)
industry %<>% add_row(`...1` = rep(NA, 4), .before = 1)

# 파일 내보내기
write_xlsx(industry, 
           path = "전국오염원조사/가확정/2022년기준_전국오염원_조사자료_산업계(가확정).xlsx")



########  토지계  ##############################################################
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/가확정/토지계/",
  pattern = "*.xls", full.names = T
)

# map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
landuse <- files %>%
  map_dfr(read_excel, skip = 2, col_names = F) 

# 제목 행 추가(기존 자료와 합치기 위해 동일한 갯수로 추가)
landuse %<>% add_row(.before = 1)

# 파일 내보내기
write_xlsx(landuse, 
           path = "전국오염원조사/가확정/2022년기준_전국오염원_조사자료_토지계(가확정).xlsx")



########  양식계  ##############################################################
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/가확정/양식계/",
  pattern = "*.xls", full.names = T
)

# map_dfr : 행 병합(row-binding)하여 작성된 데이터프레임 반환
fishfarm <- files %>%
  map_dfr(read_excel, skip = 2, col_names = T) 

# 제목 행 추가(기존 자료와 합치기 위해 동일한 갯수로 추가)
fishfarm %<>% add_row(`...1` = rep(NA, 2), .before = 1) %>% 
  select(-1)

# 파일 내보내기
write_xlsx(fishfarm, 
           path = "전국오염원조사/가확정/2022년기준_전국오염원_조사자료_양식계(가확정).xlsx")



