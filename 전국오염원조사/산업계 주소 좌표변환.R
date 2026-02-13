#####  라이브러리 로드  ########################################################
library(tidyverse)
library(data.table)
library(readxl)
library(writexl)

library(httr)
library(jsonlite) # fromJSON()
library(progress)
################################################################################


##**************************************************************************** ##
###################################  산업계  ###################################
##**************************************************************************** ##

## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
dir <- ("전국오염원조사/산업계")
files <- list.files(dir)

# 데이터 불러오기 및 합치기
industry <- tibble()

for (file in files) {
  print(file)
  temp <- read_excel(file.path(dir, file), skip = 5, col_names = F) %>%
    # str_sub(문자열, 시작위치, 끝위치) : 연도 추출
    mutate(연도 = str_sub(file, 1, 4))

  # 2022년 이전 자료의 경우 부분위탁량(76열) 뒤에 폐기물처리 항목 추가
  if (str_sub(file, 1, 4) < 2022) {
    temp <- temp %>% mutate(폐기물처리 = "", .after = 76)
  }

  temp <- temp %>%
    select(2, 3, 5, 8, 9, 10, 11, 12, 23, 72, 81, 연도) %>%
    set_names(c(
      "관할기관", "휴업", "업소명", "시군", "읍면동", "리", "본번", "부번",
      "규모", "폐수발생량", "폐수방류량", "연도"
    ))

  industry <- bind_rows(industry, temp)
}
## *****************************************************************************

industry1 <- industry %>%
  # 휴업인 경우 삭제
  filter(is.na(휴업)) %>%
  # 폐수발생량, 폐수방류량, 연도 숫자로 지정 및 결측값 0으로 수정
  mutate_at(vars(폐수발생량, 폐수방류량, 연도), as.numeric) %>%
  mutate_at(vars(폐수발생량, 폐수방류량, 연도), ~ replace(., is.na(.), 0)) %>%
  # 폐수방류량이 음수인 경우 0으로 수정
  mutate(
    폐수방류량 = ifelse(폐수방류량 < 0, 0, 폐수방류량),
    # 지명 변경 반영
    # 양구군 남면 → 국토정중앙면 / 홍천군 동면 → 영귀미면 / 영월군 중동면 → 산솔면
    읍면동 = ifelse(시군 == "양구군" & 읍면동 == "남면", "국토정중앙면", 읍면동),
    읍면동 = ifelse(시군 == "홍천군" & 읍면동 == "동면", "영귀미면", 읍면동),
    읍면동 = ifelse(시군 == "영월군" & 읍면동 == "중동면", "산솔면", 읍면동),
    # 주소코드 추가("리"가 없는 "동"의 경우 "법정리"칸에 "동"으로 추가)
    주소 = str_c(시군, 읍면동, ifelse(is.na(리), 읍면동, 리), sep = " "),
    주소전체 =
      str_c(
        "강원도", " ", 시군, " ", 읍면동, " ",
        ifelse(is.na(리), "", str_c(리, " ")),
        본번,
        ifelse(부번 == 0 | is.na(부번), "", str_c("-", 부번))
      )
  )


##########  주소 좌표 변환  ###############################################################

## 업소명 및 주소 추출 후 ID 부여
address_ind <- industry1 %>%
  select(연도, 관할기관, 업소명, 주소전체) %>%
  filter(연도 == 2022) %>%
  rowid_to_column(var = "ID")

## 주소 목록 list 생성
address_list <- address_ind$주소전체

## progress bar 설정
pb <- progress_bar$new(
  format = " Progress: [:bar] :current / :total (:percent), Estimated completion time::eta",
  total = nrow(address_ind), # 총 tick 개수 (default 100)
  clear = FALSE, # 종료 후에도 진행 경과 출력 결과 유지 (default TRUE)
  width = 80 # 진행 경과 막대 너비
)

## tibble 사전 생성
result <- tibble()
temp <- tibble()

## 주소 검색 및 데이터 취합
for (i in 1:nrow(address_ind)) {
  # 카카오맵 검색
  place_list <- GET(
    url = "https://dapi.kakao.com/v2/local/search/address.json",
    query = list(query = address_list[i]),
    add_headers(Authorization = "KakaoAK 9e9a85a9ec8362e009da2f7bc4b3a09c")
  ) %>%
    content(as = "text") %>%
    fromJSON()

  # 업소명 및 기존 주소 불러오기
  temp_addr <- address_ind %>% filter(ID == i)

  ## 주소
  temp <- bind_cols(temp_addr, place_list$documents)
  result <- bind_rows(result, temp)

  ## 진행상황 확인
  pb$tick()
}

### 주소 검색 결과 정리
# 중복 주소 정리
# 기존 주소전체에서 카카오맵 검색결과 읍면동이 일치하지 않는 경우 삭제
result <- result %>%
  mutate(확인 = ifelse(is.na(x),
    TRUE,
    str_detect(주소전체, address$region_3depth_name)
  )) %>%
  filter(확인 == TRUE)


# 지번 주소 정리
result_jibun <- bind_cols(
  result %>% select(ID),
  result$address) %>%
  select(
    ID, address_name, b_code, region_2depth_name, region_3depth_h_name,
    region_3depth_name, mountain_yn, main_address_no, sub_address_no, x, y
  ) %>%
  set_names(c("ID", "지번주소", "법정동코드", "시군", "읍면동", "동리", "산",
              "지번_본번", "지번_부번", "지번_x", "지번_y"))
  
  # 도로명 주소 정리
  result_doro <- bind_cols(
  result %>% select(ID),
  result$road_address) %>%
  select(
    ID, address_name, road_name, main_building_no, sub_building_no,
    building_name, x, y
  ) %>% 
    set_names(c("ID", "도로명주소", "도로명", "도로_본번", "도로_부번", 
                "건물명", "도로_x", "도로_y"))

# 지번주소, 도로명 주소 합치기
result1 <- result %>%
  select(ID:주소전체) %>%
  left_join(result_jibun, by = "ID") %>% 
  left_join(result_doro, by = "ID") %>% 
  relocate(도로명주소, .after = 지번주소) %>% 
  relocate(c(지번_x, 지번_y), .before = 도로_x) %>% 
  mutate(주소확인 = ifelse(is.na(지번주소) & is.na(도로명주소), "주소확인", ""))



##########  주소 검색 안되는 경우 업소명으로 재검색  ######################################

# 주소 확인 필요한 업소 정리
check_ind <- result1 %>% 
  filter(주소확인 == "주소확인") %>% 
  select(ID:주소전체) %>% 
  mutate(검색 = str_c(관할기관, 업소명, sep = " ")) %>% 
  rowid_to_column(var = "ID_2")
  
## 주소 목록 list 생성
address_list <- check_ind$검색

## progress bar 설정
pb <- progress_bar$new(
  format = " Progress: [:bar] :current / :total (:percent), Estimated completion time::eta",
  total = nrow(check_ind), # 총 tick 개수 (default 100)
  clear = FALSE, # 종료 후에도 진행 경과 출력 결과 유지 (default TRUE)
  width = 80 # 진행 경과 막대 너비
)

## tibble 사전 생성
result_chk <- tibble()
temp <- tibble()

## 업소명 검색 및 데이터 취합
for (i in 1:nrow(check_ind)) {
  # 카카오맵 검색
  place_list <- GET(
    url = "https://dapi.kakao.com/v2/local/search/keyword.json",
    query = list(query = address_list[i]),
    add_headers(Authorization = "KakaoAK 9e9a85a9ec8362e009da2f7bc4b3a09c")
  ) %>%
    content(as = "text") %>%
    fromJSON()
  
  # 업소명 및 기존 주소 불러오기
  temp_addr <- check_ind %>% filter(ID_2 == i)
  
  ## 주소
  temp <- bind_cols(temp_addr, place_list$documents)
  result_chk <- bind_rows(result_chk, temp)
  
  ## 진행상황 확인
  pb$tick()
}

### 검색결과 정리
result_chk2 <- result_chk %>% 
  select(ID_2:주소전체, address_name, road_address_name, place_name, x, y) %>% 
  distinct(ID, .keep_all = TRUE)
  

### 시군 송부용 자료 정리
result_chk3 <- result_chk %>% 
  select(ID_2, 관할기관:주소전체, address_name, road_address_name, place_name) %>% 
  distinct(ID_2, .keep_all = TRUE) %>% 
  set_names(c("ID", "관할기관", "업소명", "기존주소", "검색_지번주소", 
              "검색_도로명주소", "검색_업소명")) %>% 
  mutate(관할기관 = str_remove(관할기관, "강원도 "))


write_xlsx(result_chk3, path = "전국오염원조사/Output/산업계 주소 수정.xlsx")




# ****************************************************************************
# ## tibble 사전 생성
# result_doro <- tibble()
# result_jibun <- tibble()
# temp_addr <- tibble()
# temp_doro <- tibble()
# temp_jibun <- tibble()
#
# ## 주소 검색 및 데이터 취합
# for (i in 1:nrow(address_ind)) {
#   # 카카오맵 검색
#   place_list <- GET(
#     url = "https://dapi.kakao.com/v2/local/search/address.json",
#     query = list(query = address_list[i]),
#     add_headers(Authorization = "KakaoAK 9e9a85a9ec8362e009da2f7bc4b3a09c")) %>%
#     content(as = "text") %>%
#     fromJSON()
#
#   # 업소명 및 기존 주소 불러오기
#   temp_addr <- address_ind %>% filter(ID == i)
#
#   ## 도로명주소 : 주소 검색결과가 없는 경우 대비 test열(NA값 입력) 추가
#   temp_doro <- bind_cols(place_list$documents$road_address, tibble(test = NA))
#
#   # 도로명주소가 없는 경우(test열이 1열에 위치) 모든 열 NA로 입력
#   if (is.na(temp_doro[1, 1])) {
#     temp_doro <- tibble(address_name = NA, building_name = NA, road_name = NA,
#                         main_building_no = NA, sub_building_no = NA,
#                         x = NA, y = NA)
#   } else {
#     temp_doro <- temp_doro %>%
#       select(address_name, building_name, road_name,
#              main_building_no, sub_building_no, x, y)
#   }
#
#   temp_doro <-  bind_cols(temp_addr, temp_doro)
#   result_doro <- bind_rows(result_doro, temp_doro)
#
#   ## 지번주소
#   temp_jibun <- bind_cols(temp_addr, place_list$documents$address)
#   result_jibun <- bind_rows(result_jibun, temp_jibun)
#
#   ## 진행상황 확인
#   pb$tick()
# }
