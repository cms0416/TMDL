#####  라이브러리 로드  ########################################################
library(tidyverse)
library(data.table)
library(readxl)
library(writexl)

library(httr)
library(jsonlite) # fromJSON()
library(progress)
################################################################################



##########  파일 불러오기  #####################################################
data <- read_excel("주소검토/홍제 취수장 5km 이내 국공유지 현황.xlsx")



##########  주소 좌표 변환  ####################################################

# 카카오맵 REST API 키 설정
# kakao developers(https://developers.kakao.com/) → 앱 → 앱 선택 → 메뉴 → 앱 → 일반 → 앱 키
REST_API_KEY <- "0c7cd88bfcd65c351a4af41ef4cd25fa"

## 업소명 및 주소 추출 후 ID 부여
address_ind <- data %>%
  rowid_to_column(var = "ID")

## 주소 목록 list 생성
address_list <- address_ind$주소

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
    add_headers(Authorization = paste0("KakaoAK ", REST_API_KEY))
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
    str_detect(주소, address$region_3depth_name)
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
  select(ID:주소) %>%
  left_join(result_jibun, by = "ID") %>% 
  left_join(result_doro, by = "ID") %>% 
  relocate(도로명주소, .after = 지번주소) %>% 
  relocate(c(지번_x, 지번_y), .before = 도로_x) %>% 
  mutate(주소확인 = ifelse(is.na(지번주소) & is.na(도로명주소), "주소확인", ""))

coordinate <- result1 %>% 
  select(ID:도로명주소, 지번_x:도로_y)

data_coordinate <- address_ind %>% 
  left_join(coordinate %>% select(ID, 지번_x, 지번_y), by = "ID")

# 파일 내보내기
write.csv(data_coordinate, file = "주소검토/Output/주소_좌표변환.csv",
          fileEncoding = "EUC-KR")

