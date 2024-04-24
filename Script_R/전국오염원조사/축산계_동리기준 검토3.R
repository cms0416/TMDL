#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(janitor)
library(readxl)
library(writexl)

library(httr)
library(jsonlite) # fromJSON()
library(progress)


#####  축산계 자료 불러오기  ###################################################
축산계_원본 <- read_excel("전국오염원조사/축산계/2022년기준_전국오염원_조사자료_축산계_가축분뇨현황.xlsx",
                        skip = 6, col_names = F) %>% 
  mutate(연도 = 2022, .before = 1)

share <- read_excel("전국오염원조사/단위유역별 점유율.xlsx") %>% 
  filter(축산계 != 0) %>% 
  group_by(주소, 단위유역, 시군) %>%
  summarise(축산계 = sum(축산계) / 100, .groups = "drop")

share_2 <- share %>% 
  filter(축산계 > 0.98) %>% 
  select(주소, 단위유역) %>% 
  rename(동리코드 = 주소)

#####  데이터 정리  ############################################################

## 변수명 지정 및 데이터 정리
축산계_정리 <- 축산계_원본 %>%
  select(연도, 2, 12:17, 18) %>% 
  set_names(c(
    "연도", "농장명", "시군", "읍면동", "리", "본번", "부번", "축종", "사육두수"
  )) %>%
  # 사육두수, 연도 숫자로 지정
  mutate(across(c(사육두수, 연도), as.numeric)) %>%
  # 사육두수 결측인 경우 0으로 수정
  mutate(across(c(사육두수), ~ replace(., is.na(.), 0))) %>%
  # 사육두수 0인 경우 제외
  filter(!사육두수 == 0) %>% 
  mutate(
    # 동리코드 추가("리"가 없는 "동"의 경우 "리"칸에 "동"으로 추가)
    동리코드 = str_c(시군, 읍면동, ifelse(is.na(리), 읍면동, 리), sep = " "),
    # 괄호안에 작성된 내용 삭제
    본번 = str_remove(본번, "\\(.{1,}"),
    # 쉼표로 구분된 내용 삭제
    본번 = str_remove(본번, ".{1,}\\,") %>% str_trim(),
    # "외 00필지" 삭제
    본번 = str_remove(본번, "(외).{1,}"),
    부번 = str_remove(부번, "(외).{1,}"),
    # 주소 합치기
    주소 = 
      str_c(
        "강원특별자치도", " ", 시군, " ", 읍면동, " ",
        ifelse(is.na(리), "", str_c(리, " ")),
        본번,
        ifelse(부번 == 0 | 부번 == "" | is.na(부번), "", str_c("-", 부번))
      ),
    # 축산계 축종 변환(총량 기술지침 발생원단위 기준 축종명으로 변경)
    축종 = case_when(
      축종 == "한우(소)" ~ "한우",
      축종 == "유우(젖소)" ~ "젖소",
      축종 == "돼지" ~ "돼지",
      축종 == "마필(말)" ~ "말",
      축종 == "산양(염소포함)" | 축종 == "면양(육양포함)" | 축종 == "사슴" ~ "양, 사슴",
      축종 == "개" ~ "개",
      축종 == "닭" | 축종 == "오리" | 축종 == "타조" | 축종 == "가금기타" ~ "가금",
      TRUE ~ "-"
    )
  ) %>% 
  left_join(share_2, by = "동리코드")



##########  주소 좌표 변환  ###############################################################

## 업소명 및 주소 추출 후 ID 부여
address_ind <- 축산계_정리 %>%
  filter(is.na(단위유역)) %>% 
  select(연도, 시군, 농장명, 주소, 동리코드, 축종, 사육두수, 단위유역) %>%
  # filter(연도 == 2022) %>%
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
# 기존 주소에서 카카오맵 검색결과 읍면동이 일치하지 않는 경우 삭제
result1 <- result %>%
  mutate(확인 = ifelse(is.na(x),
                     TRUE,
                     str_detect(주소, address$region_3depth_name)
  )) %>%
  filter(확인 == TRUE)


# 지번 주소 정리
result_jibun <- bind_cols(
  result1 %>% select(ID),
  result1$address) %>%
  select(
    ID, address_name, b_code, region_3depth_h_name,
    region_3depth_name, mountain_yn, main_address_no, sub_address_no, x, y
  ) %>%
  set_names(c("ID", "지번주소", "법정동코드", "읍면동", "동리", "산",
              "지번_본번", "지번_부번", "x", "y"))

# 도로명 주소 정리
result_doro <- bind_cols(
  result1 %>% select(ID),
  result1$road_address) %>%
  select(
    ID, address_name, road_name, main_building_no, sub_building_no,
    building_name, x, y
  ) %>% 
  set_names(c("ID", "도로명주소", "도로명", "도로_본번", "도로_부번", 
              "건물명", "도로_x", "도로_y"))

# 지번주소, 도로명 주소 합치기
result1 %<>%
  select(ID:사육두수) %>%
  left_join(result_jibun, by = "ID") %>% 
  left_join(result_doro, by = "ID") %>% 
  relocate(도로명주소, .after = 지번주소) %>% 
  relocate(c(x, y), .before = 도로_x) %>% 
  mutate(주소확인 = ifelse(is.na(지번주소) & is.na(도로명주소), "주소확인", ""))

  