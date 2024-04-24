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

## *****  파일 불러오기  *******************************************************
# 데이터 경로지정 및 데이터 목록
files <- list.files(
  path = "전국오염원조사/0시군제출자료 검토/축산계",
  pattern = "*.xlsx", full.names = T
)

# 데이터 불러오기 및 합치기
축산계_원본 <- files %>%
  map_dfr(~ {
    data1 <- read_excel(.x, skip = 6, col_names = F)
    
    # 검증상태 열에 "검증 경고"가 하나도 없는 경우 1열 전체 행이 NA가 되어 제외됨
    # 따라서 "검증 경고"가 있는 파일과 없는 파일이 열 갯수가 달라지는 문제 발생
    # 1열에 "검증 경고" 갯수가 0인 경우는 이미 "검증상태" 열이 제외된 상태 이므로
    # "연도"열만 추가 해주고, 아닌 경우 "검증상태" 열을 제거한 후에 "연도"열 추가
    if (length(which(data1$`...1` == "검증 경고")) == 0) {
      data2 <- data1 %>% mutate(연도 = 2023, .before = 1)
    } else {
      data2 <- data1 %>% select(-1) %>% mutate(연도 = 2023, .before = 1) 
    }
      
  })
## *****************************************************************************


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
  # filter(!사육두수 == 0) %>% 
  mutate(
    # 동리코드 추가("리"가 없는 "동"의 경우 "리"칸에 "동"으로 추가)
    동리코드 = str_c(시군, 읍면동, ifelse(is.na(리), 읍면동, 리), sep = " "),
    본번_수정 = 본번,
    부번_수정 = 부번,
    수정사항 = case_when(
      str_detect(본번, "\\(") ~ "괄호 및 내부 내용 삭제",
      str_detect(본번, "\\,") ~ "여러 지번을 쉼표로 구분한 경우 대표지번만 작성",
      str_detect(본번, "(외).{1,}") ~ "외 00필지 삭제",
      str_detect(부번, "(외).{1,}") ~ "외 00필지 삭제",
      str_detect(본번, "-") & !is.na(부번) ~ "부번 중복",
      부번 == 0 ~ "부번 없는 경우 비워 둘것"
    ),
    # 괄호안에 작성된 내용 삭제
    본번_수정 = str_remove(본번_수정, "\\(.{1,}"),
    # 쉼표로 구분된 내용 삭제
    본번_수정 = str_remove(본번_수정, "\\,.{1,}") %>% str_trim(),
    # "외 00필지" 삭제
    본번_수정 = str_remove(본번_수정, "(외).{1,}"),
    부번_수정 = str_remove(부번_수정, "(외).{1,}"),
    # 본번열과 부번열에 부번을 중복 입력하는 경우 삭제
    부번_수정 = ifelse(str_detect(본번_수정, "-"), NA, 부번_수정),
    # 주소 합치기
    주소 = 
      str_c(
        "강원특별자치도", " ", 시군, " ", 읍면동, " ",
        ifelse(is.na(리), "", str_c(리, " ")),
        본번_수정,
        ifelse(부번_수정 == 0 | 부번_수정 == "" | is.na(부번_수정), "", str_c("-", 부번_수정))
      )
    # 축산계 축종 변환(총량 기술지침 발생원단위 기준 축종명으로 변경)
    # 축종 = case_when(
    #   축종 == "한우(소)" ~ "한우",
    #   축종 == "유우(젖소)" ~ "젖소",
    #   축종 == "돼지" ~ "돼지",
    #   축종 == "마필(말)" ~ "말",
    #   축종 == "산양(염소포함)" | 축종 == "면양(육양포함)" | 축종 == "사슴" ~ "양, 사슴",
    #   축종 == "개" ~ "개",
    #   축종 == "닭" | 축종 == "오리" | 축종 == "타조" | 축종 == "가금기타" ~ "가금",
    #   TRUE ~ "-"
    # )
  ) %>% 
  left_join(share_2, by = "동리코드")



##########  주소 좌표 변환  ###############################################################

## 업소명 및 주소 추출 후 ID 부여
address_ind <- 축산계_정리 %>%
  select(-c(동리코드, 본번_수정, 부번_수정)) %>%
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
  distinct(ID, .keep_all = TRUE)

  # filter(확인 == TRUE)


# 지번 주소 정리
result_jibun <- bind_cols(
  result1 %>% select(ID, 확인),
  result1$address) %>%
  select(
    ID, address_name, b_code, region_3depth_name, mountain_yn, 
    main_address_no, sub_address_no, x, y, 확인
  ) %>%
  set_names(c("ID", "지번주소", "법정동코드", "읍면동", "산",
              "지번_본번", "지번_부번", "x", "y", "확인")) %>% 
  mutate(리 = str_extract(읍면동, "[가-힣0-9]{1,}(리)"), 
         읍면동 = str_remove(읍면동, "[가-힣0-9]{1,}(리)") %>% str_trim(),
         지번주소 = ifelse(확인 == FALSE, NA, 지번주소),
         읍면동 = ifelse(확인 == FALSE, NA, 읍면동)) %>% 
  relocate(리, .after = 읍면동) %>% 
  select(-확인)

         # 읍면동_확인 = str_detect(지번주소, 읍면동),
         # 리_확인 = str_detect(지번주소, 리)

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
  select(ID:단위유역) %>%
  left_join(result_jibun, by = "ID") %>% 
  left_join(result_doro, by = "ID") %>% 
  # relocate(도로명주소, .after = 지번주소) %>% 
  relocate(c(x, y), .before = 도로_x) %>% 
  mutate(지번_본번 = ifelse(산 == "Y", str_c("산 ", 지번_본번), 지번_본번),
         주소확인 = ifelse(is.na(지번주소) & is.na(도로명주소), "O", ""),
         수정사항 = case_when(
           !is.na(수정사항) &주소확인 == "O" ~ str_c(수정사항, ", 주소 검색 불가"),
           is.na(수정사항) & 주소확인 == "O" ~ "주소 검색 불가",
           .default = 수정사항
         )
           ) %>% 
  relocate(c(수정사항, 주소확인), .after = 단위유역) %>% 
  select(-c(산, 건물명:도로_y))


### 파일 내보내기
write_xlsx(result1, path = "전국오염원조사/0시군제출자료 검토/Output/축산계_제출자료_검토.xlsx")
