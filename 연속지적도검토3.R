################################################################################
#  <연속지적도 전처리>
# 1. 연속지적도, 동리, 단위유역도 shp 동일 좌표계(EPSG 5186)로 통일
# 2. 연속지적도와 동리 통합(UNION) : 연속지적도에 일부 누락 존재, 동리로 메꿔줌
# 3. 2번 자료와 단위유역도 교차(INTERSECTION) : 유역 정보 추가
# 4. 속성에서 BCHK, SGG_OID, COL_ADM_SE, LI_CD, 시도명 등 불필요한 항목 제외
#    (교차 작업시 필요한 속성만 선택 가능)
# 5. 면적계산 : 속성테이블 → 필드계산기 → area($geometry)
################################################################################


#####  라이브러리 로드  ########################################################
library(tidyverse)
library(magrittr)
library(janitor)
library(readxl)
library(writexl)
library(sf)


#####  파일 불러오기  ##########################################################
## 법정동 코드
법정동코드 <- read_excel("주소 검토/법정동코드_강원_240619.xlsx") %>%
  mutate(동리 = ifelse(
    str_sub(읍면동, -1) == "읍" | str_sub(읍면동, -1) == "면",
    리,
    읍면동
  )) %>%
  filter(
    폐지여부 == "존재", 시도 == "강원특별자치도", !is.na(읍면동), !is.na(동리)
  ) %>%
  select(-c(폐지여부, 시도, 리))

## 지목
지목 <- read_excel("D:/GIS/토지이용/연속지적도/지목부호.xlsx")

## 연속지적도 파일
연속지적도 <- st_read(
  "D:/GIS/토지이용/연속지적도/2024년 6월/연속지적도_강원_한강_240614(유역도결합).shp"
) 


#####  데이터 정리  ############################################################
## 데이터 정리
연속지적도_정리 <- 연속지적도 %>%
  # 지리정보(geometry) 제거
  st_drop_geometry() %>%
  rename(단위유역 = SW_NAME) %>%
  filter(면적 != 0, 단위유역 == "섬강A") %>%
  mutate(
    법정동코드 = str_sub(PNU, 1, 10) %>% as.numeric()
  ) %>%
  left_join(법정동코드, by = "법정동코드") %>%
  mutate(
    지번 = str_sub(JIBUN, 1, -2) %>% str_trim(),
    지목약자 = str_sub(JIBUN, -1),
    .after = 동리
  ) %>%
  mutate(
    시군구 = ifelse(is.na(PNU), 시군구명, 시군구),
    읍면동 = ifelse(is.na(PNU), 읍면동명, 읍면동),
    동리 = ifelse(is.na(PNU), 동리명, 동리),
    지목약자 = ifelse(is.na(PNU), "임", 지목약자)
  ) %>% 
  left_join(지목, by = "지목약자") %>% 
  mutate(지목 = ifelse(지목약자 == "가", "가", 지목)) %>% 
  # 지적도와 동리 자료를 합치면서 경계 불일치로 필지가 분할된 경우 다시 합치기
  group_by(PNU, 시군구, 읍면동, 동리, 지번, 지목) %>% 
  summarise(면적 = sum(면적, na.rm=TRUE) / 10^6, .groups = "drop")

  # mutate(면적 = set_units(면적, km^2))


연속지적도_정리2 <- 연속지적도_정리 %>%
  # 지리정보(geometry) 제거
  st_drop_geometry() %>%
  filter(is.na(PNU))


## 동리별 지목별 면적계산
지목면적 <- 연속지적도_정리 %>%
  # 지리정보(geometry) 제거
  # st_drop_geometry() %>% 
  group_by(시군구, 읍면동, 동리, 지목) %>% 
  summarise(지목면적 = sum(면적, na.rm=TRUE), .groups = "drop") %>% 
  group_by(시군구, 읍면동, 동리) %>% 
  group_modify(~ .x %>%
                 adorn_totals(
                   where = "row", fill = "소계",
                   na.rm = TRUE, name = "소계"
                 )) %>% 
  mutate(지목 = as.factor(지목) %>% relevel(ref = "소계")) %>% 
  arrange(시군구, 읍면동, 동리, 지목)
  
## 농지 면적 현황
농지면적 <- 지목면적 %>% 
  filter(지목 %in% c("소계", "전", "답")) %>% 
  pivot_wider(
    names_from = 지목, 
    values_from = 지목면적
    ) %>% 
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>% 
  mutate(
    답_비율 = round(답 / 소계 * 100, 2),
    전_비율 = round(전 / 소계 * 100, 2)
    ) %>% 
  mutate(across(where(is.numeric), ~round(., 3)))
  # adorn_pct_formatting(
  #   "답_비율":"전_비율", digits = 2, rounding = "half up", affix_sign = TRUE
  #   )
  
## 섬강A 농지만 선택해서 저장
섬강A_농지 <- 연속지적도 %>% 
  filter(SW_NAME == "섬강A") %>%
  mutate(jimok = str_sub(JIBUN, -1)) %>% 
  filter(jimok %in% c("전", "답")) %>% 
  set_names(c("pnu", "sigun", "emd", "dongli", "jibun", "SW_NAME", "area", "geometry", "jimok"))

st_write(섬강A_농지, "D:/GIS/토지이용/연속지적도/2024년 6월/섬강A_농지.shp", append = FALSE)

