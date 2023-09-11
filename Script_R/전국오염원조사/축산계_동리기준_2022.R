livestock_sum_2022 <- livestock_sum %>% 
  filter(시군구 == "철원군") %>% 
  filter(연도 == 2022) %>% 
  mutate(동리 = str_remove(주소, "철원군") %>% 
           str_remove(., "[가-힣0-9]{1,}(읍|면)") %>% 
           str_trim()) %>% 
  filter(동리 %in% c("화지리", "사요리", "외촌리", "율이리", "내포리", "대마리", 
                   "중세리", "산명리", "가단리", "유정리", "홍원리", "독검리", 
                   "지포리", "강포리", "이평리", "상노리", "중강리")) %>% 
  select(-c(축산계, 연도, 사육두수)) %>% 
  pivot_wider(
    names_from = 축종, 
    values_from = 총사육두수
  ) %>%
  mutate(across(c(젖소:가금), ~replace(., is.na(.), 0))) %>% 
  mutate(합계 = pmap_dbl(select(., 젖소:가금), sum)) %>% 
  pivot_wider(
    names_from = 단위유역, 
    names_glue = "{단위유역}_{.value}",
    names_sort = TRUE,
    names_vary = "slowest",
    values_from = c("젖소":"합계")
  ) %>% 
  mutate(
    동리 = factor(동리, levels = c(
      "화지리", "사요리", "외촌리", "율이리", "내포리", "대마리", 
      "중세리", "산명리", "가단리", "유정리", "홍원리", "독검리", 
      "지포리", "강포리", "이평리", "상노리", "중강리"))
  ) %>% 
  arrange(동리) 
  # mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))

