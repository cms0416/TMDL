# 관련 패키지 로드
library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)

# Excel 파일 불러오기_readxl
OBS <- read_excel("D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/총량측정망 수질/1.한강수계 총량측정망(05-18)_0제거.xls",
                  skip = 2) %>% select("총량지점명", "일자", "BOD(㎎/L)", "총인(T-P)(㎎/L)")

# 결측치 제거
OBS <- na.omit(OBS)

# 변수명 변경(R 변수명 규칙에 맞게 조정)
setnames(OBS, c("BOD(㎎/L)", "총인(T-P)(㎎/L)"), c("BOD", "TP"))

# 년 추가_lubridate, dplyr
OBS <- OBS %>% mutate(년 = year(OBS$일자))
OBS$년 <- as.integer(OBS$년)

# 지점별 연평균 계산_dplyr, tidyr
BOD_med <- OBS %>%
  group_by(총량지점명, 년) %>%                                 # 지점별, 연도별 분리
  summarise(BOD = round(mean(BOD, na.rm=TRUE),3)) %>%  # BOD 연평균(소수점 3자리로 표시)
  spread(년, BOD)                                              # 연평균 데이터 포맷 변환(긴 형식 -> 넓은 형식)_tidyr

TP_med <- OBS %>%
  group_by(총량지점명, 년) %>%                                       # 지점별, 연도별 분리
  summarise(TP = round(mean(TP, na.rm=TRUE),3)) %>%   # TP 연평균(소수점 3자리로 표시)
  spread(년, TP)       

# 지점별 변환평균 계산 및 정리
for(i in 2005:2016) {
  assign(paste0(i-2000, "~", i-1998, " BOD"), OBS %>%
  group_by(총량지점명) %>%                                                                      # 지점별 분리
  filter(년 %in% c(i, i+1, i+2)) %>%                                                            # 해당연도만 추출
  summarise(med_BOD = round(median(BOD), 3)))           # BOD 중앙값(소수점 3자리로 표시)

  setnames(get(paste0(i-2000, "~", i-1998, " BOD")), "med_BOD", paste0(i-2000, "~", i-1998))    # 변수명 변환_data.table
 
  BOD_med <- BOD_med %>% left_join(get(paste0(i-2000, "~", i-1998, " BOD")), by = "총량지점명")         # 연평균 및 변환평균 합치기
  
  assign(paste0(i-2000, "~", i-1998, " TP"), OBS %>%
  group_by(총량지점명) %>%                                                                      # 지점별 분리
  filter(년 %in% c(i, i+1, i+2)) %>%                                                            # 해당연도만 추출
  summarise(med_TP = round(median(TP), 3)))     # TP 중앙값(소수점 3자리로 표시)  
  
  setnames(get(paste0(i-2000, "~", i-1998, " TP")), "med_TP", paste0(i-2000, "~", i-1998))      # 변수명 변환_data.table
  
  TP_med <- TP_med %>% left_join(get(paste0(i-2000, "~", i-1998, " TP")), by = "총량지점명")            # 연평균 및 변환평균 합치기
}

# 수계 순서에 맞춰 데이터 순서 조정(지점 추가 및 변경 시 수정)
OBSERVATORY <- c("골지A","오대A","주천A","평창A","옥동A","한강A","한강B","제천A","한강C","달천A",
                 "달천B","한강D","섬강A","섬강B","청미A","양화A","복하A","한강E","흑천A","북한A",
                 "북한B","소양A","인북A","소양B","북한C","가평A","홍천A","북한D","조종A","경안A",
                 "경안B","한강F","왕숙A","한강G","탄천A","중랑A","한강H","안양A","한강I","굴포A",
                 "공릉A","임진A","한탄A","영평A","신천A","한탄B","문산A","임진B")                # 낙본A 추가 필요
POSITIONS <- rank(OBSERVATORY)
BOD_med <- BOD_med[POSITIONS,]
TP_med <- TP_med[POSITIONS,]

# 데이터 csv로 내보내기
write.csv(BOD_med, file = "D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/중앙값_BOD 내보내기.csv")
write.csv(TP_med, file = "D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/중앙값_TP 내보내기.csv")
