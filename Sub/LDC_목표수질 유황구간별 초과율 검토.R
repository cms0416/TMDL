# 관련 패키지 로드
library(readxl)
library(lubridate)
library(dplyr)
library(data.table)

# Excel 파일 불러오기_readxl
OBS <- read_excel("D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/총량측정망 수질/1.한강수계 총량측정망(05-18).xls",
                  skip = 2) %>% select(총량지점명, 일자, "BOD(㎎/L)", "총인(T-P)(㎎/L)", "총유기탄소(TOC)(㎎/L)", "유량(㎥/s)")
FLOW <- read_excel("D:/1. 수질총량/2. 기준유량/기준유량 구간최대값.xlsx")
TARGET <- read_excel("D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/총량측정망 수질/목표수질.xlsx")

# '총량지점명' 문자형(character)에서 요인형(factor)으로 변환
OBS$총량지점명 <- as.factor(OBS$총량지점명)
FLOW$총량지점명 <- as.factor(FLOW$총량지점명)
TARGET$총량지점명 <- as.factor(TARGET$총량지점명)

# 결측치 제거
OBS <- na.omit(OBS)

# 변수명 변경(R 변수명 규칙에 맞게 조정)
setnames(OBS, c("BOD(㎎/L)", "총인(T-P)(㎎/L)", "총유기탄소(TOC)(㎎/L)", "유량(㎥/s)"), c("BOD", "TP", "TOC", "유량"))

# 연도 추가_lubridate, dplyr
OBS <- OBS %>% mutate(년 = year(OBS$일자)) %>% 
  left_join(TARGET, by = "총량지점명")
OBS$년 <- as.integer(OBS$년)

# 유량구간 및 목표수질 초과여부 확인
OBS1 <- OBS %>% left_join(FLOW, by = "총량지점명") %>% 
  mutate(유량구간 = ifelse(유량 <= 갈수기, "갈수기",
                       ifelse(유량 <= 저수기, "저수기",
                              ifelse(유량 <= 평수기, "평수기",
                                     ifelse(유량 <= 풍수기, "풍수기", "홍수기"))))) %>% 
  mutate(BOD초과 = ifelse(BOD>BOD_목표수질,1,0)) %>% 
  mutate(TP초과 = ifelse(TP>TP_목표수질,1,0)) %>% 
  mutate(TOC초과 = ifelse(TOC>TOC_목표수질,1,0))
                                           
# 평가기간/유량구간 별 측정자료수 및 초과율 구하기 
for(i in 2010:2016) {
  assign(paste0("LDC_", i, "_", i+2), OBS1 %>%
           group_by(총량지점명) %>%                 # 지점별 분리
           filter(년 %in% c(i, i+1, i+2)) %>%       # 해당연도만 추출(3년씩)
           select(총량지점명, 년, 유량구간, BOD초과, TP초과, TOC초과) %>% 
           summarise(전체자료수 = n(), 
                     홍수기 = sum(유량구간 == "홍수기"), 
                     풍수기 = sum(유량구간 == "풍수기"),
                     평수기 = sum(유량구간 == "평수기"),
                     저수기 = sum(유량구간 == "저수기"),
                     갈수기 = sum(유량구간 == "갈수기"),
                     BOD_전체초과율 = round(sum(BOD초과) / 전체자료수 * 100, 2), 
                     BOD_홍수기 = round(sum(유량구간 == "홍수기" & BOD초과) / sum(유량구간 == "홍수기") * 100, 2),
                     BOD_풍수기 = round(sum(유량구간 == "풍수기" & BOD초과) / sum(유량구간 == "풍수기") * 100, 2),
                     BOD_평수기 = round(sum(유량구간 == "평수기" & BOD초과) / sum(유량구간 == "평수기") * 100, 2),
                     BOD_저수기 = round(sum(유량구간 == "저수기" & BOD초과) / sum(유량구간 == "저수기") * 100, 2),
                     BOD_갈수기 = round(sum(유량구간 == "갈수기" & BOD초과) / sum(유량구간 == "갈수기") * 100, 2),
                     TP_전체초과율 = round(sum(TP초과) / 전체자료수 * 100, 2),
                     TP_홍수기 = round(sum(유량구간 == "홍수기" & TP초과) / sum(유량구간 == "홍수기") * 100, 2),
                     TP_풍수기 = round(sum(유량구간 == "풍수기" & TP초과) / sum(유량구간 == "풍수기") * 100, 2),
                     TP_평수기 = round(sum(유량구간 == "평수기" & TP초과) / sum(유량구간 == "평수기") * 100, 2),
                     TP_저수기 = round(sum(유량구간 == "저수기" & TP초과) / sum(유량구간 == "저수기") * 100, 2),
                     TP_갈수기 = round(sum(유량구간 == "갈수기" & TP초과) / sum(유량구간 == "갈수기") * 100, 2)) %>% 
           mutate(평가기간 = paste0(i, "~", i+2)) %>%       # 평가기간 추가
           left_join(TARGET, by = "총량지점명") %>%         # 목표수질 추가
           select(총량지점명, 평가기간, BOD_목표수질, TP_목표수질, everything(), -TOC_목표수질)   # 열 순서 변경(평가기간을 두번째로)
  )
}

# 연도별 데이터 하나로 합치기
group <- bind_rows(LDC_2010_2012, LDC_2011_2013, LDC_2012_2014, LDC_2013_2015, LDC_2014_2016, 
                   LDC_2015_2017, LDC_2016_2018)

# 수계 순서에 맞춰 데이터 순서 조정(지점 추가 및 변경 시 수정)
총량지점명 <- c("골지A","오대A","주천A","평창A","옥동A","한강A","한강B","제천A","한강C","달천A",
                "달천B","한강D","섬강A","섬강B","청미A","양화A","복하A","한강E","흑천A","북한A",
                "북한B","소양A","인북A","소양B","북한C","가평A","홍천A","북한D","조종A","경안A",
                "경안B","한강F","왕숙A","한강G","탄천A","중랑A","한강H","안양A","한강I","굴포A",
                "공릉A","임진A","한탄A","영평A","신천A","한탄B","문산A","임진B") 
총량지점명 <- as.factor(총량지점명)
ID <- c(1:48)
OBSRANK <- data.frame(총량지점명, ID)
group <- group %>% left_join(OBSRANK, by = "총량지점명") %>% arrange(ID) %>% select(-ID)

# 데이터 csv로 내보내기
write.csv(group, file = "D:/1. 수질총량/3. 목표수질/총량측정망 수질 현황 정리/LDC_유량구간별초과율평가.csv")

