################################################################################
# 라이브러리 로드
library(tidyverse)
library(data.table)
library(readxl)
library(writexl)
library(ggthemes)
library(extrafont)

################################################################################


### Excel 파일 불러오기_readxl
obs <- read_excel("Data/총량측정망0722.xlsx")

target <- read_excel("Data/목표수질.xlsx")



###################  데이터 정리  ##############################################

## obs_측정값 정리
obs1 <- obs %>%
  left_join(target, by = "총량지점명") %>%
  filter(총량지점명 %in% c(
    "골지A", "오대A", "주천A", "평창A", "옥동A", "한강A", "섬강A",
    "섬강B", "북한A", "북한B", "소양A", "인북A", "소양B", "북한C",
    "홍천A", "한탄A", "제천A", "한강B", "한강C", "달천A", "달천B",
    "한강D"
  )) %>%
  filter(연도 >= 2015) %>%
  # BOD열에서 결측치 제거(결측치가 아닌 값들만 필터)
  filter(!is.na(BOD)) %>%
  select(-c(수온, pH, EC, DO, COD, SS, TN)) %>%
  # 월 및 계절 추가_lubridate, dplyr
  mutate(
    월 = month(일자),
    계절 = ifelse(월 >= 3 & 월 <= 5, "봄",
      ifelse(월 >= 6 & 월 <= 8, "여름",
        ifelse(월 >= 9 & 월 <= 11, "가을", "겨울")
      )
    ),
    TP_실측부하량 = TP * 유량 * 86.4,
    TP_목표부하량 = TP_목표수질 * 유량 * 86.4
  ) %>%
  # '계절' 요인형(factor)으로 변환 후 계절 순서로 요인 순서 변경
  mutate(계절 = factor(계절, levels = c("봄", "여름", "가을", "겨울")))



###################  그래프 작성  ##############################################

TP_plot <- obs1 %>%
  filter(총량지점명 == "오대A", 연도 == 2021) %>% 
  arrange(desc(유량)) %>% 
  rowid_to_column(var = "유량크기순")

  

TP_plot %>% 
  ggplot() +
  geom_line(aes(x = 유량크기순, y = TP_목표부하량, color = "red"), size = 1) +
  geom_point(aes(x = 유량크기순, y = TP_실측부하량, fill = 계절, color = "black"), shape = 21, alpha = 0.6, size = 3) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  scale_y_log10(name = "T-P 부하량(kg/d)", labels = scales::label_number()) +
  # geom_hline(aes(yintercept = TP_목표수질), colour = "red", linetype = "dashed", size = 0.7) +
  labs(title = paste("'20년 T-P (계절별)")) +
  theme_bw() +
  scale_fill_discrete(name = "계절") +             # 범례 제목
  scale_color_manual(name = NULL, values = c("black", "red"),
                     labels = c("실측부하량", "목표부하량")) +
  guides(color = guide_legend(override.aes = list(shape = c(21, NA), linetype = c(NA, 1)))) +
  theme(
    plot.title = element_text(family = "NanumBarunGothic", size = 17, face = "bold", hjust = 0.5), # 그래프 제목 설정
    axis.title.x = element_text(family = "NanumBarunGothic", size = 16, face = "bold"), # x축 제목 설정
    axis.title.y = element_text(family = "NanumBarunGothic", size = 13, face = "bold"), # y축 제목 설정
    axis.text.x = element_text(size = 12), # x축 수치 사이즈
    axis.text.y = element_text(size = 12), # y축 수치 사이즈
    legend.title = element_text(family = "NanumBarunGothic", size = 10, face = "bold"),
    legend.text = element_text(family = "NanumBarunGothic", size = 10),
    legend.position = c(0.67, 0.92), # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )


sec.axis = sec_axis(trans = ~.*100, name = "유량(㎥/s)", labels = scales::label_number())


TP_plot %>% 
  ggplot() +
  geom_bar(aes(x = 유량크기순, y = 유량/100), stat="identity", colour="black", fill = "steelblue2") +
  geom_point(aes(x = 유량크기순, y = TP, fill = 계절), color = "black", shape = 21, alpha = 0.6, size = 3) + 
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  scale_y_continuous(name = "T-P 부하량(kg/d)", sec.axis = sec_axis(~.*100, name = "유량(㎥/s)"))
                                      


TP_plot %>% 
  ggplot() +
  geom_bar(aes(x = 유량크기순, y = 유량), stat="identity", colour="black", fill = "steelblue2") +
  geom_point(aes(x = 유량크기순, y = log10(TP) / 0.006, fill = 계절), color = "black", shape = 21, alpha = 0.6, size = 3) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  
  scale_y_continuous(position = "right", name = "유량(㎥/s)", 
                sec.axis = sec_axis(~10^(. * 0.006), 
                                    name = "T-P 부하량(kg/d)", 
                                    breaks = breakfun
                                    )
                ) +
  geom_hline(aes(yintercept = log10(TP_목표수질) / 0.006), colour = "red", linetype = "dashed", size = 0.7) +
  labs(title = paste("'20년 T-P (계절별)")) +
  theme_bw() +
  theme(
    plot.title = element_text(family = "NanumBarunGothic", size = 17, face = "bold", hjust = 0.5), # 그래프 제목 설정
    axis.title.x = element_text(family = "NanumBarunGothic", size = 16, face = "bold"), # x축 제목 설정
    axis.title.y = element_text(family = "NanumBarunGothic", size = 13, face = "bold"), # y축 제목 설정
    axis.text.x = element_text(size = 12), # x축 수치 사이즈
    axis.text.y = element_text(size = 12), # y축 수치 사이즈
    legend.title = element_text(family = "NanumBarunGothic", size = 10, face = "bold"),
    legend.text = element_text(family = "NanumBarunGothic", size = 10),
    legend.position = c(0.67, 0.92), # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )



TP_plot %>% 
  ggplot() +
  geom_bar(aes(x = 유량크기순, y = log10(유량)), stat="identity", colour="black", fill = "steelblue2") +
  geom_point(aes(x = 유량크기순, y = TP, fill = 계절), color = "black", shape = 21, alpha = 0.6, size = 3) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  
  scale_y_log10(name = "T-P 부하량(kg/d)", labels = scales::label_number(),
                     sec.axis = sec_axis(~10^., 
                                         name = "유량(㎥/s)", 
                                         labels = scales::label_number()
                     )
  ) +
  geom_hline(aes(yintercept = TP_목표수질), colour = "red", linetype = "dashed", size = 0.7) +
  labs(title = paste("'20년 T-P (계절별)")) +
  theme_bw() +
  theme(
    plot.title = element_text(family = "NanumBarunGothic", size = 17, face = "bold", hjust = 0.5), # 그래프 제목 설정
    axis.title.x = element_text(family = "NanumBarunGothic", size = 16, face = "bold"), # x축 제목 설정
    axis.title.y = element_text(family = "NanumBarunGothic", size = 13, face = "bold"), # y축 제목 설정
    axis.text.x = element_text(size = 12), # x축 수치 사이즈
    axis.text.y = element_text(size = 12), # y축 수치 사이즈
    legend.title = element_text(family = "NanumBarunGothic", size = 10, face = "bold"),
    legend.text = element_text(family = "NanumBarunGothic", size = 10),
    legend.position = c(0.67, 0.92), # 범례 위치
    legend.direction = "horizontal", # 범례 방향
    legend.box = "horizontal", # 범례 배치 방향
    legend.background = element_rect(fill = "white", color = "black"), # 범례 배경 및 테두리 색
    legend.margin = margin(5, 5, 5, 5)
  )



data<- data.frame(
  Day=c(1,2,3,1,2,3,1,2,3),
  Name=rep(c(rep("a",3),rep("b",3),rep("c",3))),
  Var1=c(1090,484,64010,1090,484,64010,1090,484,64010),
  Var2= c(4,16,39,2,22,39,41,10,3))

# Max of secondary divided by max of primary
upper <- log10(1) / 156.544

breakfun <- function(x) {
  10^scales::extended_breaks()(log10(x))
}

ggplot(data)  + 
  geom_bar(aes(fill=Name, y=Var2, x=Day),
           stat="identity", colour="black", position= position_stack(reverse = TRUE))+
  geom_line(aes(x=Day, y=log10(Var1) / upper),
            stat="identity",color="black", linetype="dotted", size=0.8)+
  geom_point(aes(Day, log10(Var1) / upper), shape=8)+
  labs(title= "",
       x="",y=expression('Var1'))+
  scale_y_continuous(
    position = "right",
    name = "Var2",
    sec.axis = sec_axis(~10^ (. * upper), name= expression(paste("Var1")),
                        breaks = breakfun)
  )+
  theme_classic() +
  scale_fill_grey(start = 1, end=0.1,name = "", labels = c("a", "b", "c"))