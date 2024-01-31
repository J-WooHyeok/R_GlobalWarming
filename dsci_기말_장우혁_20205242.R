library(tidyverse)
library(ggplot2)
library(dplyr)
getwd()
setwd("/Users/jang-woohyeok/Desktop/WooHyeok/23_1/데이터사이언스 기초/FinalProject")
#-----------------------<온실가스 추세>----------------------------------
#온실가스추세 데이터를 gas변수로 읽기
gas <- read_csv("온실가스추세.csv", locale = locale("ko", encoding = "EUC-KR"), col_names = T)
gas

#gas데이터에서 총배출량 행을 선택하되, 첫번째 열("year"과 "total")은 제외하고 가져오기
총배출량 <- gas[1,-1]

#총배출량 데이터의 행과 열의 위치를 바꿔줌(전치행렬)
총배출량 = t(총배출량)

#열이름과 행이름을 깔끔하게 지워주기
colnames(총배출량) <- NULL
rownames(총배출량) <- NULL

#열이름을 year로 지정해주고, 1990부터 2020까지 새로운 데이터를 만듦
year <- paste0(1990:2020)

#총배출량데이터와 새로 만든 year데이터를 합친다!!
총배출량 <- cbind(year, 총배출량)

#새로 합친 데이터의 열 이름 지정해주기
colnames(총배출량) <- c("year", "total")
총배출량_df <- as.data.frame(총배출량)
colnames(총배출량_df) <- c("year", "total")

#total과 year을 numeric형식으로 변환해주기
str(총배출량_df)
총배출량_df$year <- as.numeric(총배출량_df$year)
총배출량_df$total <- as.numeric(총배출량_df$total)

#그래프를 그리기 전 결측값 제거
배출 = 총배출량_df
배출
str(배출)
boxplot(배출$total)
b1 = summary(배출$total)[2]
b2 = summary(배출$total)[5]
bIQR = b2 - b1
배출$total = ifelse(b2 + bIQR * 1.5 < 배출$total |
                    b1 - bIQR * 1.5 > 배출$total, Na, 배출$total)
table(is.na(배출$total))

# 연도별 온실가스 배출량을 그래프로 그려보기
ggplot(총배출량_df, aes(x = year, y = total)) +
  geom_point(color = "red", size = 2) +
  xlab("Year") +
  ylab("Total") +
  ggtitle("Trend of Total Emissions") 

# ---------------------------------------------------------------------------------

# ---------------------------------<강수량 추세>-----------------------------------

rainMonth <- read_csv("월별강수량.csv", locale = locale("ko", encoding = "EUC-KR"), col_names = T)
rainMonth
rainCnt <- read_csv("강수빈도.csv", locale = locale("ko", encoding = "EUC-KR"), col_names = T)
rainCnt

#월별 강수량에서 7월 데이터면서 area열은 빼놓고 모아오기
rainMonth7 = data.frame(rainMonth %>% filter(month == 7) %>% select(-area))
rainMonth7


#월별 강수빈도에서 7월 데이터만 모아오기
rainCnt7 = data.frame(rainCnt %>% filter(month == 7))
rainCnt7

#두 데이터 합치기
rain <- left_join(rainCnt7, rainMonth7, by = c('year', 'month'))
rain

#그래프그리기
rain %>% ggplot(aes(x = year, y = count)) +
  geom_bar(stat = "identity", fill = "forestgreen", color = "black", width = 0.5, position = "dodge") +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Count Trend")

rain %>% ggplot(aes(x = year, y = rain)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", width = 0.5) +
  xlab("Year") +
  ylab("Rain") +
  ggtitle("Rain Trend")


#결측값이 있나 확인하기(이상기후)
boxplot(rain$count) #0개
b1 = summary(rain$count)[2]
b2 = summary(rain$count)[5]
bIQR = b2 - b1
rain$count = ifelse(b2 + bIQR * 1.5 < rain$count |
                      b1 - bIQR * 1.5 > rain$count, NA, rain$count)
table(is.na(rain$count))

boxplot(rain$rain)  #2개
b1 = summary(rain$rain)[2]
b2 = summary(rain$rain)[5]
bIQR = b2 - b1
rain$rain = ifelse(b2 + bIQR * 1.5 < rain$rain |
                     b1 - bIQR * 1.5 > rain$rain, NA, rain$rain)
table(is.na(rain$rain))

#NA행 전체 제거
rain <- na.omit(rain)

#--------------------------------------------------------------------
#회귀분석을 위해 year열을 기준으로 rain데이터와 배출데이터를 합친다.
analyze <- left_join(배출, rain, by = 'year')

#NA행 전체 제거
analyze <- na.omit(analyze)

#종속변수 : 지구온난화(total데이터), 독립변수 : 강수량(rain)
analyze_rain = analyze %>% select(total, rain)

#회귀식 모델 산출 및 시각화
analyze_model = lm(rain~total, data = analyze_rain)

plot(analyze_rain)
abline(analyze_model, col = "orange")
summary(analyze_model)


#--------------------------------------------------------------
#종속변수 : 지구온난화(total데이터), 독립변수 : 강수빈도(count)
analyze_count = analyze %>% select(total, count)

#회귀식 모델 산출 및 시각화
analyze_model = lm(count~total, data = analyze_count)

plot(analyze_count)
abline(analyze_model, col = "orange")
summary(analyze_model)

#p-value = 0.06145로 유의수준보다 미세하게 높은 결과를 보았다.

#1차 회귀분석으로는 유의미한 결과를 도출하지 못할것이라고 판단하여
#3차 회귀분석을 해보았다.
# 3차 회귀분석 모델 적합
analyze_model_3rd <- lm(count ~ poly(total, 3), data = analyze_count)

# 예측값 계산
predicted_values <- predict(analyze_model_3rd)

# 데이터 시각화
ggplot(analyze_count, aes(x = total, y = count)) +
  geom_point(color = "red", size = 2) +
  geom_line(aes(y = predicted_values), color = "blue", size = 1) +
  xlab("Total") +
  ylab("Count") +
  ggtitle("3rd Order Regression")

# 2023년을 예측하기 위한 새로운 데이터 생성
new_data <- data.frame(total = 2023)

# 3차 회귀분석 모델을 활용하여 2023년의 강수빈도(count) 값을 예측
prediction <- predict(analyze_model_3rd, newdata = new_data)

# 예측 결과 출력
prediction

#결국 귀무가설을 기각하지 못하여 지구온난화와 강수량은 관련이 없다 라는 결론이 도출되었다.
#-----------------------------------------------------------
