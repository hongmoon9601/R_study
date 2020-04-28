##############################################################################################################################################################
#시작하세요 데이터분석 with R
############################################################################################################################################
#201p 시간의 흐름에 따라 보기

company <-c("A", "A", "A", "A", "B", "B","B","B")
year <-c("1980", "1990", "2000", "2010", "1980", "1990", "2000", "2010")
sales <- c(2750, 2800, 2830, 2840, 2760, 2765, 2775, 2790)

coSaleDF <- data.frame(company, year, sales)

coSaleDF

install.packages("ggplot2")
library(ggplot2)
ggplot(coSaleDF, aes(x=year, y=sales)) + geom_line(aes(group=company))

# 선 색상 및 두꼐 설정
ggplot(coSaleDF, aes(x=year, y=sales)) +geom_line(size=2, aes(group=company, colour=company))

#점찍기
ggplot(coSaleDF, aes(x=year, y=sales)) +geom_line(size=2, aes(group=company, colour=company))+
  geom_point(size=2)




# 상관관계 보기 P203

str(cars)
plot(cars$speed, cars$dist, xlab="속도", ylab="제동거리")

#lowess 회귀분석값을 추정하는 함수와 그래프에 lines라는 두 항목간의 관계를 표현하는 추세선 그리기
lowess(cars$speed, cars$dist)
lines(lowess(cars$speed, cars$dist))

str(iris)
levels(iris$Species)
plot(iris)
plot(iris$Petal.Width, iris$Petal.Length)

str(iris)
cor(iris$Sepal.Length, iris$Sepal.Width)
plot(iris$Sepal.Length, iris$Sepal.Width)

cor(iris$Petal.Length, iris$Petal.Width)
plot(iris$Petal.Length, iris$Petal.Width)


#트리맵 P211
install.packages("read_xlsx")
library(readxl)
sales_df <- data.frame(read_xlsx("C:/Users/hong/Documents/R_excel_txt/P211_트리맵.xlsx",sheet=1))
sales_df
str(sales_df)

install.packages("treemap")
library(treemap)
treemap(sales_df, vSize="saleAmt", index = c("product", "region"), title="A기업 판매현황")
treemap(sales_df, vSize="saleAmt", index = c("region", "product"), title="A기업 판매현황")


#242P 표준정규분포표
pnorm(800, mean=600, sd=100)
pnorm(65, mean=50, sd=5)

#247P 중심극한정리
Aclass2 <- c(rep(10, 100),rep(50, 100), rep(100, 100))
mean(Aclass2)
hist(Aclass2)

par(mfrow = c(2,2))
asm2 <- sapply(1:10000, function(i) mean(sample(Aclass2, 2, replace=F)))
hist(asm2, main=" 표본크기 2")

asm2 <- sapply(1:10000, function(i) mean(sample(Aclass2, 30, replace=F)))
hist(asm2, main=" 표본크기 30")

asm2 <- sapply(1:10000, function(i) mean(sample(Aclass2, 50, replace=F)))
hist(asm2, main=" 표본크기 50")

asm2 <- sapply(1:10000, function(i) mean(sample(Aclass2, 80, replace=F)))
hist(asm2, main=" 표본크기 80")



# 251P 내가 확인하고 싶은 가설
# 대립가설 (alternative hypothesis): 가설 검정에서 연구자의 주장이 담긴 진술
# 귀무가설 (null hypothesis): 가설 검정에서 연구자의 주장에 대한 부정진술
# p-value(유의확률): 귀무가설이 참이라는 가정 아래 얻은 통계량이 귀무가설을 얼마나 지지하는지를 나타낸 확률
# p-value >=0.05 대립가설 기각 , 귀무가설 채택 , p-value< 0.05 귀무가설 기각, 대립가설 채택

summary(lm(cars$speed~ cars$dist))

#255P  t-검정
#: 추출된 두 표본을 근거로 그 원천인 모집단을 추정해 두 모집단이 서로 같은지 다른지 확인하는 기법 (평균을 이용)
before_study <- c(34,76,76,63, 73, 75, 67, 78, 81, 77, 80, 43, 65, 76, 63, 54, 64, 85, 54, 70, 71, 71, 55, 40, 78, 76, 100, 51, 93, 64, 42, 63, 61, 82, 67, 98, 59, 63, 84, 50, 67, 80, 83, 66, 86, 57, 48)
after_study<- c(74, 87, 89, 98, 65, 82, 70, 70, 70, 84, 56, 76, 72, 69, 73, 61, 83, 82, 89, 75, 48, 72, 80, 66, 82, 71, 49, 54, 70, 65, 74, 63, 65, 101, 82, 75, 62, 83, 90, 76, 87, 90, 78, 63, 59, 79, 74, 65, 77, 74)

mean(before_study)
mean(after_study)
par(mfrow= c(1,1))
boxplot(before_study, after_study, names=c("수강전", "수강후"))

# t-검정이란 : 두 집단의 평균값이 다른가 (두 표본의 차이가 아닌 두 표본의 모집단 간 차이를 검정하는 것이다.)
# t-검정의 가정 : 두 모집단은 정규분포를 따른다.
# 표본의 크기가 30개를 넘으면 중심극한정리에 의해 정규분포를 따른다고 가정 가능
# 표본의 크기가 30미만이면 shapiro-wilk 검정을 통해 정규성 검증 가능
# ex) shapiro-wilk
shapiro.test_vector <- c(74, 87, 89, 98, 65, 82, 70, 70, 70)
shapiro.test(shapiro.test_vector)
# p-value가 0.05보다 크기에 귀무가설 채택 -> 주어진 데이터가 정규 분포를 따른다.

#t-test
var.test(before_study, after_study)
# 귀무가설: 분산이 같다, 대립가설: 분산이 다르다.

# t-검정을 통한 학원 수업 수강 후 성적의 향상 여부
t.test(before_study, after_study, paried=TRUE)
#paired (대응표본인 경우 TRUE, 독립표본의 경우 FALSE)
# 대응표본: 한 집단으로부터 두 번 반복해 샘플 추출  ex( 백신을 맞기 전과 후 적혈구 수치 비교)
# 독립표본: 서로 독립된 집단에서 각각 샘플 추출 ex(나라별 키 비교, a와 b학교의 성적 비교)
# -> p-value는 0.0383 대립 채택
# alternative hypothesis: true difference in means is not equal to 0

t.test(before_study, after_study, paried=TRUE, alternative="less")
t.test(before_study, after_study, paried=TRUE, alternative="greater")



# P271 lm 함수 _회귀 분석
str(cars)
lm_result <- lm(formula= dist ~ speed, data= cars)
lm_result
summary(lm_result)
plot(cars$speed, cars$dist)
abline(lm_result)

par(mfrow = c(2,2))
plot(lm_result)
par(mfrow = c(1,1))

# P280 , 예측하기
speed <- c(50, 60, 70, 80, 90, 100)
df_input <- data.frame(speed)
df_input

predict(lm_result, df_input)
predict_dist <- predict(lm_result, df_input)
cbind(df_input, predict_dist)

#구간추정 interval = confidence 모델계수에 대한 불확실성을 감안한 구간 추정
predict_dist <- predict(lm_result, df_input, interval="confidence", level=0.95)
predict_dist
cbind(df_input, predict_dist)

# 구간 추정 interval = prediction  모델계수에 대한 불확실성과 결과의 오차까지 감안한 구간 추정
predict_dist <- predict(lm_result, df_input, interval="prediction", level=0.95)
predict_dist
cbind(df_input, predict_dist)

# P290 사과 품종
library(readxl)
apple_df <- data.frame(read_xlsx("C:/Users/hong/Documents/R_excel_txt/P288_사과품종.xlsx", sheet=1))
apple_df
colnames(apple_df)[1] <- "model"
colnames(apple_df)[2] <- "weight"
colnames(apple_df)[3] <- "sugar"
colnames(apple_df)[4] <- "acid"
colnames(apple_df)[5] <- "color"
apple_df

str(apple_df)

boxplot(weight ~ model, data= apple_df, ylab="무게")
boxplot(sugar ~ model, data= apple_df, ylab="당도")
boxplot(acid ~ model, data= apple_df, ylab="신도")

library("ggplot2")
k <- ggplot(apple_df, aes(factor(color), fill= factor(model)))
k+ geom_bar()

# P299  의사결정 트리 rpart 함수
str(iris)
levels(iris$Species)
nrow(iris)
table(iris$Species)

#createDataPartiton(추출할 팩터 데이터, 항목별 추출할 비율, list 타입니냐 아니냐)
install.packages("caret")
library(caret)
iris_row_idx <- createDataPartition(iris$Species, p=0.8, list=FALSE)
str(iris_row_idx)
iris_train_data <- iris[iris_row_idx,]
str(iris_train_data)
table(iris_train_data$Species)

iris_test_data <- iris[-iris_row_idx,]
str(iris_test_data)
table(iris_test_data$Species)

# decision tree의 rpart 실행하기
summary(iris_train_data)
summary(iris_test_data)

install.packages("rpart")
library(rpart)
iris_rpart_result <- rpart(Species ~., data= iris_train_data, control=rpart.control(minsplit = 2))
iris_rpart_result

# decision tree 시각화
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(iris_rpart_result)

#가지치기 cp (Complexity Parameter)
iris_rpart_result$cptable

iris_trune_tree <- prune(iris_rpart_result, cp=0.0125)
rpart.plot(iris_trune_tree)

# test data로 예측해보기
str(iris_test_data)
predict(iris_rpart_result, iris_test_data, type="class")

# 실제와 비교해보기
actual <- iris_test_data$Species
expect <- predict(iris_rpart_result, iris_test_data, type="class")
iris_predict_df <- data.frame(actual, expect)
iris_predict_df

# 혼동행렬 만들기
table(iris_predict_df)

# 혼동행렬 confusion matrix의 accuracy, precision, sensitivity(recall), specificity
library(caret)
confusionMatrix(expect, actual, mode="everything")
install.packages("e1071")
library(e1071)
confusionMatrix(expect, actual, mode="everything")

summary(apple_df)
apple_df$color <- as.factor(apple_df$color)
apple_df$model <- as.factor(apple_df$model)

apple_train_idx <- createDataPartition(apple_df$model, p=0.8, list=FALSE)
nrow(apple_df)
nrow(apple_train_idx)

apple_train_data <- apple_df[apple_train_idx,]
apple_test_data <- apple_df[-apple_train_idx,]

apple_rpart_result <- rpart(model ~., data=apple_train_data, control=rpart.control(minsplit = 2))
rpart.plot(apple_rpart_result)

#confusion matrix , complexity parameter
actual <- apple_test_data$model
expect <- predict(apple_rpart_result, apple_test_data, type="class")

confusionMatrix(expect, actual, mode="everything")

apple_rpart_result$cptable
library(rpart.plot)
apple_prune_tree <- prune(apple_rpart_result, cp = 0.0625)
rpart.plot(apple_prune_tree)


#검정
actual <- apple_test_data$model
expect <- predict(apple_prune_tree, apple_test_data, type="class")
library(caret)
confusionMatrix(expect, actual, mode="everything")



# P317 사과의 품종 직접 정하기
str(apple_df)
hong_apple <- apple_df[apple_df$color == "홍색", 2:4]
nrow(hong_apple)
juk_apple <- apple_df[apple_df$color == "적색", 2:4]
nrow(juk_apple)

plot(hong_apple)

hong_apple2 <- hong_apple[hong_apple$acid < 0.36 & hong_apple$weight <350,]
plot(hong_apple2)

plot(hong_apple2$acid, hong_apple2$sugar, ylab="sugar", xlab="acid")




# 군집분석

x <- c(10, 11, 12, 10, 11, 12, 20, 21, 22, 20, 21, 22)
y <- c(4000, 3900, 4000, 1000, 800, 1000, 4000, 3900, 4000, 1000, 800, 1000)

simpleSquare <- data.frame(x,y)
simpleSquare

#scale함수를 이용한 표준화           관측값 - 평균 / sd (표준편차)
ss_scaled <- scale(simpleSquare)
ss_scaled
summary(ss_scaled)
sd(ss_scaled[,"x"])
sd(ss_scaled[,"y"])

plot(ss_scaled, type="n")
text(ss_scaled)

# 개체간의 거리를 표현한 거리행렬 (유클리디안, 매하탄, 캔버라, 민코우스키 거리)
ss_scaled_dist <- dist(ss_scaled)
ss_scaled_dist
min(ss_scaled_dist)
max(ss_scaled_dist)

#계층적 군집분석 실행 (기본값: 최장연결법  complete)
result_hclust <- hclust(ss_scaled_dist)
result_hclust

#덴드로그램 그리기
plot(result_hclust)

#2,3,4,5개의 군집으로 나눌떄
cutree(result_hclust, k=2)
cutree(result_hclust, k=3)
cutree(result_hclust, k=4)
cutree(result_hclust, k=5)

group2 <-cutree(result_hclust, k=2)
group3 <- cutree(result_hclust, k=3)
group4 <- cutree(result_hclust, k=4)
group5 <- cutree(result_hclust, k=5)

#pch: 점모양 cex: 크기, col: 색상
par(mfrow=c(2,2))
plot(ss_scaled,pch=16, cex=2, col=group2, main="2개 그룹룹")
plot(ss_scaled,pch=16, cex=2, col=group3, main="3개 그룹룹")
plot(ss_scaled,pch=16, cex=2, col=group4, main="4개 그룹룹")
plot(ss_scaled,pch=16, cex=2, col=group5, main="5개 그룹룹")
par(mfrow=c(1,1))

# 비계층적 군집분석 k-medoids알고리즘의 pam
install.packages("cluster")
library(cluster)
pam_result <- pam(simpleSquare, k=4, stand=T)

#군집의 중심이 되는 개체
pam_result$medoids

#군집결과
pam_result$clustering

plot(simpleSquare, pch=16, cex=2, col=pam_result$clustering, main="4개 그룹")
points(pam_result$medoids, pch=3, cex=3)


# fviz_nbclust함수를 이용한 elbow method와 실루엣 silhouette 기법 테스트
install.packages("rlang")
install.packages("factoextra")

library(factoextra)
library(cluster)

# elbow method 검증법
fviz_nbclust(scale(simpleSquare), pam, method="wss", k.max=8)

# 실루엣 silhouette 검증 기법 
fviz_nbclust(scale(simpleSquare), pam, method="silhouette", k.max=8)

# 사과 품종을 이용한 클러스터링 군집분석
summary(apple_df)
apple_df_without_model <- apple_df[ , c(2:5)]
summary(apple_df_without_model)


# 범주형 데이터 color를 포함해 군집분석하기위해 kproto 이용
install.packages("clustMixType")
library(clustMixType)

kproto_result <- kproto(apple_df_without_model, 2)

str(kproto_result)
kproto_result$cluster
kproto_result$tot.withinss

wss <- 0
for(i in 1:10){
  kproto_result <- kproto(apple_df_without_model, i)
  wss[i] <- kproto_result$tot.withinss
}

plot(wss, type="b")


#4의 군집으로 나눅
cluster_result <- kproto(apple_df_without_model, 4)
cluster_result$cluster
table(cluster_result$cluster)

# 새로 나워진 사과 품종
cluster_result$cluster

apple_df[ , "model"]
table(cluster_result$cluster, apple_df[ , "model"])

#항목별 군집간의 차이 확인하기
# clporiles ( kproto 분석결과, 군집분석 시 사용한 데이터)
clprofiles(cluster_result, apple_df_without_model)








##################################################################################################################


#연관 분석
#배낭여행과 연관된 검색어 
searchL <- list(
  c("동남아", "푸켓", "수영복", "유심", "패키지", "가족여행", "자유여행", "리조트", "베트남")
  , c("가족여행", "패키지", "유럽", "푸켓", "자유여행", "환율", "신혼여행", "신발사이즈표")
  , c("보라카이", "신혼여행", "날씨", "환율", "비행기표", "풀빌라", "시차")
  , c("패키지", "동남아", "가족여행", "휴양지", "여행지추천", "특가")
  , c("일본", "번역기", "후쿠오카", "온천", "가족여행", "리조트", "포켓와이파이")
  , c("몰디브", "신혼여행", "항공권", "동남아", "비행시간", "숙소", "비자", "발리", "풀빌라")
  , c("호텔", "동남아", "세부", "호핑투어", "리조트", "신혼여행", "풀빌라", "필리핀")
  , c("푸켓", "풀빌라", "여행", "신혼여행", "자유여행", "와이파이", "코타키나발루")
  , c("동남아", "보홀", "보라카이", "팔라완", "가족여행", "스쿠버다이빙", "여행책")
  , c("푸켓", "가족여행", "보라카이", "동남아", "스쿠버다이빙", "리조트", "피피섬")
  , c("배낭여행", "유럽", "호스텔", "북유럽", "서유럽", "파리", "루브르", "에투알", "에펠")
  , c("이탈리아", "베네치아", "토스카니", "피렌체", "바티칸", "여행지도")
  , c("하와이", "괌", "푸켓", "세부", "리조트", "가족여행", "골드카드")
  , c("괌", "푸켓", "세부", "호텔", "풀빌라", "가족여행", "힐튼", "쉐라톤")
  , c("베네치아", "피렌체", "신혼여행", "로마", "패키지")
  , c("배낭여행", "유럽", "호텔팩", "공항", "환율", "픽업서비스", "런던", "파리", "체코", "호스텔")
  , c("특가", "파리", "환율", "스위스", "이탈리아", "오스트리아", "와이파이", "호스텔")
  , c("지중해", "유럽", "특가", "배낭여행", "패키지", "파리", "스위스", "이탈리아", "오스트리아")
  , c("유럽", "동유럽", "날씨", "체코", "환율", "비엔나", "배낭여행", "부다페스트", "호스텔")
  , c("유심", "체코", "신혼여행", "크로아티아", "패키지", "비엔나", "류블랴냐", "독일", "동유럽", "부다페스트")
  , c("패키지", "지중해", "호텔", "유럽", "동유럽", "폴란드", "부다페스트", "신혼여행", "프라하", "크로아티아")
  , c("동유럽", "폴란드", "체코", "프라하", "독일", "크로아티아", "날씨")
  , c("이스탄불", "호스텔", "유럽", "자유여행", "배낭여행", "지중해", "날씨", "파묵칼레")
  , c("신혼여행", "이탈리아", "지중해", "날씨", "유럽", "자유여행", "와이파이", "유심")
  , c("이탈리아", "지중해", "산토리니", "아테네", "유럽", "터키")
  , c("유심", "터키", "유럽", "그리스", "지중해", "이탈리아")
  , c("배낭여행", "유심", "지중해", "아테네", "산토리니", "메테오라", "로마", "베네치아")
  , c("유럽", "날씨", "동유럽", "사진", "우산", "3박10일", "패키지")
)  


str(searchL)
searchV <- unlist(searchL)
str(searchV)
searchT <- table(searchV)
searchT

searchT <- sort(searchT, decreasing=T)
searchT[1:5]

# "유럽" %in% x: 벡터 x 요소 중 "유럽"이 있으면 TRUE, 없으면 FALSE
searchEuropeIdx <- sapply(searchL, function(x){"유럽" %in% x})

# 산출된 논리벡터 확인("유럽"이라는 단어를 포함한 벡터의 위치는 TRUE)
searchEuropeIdx

# 유럽을 조회한 사람들의 검색어만 따로 분리
searchEuropeL <- searchL[searchEuropeIdx]

# 분리한 검색어 내역 확인(유럽을 조회한 11명의 검색어 모음)
str(searchEuropeL)

# 테이블 변환
searchEuropeT <- table(unlist(searchEuropeL))

# 빈도수 기준으로 정렬
searchEuropeT <- sort(searchEuropeT, decreasing=T)

# 상위 5개 검색어 확인
searchEuropeT[1:5]



#연광성 분석 - 지.신.향 (지지도, 신뢰도, 향상도)
#지지도 : (조건과 결과 항목을 포함하는 거래 수 )  / (전체 거래수)
#신뢰도 : (조건과 결과 항목을 포함하는 거래수)     /  (조건 항목을 포함하는 거래 수)
# 향상도 : (연광성 규칙의 지지도)   /   (조건의 지지도 * 결과의 지지도 )


# 최초 수행 시 패키지 설치
install.packages("arules")

# arules 패키지 로드
library(arules)

# 고객별 구매 품목을 리스트로 생성
buyItems <- list(
  c("삼겹살", "생수", "소주", "과자")
  ,c("삼겹살", "생수", "소주", "사과")
  ,c("장어", "생수", "소주", "양파")
  ,c("땅콩", "생수", "맥주", "오이")
  ,c("땅콩", "생수", "맥주", "감")
)

# 트랜잭션 데이터로 형변환
buyItemStr <- as(buyItems, "transactions")

# 변환된 트랜잭션 확인(11개 항목에 대해 5개 거래 존재)
buyItemStr

# 트랜잭션 데이터는 inspect 함수를 통해 내용을 확인
inspect(buyItemStr)

# apriori 함수 수행(지지도 0.1, 신뢰도 0.8 이상인 연관성 규칙 구하기)
buyItemResult <- apriori(buyItemStr, parameter=list(support=0.1, confidence=0.8))


######결과 보기
# 도출된 연관성 규칙 5개만 확인
buyItemResult[1:5]

# 연관성 규칙 상세 보기
inspect(buyItemResult[1:5])

# 향상도가 1.2 이상인 연관성 규칙만 선택
subBuyResult <- subset(buyItemResult, subset=lift > 1.2 )

# subset 결과
subBuyResult

# 연관성 규칙 5개만 확인
inspect(subBuyResult[1:5])


# lhs에 삼겹살이 포함된 연관성 규칙
inspect(subset(buyItemResult, subset=lhs %in% c("삼겹살")))

# lhs에 삼겹살과 과자가 포함된 연관성 규칙
inspect(subset(buyItemResult, subset=lhs %ain% c("삼겹살", "과자")))

# lhs가 삼겹살 or 과자 or 삼겹살과 과자인 연관성 규칙
inspect(subset(buyItemResult, subset=lhs %oin% c("삼겹살", "과자")))

# lhs 항목 중 "겹"이라는 글자를 포함하는 연관성 규칙
inspect(subset(buyItemResult, subset=lhs %pin% "겹"))

# 지지도, 신뢰도, 향상도 기준으로 정렬
subBuyResult_order <- sort(subBuyResult, by=c("support", "lift", "confidence"))

# 상위 10개만 확인
inspect(subBuyResult_order[1:10])

# 처음 실행 시 패키지를 설치
install.packages("arules")

# arules 패키지 로드
library(arules)

# 항목별 빈도수 시각화(최소 지지도 0.2 이상인 항목에 대해서만 빈도수 확인)
# itemFrequencyPlot 함수는 "트랜잭션 데이터"를 입력 항목으로 받습니다.
itemFrequencyPlot(buyItemStr, support=0.2)

# 최초 실행 시 패키지 설치 필요
install.packages("arulesViz")

# 패키지 로드
library(arulesViz)

# 연관성 분석 결과 객체
subBuyResult_order

# 3번째 연관성 규칙 확인
inspect(subBuyResult_order[3])

# 3번째 연관성 규칙을 평행좌표 그래프로 표현
plot(subBuyResult_order[3], method="paracoord")

# 3,5,33,50번째 연관성 규칙 확인
inspect(subBuyResult_order[c(3, 5, 33, 50)])

# 3,5,33,50번째 연관성 규칙을 하나의 평행좌표 그래프에 표현
plot(subBuyResult_order[c(3, 5, 33, 50)], method="paracoord")

# 처음 10개의 연관성 분석 확인
inspect(subBuyResult_order[1:10])

# 10개의 연관성 규칙에 대한 네트워크 그래프 그리기
plot(subBuyResult_order[1:10], method="graph")

# 고객 검색 내역 확인
str(searchL)

# 트랜잭션 데이터 변환
searchT <- as(searchL, "transactions")

# 생성된 트랜잭션 확인
searchT

# 트랜잭션 내용 확인
inspect(searchT)

# 처음 실행 시 설치
install.packages("arules")

# arules 패키지 로드
library(arules)

# 연관성 분석(지지도 0.1 이상, 신뢰도 0.8 이상 연관성 규칙 도출)
aResult <- apriori(searchT, parameter=list(support=0.1, confidence=0.8))

# 도출된 연관성 규칙 지지도, 향상도, 신뢰도 기준으로 정렬
aResult <- sort(aResult, by=c("support", "lift", "confidence"))

# 연관성 규칙 확인
inspect(aResult)

# 배낭여행을 포함하는 연관성 규칙 추출
packResult <- subset(aResult, subset=lhs %in% c("배낭여행") | rhs %in% c("배낭여행"))

# 연관성 규칙 확인
inspect(packResult)

# 조건 항목만 별도 추출해 리스트로 변환
packLhs <- as(lhs(packResult), "list")

# 조건 항목 확인
str(packLhs)

# 결과 항목만 별도 추출해 리스트로 변환
packRhs <- as(rhs(packResult), "list")

# 결과 항목 확인
str(packRhs)

# 조건과 결과 항목을 벡터로 변환
vPackWord <- unlist(packLhs, packRhs)

# 배낭여행과 연관된 검색어 확인
vPackWord

# 중복 항목 제거
unique(vPackWord)

# 최초 실행 시 패키지 설치 필요
install.packages("arulesViz")

# 패키지 로드
library(arulesViz)

# 검색어 네트워크 그래프
plot(aResult, method="graph")
