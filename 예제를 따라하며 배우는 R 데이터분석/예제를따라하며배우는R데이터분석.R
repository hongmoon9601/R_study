# 예제를 따라하며 배우는 R 데이터 분석



#Chapter 2 R 기초 활용

##여러개의 변수값을 하나로 결합하여 특정한 변수명에 입력하기 위하여 함수 c()를 사용
name <- c("김성민", "민경우", "윤은상", "이원", "정만식", "최수진")
sales_1Q <-c(100, 90, 92, 85, 80, 75)
sales_2Q <-c(90, 95, 97, 85, 80, 74)

name <- c(name, "홍진수")
sales_1Q <- c(sales_1Q, 93)
sales_2Q <-c(sales_2Q, 92)

name
sales_1Q
sales_2Q

#R의 데이터 셋 : 벡터, 행렬, 데이터프레임
#벡터: 같은 종류의 데이터를 여러개 묶은것
#행렬: 벡터를 여러개의 행과 열로 나타낸것
# 데이터프레임: 여러개의 벡터를 하나로 묶은 것

df_1 <- data.frame(name, sales_1Q, sales_2Q)
df_1
str(df_1)

library(writexl)
write_xlsx(df_1,"C:/Users/hong/Desktop/빅데이터가만드는세상_이종형/R/example.xlsx")
library(readxl)
read_xlsx("C:/Users/hong/Desktop/빅데이터가만드는세상_이종형/R/example.xlsx")


df_1$name
df_1[,1]

df_1[sales_1Q>90,]
df_1[,sales_1Q>90]

sum_sales <- c(sales_1Q + sales_2Q)
avg_sales <- sum_sales/2
avg_sales

df_1 <- data.frame(df_1, sum_sales, avg_sales)
df_1

grade <- ifelse(avg_sales >=95, "S등급", ifelse(avg_sales >=90, "A등급", "B등급"))
df_1 <- data.frame(df_1, grade)
df_1
summary(df_1)
sd(avg_sales)
df_1[, c(1:3, 5,6)]

df_1[avg_sales >= 80,]
df_1[avg_sales>=80 & avg_sales <=90,]
df_1[avg_sales> 90 | avg_sales < 80,]
df_1[grade=="A등급",]

subset(df_1, select = c("name","sales_1Q", "sales_2Q"))
subset(df_1, select= c(1:3, 5))
subset(df_1, grade=="A등급", select= c(1:6))
subset(df_1[grade == "A등급",], select = c(1:6))

write.csv(df_1, "C:/Users/hong/Desktop/빅데이터가만드는세상_이종형/R/df_1.csv")
read.csv("C:/Users/hong/Desktop/빅데이터가만드는세상_이종형/R/df_1.csv")

install.packages("readxl")
install.packages("writexl")
library(readxl)
library(writexl)

writexl::write_xlsx(df_1, path ="C:/Users/hong/Desktop/빅데이터가만드는세상_이종형/R/df_1.xlsx")
read_xlsx("C:/Users/hong/Desktop/빅데이터가만드는세상_이종형/R/df_1.xlsx", sheet=1)

setwd("C:/Users/hong/Desktop/빅데이터가만드는세상_이종형/R/")
read_xlsx("df_1.xlsx")

# 막대그래프 barplot 생성하기
# barplot: 범주별로 조사된 빈도 즉 개수를 막대로 나타내는 그래프
barplot(sales_1Q, names=name)

barplot(sales_1Q, names=name, xlab="사원명", ylab = "1분기 판매실적", main="사원별 1분기 판매실적", col= rainbow(7))

# 히스토그램 histogram 작성
# histogram은 변수값의 최대값과 최소값 사이를 적당한 개수의 동일한 구간으로 나누고 각 구간에 속한 데이터의 개수를 막대로 나타내는 그래프 
hist(sales_1Q, xlab= "1분기 판매실적", ylab="빈도", main="1분기 판매실적에 관한 분포", col= "yellow")


#pie 차트 작성
pie(sales_1Q, labels = name, main = "사원별 1분기 판매실적적")

name_1 <- paste(name, sales_1Q)
name_1

pie(sales_1Q, labels = name_1, main="사원별 1분기 판매실적", col = rainbow(length(name)))

# 1분기 총 판매실적 중 사원별 판매 비율을 계산
# 계산한 비율을 소수 첫 번째 자리로 반올림
pct_sales_1Q <- round(sales_1Q * 100/ sum(sales_1Q),1)
pct_sales_1Q

name_2 <- paste(name, "\n", pct_sales_1Q, "%")
name_2

pie(sales_1Q, labels = name_2, col = rainbow(length(name)))


# 꺽은선 그래프 line graph 작성
# 1분기 판매실적이 꺽은선으로 나타나도록 type="l"을 적용

plot(sales_1Q, type="l", xlab= "사원명", ylab= "1분기 판매실적", main="사원별 1분기 판매실적적")

plot(sales_1Q, type="l", xlab= "사원명", ylab= "1분기 판매실적", axes =F, main="사원별 1분기 판매실적적")
axis(1, at=1:7, lab = name)
axis(2, ylim= c(0,100))

sales <- cbind( sales_1Q, sales_2Q)
sales
matplot(sales, pch= c(1,2),type=c("b","b"), lty=c(1,2), axes=F, ann=F)
axis(1, at=1:7, lab= name)
axis(2, ylim=c(0,100))

title(main="사원별 1, 2분기 판매실적", font =3, cex=1)
title(xlab = "사원명")
title(ylab="판매실적적")

legend(1, 80, c("1분기 판매실적", "2분기 판매실적"), lty=1:2, col= c("black", "red"), text.col = c("black", "red"))


#baxplot
# 분위를 나타낼때 사용 barplot과 다름 알아라

summary(sales_1Q)
boxplot(sales_1Q, col= "yellow", main="1분기 판매실적적", horizontal =TRUE)
data_sales <- cbind(sales_1Q, sales_2Q, avg_sales)
boxplot(data_sales, col =c("skyblue", "pink", "yellow"), names = c("1분기", "2분기", "3분기"), main="여러 판매실적 비교")

par(mfrow=c(2,2))
boxplot(sales_1Q, col="skyblue", main= "1분기 판매실적적")
boxplot(sales_2Q, col="pink", main= "2분기 판매실적적")
boxplot(avg_sales, col="yellow", main= "1,2분기 평균 판매실적적")
boxplot(data_sales, col =c("skyblue", "pink", "yellow"), names = c("1분기", "2분기", "3분기"), main="여러 판매실적 비교")

par(mfrow= c(1,1))
















###### Chapter 3  건강검진 빅데이터 분석하기

df_data <- read.csv("C:/Users/hong/Desktop/빅데이터가만드는세상_이종형/R/건강검진정보(2017).csv")
str(df_data)
head(df_data)

colnames(df_data)[3] <- "gender"
colnames(df_data)[4] <- "age"
colnames(df_data)[6] <- "height"
colnames(df_data)[7] <-"weight"

str(df_data)

new_data <- subset(df_data, select = c("gender", "age", "height", "weight"))
str(new_data)

head(new_data)

#비만도 계산하기
new_data$bmi <- new_data$weight/((new_data$height/100)^2)
head(new_data)

new_data$bmi_level <- ifelse(new_data$bmi >=30, "고도비만",
                             ifelse(new_data$bmi >=25, "비만",
                                    ifelse(new_data$bmi>=23,"과체중",
                                           ifelse(new_data$bmi >= 18.5, "정상", "저체중중"))))

head(new_data)

# 대표값과 결측값 처리
summary(new_data)

new_data$gender <- factor(new_data$gender, levels = c(1,2), labels = c("male", "female"))
summary(new_data)

new_data$age <- factor(new_data$age)
summary(new_data$age)

new_data$bmi_level <- factor(new_data$bmi_level)
summary(new_data$bmi_level)

# 결측값이 포함된 행 데이터를 제거
new_data <- na.omit(new_data)
summary(new_data)

#교차표 작성
#변수 gender와 bmi_level간의 교차표 작성
table(new_data$gender, new_data$bmi_level)

# 행 합계 기준 비율 분석(남자는 남자 여자는 여자 한에서)
prop.table(table(new_data$gender, new_data$bmi_level), margin = 1)

#열 합계 기준 비율 분석( 남녀 섞은 상태)
prop.table(table(new_data$gender, new_data$bmi_level), margin = 2)


#변수 age와 bmi_level간의 교차표 작성
table(new_data$age, new_data$bmi_level)

new_data$new_age <- ifelse((new_data$age == "5" | new_data$age == "6"),"20대",
                       ifelse((new_data$age == "7" | new_data$age == "8"), "30대",
                       ifelse((new_data$age == "9" | new_data$age =="10"), "40대",
                       ifelse((new_data$age == "11" | new_data$age == "12"), "50대",
                       ifelse((new_data$age == "13" | new_data$age == "14"), "60대",
                       ifelse((new_data$age == "15" | new_data$age == "16"), "70대","80대 이상"))))))
new_data$new_age <-factor(new_data$new_age)

table(new_data$new_age, new_data$bmi_level)

#행 합계 기준 비율 분석
round(prop.table(table(new_data$age, new_data$bmi_level), margin=1),4)

#일부데이터를 추출하여 교차표 작성하기(여성으로만 female)
new_data2 <- new_data[new_data$gender == "female",]
head(new_data2)

str(new_data2)

#여성의 연령대별 비만도 수준에 대한 교차표 (빈도)
table(new_data2$new_age, new_data2$bmi_level)

#여성의 연령대별 비만도 수준에 대한 교차표(비율)
round(prop.table(table(new_data2$new_age, new_data2$bmi_level), margin=1), 4)






#############################################################################################
####Chapter 4 빅데이터 시각화 분석


install.packages("ggplot2")
library(ggplot2)

#성별에 따른 인원수를 막대그래프로 작성
ggplot(new_data, aes(x=gender)) + geom_bar()

# 성별 범례를 그래프에 추가
ggplot(new_data, aes(x=gender, fill = gender)) + geom_bar()

#전체 검진자 몸무게의 히스토그램
ggplot(new_data, aes(x=weight)) + geom_histogram()
new_data$weight

# 성별에 따라 세분화된 몸무게의 히스토그램(범례포함)
ggplot(new_data, aes(x=weight, fill = gender)) + geom_histogram()


#비만도 수준으로 세분화한 남성 검진자 BMI의 히스토그램
ggplot(new_data[new_data$gender == "male",],
       aes(x=bmi, fill = bmi_level)) +
  geom_histogram()+
  xlim(10, 100)

#비만도 수준으로 세분화한 여성 검진자 BMI의 히스토그램
ggplot(new_data[new_data$gender == "female",],
       aes(x=bmi, fill = bmi_level)) +
  geom_histogram()+
  xlim(10, 100)


# 산점도 시각화 분석 geom_point()
#성별에 따른 키와 몸무게의 산점도
ggplot(new_data, aes(x=height, y = weight, shape = gender, color=gender)) + geom_point()

# 성별에 따른 키와 몸무게를 산점도로 나타내기
# 몸무게에 따라 점의 크기가 변하도록 설정
ggplot(new_data, aes(x= height, y = weight, shape= gender, color=gender)) + 
  geom_point(aes(size= weight)) +
  scale_size(range= c(1,3))

# 연령대별 BMI의 상자그림
ggplot(new_data, aes(x=new_age, y=bmi, fill=new_age)) + geom_boxplot()

# 상자그림의 제목, 축 제목, 범례 제목 추가
ggplot(new_data, aes(x=new_age, y=bmi, fill=new_age)) + geom_boxplot() +
  xlab("연령")+ theme(axis.title.x=element_text(size =12))+ 
  ylab("bmi")+ theme(axis.title.y=element_text(size =11)) +
  ggtitle("상자그림(Boxplot)을 활용한 연령별 BMI 분석") +
  theme(plot.title=element_text(hjust=0.5, size =14)) + 
  scale_fill_discrete(name="연령령")

#비만도 수준에 따른 BMI의 상자그림
ggplot(new_data, aes(x=bmi_level, y=bmi, fill=bmi_level)) + geom_boxplot()

#비만도 수준에 따른 BMI의 상자그림 (정렬기준: 비만도 수준)
ggplot(new_data, aes(x=bmi_level, y=bmi, fill=bmi_level)) + geom_boxplot()+
  scale_x_discrete(limits= c("고도비만", "비만", "과체중", "정상", "저체중"))




####ggmap 을 활용한 서울시 응급의료병원 지도 작성
install.packages("xlsx")
library(xlsx)
er_data <- read_xlsx("C:/Users/hong/Desktop/빅데이터가만드는세상_이종형/R/병원·약국찾기.xls")

head(er_data)
data.frame(er_data)
str(er_data)

colnames(er_data)[2] <- "name"
colnames(er_data)[6] <- "address"
typeof(er_data)


covid_data <- subset(er_data, select= c("name", "address"))

write.xlsx(covid_data, 'covid_data.xlsx')

?write_xlsx


head(covid_data)
data.frame(covid_data)
typeof(covid_data)

install.packages("ggmap"); library(ggmap)
install.packages("ggplot2"); library(ggplot2)


#geocode() 함수를 사용하여 위도(latitude), 경도(longitud) 찾기
#병원의 한글 주소를 인코딩하기 위하여 enc2utf8() 이용
# enc2utf8() 함수는 인수로 문자형을 사용
#as.character() 함수를 이용하여 변수 address의 속성을 문자형으로 변환

covid_data$address <- as.character(covid_data$address)
covid_geocode <- geocode(enc2utf8(covid_data$address))

covid_geocode <- geocode('covid_data',source = "google")

covid_geocode
?geocode
str(covid_data)

covid_data$address <- enc2utf8(covid_data$address)
head(covid_data$address)

covid_data$address <- as.character(covid_data$address)
data_lonlat <- mutate_geocode(covid_data, address, source("google"))



head(covid_data)
covid_data <- read_xlsx("C:/Users/hong/Desktop/빅데이터가만드는세상_이종형/R/covid_data.xlsx")
head(covid_data)

covid_data$Longitude <- as.numeric(covid_data$Longitude)
covid_data$Latitude <- as.numeric(covid_data$Latitude)

str(covid_data$Longitude[1])
covid_data$Longitude[1]

head(covid_data)

covid_geocode <- subset(covid_data, select = c('Longitude', "Latitude"))
head(covid_geocode)
center_geocode <- c(mean(covid_geocode[,1]), mean(covid_geocode[,2]))
center_geocode


install.packages("ggmap")
library(ggmap)
covid_map <- ggmap(get_navermap())




## 텍스트 마이닝과 워드 클라우드 분석

install.packages("rJava")
install.packages("memoise")
# 한글 자연어 처리 패키지 설치
install.packages("KoNLP")

#데이터 전처리 패키지 dplyr 및 문자열 처리 패키지 stringr 설치
install.packages("dplyr")
install.packages("stringr")

library(rJava); library(memoise); library(KoNLP); library(dplyr); library(stringr)

#한글 형태소 분석이 가능한 NIA 한국정보화진흥원 사전을 사용하도록 설정
useNIADic()

text <- readLines("C:/Users/hong/Desktop/빅데이터가만드는세상_이종형/R/긴급재난지원금.txt")
text




library(tidyverse)
library(wordcloud2)
library(KoNLP)
library(rvest)

news_url <- "https://search.naver.com/search.naver?where=nexearch&query=%EA%B8%B4%EA%B8%89%EC%9E%AC%EB%82%9C%EC%A7%80%EC%9B%90%EA%B8%88&sm=top_lve.agallgrpmamsi0en0sp0&ie=utf8"

news_html <- read_html(news_url)

news_urls <- news_html %>% 
  html_nodes(“.news.mynews.section._prs_nws”) %>% 
  html_nodes(“._sp_each_url”) %>% 
  html_attr(“href”)







## 네이버 뉴스에서 원하는 키워드의 검색 결과를 웹크롤링(스크래핑)하는 코드
## 제작: hkim (dr-hkim.github.io)

## 패키지 불러오기
library(rvest)
library(dplyr)

## 변수 입력하기
QUERY <- "미국+기준금리" # 검색키워드
DATE  <- as.Date(as.character(20181119),format="%Y%m%d") # 검색시작날짜 & 검색종료날짜
DATE <- format(DATE, "%Y.%m.%d")
PAGE  <- 1

naver_url_1 <- "https://search.naver.com/search.naver?&where=news&query="
naver_url_2 <- "&pd=3&ds="
naver_url_3 <- "&de="
naver_url_4 <- "&start="

## 날짜 리스트 만들기
DATE_START <- as.Date(as.character(20181119),format="%Y%m%d") # 시작일자
DATE_END   <- as.Date(as.character(20181121),  format="%Y%m%d") # 종료일자
DATE <- DATE_START:DATE_END
DATE <- as.Date(DATE,origin="1970-01-01")

## 게시물 번호 리스트 만들기
PAGE <- seq(from=1,to=41,by=10) # 시작값과 종료값을 지정해줄 수 있습니다.
PAGE <- seq(from=1,by=10,length.out=5) # 시작값과 원하는 갯수를 지정할 수도 있습니다.

## 네이버 검색결과 url 리스트에서 관련기사 url 리스트 만들기
news_url <- c()
news_date <-c() 

for (date_i in DATE){
  for (page_i in PAGE){
    dt <- format(as.Date(date_i,origin="1970-01-01"), "%Y.%m.%d")
    naver_url <- paste0(naver_url_1,QUERY,naver_url_2,dt,naver_url_3,dt,naver_url_4,page_i)
    html <- read_html(naver_url)
    temp <- unique(html_nodes(html,'#main_pack')%>% # id= 는 # 을 붙인다
                     html_nodes(css='.news ')%>%    # class= 는 css= 를 붙인다 
                     html_nodes(css='.type01')%>%
                     html_nodes('a')%>%
                     html_attr('href'))
    news_url <- c(news_url,temp)
    news_date <- c(news_date,rep(dt,length(temp)))
  }
  print(dt) # 진행상황을 알기 위함이니 속도가 느려지면 제외
}

NEWS0 <- as.data.frame(cbind(date=news_date, url=news_url, query=QUERY))
NEWS1 <- NEWS0[which(grepl("news.naver.com",NEWS0$url)),]         # 네이버뉴스(news.naver.com)만 대상으로 한다   
NEWS1 <- NEWS1[which(!grepl("sports.news.naver.com",NEWS1$url)),] # 스포츠뉴스(sports.news.naver.com)는 제외한다  
NEWS2 <- NEWS1[!duplicated(NEWS1), ] # 중복된 링크 제거 


## 뉴스 페이지에 있는 기사의 제목과 본문을 크롤링
NEWS2$news_title   <- ""
NEWS2$news_content <- ""

for (i in 1:dim(NEWS2)[1]){
  html <- read_html(as.character(NEWS2$url[i]))
  temp_news_title   <- repair_encoding(html_text(html_nodes(html,'#articleTitle')),from = 'utf-8')
  temp_news_content <- repair_encoding(html_text(html_nodes(html,'#articleBodyContents')),from = 'utf-8')
  if (length(temp_news_title)>0){
    NEWS2$news_title[i]   <- temp_news_title
    NEWS2$news_content[i] <- temp_news_content
  }
}

NEWS2$news_content <- gsub("// flash 오류를 우회하기 위한 함수 추가\nfunction _flash_removeCallback()", "", NEWS2$news_content)
NEWS <- NEWS2 # 최종 결과 저장

save(NEWS, file="C:/Users/hong/Desktop/빅데이터가만드는세상_이종형/R") # working directory 아래 subfolder "DATA0" 에 저장




data1 <- c(1,3,6)
var(data1)
sd(data1)
cor(data1)








