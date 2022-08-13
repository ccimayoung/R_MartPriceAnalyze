
#경영프로그래밍 과제 - 미디어학과 2018110027 심아영


#파일 경로 및 라이브러리
setwd("C:/Users/AY/Desktop")
library(dplyr)
library(ggplot2)
library(stringr)
library(foreign)
library(readxl)

# 파일 불러오기 및 df에 지정
df = read.csv("한국소비자원_생필품_35개.csv")
head(df)

# 파일 변수 요인 및 구성 파악
str(df) 
summary(df) 

# 사용하지 않는 조사일, 원플러스원 열 삭제
df_new = df[,-2]
df_new= df_new[,-6]
head(df_new)

# 데이터명 수정
df_new = rename(df_new, name = "상품명")
df_new = rename(df_new, price = "판매가격")
df_new = rename(df_new, mart = "판매업소")
df_new = rename(df_new, made = "제조사")
df_new = rename(df_new, sale = "세일여부")

head(df_new)

# 상품군 별로 묶어주는 group 파생변수 생성성
df_new$group = ifelse((df_new$name == "농심 신라면 큰사발면(114g)"| df_new$name =="스팸 클래식(200g)"|df_new$name =="신라면(5개입)"|df_new$name =="오뚜기 크림스프(80g)"|df_new$name =="진라면 순한맛(5개입)"|df_new$name =="햇반(210g)"),"가공식품",
                      ifelse((df_new$name == "도브 뷰티 너리싱 바디워시(1kg)"|df_new$name =="리엔 흑모비책크림염색 자연갈색(60g)"|df_new$name =="마미손 고무장갑 중(2세트)"|df_new$name =="미쟝센 퍼펙트세럼 샴푸(680ml)"|df_new$name =="자연퐁 솔잎(490ml)"), "공산품",
                             ifelse((df_new$name == "오리온 포카칩 오리지널(66g)"|df_new$name =="포테토칩 오리지날(125g)"|df_new$name =="해태 오예스(12개입)"|df_new$name =="허니버터아몬드(210g)"),"과자류",
                                    ifelse((df_new$name == "백설 식용유(1.5L)"|df_new$name =="백설 진한참기름"|df_new$name =="청정원 미원 맛소금(500g)"),"기름/양념",
                                           ifelse((df_new$name == "감자(껍질 있는 감자, 100g)"|df_new$name =="당근(흙당근, 100g)"), "농산품",
                                                  ifelse((df_new$name == "갈치(생물, 100g)"|df_new$name =="고등어(생물, 300~500g)"),"수산",
                                                         ifelse((df_new$name == "남양유업 맛있는우유GT(1L)"|df_new$name =="매일우유 오리지널(900ml)"|df_new$name =="서울우유 흰우유(1L)"),"유제품",
                                                                ifelse((df_new$name == "삼다수(2L)"|df_new$name =="아이시스(2L)"|df_new$name =="코카콜라(1.8L)"),"음료",
                                                                       ifelse((df_new$name == "참이슬 후레쉬(360ml)"|df_new$name =="처음처럼(360ml)"|df_new$name =="테라(500ml)"), "주류", "축산" )))))))))                                                


# 제품군과 이름으로 그룹화한 뒤 평균가격, 최고가, 최저가, 표준편차 확인                     
df_new %>% group_by(group,name) %>% summarise(mean_price = mean(price),
                                              max_price=max(price),
                                              min_price=min(price),
                                              sd_price=sd(price))


# 제품별 평균가격, 표준편차를 변수에 지정하여 그래프 출력
mean_name = df_new %>% group_by(name) %>% summarise(mean_price = mean(price))
sd_name = df_new %>% group_by(name) %>% summarise(sd_price = sd(price))

ggplot(data = mean_name, aes(x=mean_price, y=name))+geom_col()
ggplot(data = sd_name, aes(x=sd_price, y=name))+geom_col()


# 상품군 별 가격 표준편차 변수에 지정하여 그그래프 출력
sd_group = df_new %>% group_by(group) %>% summarise(sd_price = sd(price))
sd_group
ggplot(data = sd_group, aes(x=sd_price, y=reorder(group, sd_price)))+geom_col()


# 마트를 상품군 개수로 정렬한 뒤 상위 15개 지점, 하위 15개 지점 출력 
n_name = df_new %>% group_by(mart) %>% summarise(n = n())
n_name = n_name[c(order(-n_name$n)),]
n_name_high = head(n_name, 15)
ggplot(data = n_name_high, aes(x=n, y=mart))+ geom_col()+xlim(0,30)

n_name_low = tail(n_name, 15)
ggplot(data = n_name_low, aes(x=n, y=reorder(mart, n)))+  geom_col() +xlim(0,30)


# 서울우유 흰우유 가격의 상위 15개 지점, 하위 15개 지점 출력 
water_price = df_new %>% filter(name=="서울우유 흰우유(1L)") %>% summarise(mart = mart, price = price)

water_price 
water_price_high = head(water_price, 15)
water_price_low = tail(water_price, 15)
ggplot(data = water_price_high, aes(y=mart, x=price))+  geom_col()+xlim(0,3000)
ggplot(data =water_price_low, aes(y=mart, x=price))+  geom_col()+xlim(0,3000)


# 농심 신라면 큰사발면 가격의 상위 15개 지점, 하위 15개 지점 출력 
ramen_price = df_new %>% filter(name=="농심 신라면 큰사발면(114g)") %>% summarise(mart = mart, price = price)
ramen_price_high = head(ramen_price, 15)
ramen_price_low = tail(ramen_price, 15)
ggplot(data = ramen_price_high, aes(y=mart, x=price))+  geom_col()
ggplot(data =ramen_price_low, aes(y=mart, x=price))+  geom_col()

