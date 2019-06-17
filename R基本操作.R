x=1
print(x)

x<-1
y<-2
x+y

1/2
2*3

# ==表示判断， !=表示不等于, %%求余数，%/%求整除的倍数
x=3
print(x!=2)
# 5%%3=2
# 25%/%4=6
x=[1,2,3]
y=[2,3,4]
print(x|y)
x&y

# %in% 一个矢量的每个元素是否在另一个矢量中
6:10 %in% 1:36
# [1] TRUE TRUE TRUE TRUE TRUE
which(6:10 %in% 1:36)
# [1] 1 2 3 4 5

# & And, binary, vectorized 一个数
# && And, binary, not vectorized, 矢量
# | Or, binary, vectorized 一个数
# || Or, binary, not vectorized，一矢量
# ！表示否定

# integer: 2L
# complex: 1+4i
a=4.5
class(a) #numeric 检验的是整体
typeof(a) # double 检验的是每一行元素是什么
length(a) # 1
attributes(a) # does it have any attibutes, # NULL

# Data Structure
# Homogeneous同质情况下，vs. Heterogeneous异质
# 1d, atommic vector原子矢量 vs. List
# 2d, matrix vs. Data Frame
# nd, arrary 

# 用c来组织一个list
a=matrix(1:6,2,3) # 构造2行3列的矩阵
class(a) # "matrix"
typeof(a) # integer
e=data.frame(b=c(1,2,3),name=c("a","b","c")) # 构造3行2列的dataframe
class(e) # "data.frame"
typeof(e) # "list"
# seq(10)==[1:10]

# 转化
x=0:6
x
as.character(x) #"0" "1" "2" "3" "4" "5" "6"
as.numeric(x) # 0 1 2 3 4 5 6
as.logical(x) # FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE 大于零的都是true
as.complex(x) # 0+0i 1+0i 2+0i 3+0i 4+0i 5+0i 6+0i
x=c("a","b","c")
as.numeric(x) #NA NA NA

m=matrix(nrow=2,ncol=3) # matrix
m
#      [,1] [,2] [,3]
# [1,]   NA   NA   NA
# [2,]   NA   NA   NA
dim(m)
# 2 3

matrix(1:6, 2,3)
#      [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6

# list
x=list(1,"a", TRUE, 1+4i)
x

# data frame
x = data.frame(a=1:4, b=c(TRUE,TRUE,FALSE,FALSE))
x
#   a     b
# 1 1  TRUE
# 2 2  TRUE
# 3 3 FALSE
# 4 4 FALSE
nrow(x) # 4
ncol(x) # 2

mtcars
colnames(mtcars)
rownames(mtcars)

# missing value
0/0 #  NaN, not a number
is.na(0/0) # true
is.nan(0/0) # true
# 是nan一定是na，但是na不一定是nan

cars
class(cars) # data frame
data(cars)
# 提取某一行某一列的数字
cars[1,1] # 4
cars[1:10,1] # 4  4  7  7  8  9 10 10 10 11
cars[c(1,3,5), 1] # 4 7 8
cars[,2] # 取全部
cars[-1,] # 去掉第一行，负号表示去掉某一行或某一列
cars[-1:-30,] # 去掉1到30行
# 按名字提取
cars[1:10, "speed"] # 4  4  7  7  8  9 10 10 10 11
cars[1:10, ncol(cars)] # 用nrow，ncol提取最后一行一列

# 筛选
x=c(1,2,3,11,12,13)
x<10 # TRUE  TRUE  TRUE FALSE FALSE FALSE
x[x<10] # c(1 2 3)
cars$speed<20
cars[cars$speed<20,] # 限定条件用方括号扩起来，选出了速度小于20的行，

####### programming in R
# if statement
# if (condition) {
#   action1
# }else{
#   action2
# }

num=50
if (num > 100){
  print("greater")
} else{
  print("not greater")
} 
print("done")

# for loop
i=2
j=4
for (i in 1:3){
  for (j in 1:6) {
    mymat[i,j] = i*j
  }
}
mymat[2,4]

for(i in 1:10){
  print(i*(i-1))
}
# Fn=Fn-1+Fn-2 F1=1, F2=1
f=c() ############################# 先定义函数f
for (i in 1:10){
  if (i==1|i==2){  # |表示或者
    f[1]=1  #####################用中括号
  }else{
    f[i]=f[i-1]+f[i-2]
  }
}
print(f[1:10])


f=c()  ####################先定义函数
f[1]=1
f[2]=1
for (i in (3:20)){
  f[i]=f[i-1]+f[i-2] ##############用中括号
}
f[3:20]

# while loop
y=c()
while(i<=10){
  y[i]=.....
}

Data = matrix(seq(15),3,5)
for (i in 1:3){
  print(sum(Data[i,]))
}
# sapply表示对某个apply a function over a list or vector
sapply(Data, FUN =  sqrt) # sapply 输出结果是一个vector矢量
                          # lapply 输出结构是一个list
sapply(Data, FUN = sum)

# prepare workspace
getwd()
setwd()
############## load in data
# txt file: read.table()
# csv file: read.csv(file="path/file_name", header = FALSE)  FALSE表示标题不被列如行内容

# output txt file
# write.table(dataset, "output_name", row.names=F, col.names=F)
# output csv file
# write.csv(dataset, "output_name")

# data exploration
summary(mtcars)
str(mtcars) # structure
dim(mtcars) # dimension
colnames(mtcars)
rownames(mtcars)
length(mtcars)
unique(mtcars)

table()
barplot() #条形图
pie()

by()
apply()
mean()
median()
sd() # standard deviation
quantiles()
density()
boxplot()
plot()
sort()
lines()
by()
apply()
library(lattice)
library(tabplot)


# numeric variable
cor() # correlation
library()

aggregate()  #合集
aggregate(mtcars$mpg, by = list(mtcars$gear), FUN = mean)

# library(dplyr)
grp = group_by(data,group)
summarise(grp, mean=mean(age), sd=sd(age))

# sorting
newdata = mtcars[order(mpg, cyl)]

#merge
total=merge(dataframeA, dataframeB, by="ID")
grp=group_by(data, group)
summarise(grp, mean=mean(age), sd=sd(age))

#cbind-ing, rbind-ing
x=1:3
y=10:12
cbind(x,y)

rbind(x,y)


############# 免费材料 #########################

library("rmarkdown")
library(ggplot2)
# mydata = read.table("mydata.txt")
# mydata = read.csv("mydata.csv", header = TRUE)
a = c(1,2,3,4,5)
a+1 # a = c(2,3,4,5,6)
var(a) # 求方差，2.5
###########    dplyr  ###########
# nycflights13
library(nycflights13)
dim(flights)#336776     19
# print the last 5 rows of mydata
tail(flights, n=5)
head(flights)
head(flights, n=5)
str(flights)
library(dplyr)  ############################ very important prerequest
# filter()
filter_1 = filter(flights, month == 9|month == 10, day<2)
filter_1
# slice 取特定的某几行查看
slice_1 = slice(flights, 1:20) # 取行的某一些
slice_1
# arrange 排序
arrange(flights[1:5,], dep_delay) #默认正序排列
arrange(flights[,1:8], desc(dep_delay)) # desc=descending
# select
# select columns by name
select(flights[1:5,], year, month, day)
select(flights[1:5,], year:day)
select(flights[1:5,], -year, -month)
select(flights, -starts_with("dep_"))
select(flights, starts_with("dep_"))
# distinct
distinct(flights, origin, dest) # 看看有多少种不重复的组合
# mutate 变异， 对现有列进行运算后的新产生的一列
add_flights = mutate(flights, gain = arr_delay - dep_delay, gain_per_hour = gain / (air_time / 60))
select(add_flights, gain, gain_per_hour)

# sample_n()
set.seed(11111) #固定住取来的sample
sample_n(flights,10) # 仁取十行
sample_frac(flights, 0.01) # 取百分之一

by_origin = group_by(flights, origin)
delay = summarise(by_origin, count = n(), dist = mean(distance, na.rm = TRUE), delay = mean(arr_delay, na.rm = TRUE))
delay
filter_delay = filter(delay, delay >6)
filter_delay

delay_pipe = flights %>%
  filter(!is.na(dep_delay))%>%
  group_by(origin) %>%
  summarise(
    dist = mean(distance, na.rm = TRUE),
    delay = mean(dep_delay),
    count = n() ) %>%
    filter(count > 0)
delay_pipe
############   visualization ###############

library(ggplot2)
ggplot(delay,aes(x=dist, y=delay)) +  #aesthetic mapping
  geom_point(aes(color=count, size = count), alpha = 1) +
  xlab("Distance") +
  ggtitle("Distance vs. Delay") +
  geom_smooth() +
  scale_size_area()
# 继续学习的参考 "https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf"

# case: crimes in texas
library(crimelinkage)
library(maps)
library(ggplot2)
library(plyr)
library(ggmap)
str(crime)
head(crime)
violent_crime = subset(crime, offense %in% c("aggravated assault", "murder", "rape", "robbery"))
# record levels of vilent crimes by violence level
violent_crime$offense = factor(violent_crime$offense, levels = c("robbery", "aggravated assault", "rape", "murder"))
HT = get_map(location = "Houston, Texas", zoom = 14, source = "osm") 
ggmap(HT) + geom_point(data = violent_crime, aes(x=lon, y=lat, color=offense, size = offense))
ggmap(HT) + stat_bin2d(
  aes(x=lon, y=lat, color=offense, fill = offense),
  size = 0.5, bins = 30, alpha = 1/2,
  data = violent_crime)

library(ggplot2)
head(crime)
distinct(crime, offense)
data_test = crime %>%
  select(month,day, offense) %>%
  filter(offense=="theft") %>%
  group_by(month, day) %>%
  summarise(n=n())
data_test
ggplot(data_test, aes(x=day, y=month, fill = n)) +
  geom_tile(color="white", size=0.1) +
  labs(title="Robbery Frequency by Month and Weekday")+
  scale_fill_gradient(low = "white", high = "steelblue")


# Time series analysis and forecasting
kings = scan("http://robjhyndman.com/tsdldata/misc/kings.dat", skip=3)
kings
# [1] 60 43 67 50 56 42 50 65 68 43 65 34 47 34 49 41 13 35 53 56 16 43 69 59 48 59 86 55 68 51 33 49 67 77 81 67
# [37] 71 81 68 70 77 56

kingstimeseries = ts(kings)
kingstimeseries
plot.ts(kingstimeseries)
library(tseries)
kpss.test(kingstimeseries)
# KPSS Level = 0.69062, Truncation lag parameter = 1, p-value = 0.0144
# conclusion: not statioanry , so diffrerecing is needed
kingtimeseriesdiff1 = diff(kingstimeseries, differences = 1)
kpss.test(kingtimeseriesdiff1)
# KPSS Level = 0.034741, Truncation lag parameter = 1, p-value = 0.1
tsdisplay(kingstimeseries, lag.max = 30)

auto.arima(kingstimeseries)
library(auto.arima)
model = arima(kingstimeseries, order=c(0,1,1))
model
mforecast=forecast(model, h=3)
plot(mforecast)

###############  Twitter sentiment analysis ##############################
library(twitteR)
api_key = "	eGY2Qe91w6bz1r5ivGAOjnGf7"
api_secret = "	fqa2P4Svf9hXNCsw81pUjBy5RJJcri87kBfDBwdhmvHGM3r31I"
access_token = "771521839-TaLglpRcGUsgKSFHlEHNvNSjYcByHt74KmjHVoPH"
access_token_secret = "pmSkaIufQQQoGc2HVTXYJgCUtH6SGbAgnIPFDgRMWs5dg"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

user = getUser("amazon")
amazon = userTimeline(user,n=500)
# summarize information about a list of tweets in a data frame
amazon = twListToDF(amazon)
amazon = amazon$text

# get the hashtags
library(stringr)
amazonHashTags = str_extract_all(amazon, "#\\w+")
amazonHashTags
freq = table(unlist(amazonHashTags))
freq

library(wordcloud)
wordcloud(names(freq), freq, colors = "steelblue")
title("what is Amazon tweeting about?\n\n")

########### twitter sentiment analysis, amazon vs. ebay ##############################
# if score>0, positive opinion
# if score<0, negative opinion
# if score=0, neutral opinion

# attach libraries
library(twitteR)
library(plyr)
library(stringr)
# tweets with amazon prime and ebay
amazon_tweets = searchTwitter("amazon", n=500, lang = "en")
ebay_tweets = searchTwitter("ebay", n=500, lang = "en")
head(amazon_tweets)
# get text
amazon_text = sapply(amazon_tweets, function(x) x$getText())
ebay_text = sapply(ebay_tweets, function(x), x$getText())
head(amazon_text)
# join texts
ecommerce = c(amazon_text, ebay_text)
# apply function score.sentiment
scores = score.sentiment(ecommerce, pos, neg, .progress="text")
head(scores)
# add variables to data frame
scores$ecommerce = factor(c("amazon", "ebay"))
scores$very.pos = as.numeric(scores$score>=2)
scores$very.neg = as.numeric(scores$score<=-2)
head(scores)

# boxplot
ggplot(scores, aes(x = ecommerce, y=score, group=ecommerce))+
  geom_boxplot(aes(fill=ecommerce))+
  scale_fill_manual(values = c(amazon="grey", ebay="maroon4"))+
  geom_jitter(colour="fray40",
              position = position_jitter(width=0.2), alpha=0.3)+
  labs(title="Amazon vs. ebay Sentiment Scores")


