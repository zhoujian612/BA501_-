###################  Zillow project ####################
# 画图命令 plot(x,y,data=dataset_name)
# scatter plot, line chart
# main: title for plot, xlab: label for xaxis, xlim: rang of xaxis
# boxplot(x,data=dataset_naem)可以看到25%，75%，median，mini,max
# hist(x,data=dataset_name) 反应频率 看所有x值，y轴只能是频率
# barplot(data) x轴可以是一部分子集，y轴可以不是频率
# pie(data)
# ggplot2
# ggplot(temp,aes(x=pretest2,y=posttest2))+geom_point(size=1)+geom_density2d()
# 向量也叫矢量，是既有大小又有方向

x<-c(1,2,3)
y<-c(2,3)
x*y
# 2 6 6 对应相乘，第三个数没有对应相乘，只能返回和y的第一个相乘

x=c(1,2,3,NA,4,3)
meanimput <- function(x){   # define an fucntion
  x[is.na(x)] = mean(x,na.rm = T)
  return(x)     #很重要
}
meanimput(x)

# EDA means explotory data analysis
getwd() # where is input, output
setwd("/Users/jianzhou/Desktop/github download/r_zillow project")
getwd()
# Material zillow kaggle
# 插入数据
# stringasfactors 表示是否string要看作数据的factor varaibles or as just plain strings
train = read.csv("/Users/jianzhou/Desktop/github download/r_zillow project/train_2016_v2.csv", stringsAsFactors = FALSE)
# 有时候还要加个功能header = TRUE, 如果FALSE就把现有的header当作了第一行的数据了
property = read.csv("/Users/jianzhou/Desktop/github download/r_zillow project/properties_2016.csv", stringsAsFactors = FALSE)
head(train) 
property # 可以看出R确实读取大数据很慢
train
table(train$transactiondate)
# understand the data
summary(train)
str(train)
dim(train) # nrow, ncol
head(train)
colnames(train)

# combine data
# all.x, logical; if TRUE, then extra rows will be added to the output, 
# one for each row in x that has no matching row in y. 
# These rows will have NAs in those columns that are usually filled with values from y. 
# The default is FALSE, so that only rows with data from both x and y are included in the output.
length(unique(train$parcelid))
length(unique(property$parcelid))
property = subset(property, parcelid %in% train$parcelid) # 把property的集合缩小 让property和train数据集size相等
# %in% 意思是检查前者是否在后者里
merge_train = merge(train, property, by = "parcelid", all.x = T) #合并 全部加进来 其中train是x，property是y，所以这里保留x的所有行
summary(merge_train$logerror)
write.csv(merge_train, "train_property.csv")

train_property = read.csv("train_property.csv", stringsAsFactors = FALSE) # 直接读进清洗过的data
table(train_property$transactiondate)
sum(is.na(train_property)) / (ncol(train_property)*nrow(train_property))
# check how many NAs in each feature
length(which(is.na(train_property$calculatedfinishedsquarefeet.x)))
# 658
str(train_property)
dim(train_property)
head(train_property)
tail(train_property)
colnames(train_property)
sum(is.na(train_property$calculatedfinishedsquarefeet.x)) #非na值为1，na值为0，求和
# 658
num.NA = sort(colSums(sapply(train_property, FUN = is.na)))
# sapply 意味检查dataset的每一个值是不是na,
# colSums 看每一列有多少个NA
# sort(colSums(....))按colSums整合
num.NA
remain.col = names(num.NA)[which(num.NA <= 0.2 * nrow(train_property))] #dim(train)[1] = nrow(train)， 
# R会把有missing value 的整个一行都去掉，所以要及时清楚含过多NA的column
reduced_train = train_property[, remain.col] #选全部的行
dim(reduced_train)
sum(is.na(reduced_train)) / (nrow(reduced_train)*ncol(reduced_train)) #NA的在整体中的比例
complete.cases(reduced_train) # 检查每个元素是否都是非NA
sum(complete.cases(reduced_train)/nrow(reduced_train))
# how to treat missing values?
# (0) check if missing values are random, if systematically missing, check data quality
# 1.remove features with too many missing values or remove all rows with NA if you haave a lot of dat
# 2. add new levels to represent NA
# 3. imputation, average; average by group; model based imputation, predict the missing value

################  write a function to replace missing value  ########################
x = c(1,2,3,NA,4,3)
meanimput = function(x){ #定义函数的方法
  x[is.na(x)] = mean(x, na.rm = T)
  return(x)  #别忘了return
}
meanimput(x) # 1.0 2.0 3.0 2.6 4.0 3.0 因为是vector 所以里面datatype都是一样的
y = c(2,3,4,NA,5,6)
meanimput(y)











# explore the data
# 1) numeric variables
# 2) categorial variables
# 3) numeric variables with response
# 4) categorial variables with response

# numeric variables
mean(reduced_train$logerror)
sd(reduced_train$logerror)
median(reduced_train$logerror)
quantile(reduced_train$logerror, c(0.1,0.25,0.5,0.75,0.9)) # means 10%, 25%....
plot(density(reduced_train$logerror)) ########################################################绘图
summary(reduced_train$logerror)

# character varaibles
table(reduced_train$transactiondate)  ########################################################绘图
barplot(table(reduced_train$transactiondate))   ########################################################绘图

with(reduced_train, plot(as.Date(transactiondate), logerror, pch=20)) #with命令， 在指定集合内########################################################绘图
reduced_train$txnmonth = sapply(strsplit(reduced_train$transactiondate, "-"), function(x) x[2]) # 创建一个新的月份col

boxplot(subset(reduced_train, txnmonth=="01")$logerror,
        subset(reduced_train, txnmonth=="06")$logerror)  ##########绘图
boxplot(subset(reduced_train, txnmonth=="01" & abs(logerror)<0.09)$logerror,
        subset(reduced_train, txnmonth=="06" & abs(logerror)<0.09)$logerror) # abs=absolute value  ##########绘图

library(lattice)
bwplot(logerror ~ txnmonth, data = reduced_train)##########绘图
bwplot(logerror ~ txnmonth, data = subset(reduced_train, abs(logerror)<0.09))##########绘图

err.month = by(reduced_train, reduced_train$txnmonth, function(x){
  return(mean(x$logerror))})
plot(names(err.month), err.month, type = "l") #x,y轴

err.bedroomcnt = by(reduced_train, reduced_train$bedroomcnt, function(x){
  return(mean(x$logerror))})
plot(names(err.bedroomcnt), err.bedroomcnt, type = "l")

library(corrplot)# ##############如果有很多的元素要一起看 反应多对多 是一种比例关系
correlations = cor(reduced_train[, c("logerror", "bathroomcnt", "bedroomcnt", "roomcnt",
                                     "taxamount", "calculatedbathnbr", "fullbathcnt", 
                                     "lotsizesquarefeet", "finishedsquarefeet12")],
                   use = "pairwise.complete.obs")
corrplot(correlations, method="circle", tl.cex = 0.5, type = "upper") # tl.cex表示字体

library(tabplot) ##############如果有很多的元素要一起看，反应一对多 是一种比例关系
tableplot(reduced_train, select = c("logerror", "bathroomcnt", "bedroomcnt", "roomcnt",
                                    "taxamount", "calculatedbathnbr", "fullbathcnt", 
                                    "lotsizesquarefeet", "finishedsquarefeet12"), sortCol = "logerror", from = 0, to = 5)
tableplot(reduced_train, select = c("logerror", "yearbuilt", "regionidcounty"))

train_property = read.csv("train_property.csv", stringsAsFactors = FALSE) 
# check how many NAs in each feature
num.NA = sort(colSums(sapply(train_property, is.na)))
num.NA
remain.col = names(num.NA)[which(num.NA <= 0.2 * nrow(train_property))] #dim(train)[1] = nrow(train)， 
# R会把有missing value 的整个一行都去掉，所以要及时清楚含过多NA的column
reduced_train = train_property[, remain.col] #选全部的行
ncol(reduced_train)
summary(reduced_train$bathroomcnt)
summary(reduced_train$bedroomcnt)
summary(reduced_train$roomcnt)
with(reduced_train, plot(bathroomcnt+bedroomcnt, roomcnt))
# why there are more bedroomcnt+bathroomcnt> roomcnt
#assumption: error in data

boxplot(subset(reduced_train, roomcnt<bathroomcnt+bedroomcnt)$logerror,
        subset(reduced_train, roomcnt>=bathroomcnt+bedroomcnt)$logerror)
quantile(abs(reduced_train$logerror), 0.9) #求得90%的quantile都在0.145
boxplot(subset(reduced_train, roomcnt<bathroomcnt+bedroomcnt & abs(logerror)<0.145)$logerror,
        subset(reduced_train, roomcnt>=bathroomcnt+bedroomcnt & abs(logerror)<0.145)$logerror)
boxplot(abs(subset(reduced_train, roomcnt<bathroomcnt+bedroomcnt & abs(logerror)<0.145)$logerror),
        abs(subset(reduced_train, roomcnt>=bathroomcnt+bedroomcnt & abs(logerror)<0.145)$logerror))

################## t.test ##############################################################
with(reduced_train, t.test(logerror ~ (roomcnt<bathroomcnt+bedroomcnt))) # p-value = 0.8946 表示两组没什么差别
# 这里用到weltch t-test, 因为两组的variance 不一样
reduced_train$room_wrong = reduced_train$roomcnt < reduced_train$bathroomcnt + reduced_train$bedroomcnt
t.test(reduced_train$logerror ~ reduced_train$room_wrong)

with(reduced_train, t.test(abs(logerror) ~ (roomcnt<bathroomcnt+bedroomcnt)))
t.test(abs(reduced_train$logerror) ~ reduced_train$room_wrong)











#练习 %in%
x=c(1,2,3)
1 %in% x # TRUE

# merge
x = data.frame(a=c(1:5), b=c(TRUE,TRUE,FALSE,FALSE,TRUE), d=c(11,12,13,14,15))
y = data.frame(c=6:9, b=c(TRUE,TRUE,FALSE,FALSE))
x
y
z=merge(x,y,by.x="a")
z
nrow(x)
nrow(y)
nrow(z)

# 处理missing value
# is.na 检测有无missing value

x=c(5,6,NA,7,8)
is.na(x)
which(is.na(x))
x[which(is.na(x))] # 显示na的yuans
x[-which(is.na(x))] # 显示不是na的元素

# 作业题 how to get random numbers
sample(1:6, 1, replace = TRUE)  #掷骰子
rnorm(10, 0,1) #正太分布
rpois(10,3) #柏松分布
binom.test() # 二项分布

rnorm(n=100,mean=0,sd=1)
plot(density(rnorm(n=100)))
dnorm(x = 0) #density pdf