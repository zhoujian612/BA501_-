########## R第三节课 ##########

x=c(1,2,3,4,5)
y=c(2,3)
x*y  # 2 6 6 12 10  对应点相乘，多余项返回跟第一个相乘


######################################## probability ##############
plot(density(rnorm(n=100))) #概率分布图
rnorm(n=10, mean=6,sd=2) #r means random, 什么都不写的�话用正态分布0，1

# rbinom(n=100,size=200,prob=0.5)
dnorm(x=0) #  默认是正态分布   求概率 p(x=0) PDF, density
pnorm(q=0) #0.5 默认是正太分布，CDF
qnorm(p=0.68) # 默认是正态分布  求的是z-score

#################### sample ######################
sample(1:20, 2)

reduced_train = read.csv("train_property.csv", stringsAsFactors = F)
head(reduced_train)

################################################ one sample t test  ########################################
# one sample t.test 都是检验数值组的mean是不是0
# define group
reduced_train$group_room_wrong = reduced_train$roomcnt < reduced_train$bathroomcnt + reduced_train$bedroomcnt
summary(reduced_train$group_room_wrong)
# does logerror has significant differernce between two groups?
t.test(reduced_train$logerror) # p-value < 2.2e-16
#  one sample test, Null Hypothesis: mean(logerror)=0 所以结论是logerror不等于零
# t.test(reduced_train$logerror, alternative = "greater")
# alternative = c("two.sided", "less", "greater") , alternative hypothesis
# two.sided mu!=0
# less mu<0
# greater mu>0

t.test(reduced_train$bedroomcnt)

########################################### two sample t.test ######################################
# method 1
t.test(x=reduced_train$logerror[reduced_train$group_room_wrong == TRUE],
       y=reduced_train$logerror[reduced_train$group_room_wrong == FALSE])
# method 2
t.test(reduced_train$logerror ~ reduced_train$group_room_wrong)
#options: paired=FALSE, var.equal=FALSE
# student t.test assume two sample variance is equal,
# welch t.test assume two sample variance is not equal
# method 3
with(reduced_train, t.test(logerror ~ group_room_wrong))
# p-value = 0.8946 表示roomcnt的真和假的两部分对logerror的影响不大
# z.test vs. t.test
# z when variance is known, sample size is very big

# t.test 的实例
reduced_train$fips = as.character(reduced_train$fips) #county de ID
error.fip = by(reduced_train, reduced_train$fip, function(x){
  return(mean(x$logerror))})
barplot(error.fip, ylab = "mean of logerror") # 图中看出"6111"的这个county明显不同，要做t.test来检验
with(reduced_train, t.test(logerror ~ (fips == "6111")))
# p-value = 0.001154 所以确实发现fips对logerror有大影响

#################################### proportion test #################################################
x1 = c(x=30, n=100) # proportion=x/n
x2 = c(80,200)
prop.test(x1, x2)

summary(with(reduced_train, taxamount/taxvaluedollarcnt))
reduced_train$tax_rate = reduced_train$taxamount/reduced_train$taxvaluedollarcnt
with(reduced_train[1:2, ], prop.test(x = taxamount, n = taxvaluedollarcnt))

################################ correlation #############################################
# assumption: fewer number of houses for certain region id zip caused logerror larger
# how to verify
num.zip = by(reduced_train, reduced_train$regionidzip, function(x){
  return(dim(x)[1])})
reduced_train$num.zip = num.zip[reduced_train$regionidzip]
with(reduced_train, cor(abs(logerror), num.zip, use = "pairwise.complete.obs")) # complete只要这些没有missing value的


#  correlation matrix 同时比较多个
library(corrplot)
cols_to_use = c("logerror", "taxvaluedollarcnt", "structuretaxvaluedollarcnt", "landtaxvaluedollarcnt", "taxamount")
M = cor(reduced_train[,cols_to_use], use = "na.or.complete")
corrplot(M, method = "square", tl.cex = 0.5)
corrplot(M, method = "number", tl.cex = 1)













