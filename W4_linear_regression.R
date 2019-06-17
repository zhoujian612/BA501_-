train <- read.csv("train_property.csv", stringsAsFactors = F)

num.NA <- sort(colSums(sapply(train, is.na)))
remain.col <- names(num.NA)[which(num.NA <= 0.2 * dim(train)[1])] 
train <- train[, remain.col]

mod1 <- lm(logerror ~ 
             taxvaluedollarcnt + landtaxvaluedollarcnt + 
             structuretaxvaluedollarcnt + taxamount +
             calculatedfinishedsquarefeet + finishedsquarefeet12 +
             lotsizesquarefeet,
           data = train)
summary(mod1)
# Seeing NA means almost perfect correlation between features
alias(mod1)
mod1 <- lm(logerror ~ 
             taxvaluedollarcnt + landtaxvaluedollarcnt + 
             taxamount +
             calculatedfinishedsquarefeet + lotsizesquarefeet,
           data = train)
train$fips <- as.character(train$fips)

train.sub <- train[ , 
                   c("logerror", "taxvaluedollarcnt", "landtaxvaluedollarcnt",
                      "taxamount",
                      "calculatedfinishedsquarefeet", "lotsizesquarefeet")]
train.sub <- train.sub[which(apply(train.sub, 1, function(x) 
  length(which(is.na(x))) == 0)), ]
mod1 <- lm(logerror ~ ., data = train.sub)

# See extremely small estimate, because of the magnitude of features, to compare the relative importance
# Standardize
train.sub[, -1] <- scale(train.sub[, -1])
mod2 <- lm(logerror ~ ., data = train.sub)
# standardizing won't change the significant of features, but the estimate will change.

train.sub$tax.rate <- with(train.sub, calculatedfinishedsquarefeet / lotsizesquarefeet)
mod3 <- lm(logerror ~
             taxvaluedollarcnt + landtaxvaluedollarcnt +
             calculatedfinishedsquarefeet +
             lotsizesquarefeet + tax.rate,
           data = train.sub)

x = train.sub[, -1]
y <- train.sub[, 1]
# to calculate the XT*X
t(x) %*% x
# error due to only taking matrix as argument
x <- as.matrix(x)
t(x) %*% x
# note that X dim is n * (p+1), XT*X dim is (p+1) * (p+1)
# inverse
xtxi <- solve(t(x) %*% x)
# beta estimator
xtxi %*% t(x) %*% y

# sigma estimator
head(mod2$res)
sqrt(sum(mod2$res^2)/(dim(train.sub)[1] - 6)) 

# R square
1 - sum(mod2$res^2)/sum((y-mean(y))^2)
# adjusted R square
1 - (sum(mod2$res^2)/sum((y-mean(y))^2)) * 
  (dim(train.sub)[1] - 1) /(dim(train.sub)[1] - 6)

# F test score, switch to slides
sst = sum((y - mean(y))^2) # sum of square total, df = n - 1 = 79788
ssr = sum(mod2$res^2) #  sum of square residual, df = n-1-p = 79783
ssm = sum((y - mean(y))^2) - sum(mod2$res^2) # sum of square model, df = 5
Fstats = (ssm)/(5) / (ssr / (dim(train.sub)[1] - 5 -1))
1 - pf(Fstats, 5, (dim(train.sub)[1] - 5 -1)) # def = p and n-1-p

# residual
head(sort(mod2$res))
plot(mod2$fit, mod2$res, xlab = 'Fitted', ylab = 'residual')
which.min(mod2$res)

plot(mod2)
# first plot we can check unbiased/biased and homo/hetero of the residual
# Def not having homo, reason is model miss important features.
# second plot to check the normality of the residual. 
# qqplot: for ith percentile data point, find ith percentile in normal distribution.

# collinearity
# variance inflation example
n <- 100
nosim <- 1000
set.seed(1)
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
betas <- sapply(1:nosim, function(i){
  y <- x1+rnorm(n, sd=.3) 
  c(coef(lm(y ~ x1))[2], coef(lm(y ~ x1+x2))[2], coef(lm(y ~ x1+x2+x3))[2]) 
})
round(apply(betas,1,sd),5)

n <- 100
nosim <- 1000
set.seed(1)
x1 <- rnorm(n)
x2 <- x1/sqrt(2) + rnorm(n)/sqrt(2)
x3 <- x1*0.95 + rnorm(n)*sqrt(1-0.95^2)
betas <- sapply(1:nosim, function(i){
  y<- x1 + rnorm(n, sd=.3) 
  c(coef(lm(y ~ x1))[2], coef(lm(y ~ x1+x2))[2], coef(lm(y ~ x1+x2+x3))[2]) 
})
round(apply(betas,1,sd),5)
# observed due to collineariy, variance increased

# check VIF
summary(lm(x3 ~ x1 + x2)) # R squared is 0.874 
# vif should be
1/(1 - 0.874)

library(glmnet)
# glmnet only takes matrix, can use is.data.frame() or is.matrix() to test
# glmnet standardizes every feature, even categorical feature
# http://stackoverflow.com/questions/17887747/how-does-glmnets-standardize-argument-handle-dummy-variables

train.sub <- train[ , 
                    c("logerror", "regionidzip", "taxvaluedollarcnt", "landtaxvaluedollarcnt",
                      "taxamount",
                      "calculatedfinishedsquarefeet", "lotsizesquarefeet")]
train.sub <- train.sub[which(apply(train.sub, 1, 
                                   function(x) length(which(is.na(x))) == 0)), ]
train.sub$regionidzip <- as.character(train.sub$regionidzip)

ind <- model.matrix( ~., train.sub[, -1])
# Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]). 
# if character type feature only has one value.

dep <- train.sub$logerror
fit <- glmnet(x=ind, y=dep) # default is alpha = 1, lasso
plot(fit)
# Understand the plot
# The top row indicates the number of nonzero coefficients at the current λ,
# which is the effective degrees of freedom (df) for the lasso.
# y axis is the value of coefficient
# x axis is the sum of absolute value of coefficients
plot(fit, label = T)
plot(fit, xvar = "lambda", label = T)

vnat=coef(fit)
vnat=vnat[-1,ncol(vnat)] # remove the intercept, and get the coefficients at the end of the path
axis(4, at=vnat,line=-.5,label=colnames(ind),las=1,tick=FALSE, cex.axis=0.5) 

print(fit)
# Df is the non zero beta, 
# saturated model is a model with a parameter for every observation so that the data are fitted exactly.
# Deviance_model = 2*(loglikelihood_saturate_model - loglikelihood_current_model)
# Deviance_null = 2*(loglikelihood_saturate_model - loglikelihood_intercept_only_model)
# Deviance percentage = 1 -  Deviance_model / Deviance_null
# lambda value

coef(fit, s = 1/exp(2)) # s stands for lambda
coef(fit, s = 1/exp(8))

# We can choose lambda by checking the picture, Still kinda subjective
# use cross validation to get optimal value of lambda, 
cvfit <- cv.glmnet(ind, dep)
plot(cvfit)
# Two selected lambdas are shown, 
cvfit$lambda.min # value of lambda gives minimal mean cross validated error
cvfit$lambda.1se # most regularized model such that error is within one std err of the minimum
x = coef(cvfit, s = "lambda.min")
coef(cvfit, s = "lambda.1se")