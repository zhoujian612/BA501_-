# continue zillow project after EDA. In real project, remember to always do EDA before building models
# load your dataset
train <- read.csv("train_property.csv", stringsAsFactors = F)

# only keep columns with < 20% rows are NA
num.NA <- sort(colSums(sapply(train, is.na)))
remain.col <- names(num.NA)[which(num.NA <= 0.2 * dim(train)[1])] 
train <- train[, remain.col]

#### simple linear regression ######
# build a simple linear regression
mod0 <- lm(logerror ~ taxvaluedollarcnt, data = train)
# check the result of model
summary(mod0)
# what is the beta? p-value from t-test for beta? p-value from F test?

# diagnostics
par(mfrow=c(2,2))
plot(mod0)

###### multiple linear regression ####
# build a linear regression model with a group of features
mod1 <- lm(logerror ~ 
             taxvaluedollarcnt + landtaxvaluedollarcnt + 
             structuretaxvaluedollarcnt + taxamount +
             calculatedfinishedsquarefeet + finishedsquarefeet12 +
             lotsizesquarefeet,
           data = train)
summary(mod1)
# check p-value from t-test of each beta, p-value from F test, R^2

# Seeing NA means almost perfect correlation between features
alias(mod1) #Find aliases (linearly dependent terms) in a linear model specified by a formula.
mod1 <- lm(logerror ~ 
             taxvaluedollarcnt + landtaxvaluedollarcnt + 
             taxamount +
             calculatedfinishedsquarefeet + lotsizesquarefeet,
           data = train)

# only keep columns to use
train$fips <- as.character(train$fips)
train.sub <- train[ , 
                   c("logerror", "taxvaluedollarcnt", "landtaxvaluedollarcnt",
                      "taxamount",
                      "calculatedfinishedsquarefeet", "lotsizesquarefeet")]
# only use rows with no NAs
train.sub <- complete.cases(train.sub)
mod2 <- lm(logerror ~ ., data = train.sub)
summary(mod2)
plot(mod2)

# See extremely small estimate, because of the magnitude of features, 
# to compare the relative importance
# Standardize
train.sub[, -1] <- scale(train.sub[, -1])
mod2 <- lm(logerror ~ ., data = train.sub)
# standardizing won't change the significant of features, 
# but the estimate will change.

plot(mod2)
# first plot we can check unbiased/biased and homo/hetero of the residual
# Def not having homo, reason is model miss important features.
# second plot to check the normality of the residual. 
# qqplot: for ith percentile data point, find ith percentile in normal distribution.