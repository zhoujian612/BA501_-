# Material: https://cran.r-project.org/doc/manuals/r-patched/R-intro.html
train <- read.csv("train_2016_v2.csv", stringsAsFactors = FALSE)
property <- read.csv("properties_2016.csv", stringsAsFactors = FALSE)

length(unique(train$parcelid))
length(unique(property$parcelid))
property <- subset(property, parcelid %in% train$parcelid)
train <- merge(train, property, by = 'parcelid', all.x = T)
write.csv(train, 'train_property.csv')

# Understand the data
summary(train)
str(train)
dim(train)
head(train)
colnames(train)

# check how many NAs in each feature
length(which(is.na(train$calculatedfinishedsquarefeet)))
num.NA <- sort(colSums(sapply(train, is.na)))
# sort(sapply(train, function(x) {sum(is.na(x))}), decreasing=TRUE)
remain.col <- names(num.NA)[which(num.NA <= 0.2 * dim(train)[1])] # trainT = train
train <- train[, remain.col]

# The percentage of data missing in train.
sum(is.na(train)) / (nrow(train) *ncol(train))

# Find out variables with largest number of missing values
# how to treat missing values
# (1) Remove features with too many missing value, 
#     or remove all rows with NA if you have a lot of data
# (2) add new level to represent NA 
# (3) imputation. Example: use library(mice)
# https://www.kaggle.com/captcalculator/house-prices-advanced-regression-techniques/imputing-missing-data-with-the-mice-package-in-r/discussion

# How do we explore data?
# 1) Numeric variables
# 2) Categorical variables
# 3) Numeric variable with response 
# 4) Categorical varialbe with response

# numeric variables
mean(train$logerror)
sd(train$logerror)
median(train$logerror)
quantile(train$logerror, c(0.1, 0.25, 0.5, 0.75, 0.9))
plot(density(train$logerror))

# Let's check the txndate, character var
table(train$transactiondate)
barplot(table(train$transactiondate))

with(train, plot(as.Date(transactiondate), logerror, pch = 20))
train$txnmonth <- sapply(strsplit(train$transactiondate, '-'), '[[', 2)
# same as
train$txnmonth <- sapply(strsplit(train$transactiondate, '-'), 
                         function(x) x[2])

# Q1 - 1.5IQR, Q1, median, Q3, Q3 + 1.5IQR, where IQR is interquartile range: Q3 - Q1
boxplot(subset(train, txnmonth == '01')$logerror,
        subset(train, txnmonth == '06')$logerror)
boxplot(subset(train, txnmonth == '01' & abs(logerror) < 0.09)$logerror,
        subset(train, txnmonth == '06' & abs(logerror) < 0.09)$logerror)

library(lattice)
bwplot(logerror ~ txnmonth, data = train)
bwplot(logerror ~ txnmonth, data = subset(train, abs(logerror) < 0.09))

err.month <- by(train, train$txnmonth, function(x) {
  return(mean(x$logerror))})
plot(names(err.month), err.month, type = 'l')

err.bedroomcnt <- by(train, train$bedroomcnt, function(x) {
  return(mean(x$logerror))})
plot(names(err.bedroomcnt), err.bedroomcnt, type = 'l')

# correlation
# check remaining features, some numerical features should be charater type
# Only three counties: (this is fip)
# 6037 Los Angeles
# 6059 Orange County
# 6111 Ventura County
train[, c('fips', 'propertylandusetypeid', 'rawcensustractandblock', 'regionidcounty', 'assessmentyear', 'regionidzip', 'censustractandblock', 'regionidcity')] <-
  as.character(train[, c('fips', 'propertylandusetypeid', 'rawcensustractandblock', 'regionidcounty', 'assessmentyear', 'regionidzip', 'censustractandblock', 'regionidcity')])

library(corrplot)
correlations <- cor(train[, c('logerror', 'bathroomcnt', 'bedroomcnt', 'roomcnt',
                              'taxamount', 'structuretaxvaluedollarcnt', 'calculatedfinishedsquarefeet',
                              'calculatedbathnbr', 'fullbathcnt', 'finishedsquarefeet12',
                              'lotsizesquarefeet')]) # see a lot of NA
correlations <- cor(train[, c('logerror', 'bathroomcnt', 'bedroomcnt', 'roomcnt',
                              'taxamount', 'structuretaxvaluedollarcnt', 'calculatedfinishedsquarefeet',
                              'calculatedbathnbr', 'fullbathcnt', 'finishedsquarefeet12', 'lotsizesquarefeet')], 
                    use = "pairwise.complete.obs")
corrplot(correlations, method = "square", tl.cex = 1, type = 'upper')
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram

library(tabplot)
tableplot(train, select = c('logerror', 'bathroomcnt', 'bedroomcnt', 'roomcnt',
                            'taxamount', 'structuretaxvaluedollarcnt', 'calculatedfinishedsquarefeet',
                            'calculatedbathnbr', 'fullbathcnt', 'finishedsquarefeet12', 'lotsizesquarefeet'))
tableplot(train, select = c('logerror', 'yearbuilt', 'regionidcounty'))

train <- read.csv('train_property.csv', stringsAsFactors = F)
# check how many NAs in each feature
num.NA <- sort(colSums(sapply(train, is.na)))
# sort(sapply(train, function(x) {sum(is.na(x))}), decreasing=TRUE)
remain.col <- names(num.NA)[which(num.NA <= 0.2 * dim(train)[1])] # trainT = train
train <- train[, remain.col]

# Data exploration
# First check each feature and features with relationship with each other
# Take bathroomcnt, bedroomcnt and roomcnt for example
summary(train$bathroomcnt)
summary(train$bedroomcnt)
summary(train$roomcnt)
with(train, plot(bathroomcnt + bedroomcnt, roomcnt))
# Why there are so many house with roomcnt smaller than bath + bed?
# Assumption: error in data
boxplot(subset(train, roomcnt < bathroomcnt + bedroomcnt)$logerror,
        subset(train, roomcnt >= bathroomcnt + bedroomcnt)$logerror)
quantile(abs(train$logerror), 0.9)
boxplot(subset(train, roomcnt < bathroomcnt + bedroomcnt & abs(logerror) < 0.145)$logerror,
        subset(train, roomcnt >= bathroomcnt + bedroomcnt & abs(logerror) < 0.145)$logerror)
boxplot(abs(subset(train, roomcnt < bathroomcnt + bedroomcnt & abs(logerror) < 0.145)$logerror),
        abs(subset(train, roomcnt >= bathroomcnt + bedroomcnt & abs(logerror) < 0.145)$logerror))
with(train, t.test(logerror ~ (roomcnt < bathroomcnt + bedroomcnt)))
with(train, t.test(abs(logerror) ~ (roomcnt < bathroomcnt + bedroomcnt)))

# understand t.test
# t.test(x, y = NULL, alternative = c("two.sided", "less", "greater"), 
# mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95, ...)
# Welch t-test (var.equal = FALSE) and student t-test(var.equal = TRUE)
correct.rmcnt <- subset(train, roomcnt >= bathroomcnt + bedroomcnt)
wrong.rmcnt <- subset(train, roomcnt < bathroomcnt + bedroomcnt)
train$logerror.abs <- abs(train$logerror)
stderr <- sqrt(var(correct.rmcnt$logerror.abs) / dim(correct.rmcnt)[1] +
                 var(wrong.rmcnt$logerror.abs) / dim(wrong.rmcnt)[1])
t.score <- (mean(correct.rmcnt$logerror.abs) - mean(wrong.rmcnt$logerror.abs)) / stderr
p.val <- 2 * (1 - pt(t.score, df = 32425))
2 * (1 - pnorm(t.score)) # compares with normal distribtion

# For category variables
# (1) if level is not too many, we can use bar chart to check the difference first
#     and then use t test to compare. Take fip for example. 

table(train$fips)
str(train$fips)
library(lattice)
train$fips <- as.character(train$fips)
error.fip = by(train, train$fip, function(x) {
  return(mean(x$logerror))})

with(train, t.test(logerror ~ (fips == '6111')))
# 6037 Los Angeles
# 6059 Orange County
# 6111 Ventura County

hs.not.6111 <- subset(train, fips != '6111')
hs.6111 <- subset(train, fips == '6111')
stderr <- sqrt(var(hs.6111$logerror) / dim(hs.6111)[1] +
       var(hs.not.6111$logerror) / dim(hs.not.6111)[1])
t.score <- (mean(hs.not.6111$logerror) - mean(hs.6111$logerror)) / stderr

# if we use pooled variance
with(train, t.test(logerror ~ (fips == '6111')), var.equal = TRUE)
n1 <- dim(hs.6111)[1]
n2 <- dim(hs.not.6111)[1]
pooled.stderr <- sqrt((var(hs.6111$logerror) * (n1 - 1) +
                 var(hs.not.6111$logerror) * (n2 - 1))
                 / (n1 + n2 - 2)) * sqrt(1/n1 + 1/n2)
# a bit larger than stderr. 
# pooled.stderr^2 - stderr^2 = (s2^2 - s1^2) (n2 - n1) / (n1 + n2 - 2)
# with(train, anova(lm(logerror ~ fips)))

# Now think about why 6111 have larger logerror, maybe we have too few data points
num.fip <- by(train, train$fips, function(x){
  return(length(unique(x$parcelid)))})
train$num.fip <- num.fip[train$fips]
# conclusion, we should include boolean feature indicating whether fips = 6111.
# Also if other feature having level with sparse data, we should expect large log.error.

# (2) if category variables has too many levels, Take regionidcity for example.
# Disadvantage of using such variable as it is.
# Find similar levels and collapse them, how to find similar level though?
table(train$regionidzip)
train$regionidzip <- as.character(train$regionidzip)
error.zip <- by(train, train$regionidzip, function(x) {
  return(mean(x$logerror))
})
plot(density(error.zip))
train$error.zip <- error.zip[train$regionidzip]
summary(train$error.zip) # seeing NA due to regionidzip is NA. 
# How to impute
# Set up new level, find per region city what's most likely zip code and
# more advanced imputation, like libarry(mice)

summary(lm(logerror ~ regionidzip, train))
summary(lm(logerror ~ error.zip, train))

train$regionidzip.new <- ifelse(train$error.zip < quantile(error.zip, 0.05), '1',
                                ifelse(train$error.zip < quantile(error.zip, 0.95), '2', '3'))
                                     #  ifelse(train$error.zip < quantile(error.zip, 0.75), '3', '4')))
summary(lm(logerror ~ regionidzip.new, train))

# check these extreme cases and find out they also have relative sparse data.
error.zip[which(error.zip < -0.1)] # 96226
error.zip[which(error.zip > 0.1)]
dim(subset(train, regionidzip == 96226))

# Assumption: few number of houses for certain region id zip caused logerror large.
# How to verify
num.zip <- by(train, train$regionidzip, function(x) {
  return(dim(x)[1])})
train$num.zip = num.zip[train$regionidzip]
with(train, cor(logerror, num.zip, use = 'pairwise.complete.obs'))
with(train, cor(abs(logerror), num.zip, use = 'pairwise.complete.obs'))

# Continuous variables, take tax amount for example
# taxvaluedollarcnt: value to be taxed
# structuretaxvaluedollarcnt: value to be taxed from structure
# landtaxvaluedollarcnt: value to be taxed from land
# taxamount: actual paid tax
with(train, plot(structuretaxvaluedollarcnt + landtaxvaluedollarcnt, taxvaluedollarcnt))

summary(with(train, taxamount/taxvaluedollarcnt))

with(train, plot(taxamount/taxvaluedollarcnt, logerror))
with(subset(train,taxamount/taxvaluedollarcnt <= 0.1),
     plot(taxamount/taxvaluedollarcnt, logerror))
with(subset(train,taxamount/taxvaluedollarcnt <= 0.1), 
     cor(logerror, taxamount/taxvaluedollarcnt, use = 'pairwise.complete.obs')) #-0.04
with(subset(train,taxamount/taxvaluedollarcnt <= 0.1), 
     cor(logerror, taxamount, use = 'pairwise.complete.obs')) # -0.004
with(subset(train,taxamount/taxvaluedollarcnt <= 0.1), 
     cor(logerror, taxvaluedollarcnt, use = 'pairwise.complete.obs')) # 0.004

train$living.per <- with(train, calculatedfinishedsquarefeet/lotsizesquarefeet)
with(train, cor(logerror, calculatedfinishedsquarefeet, use = 'pairwise.complete.obs'))
with(train, cor(logerror, lotsizesquarefeet, use = 'pairwise.complete.obs'))
with(train, cor(logerror, calculatedfinishedsquarefeet/lotsizesquarefeet,
                use = 'pairwise.complete.obs'))

