# 自测的作业


#Which situation is most preferred to use R?
#A) statistical analysis of the clicinal effect of a new medicine.[spss]
###############B) analysis of user data in a social app startup.[cheap]
#C) Build a real time demand forecasting model[machine learning uses python]
#D) Voice recognition deep learning project[machine learning uses python]

#Which of the following task is least preferred to use R?
#A) build statistical models
#B) exploratory data analysis
#C) visualization
##################D) collaborative coding[not friendly with other programming languages]

#Which of the following is not a data type in R
#A) character
#B) logical
#################C) dataframe[dataframe]
#D) complex

#X is a 4*4 matrix. Which of the following return a different result than others?
#A) X[1:4, c(2,3,4)]
#B) X[:, c(2, 3, 4)]
#C) X[, -1]  [delete the first column]
##################D) X[:, seq(4)]

#You have a dataset with user and their spending. You want to understand the distribution of user spending. Which of the following visualizations will you choose?
#A) scatter plot[too crowded to see]
#B) boxplot[no detail to show the distribution, can only show the quantile boundaries]
##########C) histogram[successive on x-axis, good to show distribution]
#D) barchat[barchart is not successive on x-axis,not good to show distribution]

# 并且功能用&
x <- 5
if((x > 0) & (1<2)) {
  print("Non-negative number")
} 

# 或者功能用|
x <- 5
if((x > 0) | (1<2)) {
  print("Non-negative number")
} 




mtcars[1, "brand"] = "Mazda"
mtcars
# strsplit
mtcars$brand = sapply(strsplit(rownames(mtcars), split = " "), "[[", 1) #split后边接的是一个操作，比如取一个值或者求一个东西
sapply(split(mtcars$mpg, mtcars$brand), mean)
sapply(split(mtcars$mpg, mtcars$brand), median)

correlations = cor(mtcars[,c(-12)], use = "complete.obs") #必须全部元素为numeric,所以去掉第12列brand
library(corrplot)
corrplot(correlations, method = "number")

# for loop
i=2
j=4
for (i in 1:3){
  for (j in 1:6) {
    mymat[i,j] = i*j
  }
}
mymat[2,4]




chess = matrix(0, nrow = 8, ncol = 8)
dice = sample(1:6,1)
x = sample(1:8,1)
y = sample(1:8,1)

move_chess = function(x,y){
  if(dice == 1){
    if(y == 8){
      x_next = x
      y_next = y-2
      chess[x_next, y_next] = 1
    }else{
      x_next = x
      y_next = y+1
      chess[x_next, y_next] = 1
    }
  } else if(dice == 2){
    if(x == 8){
      x_next = x-2
      y_next = y
      chess[x_next, y_next] = 1
    }else{
      x_next = x+1
      y_next = y 
      chess[x_next, y_next] = 1
    }
  } else if(dice == 3){
    if(y == 1){
      x_next = x
      y_next = y+2
      chess[x_next, y_next] = 1
    }else{
      x_next = x
      y_next = y-1
      chess[x_next, y_next] = 1
    }
  } else if(dice == 4){
    if(x == 1){
      x_next = x+2
      y_next = y
      chess[x_next, y_next] = 1
    }else{
      x_next = x-1
      y_next = y
      chess[x_next, y_next] = 1
    }
  } else if(dice == 5){
    x_next = x
    y_next = y
    chess[x_next, y_next] = 1
  } else if(dice == 6){
    x_next = x
    y_next = y
    chess[x_next, y_next] = 1
  }
  return(x_next, y_next)
}

moves = 0
while (min(chess) < 1) {                                     #moves 的次数和move_chess 同步
  move_chess
  moves = moves +1
}
moves
