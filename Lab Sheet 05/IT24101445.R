#1
setwd("C:\\Users\\it24101445\\Documents\\IT24101445")

branch_data<-read.table("Exercise.txt", header=TRUE , sep = ",")

fix(branch_data)
attach(branch_data)

#2
#Sales       - quantitative , ratio scale
#Advertising - quantitative , ratio scale
#Years       - quantitative , ratio scale
#Branch      - quantitative , nominal scale

#3
boxplot(branch_data$Sales_X1, main="Box plot for sales",outline=TRUE,outpch=8,horizontal=TRUE)

#4
summary(branch_data$Advertising_X2)
IQR(branch_data$Advertising_X2)

#5
find_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  outliers <- x[x < lower | x > upper]
  return(outliers)
}

find_outliers(branch_data$Years)