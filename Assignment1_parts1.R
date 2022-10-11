x<- c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70)
result.mean <- mean(x)
print(result.mean)
median.result <- median(x) 
print(median.result) 
(min(x)+max(x))/2 
quantile(x, prob=c(.25,.75), type=1) 
