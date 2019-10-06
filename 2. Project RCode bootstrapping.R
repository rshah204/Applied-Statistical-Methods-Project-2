library(boot)

x=df3
y=df4
total <- c(x,y)
diff <- function(x,i) mean(x[i[741:1505]]) - mean(x[i[1:740]])
id <- as.factor(c(rep("x",length(x)),rep("y",length(y))))
b <- boot(total, diff, strata=id, R = 2000)
hist(b$t, freq=)
ci <- boot.ci(b)
p.value <- sum(b$t>=b$t0)/b$R


df4nn=df4-mean(df4)
df3nn=df3-mean(df3)

# Performing bootstrap test
boot <- rep(0,2000) 
for(i in 1:2000)
{
  a <- sample(df4, 765, replace=T)
  b <- sample(df3, 740, replace=T)
  boot[i] <- mean(a)-mean(b)
}

# Histogram of expected difference in means
hist(boot, freq = FALSE, main = "Expected difference between 
     means of words' length", xlab = "Difference in means")
lines(density(boot), lwd = 2, col = 'red')

# Observed value of test statistic
mean(df4)-mean(df3)

quantile(boot,c(.025,.975))

sm = mean(df4)-mean(df3)
# sum(abs(boot) > sm)/2000
(sum(boot < -sm) + sum(boot > sm))/2000
randTest <- rep(0,2000) 
x <- c(df3,df4)
for(i in 1:2000)
{
  y <- sample(x)
  randTest[i] <- mean(y[741:1505])-mean(y[1:740])
}
###  compute the p-value ###
# sum(abs(randTest) > sm)/2000 
(sum(randTest < -sm) + sum(randTest > sm))/2000

par(mfrow=c(1,1))
hist(randTest, freq = FALSE, main = "Expected difference between 
     means of words' length", xlab = "Difference in means")
lines(density(randTest), lwd = 2, col = 'red')

quantile(randTest,c(.025,.975))
