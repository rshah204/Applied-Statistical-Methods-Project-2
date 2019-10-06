# . Length of a Dataset: length()
# . Summary Statistics: summary()
# . Normality Testing: qqplot() abline(), ks.test(), shapiro.test()
# . T-Test: t.test()
# . Equal Variance test: var.test()
# . Quantile from T-Dist: qt()
# . Quantile from F-Dist: qf()
# . Histograms: hist()
# . Boxplots: boxplot()

# To remove the outliers
df1n = df1[!df1 %in% boxplot.stats(df1)$out]

# Importing the samples of sentences' word count for book and article
df1 = as.numeric(read.table("LifeOfPi.csv", nrows=1, skip=0, header = FALSE))
df2 = as.numeric(read.table("SpaceX.csv", nrows=1, skip=0, header = FALSE))
# Sample size
n1 = length(df1) # Book 
n2 = length(df2) # Article
# Summary Statistics
summary(df1) # Book 
summary(df2) # Article

# Creating Boxplots
par(mfrow=c(1,2))
boxplot(df1, main = "sentences' word count in Book")
boxplot(df2, main = "sentences' word count in Article")

# Creating Histograms
par(mfrow=c(1,2))
hist(df1, freq = FALSE, main = "Histogram of sentences' word count in Book", 
     xlab = "word count", ylim = c(0,0.055))
lines(density(df1), lwd = 2, col = 'red')
hist(df2, freq = FALSE, main = "Histogram of sentences' word count in Article",
     xlab = "word count")
lines(density(df2), lwd = 2, col = 'red')

# Drawing the QQ-plot (drawn for standardized data)
par(mfrow=c(1,2))
qqnorm((df1-mean(df1))/sd(df1), main = "Normal Q-Q Plot for Book") 
abline(0,1) # drawing a 45-degree reference line
qqnorm((df2-mean(df2))/sd(df2), main = "Normal Q-Q Plot for Article") 
abline(0,1) # drawing a 45-degree reference line

# Log transformation of samples
ledf1 = log(df1) # book
ledf2 = log(df2) # article

# Creating Boxplots
par(mfrow=c(1,2))
boxplot(ledf1, main = "Log of sentences' word count in Book")
boxplot(ledf2, main = "Log of sentences' word count in Article")

# Creating Histograms
par(mfrow=c(1,2))
hist(ledf1, freq = FALSE, main = "Histogram of Log of sentences' word count in Book", 
     xlab = "log(word count)")
lines(density(ledf1), lwd = 2, col = 'red')
hist(ledf2, freq = FALSE, main = "Histogram of Log of sentences' word count in Article",
     xlab = "log(word count)")
lines(density(ledf2), lwd = 2, col = 'red')

# Drawing the QQ-plot (drawn for standardized data)
par(mfrow=c(1,2))
qqnorm((ledf1-mean(ledf1))/sd(ledf1), main = "Normal Q-Q Plot for Book") 
abline(0,1) # drawing a 45-degree reference line
qqnorm((ledf2-mean(ledf2))/sd(ledf2), main = "Normal Q-Q Plot for Article") 
abline(0,1) # drawing a 45-degree reference line

# Performing Shapiro-Wilk test on log transformed samples
shapiro.test(ledf1) # book
shapiro.test(ledf2) # article

# Performing Welch's t-test
t.test(ledf1, ledf2, var.equal=FALSE, alternative = "two.sided")

# Importing the samples of words' length for book and article
df3 = as.numeric(read.table("LifeOfPi.csv", nrows=1, skip=1, header = FALSE))
df4 = as.numeric(read.table("SpaceX.csv", nrows=1, skip=1, header = FALSE))
# Sample size
n3 = length(df3) # Book 
n4 = length(df4) # Article
# Summary Statistics
summary(df3) # Book 
summary(df4) # Article

# Creating Boxplots
par(mfrow=c(1,2))
boxplot(df3, main = "word length in Book")
boxplot(df4, main = "word length in Article")

# Creating Histograms
par(mfrow=c(1,2))
hist(df3, freq = FALSE, main = "Histogram of word length in Book",
     xlab = "word length")
lines(density(df3), lwd = 2, col = 'red')
hist(df4, freq = FALSE, main = "Histogram of word length in Article",
     xlab = "word length")
lines(density(df4), lwd = 2, col = 'red')

# Drawing the QQ-plot (drawn for standardized data)
par(mfrow=c(1,2))
qqnorm((df3-mean(df3))/sd(df3), main = "Normal Q-Q Plot for Book") 
abline(0,1) # drawing a 45-degree reference line
qqnorm((df4-mean(df4))/sd(df4), main = "Normal Q-Q Plot for Article") 
abline(0,1) # drawing a 45-degree reference line

#### Goodness of fit #####
## Input Data
# goals <- c(0,1,2,3,4)
# goal_freq <- c(23, 43, 21, 13, 8)
# df3
goals <- c(0,1,2,3,4,5,6,7,8)
goal_freq <- c(68,128,146,131,84,66,45,28,20)
#df4
goals <- c(0,1,2,3,4,5,6,7,8,9)
goal_freq <- c(26,117,130,141,99,113,58,34,24,17)
goal_matrix <- matrix(cbind(goal_freq, goals), ncol = 2)

## Performing the test
results_goal <- goodfit(x = goal_matrix,
                        type = "poisson",
                        method = "MinChisq")
results_goal
summary(results_goal)


# Bootstrapping
library(boot)
x=df3
y=df4
total <- c(x,y)
diff <- function(x,i) mean(x[i[741:1505]]) - mean(x[i[1:740]])

id <- as.factor(c(rep("x",length(x)),rep("y",length(y))))
b <- boot(total, diff, strata=id, R = 2000)
hist(b$t, freq=)
ci <- boot.ci(b)
quantile(b$t,c(0.025,0.975))
p.value <- sum(b$t>=b$t0)/b$R

ks.test(mean_samples, "pnorm", mean = 0, sd = 1)
ks.test(mean_samples_norm, "pnorm", mean = 0, sd = 1)
shapiro.test(mean_samples_norm)

hist(log(df1), freq = FALSE, main = 'Histogram of number of words in Book')
hist(log(df2), freq = FALSE, main = 'Histogram of number of words in Article')
hist(log(df3), freq = FALSE, main = 'Histogram of word length in Book')
hist(log(df4), freq = FALSE, main = 'Histogram of word length in Article')

hist(df3, freq = FALSE)
lines(density(df3))

shapiro.test(df4)
shapiro.test(log(df1))

wilcox.test(log(df3),log(df4), mu=0, alternative = "two.sided", conf.int = TRUE, conf.level = 0.95)
# wilcox.test(x, y = NULL,
#             alternative = c("two.sided", "less", "greater"),
#             mu = 0, paired = FALSE, exact = NULL,
#             correct = TRUE,
#             conf.int = FALSE, conf.level = 0.95, ...)

N=200 # random samples
n=10 # size of random samples
lambda = 12 # Poisson distribution parameter

# Generating random samples
samples = matrix(rpois(N * n, lambda), nrow = N)

# Calculating sample mean for each sample
mean_samples = matrix(0, nrow = N) # Initialize
for(i in 1:N){
  mean_samples[i] = mean(samples[i,1:n]) 
  # print(mean_samples[i])
}

# Reporting first 10 sample means
mean_10 = mean_samples[1:10]
print(mean_10)


mean_value = mean(mean_samples)
var_value = var(mean_samples)
sd_value = sd(mean_samples)

# par(mfrow=c(1,1))

# Plotting histogram of sample means (y-axis: density)
hist(mean_samples, freq = FALSE, main = 'Histogram of Sample Means') 
# The option freq=FALSE plots probability densities instead of frequencies
# Adding a dashed line for the mean of sample means
abline(v=mean(mean_samples), lwd=3, col='darkslategray4')
# Theoretical mean and standard deviation 
theoretical_mean = lambda
theoretical_sd = sqrt(lambda/n)
# Adding a dashed line for the theoretical mean
abline(v=theoretical_mean, lty=2, lwd=3, col='firebrick')
legend(c("Sample", "Theoretical"),x='topright', lty=c(1,2), 
       lwd=c(3,3), col=c('darkslategray4', 'firebrick'))

# Fitting a density estimate (default estimator)
d = density(mean_samples)
plot(d, main = "Kernel Density Estimate of Sample Means", bty = 'n')
polygon(d, col = "#FFCCCC", border = 'blue', lwd = 2)
# Adding a dashed line for the sample mean
abline(v=mean(mean_samples), lwd=3, col='darkslategray4')
# Adding a dashed line for the theoretical mean
abline(v=theoretical_mean, lty=2, lwd=3, col='firebrick')
legend(c("Sample", "Theoretical"),x='topright', lty=c(1,2), 
       lwd=c(3,3), col=c('darkslategray4', 'firebrick'))




qqnorm(mean_samples)
abline(a = 0, b = 1)

# Standardizing the sample means to draw QQ-plot
mean_samples_norm<-(mean_samples-mean(mean_samples))/sd(mean_samples) 
qqnorm(mean_samples_norm) # drawing the QQplot
abline(0,1) # drawing a 45-degree reference line

ks.test(mean_samples, "pnorm", mean = 0, sd = 1)
ks.test(mean_samples_norm, "pnorm", mean = 0, sd = 1)
shapiro.test(mean_samples_norm)

error = qnorm(0.975)*sd(mean_samples)/sqrt(n)
left = mean(mean_samples)-error
right = mean(mean_samples)+error
left
right
