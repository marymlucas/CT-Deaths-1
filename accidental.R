# Need to add a preamble here

# Import and prepare the data set

# Ensure we're in the current working directory and import the dataset
setwd("/Users/mary/Downloads")  # or wherever you locally save the CSV file
raw.dataframe <- read.csv("Accidental_Drug_Related_Deaths__2012-June_2017.csv")

# I have also uploaded this dataset to github:

# Inspect the dataframe
str(raw.dataframe)
summary(raw.dataframe)

names(raw.dataframe)
nrow(raw.dataframe)
ncol(raw.dataframe)

# Processing the data

# After each step I run summary() again to see the changes

# Subset the data to get only the columns I want to use
dataframe <- raw.dataframe[, c("Date", "Sex", "Race", "Age", "Death.City", "Location", "InjuryPlace", "ImmediateCauseA", "EtOH")]
summary(dataframe)

# Filter out any rows that have Age, Sex, or Race fields blank
dataframe <- dataframe[dataframe$Age != "" & dataframe$Sex != "" & dataframe$Race != "" & dataframe$Date != "",]
summary(dataframe)

# Dimensions of processed dataframe
nrow(dataframe)
ncol(dataframe)

# In the EtOH column, replace all 'y' with 'Y' so they're not counted as separate values
dataframe$EtOH[which(dataframe$EtOH == "y")] = "Y"
summary(dataframe)


# Create two new columns from Date variable for month and year
dates <- do.call('rbind', strsplit(as.character(dataframe$Date),'/',fixed=TRUE))
dataframe$Month <- dates[, 1]
dataframe$Year <- dates[, 3]

summary(dataframe)
nrow(dataframe)
ncol(dataframe)

# Data Analysis

# Drug-Related Deaths by Age

f <- fivenum(dataframe$Age)  # five number summary of the Age variable
f

# Outliers for the Age variable
# Calculate the lower and upper ends of the outlier
c(f[2] - 1.5 * IQR(f), f[4] + 1.5 * IQR(f))

# What are the outliers in the data
dataframe$Age[dataframe$Age < (f[2] - 1.5 * IQR(f))] # numbers less than the lower end
dataframe$Age[dataframe$Age > (f[4] + 1.5 * IQR(f))] # numbers greater than the upper end


# Stem plot for the Age variable
stem(dataframe$Age)

# Frequency and histogram for the age variable
table(dataframe$Age)
hist(dataframe$Age,  xlab = "Age", ylab = "Drug-Related Deaths", main = "Drug-Related Deaths by Age")

boxplot(dataframe$Age, horizontal = TRUE, col = "green", xlab = "", xaxt = "n", main = "Drug-Related Deaths by Age")
axis(side = 1, at = fivenum(dataframe$Age), labels =  TRUE, las = 2)
text(fivenum(dataframe$Age), rep(1.2, 5), srt = 90, adj = 0, labels = c("Min", "Lower Hinge", "Median", "Upper Hinge", "Max"))

# Drug-Related Deaths by Race

table(dataframe$Race)
prop.table(table(dataframe$Race))

byRace = sort(table(dataframe$Race), decreasing = TRUE) # we now sort to obtain the top 3
byRace  # display the data
prop.table(byRace)[1:3]
pie(prop.table(byRace)[1:3], col=rainbow(3), main = "Drug-Related Deaths by Race")

# Corresponds roughly to the racial demographics of the population of Connecticut
# https://www.census.gov/quickfacts/fact/table/CT/HSG495216
# 80% white

# Trend in Drug-Related Deaths by month of the year
table(dataframe$Month)
barplot(table(dataframe$Month), main="Drug-Related Deaths by Month of the Year", ylab = "Frequency", xlab = "Months", ylim = c(0, 350))

# Trend in Drug-Related Deaths over time
table(dataframe$Year)
plot(table(dataframe$Year), main="Drug-Related Deaths Over Time", ylab = "Frequency", xlab = "Years", ylim = c(0,1000), type = 'o', col="blue")


# Distribution of the Age Variable
table(dataframe$Age)
barplot(table(dataframe$Age),  xlab = "Age", ylab = "Deaths", main = "Drug-Related Deaths by Age", ylim=c(0, 120))
mean(dataframe$Age)
median(dataframe$Age)
sd(dataframe$Age)

# To plot the probability density function of this distribution
mu <- mean(dataframe$Age)
sigma <- sd(dataframe$Age)
x <- seq(mu - 3*sigma, mu + 3*sigma) # sequence of value spanning 3 standard deviations either side of the mean
pdf <- dnorm(x, mean = mu, sd = sigma)  # we get the pdf values
plot(x, pdf, type = "l", col = "blue", xlab = "Age", ylab = "PDF", main = "Probality Density Function")

 # The probabilities of deaths within 1 standard deviation, 2 standard deviations, 3 standard deviations
  
# Within 1 standard deviation: 
pnorm(mu + sigma, mean = mu, sd = sigma) - pnorm(mu - sigma, mean = mu, sd = sigma)

# Within 2 standard deviations: 
pnorm(mu + 2*sigma, mean = mu, sd = sigma) - pnorm(mu - 2*sigma, mean = mu, sd = sigma)

# Within 3 standard deviations:
pnorm(mu + 3*sigma, mean = mu, sd = sigma) - pnorm(mu - 3*sigma, mean = mu, sd = sigma)


# Central Limit Theorem
# 1000 samples of size 10 
options(digits=4)

samples <- 1000
sample.size <- 10

xbar <- numeric(samples)

for (i in 1: samples) {
  x <- sample(dataframe$Age, sample.size)
  xbar[i] <- mean(x)
}

# Histogram of the densities of the sample means
hist(xbar, prob=TRUE, main="Densities of Sample Means (Sample Size = 10)", xlab = "Sample Means")

# The mean of the sample means and the standard deviation of the sample means
mean(xbar)
sd(xbar)


# 1000 samples of size 20 - histogram of the densities of the sample means. 
options(digits=4)

samples <- 1000
sample.size <- 20

xbar <- numeric(samples)

for (i in 1: samples) {
  x <- sample(dataframe$Age, sample.size)
  xbar[i] <- mean(x)
}

# Histogram of the densities of the sample means
hist(xbar, prob=TRUE, main="Densities of Sample Means (Sample Size = 20)", xlab = "Sample Means")

# The mean of the sample means and the standard deviation of the sample means
mean(xbar)
sd(xbar)

# 1000 samples of size 30 - histogram of the densities of the sample means. 
options(digits=4)

samples <- 1000
sample.size <- 30

xbar <- numeric(samples)

for (i in 1: samples) {
  x <- sample(dataframe$Age, sample.size)
  xbar[i] <- mean(x)
}

# Histogram of the densities of the sample means
hist(xbar, prob=TRUE, main="Densities of Sample Means (Sample Size = 30)", xlab = "Sample Means")

# The mean of the sample means and the standard deviation of the sample means
mean(xbar)
sd(xbar)

# 1000 samples of size 40 - histogram of the densities of the sample means. 
options(digits=4)

samples <- 1000
sample.size <- 40

xbar <- numeric(samples)

for (i in 1: samples) {
  x <- sample(dataframe$Age, sample.size)
  xbar[i] <- mean(x)
}

# Histogram of the densities of the sample means
hist(xbar, prob=TRUE, main="Densities of Sample Means (Sample Size = 40)", xlab = "Sample Means")

# The mean of the sample means and the standard deviation of the sample means
mean(xbar)
sd(xbar)



# Sampling Methods (using a sample size of 20)
library(sampling)

samplesize = 20

# Simple Random Sampling

sample <- srswor(samplesize, nrow(dataframe))

sample.1 <- dataframe[sample != 0, ]
sample.1[c("Sex", "Race", "Age", "Month", "Year")] # for brevity

table(sample.1$Year)

# Mean of Age variable for this sample
mean(sample.1$Age)



# Sequential Sampling

N <- nrow(dataframe)
n <- samplesize

k <- floor(N / n) # set the interval, used floor() to avoid NA due to large sample size

r <- sample(k, 1) # select the first sample

sample <- seq(r, by = k, length = n) # select every kth item

sample.2 <- dataframe[sample, ]
sample.2[c("Sex", "Race", "Age", "Month", "Year")] # for brevity

# Show the frequencies for each year (Year). 
table(sample.2$Year)
table(sample.2$Year)/table(dataframe$Year)


# Show the percentages of these with respect to the entire dataset. 
options(digits = 4)
data <- sample.2$Year

# Mean of Age variable for this sample
mean(sample.2$Age)




# Stratified Sampling

order.index <- order(dataframe$Year) # Order the data 

# Draw a stratified sample using proportional sizes based on the Race variable.
data <- dataframe[order.index, ]
freq <- table(dataframe$Year)

# determine how many from each stratum will be selected. 
sizes <- round(samplesize * freq / sum(freq))
sizes
sum(sizes)

# select samples from each stratum.
st <- strata(data, stratanames = c("Year"), size = sizes, method = "srswor")

sample.3 <- getdata(data, st)
sample.3[c("Sex", "Race", "Age", "Month", "Year")] # for brevity

# Show the frequencies for each year (Year). 
table(sample.3$Year)

# Show the percentages of these with respect to the entire dataset.
options(digits = 4)
data <- sample.3$Year

table(data)/table(dataframe$Year)

# Mean of Age variable for this sample
mean(sample.3$Age)


# Comparing the means of Age variable for these three samples with the entire data.

cat("Mean of entire data set - ", mean(dataframe$Age))
cat("Mean of Simple Random Wo/R - ", mean(sample.1$Age))
cat("Mean of Systematic Sampling - ", mean(sample.2$Age))
cat("Mean of Stratified Sampling - ", mean(sample.3$Age))



# Confidence Intervals (Age variable)

# Using samples of sample size 10, 20, 30, 40 for Confidence levels of 80 and 90

pop.mean <- mean(dataframe$Age) # mean of the population
pop.sd <- sd(dataframe$Age) # standard deviation of the population
conf <- c(80, 90)  # confidence levels
alpha <- 1 - conf/100  

# Sample Size 10
sample.size <- 10
sample.data <- sample(dataframe$Age, sample.size)
sd.sample.means <- pop.sd/sqrt(sample.size)
xbar <- mean(sample.data)

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                 100*(1-i), i, 
                 xbar - qnorm(1-i/2) * sd.sample.means,
                 xbar + qnorm(1-i/2) * sd.sample.means)
  cat(str,"\n")
}

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), Precision = %.2f",
                 100*(1-i), i, 
                 2* qnorm(1-i/2) * sd.sample.means)
  cat(str,"\n")
}




sample.size <- 20
sample.data <- sample(dataframe$Age, sample.size)
sd.sample.means <- pop.sd/sqrt(sample.size)
xbar <- mean(sample.data)

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                 100*(1-i), i, 
                 xbar - qnorm(1-i/2) * sd.sample.means,
                 xbar + qnorm(1-i/2) * sd.sample.means)
  cat(str,"\n")
}

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), Precision = %.2f",
                 100*(1-i), i, 
                 2* qnorm(1-i/2) * sd.sample.means)
  cat(str,"\n")
}

sample.size <- 30
sample.data <- sample(dataframe$Age, sample.size)
sd.sample.means <- pop.sd/sqrt(sample.size)
xbar <- mean(sample.data)

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                 100*(1-i), i, 
                 xbar - qnorm(1-i/2) * sd.sample.means,
                 xbar + qnorm(1-i/2) * sd.sample.means)
  cat(str,"\n")
}

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), Precision = %.2f",
                 100*(1-i), i, 
                 2* qnorm(1-i/2) * sd.sample.means)
  cat(str,"\n")
}

sample.size <- 40
sample.data <- sample(dataframe$Age, sample.size)
sd.sample.means <- pop.sd/sqrt(sample.size)
xbar <- mean(sample.data)

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                 100*(1-i), i, 
                 xbar - qnorm(1-i/2) * sd.sample.means,
                 xbar + qnorm(1-i/2) * sd.sample.means)
  cat(str,"\n")
}

for (i in alpha) {
  str <- sprintf("%2d%% Conf Level (alpha = %.2f), Precision = %.2f",
                 100*(1-i), i, 
                 2* qnorm(1-i/2) * sd.sample.means)
  cat(str,"\n")
}

