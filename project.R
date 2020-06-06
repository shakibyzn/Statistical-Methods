
#########   SHAKIB YAZDANI , 9433873  ###########
#W####### STATISTICAL METHODS PROJECT ###########
######## loading library

library("readxl")
library(e1071)

# Read dataset
dataset <- read_excel("drug-dataset.xlsx")
names(dataset)
nrow(dataset)

# check normal distribution
p_value <- shapiro.test(dataset$Age)[2]
if (p_value > 0.05){
  
  ## Get a sample of size = 20
  sample_ages <- sample(dataset$Age,size=20)
  sample_size <- 20
  
  ### Point Estimates for Mean parameter
  true_mean <- mean(dataset$Age)
  sample_mean <- mean(sample_ages,size=20)
  print("Sample Mean:")
  print(sample_mean)
  error <- sample_mean - true_mean
  
}

hist(dataset$Age,breaks=10)
skewness(dataset$Age)


hist(sample_ages,breaks=10)
skewness(sample_ages)

#### Interval Estimates with knowing the standard deviation of the population
#### Confidence Interval = 0.95

z_critical <- qnorm(0.975) 
sd_ages <- sd(dataset$Age)
error <- z_critical * (sd_ages / sqrt(sample_size))
confidence_interval <- c(sample_mean - error, sample_mean + error)
print("Confidence Interval: ")
print(confidence_interval)

### Using t-test ( one-sample t-test)
print(t.test(sample_ages,mu=62))

### Using t-test ( two-sample t-test)
### to see whether the variances are equal or not we can see it by boxplot
boxplot(Age ~ Gender)
print(t.test(Age~Gender,mu=1,alt='two.sided',conf=0.95,var.eq=F,paired=F))


#### Confidence Intervals: Chi Squared Distribution, mean of population is unknown
#### We can go through all this with standard deviation or use of Var-test ####
#### Finding left and right densities 
#### Confidence Interval = 0.95


sd_confidence_interval <- c(
  sqrt(((sample_size-1)*sd(sample_ages)^2)/qchisq(c(.025),df=19, lower.tail=FALSE)),
  sqrt(((sample_size-1)*sd(sample_ages)^2)/qchisq(c(.975),df=19, lower.tail=FALSE))
)

print("Confidence Intervals: Chi Squared Distribution:")
print(sd_confidence_interval)
print("population standard deviation")
print(sd(dataset$Age))

### Using Var-test or F-test
print(var.test(sample(dataset$Age,size=20),sample(dataset$Age,size=20)))

