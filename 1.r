##Statistics with R: Hypothesis Testing, Crosstab Tables, and
##Chi-Squared, Lesson 7 by Courtney Brown
##https://www.youtube.com/watch?v=AyNQ7Wznf_k

## Error associated with the hypothesis
## 2 types
## Errors you wanna avoid:
### Disease ->  conventional <--> middle work
### unconventional <--> hardly work


## Galileo
## Type I: if H0=True and reject it || => Safety of established system
## Type II: if H0=False and do not reject it || => Error for not
## adopting a new system

## alpha-level is the discriminability for type 1 errors || alpha=0.5

## Increase in alpha-level => increase in probability to have type II
## errors.

## => p-value: probability of actually committing a mistake of TI

###### 12.000 yeas ago - 3.000 years ######

## William H. Riker - minimum winning coalition

## the agenda setting

## William McFee Mathematical Sociology
## Anatol Rapoport Mathematician

## Frog pane principles


######### MYSELF
## p-value => probability of being a behavior guided by the
## particular sample; a "minor variance"; H0 not structurally wrong
## for high p-values (>0.05).

## That is, (for high p-values) it's likely that the deviance is due
## to the sample, not the (H0) hypothesis.

###### Great differences in observation
###### In the (expected) sample-normal-distribution the value is in
###### the tip. Therefore, the integral-value beyond it is low.
## low p-values => we go with the Alternative Hypothesis
## "No, it's extremely unlikely that we hit a biased sample, that would
## be too unlikely an event."

###### Low differences in observation
###### High chances of having other samples alike (or "worst")
##### Integral-value beyond that point is high.
## high p-values => we go with the Null Hypothesis



################# ************* ##################
############### PRATICE ######################

### EXAMPLE FROM
## Statistical Hypothesis Testing with SAS and R
## http://www.d-taeger.de/index.html

##############################################
########## ********** ##################

## (A) H0 ∶ μ = μ0 vs H1 ∶ μ ≠ μ0
data <- read.table("../Data/HypTesting/blood_pressure.txt", header=T, sep=",")
t.test(data$mmhg,mu=140)
## t = -3.8693, df = 54, p-value = 0.0002961

## Calculate sample mean and standard deviation
xbar<-mean(data$mmhg)
sigma<-sd(data$mmhg)
## Set mean value under the null hypothesis
mu0<-140
## Calculate test statistic
t<-sqrt(55)*(xbar-mu0)/sigma
## Output results
t

## For alpha = 0.05, we have (two.sided test),
## The lower Quantile of T distribution:
low_quantile <- qt(0.025,nrow(data)-1)
## The upper Quantile of T distribution:
upper_quantile <- qt(0.975,nrow(data)-1)

## Our t = -3.869272, which is out of the boundaries of
## low and upper_quatile = |2.004045|
## Therefore, we reject H0.

## Now, for the p-value, we have (pt stands for probability of T
## distribution)
p.value <- 2*pt(t, nrow(data)-1)

## (B) H0 ∶ 𝜇≤𝜇0 vs H1∶𝜇>𝜇0
## AH :=> t > t_(1−𝛼, n−1)
t_critical <- qt(0.95,nrow(data)-1)
t.test(data$mmhg,mu=140,alternative="greater")
##t = -3.8693, df = 54, p-value = 0.9999
## which means, we certainly do not reject H0

## (C) H0 ∶ 𝜇 ≥ 𝜇0 vs H1 ∶ 𝜇<𝜇0
## AH :=> t_(𝛼, n−1) < t
t.test(data$mmhg,mu=140,alternative="less")
## t = -3.8693, df = 54, p-value = 0.0001481
## Which means, we reject H0 <=> H1 is true, 𝜇<𝜇0

## Once it's not equal, we find if it's greater or lower then the mean0 (𝜇0).
## In our case, it's lower.

##(A): p-value = 0.0002961
##(C): p-value = 0.0001481

## options(scipen=3)
## format.pval(2*pt(-3.86927,54),1,eps=0.0001)


## p-values
## pnorm(x) Gaussian
## pt(x,df,ncp) t
## pchisq(x,df,nc) 𝜒2  || ncp: noncentrality parameter (optional)
## pf(x,ndf,ddf,nc) F  || ncp: noncentrality parameter (optional)


### Z-test (when we wanna estimate the mean of the sample, and we know its variance)
### Z-distributions are normal

## Calculate sample mean and total sample size
xbar<-mean(blood_pressure$mmhg)
n<-length(blood_pressure$mmhg)
## Set mean value under the null hypothesis
mu0<-140
## Set known sigma
sigma<-20
## Calculate test statistic and p-values
z<-sqrt(n)*(xbar-mu0)/sigma ####### -> here we calculate the statistic
p_value_A=2*pnorm(-abs(z))  #######
p_value_B=1-pnorm(z)
p_value_C=pnorm(z)
## Output results
z
p_value_A
p_value_B
p_value_C


################################################################
### S-testing (estimation of the mean value of a population, with
### gaussian distribution assumption under the sample)
################################################################


######### Two-sample tests ##############

## Intra-mean comparison, when both deviations are known (Z)

## Set difference to be tested;
d0<-0

## Set standard deviation of sample with status 0
sigma.0 <- 10

## Set standard deviation of sample with status 1
sigma.1 <- 12

## Calculate the two means
mean.status0 <- mean(data$mmhg[data$status==0])
mean.status1 <- mean(data$mmhg[data$status==1])

## Calculate both sample sizes
n.status0 <- length(data$mmhg[data$status==0])
n.status1 <- length(data$mmhg[data$status==1])

## Calculate test statistic and two-sided p-value
dif <- ((mean.status0 - mean.status1) - d0)
dev.norm <- sqrt( (sigma.0^2 / n.status0) + (sigma.1^2 / n.status1) )

z <- dif / dev.norm ## Statistic

p_value=2*pnorm(-abs(z)) ## p-value

## Output results
z
p_value ## => reject H0 (it would be too rare to take a sample with
## these values, assuming this assumption as true)
## => the means of the samples differ from each other


################### S-test for 2 samples ################## (T distribution)
########### same sample deviation estimation, true deviation unknown #####
########## Although, we believe the deviation of each is equal.

status.0 <- data$mmhg[data$status==0]
status.1 <- data$mmhg[data$status==1]

####### In the function t.test, we state always the alternative ######
###### And, always test the opposite of the alternative ####

### For example, if we state the alternative as "greater difference"
### and the p-value is high,
### Then, we do not reject the opposite of the alternative. (we dont
### take the alternative).
### and thus we have a "lower difference"; not "greater difference".

###### If the opp. of the alt. is false (low p-value) ####
##### Then, the alternative is true ###

## Draw it; eat it; but do understand it!

t.test(status.0,status.1,mu=0,alternative="two.sided",var.equal=TRUE)
## t = -10.468, df = 53, p-value = 8.298e-15
## The alternative is true: it could be greater or lower, but not
## equal. (two sided)

t.test(status.0,status.1,mu=0,alternative="l",var.equal=TRUE)
## t = -10.468, df = 53, p-value = 8.298e-15 ## low p-value
## The Alternative is true, which is "l". i.e., (the difference is) lower.
## #=> AH is true <=> u0-u1<0 <=> u0 < u1

## The result is expected, because status1 means a population with
## true value (1) for high blood pressure.

################### Welch test #############
## "This test is also known as a two-sample t-test or
## Welch–Satterthwaite test"
####### It assumes the sample's variances are not necessarily equal
t.test(status.0,status.1,mu=0,alternative="two.sided",var.equal=FALSE)


## The test statistic T approximately follows a t-distribution with 𝜈
## degrees of freedom [Bernard Welch (1947) and Franklin Satterthwaite
## (1946) approximation].
## • t_(𝛼, 𝜈) is the 𝛼-quantile of the t-distribution with 𝜈 degrees of freedom.
## • William Cochran and Gertrude Cox (1950) proposed an alternative way
## to calculate critical values for the test statistic.

## ** By default R has no option to calculate the Cochran and Cox
## approximation **
