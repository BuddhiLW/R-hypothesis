## Chi-square, with known mean and sample Gaussian distribution (Test on variance)
## H0: Var = 400 <=> sigma0 = 20

blood_pressure <- read.table("./Data/HypTesting/blood_pressure.txt", header=T, sep=",")
## iq <- read.table("./Data/HypTesting/iq.txt", header=T, sep=",")


## The test is very sensitive to violations of the Gaussian assumption,
## especially if the sample size is small [see Sheskin (2007) for details].

mean0<-130 # Set known mean
sigma0<-20 # Set std under the null hypothesis

## Calculate squared sum;
sum_squared_diff<-sum((blood_pressure$mmhg-mean0)^2)

## Calculate test-statistic and p-values;
df<-length(blood_pressure$mmhg)
chisq<-sum_squared_diff/(sigma0^2)

## p-value for hypothesis (A)
p_value_A=2*min(pchisq(chisq,df),1-pchisq(chisq,df))
## p-value for hypothesis (B)
p_value_B=1-pchisq(chisq,df)
## p-value for hypothesis (C)
p_value_C=pchisq(chisq,df)

## Output results
chisq
df
p_value_A
p_value_B
p_value_C


#############𝝌2-test on the variance (mean unknown)##############

## Calculate sample std and sample size;
std_sample<-sd(blood_pressure$mmhg)
n<-length(blood_pressure$mmhg)

## Set std under the null hypothesis
sigma0<-20

## Calculate test-statistic and p-values;
df=n-1
chisq<-(df*std_sample^2)/(sigma0^2)
## p-value for hypothesis (A)
p_value_A=2*min(pchisq(chisq,df),1-pchisq(chisq,df))
## p-value for hypothesis (B)
p_value_B=1-pchisq(chisq,df)
## p-value for hypothesis (C)
p_value_C=pchisq(chisq,df)

## Output results
chisq
df
p_value_A ## => pretty supportive of H0
p_value_B
p_value_C

################ Paired-Variance testing; F-test #################
###############  (Independent populations) #####################
##########################################################
status0<-blood_pressure$mmhg[blood_pressure$status==0]
status1<-blood_pressure$mmhg[blood_pressure$status==1]

var.test(status0,status1,alternative="two.sided")
var.test(status0,status1,alternative="g")
var.test(status0,status1,alternative="l")
####### variances approximately equal; probably, var1>var2, by a modest amount
