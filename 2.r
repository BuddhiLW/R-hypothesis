################# PAIRED Z-TEST ####################################

## Example: To test if the mean intelligence quotient increases by 10
## comparing before training (IQ1) and after training (IQ2) (dataset
## in Table A.2). It is known that the standard deviation of the
## difference is 1.40. Note: Because we are interested in a negative


##Hypothesis: X1-X2= -10 (when trained, previously, the difference mean is 10
##points higher.)

## Read the data
iq <- read.table("./Data/HypTesting/iq.txt", header=T, sep=",")

## Set difference to test;
d0<--10
## Set standard deviation of the difference
sigma_diff<-1.40
## Calculate the mean of the difference
mean_diff<-mean(iq$IQ1-iq$IQ2)
## Calculate the sample size
n_total<-length(iq$IQ1)

## Calculate test statistic and two-sided p-value
z<-sqrt(n_total)*((mean_diff-d0)/sigma_diff)
p_value=2*pnorm(-abs(z))
## Output results
z
p_value ##=> Do not reject H0; there is in fact a mean difference of
##10 poins between groups


#################### PAIRED T-TEST ##############################
t.test(iq$IQ1, iq$IQ2, mu=-10, alternative="two.sided", paired = TRUE)
## t = -1.2854, df = 19, p-value = 0.2141
