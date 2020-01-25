install.packages("ggplot2")
install.packages("mosaic")
install.packages("Sleuth3")
library(ggplot2)        # plotting & data
library(dplyr)          # data manipulation
library(mosaic)
library(Sleuth3)
# Loading Survey data in RStudio
Survey_Data <- read.csv("/cloud/project/fifa.csv")
summary(Survey_Data)
# Data Cleansing
Survey_Data_1 <-  Survey_Data[!(is.na(Survey_Data$Preferred.Foot) | Survey_Data$Preferred.Foot=="" ) , ]
ggplot(Survey_Data_1, aes(x=Survey_Data_1$Age, color = Survey_Data_1$Preferred.Foot)) +
  geom_histogram(fill="Grey", position="dodge", bins = 10)  +   theme(legend.position="bottom") + labs(x = "Age", color = "Legend: Preferred Foot")
ggplot(Survey_Data_1, aes(x = Survey_Data_1$Preferred.Foot , y = Survey_Data_1$Stamina)) +
  geom_boxplot() + guides(fill = TRUE) +
  theme(axis.text.x = element_text(angle = 65, vjust=0.5, hjust=0.5))+ labs(x = "Preferred Foot", y = "Stamina")
ggplot(Survey_Data_1, aes(x = Survey_Data_1$Height, color = Survey_Data_1$Preferred.Foot)) +
  geom_bar() + guides(fill = TRUE) +
  theme (legend.position="bottom") + theme (axis.text.x = element_text(angle = 65, vjust=0.5, hjust=0.5))+ labs(x = "Height" , color = "Legend: Preferred Foot")
ggplot(Survey_Data_1, aes(Position)) + 
  geom_bar(aes(fill = ..count..)) + 
  ggtitle("Distribution based on Playing Position")+
  theme(axis.text.x = element_text(angle = 65, vjust=0.5, hjust=0.5))
countries_count <- count(Survey_Data_1, Nationality)
top_10_countries <- top_n(countries_count, 10, n)
top_10_countries_1 <- top_n(countries_count,10)
top_10_country_names <- top_10_countries$Nationality

country <- filter(Survey_Data_1, Nationality == top_10_country_names)
ggplot(country, aes(x = Nationality)) + 
  geom_bar(col = "orange", aes(fill = ..count..)) + ggtitle("Distribution based on Nationality of Players (Top 10 Countries)") +  theme (legend.position="bottom")
g_age_overall <- ggplot(Survey_Data_1, aes(Survey_Data_1$Age, Survey_Data_1$Penalties))
g_age_overall + 
  geom_point(aes(color=Survey_Data_1$Penalties)) + geom_smooth(color="darkblue") + 
  ggtitle("Distribution between Age and penalties")  + labs(x = "Age" , y = "Penalties", color = "Penalties distribution") +  theme (legend.position="bottom")
g_age_overall <- ggplot(Survey_Data_1, aes(Survey_Data_1$Age, Survey_Data_1$Stamina))
g_age_overall + 
  geom_point(aes(color=Survey_Data_1$Stamina)) + geom_smooth(color="darkblue") + 
  ggtitle("Distribution between Age and Stamina") + labs(x = "Age" , y = "Stamina", color = "Stamina distribution") +  theme (legend.position="bottom")
g_age_overall <- ggplot(Survey_Data_1, aes(Survey_Data_1$Age, Survey_Data_1$Strength))
g_age_overall + 
  geom_point(aes(color=Survey_Data_1$Strength)) + geom_smooth(color="darkblue") + 
  ggtitle("Distribution between Age and Strength")  + labs(x = "Age" , y = "Strength", color = "Strength distribution") +  theme (legend.position="bottom")
ggplot(Survey_Data_1, aes(x = Survey_Data_1$Overall.rating, , fill = Survey_Data_1$Work.Rate)) +
  geom_bar()
## Statistical Analysis
## One Sample T test
set.seed(0)
Spain_1 <- filter(Survey_Data_1, Survey_Data_1$Nationality == "Spain")
Spain_2 <- Spain_1[sample(nrow(Spain_1),1071),]
summary(Spain_2)
# QQ Plot
qqnorm(Survey_Data_1$Age)
qqnorm(Spain_2$Age)
order_stats <- order(Spain_2$Age)
quantile(order_stats)
quantile(order_stats, seq(.01, .99, .1))
X_h <- mean(Spain_2$Age)
X_h
mu_0 <- 30
s <- sd(Spain_2$Age)
s
n <- length(Spain_2$Age)
n
set.seed(0)
t <- (X_h - mu_0)/(s/sqrt(n))
t
set.seed(0)
two_sided_t_pval <- pt(q = t, df = n-1, lower.tail = TRUE)*2
two_sided_t_pval
plot(seq(-40, 40, .01), dt(seq(-40, 40, .01), n-1), type="l")
# add the lines for my test statistic
abline(v=c(t, -t))
text(t,.025,"t=32.85824",srt=0.2,pos=4)
text(-t,.025,"t=-32.85824",srt=0.2,pos=2)
# lower bound
X_h+(qt(0.025, n-1)*(s/sqrt(n)))
# upper bound
X_h+(qt(0.975, n-1)*(s/sqrt(n))) 

# Bootstrap one sample T test

set.seed(0)
# This data is pretty skewed so even though n is large, I'm going to do a lot of simulations
num_sims <- 1000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x = Spain_2$Age,
                            size = n,
                            replace = TRUE))
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Mean', xlab = 'Average Age of Spanish Player', ylab = 'Density')
# estimate a normal curve over it - this looks pretty good!
lines(x = seq(24, 26, .001), dnorm(seq(24, 26, .001), mean = X_h, sd = s/sqrt(n)))
set.seed(0)
# Shift the sample so that the null hypothesis is true
time_given_H0_true <- Spain_2$Age - mean(Spain_2$Age) + mu_0
# This data is pretty skewed so even though n is large, I'm going to do a lot of simulations
num_sims <- 1000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results_given_H0_true[i] <- mean(sample(x = time_given_H0_true,
                                          size = n,
                                          replace = TRUE))
}
results_given_H0_true
# Finally plot the results
hist(results_given_H0_true, freq = FALSE, main='Sampling Distribution of the Sample Mean, Given Null Hypothesis is True', xlab = 'Average Age of Spanish Player', ylab = 'Density')
# add line to show values more extreme on upper end
abline(v=X_h, col = "red")
# add line to show values more extreme on lower end
low_end_extreme <- mean(results_given_H0_true)+(mean(results_given_H0_true)-X_h)
abline(v=low_end_extreme, col="red")
set.seed(0)
# counts of values more extreme than the test statistic in our original sample, given H0 is true
# two sided given the alternate hypothesis
count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= low_end_extreme)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true >= X_h)
bootstrap_pvalue <- (count_of_more_extreme_lower_tail - count_of_more_extreme_upper_tail)/1000
bootstrap_pvalue
# two sided t p-value
two_sided_t_pval
# need the standard error which is the standard deviation of the results
bootstrap_SE_X_bar <- sd(results)
# an estimate is to use the formula statistic +/- 2*SE
c(X_h - 2*bootstrap_SE_X_bar, X_h + 2*bootstrap_SE_X_bar)
# compare to our t-methods
c(X_h+(qt(0.025, n-1)*(s/sqrt(n))), X_h+(qt(0.975, n-1)*(s/sqrt(n))))

# One Proportion Test 

set.seed(0)
summary(Survey_Data_1$Preferred.Foot)
Survey_Data_2 <- Survey_Data_1[sample(nrow(Survey_Data_1),100),]
summary(Survey_Data_2$Preferred.Foot)
n <- 18159
x <- 4211
p_hat <- 4211/18159
p_hat
z <- (p_hat - .20) / sqrt((.20*(1-.20)) / n)
z
binom.test(x=4211, n = 18159, p=(.20), alternative="greater")
pnorm(z, lower.tail = FALSE)
cat("exact binomial test")
binom.test(x=4211, n = 18159, p=(.20), alternative="greater")$conf.int
cat("normal approx")
c(.23 - (1.64)*sqrt(((.23)*(1-.23))/n), 1)
## Bootstrap One sample Proportion
Left_Football <- Survey_Data_1$Preferred.Foot
Left_Football
levels(Left_Football) <- c(1,0)
Left_Football
table(Left_Football)
# This data is pretty skewed so even though n is large, I'm going to do a lot of simulations
set.seed(0)
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(as.numeric(sample(x = Left_Football ,
                                       size = n,
                                       replace = TRUE))-1)
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Proportion', xlab = 'Left foot Player ', ylab = 'Density')
# estimate a normal curve over it - this looks pretty good!
lines(x = seq(0.10, 1.0000000, .0001), dnorm(seq(0.10, 1.0000000, .0001), mean = mean(results), sd = sd
                                             (results)))

cat("Bootstrap Confidence Interval")
c(quantile(results, c(.05, 1)))
cat("exact binomial test")
binom.test(x=4211, n = 18159, p=(.20), alternative="greater")$conf.int
cat("normal approx")
c(p_hat - (1.64)*sqrt(((p_hat)*(1-p_hat))/n), 1)
set.seed(0)
# Under the assumption that the null hypothesis is true,
Left_Football_new <- rep(c(1, 0), c(0.20*n, (1-0.20)*n))
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  results[i] <- mean(sample(x = Left_Football_new,
                            size = n,
                            replace = TRUE))
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Proportion under H
_0:p=0.5', xlab = 'Proportion of left foot football player', ylab = 'Density')
# estimate a normal curve over it - this looks pretty good!
lines(x = seq(.10, .30, .001), dnorm(seq(.10, .30, .001), mean = mean(results), sd = sd
                                     (results)))
abline(v=p_hat, col="red")
count_of_more_extreme_upper_tail <- sum(results >= p_hat)
bootstrap_pvalue <- count_of_more_extreme_upper_tail/num_sims
cat("Bootstrap p-value")
bootstrap_pvalue
cat("Exact Binomial p-value")
binom.test(x = 4211, n = 18159, p = 0.20, alternative = "greater")$p.value
cat("Normal Approximation p-value")
pnorm(z, lower.tail = FALSE)

## Two Sample test for difference in mean

set.seed(0)
England_3 <- filter(Survey_Data_1, Survey_Data_1$Nationality == "England")
England_4 <- England_3[sample(nrow(England_3),300),]

Spain <- filter(Survey_Data_1, Survey_Data_1$Nationality == "Spain")
Spain <- Spain[sample(nrow(Spain),300),]

sample_data <- rbind(Spain,England_4)
# QQ Plot
qqnorm(Survey_Data$Stamina)
qqnorm(England_4$Stamina)
qqnorm(Spain$Stamina)
# Sample Mean
X_bar_e <- mean(England_4$Stamina)
X_bar_s <- mean(Spain$Stamina)
X_bar_e
X_bar_s
# Sample Variance
s_e <- sd(England_4$Stamina)**2
s_s <- sd(Spain$Stamina)**2
s_e
s_s
# Sample Size
n_e <- length(England_4$Stamina)
n_s <- length(Spain$Stamina)
n_e
n_s
#Null Hypothises
mu <- 0
# T Test
t <- (X_bar_s - X_bar_e - mu)/sqrt((s_s/n_s) + (s_e/n_e))
t
# p-value for two sided upper
two_sided_diff <- pt(q=t, df = min(n_s-1, n_e-1), lower.tail = FALSE) * 2
two_sided_diff
Alpha <- 0.05
Confidence_Interval <- 0.95
# Lower Bound
L_bound <- (X_bar_s - X_bar_e) + (qt(0.025, min(n_s, n_e)-1)* sqrt((s_s/n_s) +(s_e/n_e)))
L_bound
# Upper Bound
U_bound <- (X_bar_s - X_bar_e) + (qt(0.975, min(n_s, n_e)-1)* sqrt((s_s/n_s) +(s_e/n_e)))
U_bound
# R built in t-test function
t.test(Spain$Stamina, England_4$Stamina)
# Histogram of the sampling distribution
mu <- mean(sample_data$Stamina)
sd <- sd(sample_data$Stamina)
h <- hist(sample_data$Stamina, xlim = c(0,100), xlab = 'Average Stamina', main = 'Histogram of sampling distribution')
lb <- mu - 1.96*sd
ub <- mu + 1.96*sd
abline(v = c(mu, lb, ub), lty = 2)
x_axis <- seq(min(sample_data$Stamina),max(sample_data$Stamina),length=600)
y_axis <- dnorm(x_axis, mu, sd)*length(x_axis)
lines(x_axis, y_axis, col = "blue")
# T- Test distribution graph
n <- min(n_e, n_s)
X <- seq(-4, 4, .01)
Y <- dt(X, n-1)
plot(X, Y, type = 'l')
abline(v = c(t, -t),  col = "blue")
# Confidence Interval graph
plot(X, Y, type = 'l')
abline(v = qnorm(0.975), col = "Green")
abline(v = qnorm(0.025), col = "Green")
abline(v = 0, col = "black")
# Difference between normal distribution & T distribution
plot(X,Y,type = 'l')
lines(X,dnorm(X), col = 'yellow')

## Bootstrap method
set.seed(0)
num_sims <- 10000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  mean_Spain <- mean(sample(x = Spain$Stamina,size = 300,
                            replace = TRUE ))
  mean_England_4 <- mean(sample(x = England_4$Stamina, size = 300,
                                replace = TRUE))
  results[i] <- mean_Spain - mean_England_4
}
# Finally plot the results
hist(results, freq = FALSE, main='Sampling Distribution of the Sample Mean', xlab = 'Average Difference Leniency Scores', ylab = 'Density')

# Bootstrap one-sided CI
c(quantile(results, c(.975, .025)))

#compare to our t-methods
t.test(Spain$Stamina,England_4$Stamina)$conf.int
set.seed(0)
num_sims <- 10000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  # idea here is if there is no relationshipm we should be able to shuffle the groups
  #shuffled_groups <- transform(sample_data, Group=sample_data(Group))
  mean_England_4 <- mean(sample(x = England_4$Stamina,
                                replace = TRUE))
  mean_Spain <- mean(sample(x = Spain$Stamina,
                            replace = TRUE))
  results_given_H0_true[i] <- mean_Spain - mean_England_4 
}
results_given_H0_true
# Finally plot the results
hist(results_given_H0_true, freq = FALSE,
     main='Dist. of the Diff in Sample Means Under Null',
     xlab = 'Average Difference Stamina under null',
     ylab = 'Density')
diff_in_sample_means <- mean_Spain - mean_England_4 
abline(v=diff_in_sample_means, col = "blue")
abline(v=abs(diff_in_sample_means), col = "red")
diff_in_sample_means
set.seed(0)
# counts of values more extreme than the test statistic in our original sample, given H0is true
# two sided given the alternate hypothesis
count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= diff_in_sample_means)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true >= abs(diff_in_sample_means))
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims
count_of_more_extreme_lower_tail
count_of_more_extreme_upper_tail
cat("Bootstrap p-value")
bootstrap_pvalue
#t-test p-value
t.test(Spain$Stamina,England_4$Stamina)$p.value

##  two sample test for Proportion

Left_foot <- filter(Survey_Data_1, Survey_Data_1$Preferred.Foot == "Left")
summary(Left_foot$Real.Face)
Left_foot <- Left_foot[sample(nrow(Left_foot),4211),]

Right_foot <- filter(Survey_Data_1, Survey_Data_1$Preferred.Foot == "Right")
summary(Right_foot$Real.Face)
Right_foot <- Right_foot[sample(nrow(Right_foot),13948),]

sample_data_1 <- rbind(Left_foot,Right_foot )
# the parts of the test statistic
# sample props
p_hat_L <- 415/4211
p_hat_L
p_hat_R <- 1239/13948
p_hat_R
# null hypothesized population prop difference between the two groups
p_0 <- 0
# sample size
n_l <- 4211
n_r <- 13948
# sample variances
den_p_l <- (p_hat_L*(1-p_hat_L))/n_l
den_p_r <- (p_hat_R*(1-p_hat_R))/n_r
# z-test test statistic
z <- (p_hat_L - p_hat_R - p_0)/sqrt(den_p_l + den_p_r)
z
# two sided p-value
two_sided_diff_prop_pval <- pnorm(q = z, lower.tail = FALSE)*2
two_sided_diff_prop_pval
# lower bound
(p_hat_L - p_hat_R)+(qnorm(0.025)*sqrt(den_p_l + den_p_r))

# upper bound
(p_hat_L - p_hat_R)+(qnorm(0.975)*sqrt(den_p_l + den_p_r))

## Bootstrap
set.seed(0)
# Make the data
Left <- rep(c(1, 0), c(415, n_l - 415))
Right <- rep(c(1,0), c(1239, n_r -1239))
num_sims <- 1000
# A vector to store my results
results <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  prop_war <- mean(sample(Left,
                          size = n_l,
                          replace = TRUE))
  prop_opps <- mean(sample(x = Right,
                           size = n_r,
                           replace = TRUE))
  results[i] <- prop_war - prop_opps
}
# Finally plot the results
hist(results, freq = FALSE, main='Dist. of the Diff in Prop', xlab = 'Difference in Prop. of real faces', ylab = 'Density')
cat("Bootstrap")
c(quantile(results, c(.025, .975)))
cat("Normal Approximation")
c((p_hat_L - p_hat_R)+(qnorm(0.025)*sqrt(den_p_l + den_p_r)), (p_hat_L - p_hat_R)+(qnorm(0.975)*sqrt(den_p_l + den_p_r)))

# Make the data
df_combined <- data.frame("Real Face used" = c(Left, Right),
                          "team" = rep(c("Left Foot footballers", "Right Foot footballers"), c(n_l, n_r)))
# Sanity checks
summary(df_combined$team)
mean(df_combined$Real.Face.used[df_combined$team=="Left Foot footballers"]) == p_hat_L
mean(df_combined$Real.Face.used[df_combined$team=="Right Foot footballers"]) == p_hat_R
set.seed(0)
num_sims <- 1000
# A vector to store my results
results_given_H0_true <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  # idea here is if there is no relationshipm we should be able to shuffle the groups
  shuffled_groups <- transform(df_combined, team=sample(team))
  prop_warriors <- mean(shuffled_groups$Real.Face.used[shuffled_groups$team=="Left Foot footballers"
                                                       ])
  prop_opponents <- mean(shuffled_groups$Real.Face.used[shuffled_groups$team=="Right Foot footballers"])
  results_given_H0_true[i] <- prop_warriors - prop_opponents
}
# Finally plot the results
hist(results_given_H0_true, freq = FALSE,
     main='Dist. of the Diff in Sample Sample Props Under Null',
     xlab = 'Average Difference in Prop. of real face under null',
     ylab = 'Density')
diff_in_sample_props <- p_hat_L - p_hat_R
abline(v=diff_in_sample_props, col = "blue")
abline(v=-diff_in_sample_props, col = "red")
set.seed(0)
# counts of values more extreme than the test statistic in our original sample, given H0 is true
# two sided given the alternate hypothesis
count_of_more_extreme_lower_tail <- sum(results_given_H0_true <= -diff_in_sample_props)
count_of_more_extreme_upper_tail <- sum(results_given_H0_true >= diff_in_sample_props)
bootstrap_pvalue <- (count_of_more_extreme_lower_tail + count_of_more_extreme_upper_tail)/num_sims
cat("Bootstrap p-value")
bootstrap_pvalue
cat("Normal Approx p-value")
two_sided_diff_prop_pval

## Chi Square goodness of fit
summary(sample_data$Work.Rate)
head(sample_data$Work.Rate)
table(sample_data$Work.Rate)
prop.table(table(sample_data$Work.Rate))
sum(((table(sample_data$Work.Rate) - 12)^2)/12)
pchisq(9381.167, df = 9-1, lower.tail = FALSE)
# Create our data under the assumption that H_0 is true
solutions_under_H_0 <- rep(c("HH", "HL", "HM", "LH", "LL", "LM", "MH", "ML", "MM"), 12)
# Sanity Check
table(solutions_under_H_0)
set.seed(0)
num_sims <- 10000
# A vector to store my results
chisq_stats_under_H0 <- rep(NA, num_sims)
# A loop for completing the simulation
for(i in 1:num_sims){
  new_samp <- sample(solutions_under_H_0, 600, replace = T)
  chisq_stats_under_H0[i] <- sum(((table(new_samp) - 12)^2)/12)
}

# What do you notice about the distribution of this statistic?

hist(chisq_stats_under_H0, freq = FALSE,
     main='Dist. of the Chi-Square Statistic Under Null',
     xlab = 'Chi-Square Stat under Null',
     ylab = 'Density')
abline(v=sum(((table(sample_data$Work.Rate) - 12)^2)/12), col="red")
sum(chisq_stats_under_H0 >= sum(((table(sample_data$Work.Rate)-12)^2)/12))/num_sims

