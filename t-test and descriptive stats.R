# This is an analysis of a hypothetical scenario where two populations of 
# mice are compare. The two populations are not linked and consists of treatment and control.
# One group is given a different diet and results are compared.

# First step: generate two populations.
set.seed(123)
control_pop <- rnorm(1000, 25, 5) # rnorm(n, mean, sd)
set.seed(123)
treatment_pop <- rnorm(1000, 22, 5)

# Simulate an experiment where samples are drawn from the populations
random_sample <- function(seed, size, pop) {
  set.seed(seed)
  return(sample(pop, size))
}

# Call random_sample function 100 times using different seeds
# And each time taking a sample of size 25
seeds <- 1:100
control_sample <- sapply(seeds, random_sample, 25, control_pop)
treatment_sample <- sapply(seeds, random_sample, 25, treatment_pop)

means_control <- rowMeans(control_sample)
means_treatment <- rowMeans(treatment_sample)

# Visual comparisons of the control and treatment sample means
stripchart(list(means_control, means_treatment), pch=1, col=c("blue", "red"), method="jitter",
           xlim=c(0, 3), vertical=TRUE,
           group.names=c("Control", "Treatment"),
           main="Sample means",
           ylab="Mouse weight (grams)")

# More graphical analysis using histogram to see 
# whether the data is normally distributed
hist(means_control, xlab="Weight (grams)",
     main="Distribution of control mean samples")

# Create another stripchart to compare individual weights 
# within sample 1 of control & treatment
stripchart(list(control_sample[, 1], treatment_sample[, 1]),
           pch=1, xlim=c(0, 3), vertical=TRUE,
           group.names = c("Control", "Treatment"),
           main="Weights of mice",
           ylab="Mouse weight (grams)")
segments(0.6, mean(control_sample[, 1]), 1.4,
         mean(control_sample[, 1]), col="blue")
segments(1.6, mean(treatment_sample[, 1]), 2.4,
         mean(treatment_sample[, 1]), col="red")

# Define function to compute confidence intervals
# Critical value = the value on the x-axis outside of whos area gives 0.05
conf_interval <- function(sample, alpha){
  critical_value <- qt(1 - (alpha/2), 24) # alpha/2 (two tails), 24 degrees of freedom
  standard_error <- sd(sample) / sqrt(25)
  lower <- mean(sample) - critical_value * standard_error
  higher <- mean(sample) + critical_value * standard_error
  return(c(lower, higher))
    }

# Compute the intervals
conf_interval(control_sample[, 1], 0.05) # (sample, alpha(p-value))
conf_interval(treatment_sample[, 1], 0.05)

# Plot of the conf_intervals
conf_plot <- function(i){
  interval <- conf_interval(control_sample[, i], 0.05)
  covered <- mean(control_pop) <= interval[2] && mean(control_pop >= interval[1])
  color <- ifelse(covered, "green", "blue")
  lines(interval, c(i, i), col=color)
}

plot(mean(control_pop) + c(-5, 5), c(1, 1), type="n",
     xlab="Weight", ylab="Interval", ylim=c(1, 100),
     main="Control sample with 95% CI")
abline(v=mean(control_pop))
for(sample in 1:100)
  conf_plot(sample)

# Make sure the data is normally distributed by checking one sample
# using qqplot and histogram
par(mfrow=c(2, 2))
hist(control_sample[, 1], main="Distribution of control sample",
xlab = "Weight (grams)")
qqnorm(control_sample[, 1], main="QQ plot of control sample")
qqline(treatment_sample[, 1], main="Distribution of treatment sample",
xlab = "Weight (grams)")
qqnorm(treatment_sample[, 1], main="QQ plot of treatment sample")
qqline(treatment_sample[, 1])

# The QQ plot show that the samples are reasonably normally distributed
# Hence, conduct a t-test
# Tobs stores the critical values on the distribution function
tobs <- (mean(control_sample[, 1]) - mean(treatment_sample[, 1])) / sqrt(var(control_sample[, 1]) / 25 +
	var(treatment_sample[, 1]) / 25)
# acquire p_value from tobs
p_value <- 1 - pt(tobs, 48)
# Conduct t-test comparing the two sample means
ttest_result <- t.test(control_sample[, 1], treatment_sample[, 1])

# Conclusion: the p-value for one tail is 3.4% and thus 6.9% using two tails. Not significant: type II error.
# Note: this is for a single sample.

# Acquire p_values for all 100 samples and plot them as a histogram
"" ttest_func <- function(i) {t.test(control_sample[, i], treatment_sample[, i]$p.value}

all_values <- sapply(1:100, )
par(mfrow=c(1, 1))
hist(all_values, main="P-values from all 100 samples",
	xlab="P-value")
sum(all_values < 0.05) "" # Check in studio

# Power calculation for sample 1
install.packages("pwr")
library(pwr)
sample_size <- 25
effect_size <- (mean(control_sample[, 1]) - mean(treatment_sample[, 1])) / sd(control_sample[, 1])
alpha <- 0.05
power <- pwr.t.test(sample_size, effect_size, alpha) # Power is 45%, quite low.

# Calculate needed sample size for power 80%
power <- 0.80
sample_size <- pwr.t.test(NULL, effect_size, alpha, power)





