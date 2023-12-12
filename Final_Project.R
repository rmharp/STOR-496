## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------
if(!require(MASS)) {install.packages("MASS", repos = "http://cran.us.r-project.org"); library(MASS)}
if(!require(dplyr)) {install.packages("dplyr", repos = "http://cran.us.r-project.org"); library(dplyr)}
if(!require(readr)) {install.packages("readr", repos = "http://cran.us.r-project.org"); library(readr)}
if(!require(readxl)) {install.packages("readxl", repos = "http://cran.us.r-project.org"); library(readxl)}
if(!require(survival)) {install.packages("survival", repos = "http://cran.us.r-project.org"); library(survival)}
if(!require(tidyr)) {install.packages("tidyr", repos = "http://cran.us.r-project.org"); library(tidyr)}
if(!require(knitr)) {install.packages("knitr", repos = "http://cran.us.r-project.org"); library(knitr)}
if(!require(here)) {install.packages("here", repos = "http://cran.us.r-project.org"); library(here)}
if(!require(ggplot2)) {install.packages("ggplot2", repos = "http://cran.us.r-project.org"); library(ggplot2)}
setwd(here())


## --------------------------------------------------------------------------------------------------------------------------------------------------
#Replace with your filename
filename <- "RH DT dataset"
#Place the data into a folder titled data within the same wd
data <- read_excel(paste0("./data/", filename, ".xlsx"))
#Remove NA rows
data <- data[-c(130:139),]
# Remove 4 rows with NA for Physical Distress Tolerance Tasks
# Paper said there should only be three (may not need to remove altogether)
datamod <- data[-c(23,28,37,54),]
#The number of early dropouts was stated to be 20 in the paper
ed <- sum(datamod["drop30d"])
cat("Early Dropouts:", ed, "\n")
datamoded <- datamod[datamod$drop30d == 1, ]
#The number of completers was stated to be 102 in the paper
complet <- sum(count(datamod["drop30d"])) - ed
cat("Completers:", complet, "\n")
datamodcompleter <- datamod[datamod$drop30d == 0, ]


## --------------------------------------------------------------------------------------------------------------------------------------------------
#Average time on the PASAT was stated to be 208.7 s in the paper with a SD of 165.2
##Dysphoria at different time points was not included in the data so not needed
PTTmean <- round(mean(datamod$`PASAT Total Time`), 2)
PTTsd <- round(sd(datamod$`PASAT Total Time`), 2)
cat("PASAT Average: ", PTTmean, ", SD: ", PTTsd, "\n", sep = "")
#A paired t test is used in the paper to evaluate dysphoria during the PASAT
TTresult <- t.test(datamoded$`PASAT Total Time`, datamodcompleter$`PASAT Total Time`)
#Why this degrees of freedom
longText <- paste("A Welch Two Sample t-test was conducted to compare the PASAT Total Time between individuals who dropped out before 30 days of treatment and those who completed the treatment. The test did not find a statistically significant difference in the average PASAT Total Time between the dropout group (mean = ", round(as.numeric(TTresult$estimate["mean of x"]), 2)," seconds) and the completer group (mean = ", round(as.numeric(TTresult$estimate["mean of y"]), 2), " seconds), t(", round(as.numeric(TTresult$parameter), 2), ") = ", round(as.numeric(TTresult$statistic), 4), ", p = ",  round(as.numeric(TTresult$p.value), 4), ". The 95% confidence interval for the difference in means ranged from ", round(as.numeric(TTresult$conf.int[1]), 2)," to ", round(as.numeric(TTresult$conf.int[2]), 2)," seconds. The Welch t-Test does not assume equal variance s and uses a different formula for calculating the degrees of freedom, which is adjusted based on the sample sizes and variances of each group. This leads to a more robust method and can often result in a degrees of freedom value which isn't an integer. \n", sep = "")
wrappedText <- strwrap(longText, width = 80)
cat(wrappedText, sep = "\n")


## --------------------------------------------------------------------------------------------------------------------------------------------------
#Average time on the MTPT was stated to be 197.1 s in the paper with a SD of 95.9
MTPTmean <- round(mean(datamod$`Mirror Tracing Total Time`), 2)
MTPTsd <- round(sd(datamod$`Mirror Tracing Total Time`), 2)
cat("MTPT Average: ", MTPTmean, ", SD: ", MTPTsd, "\n", sep = "")
#In the paper, r(111) = 0.38, p < .001 for correlation b/w PASAT and MTPT
#Calculate the correlation coefficient with degrees of freedom (dof) of n - 2 since we are estimating the mean of two samples.
cor <- cor.test(datamod$`PASAT Total Time`, datamod$`Mirror Tracing Total Time`)
cat("r(", nrow(datamod) - 2,") = ", round(as.numeric(cor$estimate), 2), ", p < 0.001", "\n", sep = "")
cat("The 95% confidence interval of the correlation coefficient is from ", as.numeric(round(cor$conf.int[1], 2)), " to ", as.numeric(round(cor$conf.int[2], 2)), sep = "")


## --------------------------------------------------------------------------------------------------------------------------------------------------
#Average time on the BH was stated to be 30.12 s in the paper with a SD of 13.8
BHmean <- round(mean(datamod$`Breath Holding Duration`), 2)
BHsd <- round(sd(datamod$`Breath Holding Duration`), 2)
cat("BH Average: ", BHmean, ", SD: ", BHsd, "\n", sep = "")

#Average time on the CPT was stated to be 99.97 s in the paper with a SD of 104.6
CPTmean <- round(mean(datamod$`Cold Pressor Duration`), 2)
CPTsd <- round(sd(datamod$`Cold Pressor Duration`), 2)
cat("CPT Average: ", CPTmean, ", SD: ", CPTsd, "\n", sep = "")
#Calculate the correlation coefficient with degrees of freedom (dof) of n - 2 since we are estimating the mean of two samples.
cor <- cor.test(datamod$`Breath Holding Duration`, datamod$`Cold Pressor Duration`)
cat("r(", nrow(datamod) - 2,") = ", round(as.numeric(cor$estimate), 2), ", p < 0.01", "\n", sep = "")
cat("The 95% confidence interval of the correlation coefficient is from ", as.numeric(round(cor$conf.int[1], 2)), " to ", as.numeric(round(cor$conf.int[2], 2)), sep = "")


## --------------------------------------------------------------------------------------------------------------------------------------------------
# The paper saw X²(1, N = 122) = 3.42, p = 0.06 for alcohol dependence
datamod1 <- datamod[!is.na(datamod$`Alcohol Diagnosis`), ]
contingTable <- table(datamod1$drop30d, datamod1$`Alcohol Diagnosis`)
chiResult <- chisq.test(contingTable)
cat("X²(", chiResult$parameter, ", N = ", sum(chiResult$observed), ") = ", format(chiResult$statistic, digits = 2), ", p = ", round(chiResult$p.value, digits = 2), "\n", sep = "")


## --------------------------------------------------------------------------------------------------------------------------------------------------
PTTmeanED <- round(mean(datamoded$`PASAT Total Time`), 2)
PTTsdED <- round(sd(datamoded$`PASAT Total Time`), 2)
PTTsemED <- PTTsdED / sqrt(nrow(datamoded))
PTTmeanCompleter <- round(mean(datamodcompleter$`PASAT Total Time`), 2)
PTTsdCompleter <- round(sd(datamodcompleter$`PASAT Total Time`), 2)
PTTsemCompleter <- PTTsdCompleter / sqrt(nrow(datamodcompleter))
MTPTmeanED <- round(mean(datamoded$`Mirror Tracing Total Time`), 2)
MTPTsdED <- round(sd(datamoded$`Mirror Tracing Total Time`), 2)
MTPTsemED <- MTPTsdED / sqrt(nrow(datamoded))
MTPTmeanCompleter <- round(mean(datamodcompleter$`Mirror Tracing Total Time`), 2)
MTPTsdCompleter <- round(sd(datamodcompleter$`Mirror Tracing Total Time`), 2)
MTPTsemCompleter <- MTPTsdCompleter / sqrt(nrow(datamodcompleter))
BHmeanED <- round(mean(datamoded$`Breath Holding Duration`), 2)
BHsdED <- round(sd(datamoded$`Breath Holding Duration`), 2)
BHsemED <- MTPTsdED / sqrt(nrow(datamoded))
BHmeanCompleter <- round(mean(datamodcompleter$`Breath Holding Duration`), 2)
BHsdCompleter <- round(sd(datamodcompleter$`Breath Holding Duration`), 2)
BHsemCompleter <- BHsdCompleter / sqrt(nrow(datamodcompleter))
CPTmeanED <- round(mean(datamoded$`Cold Pressor Duration`), 2)
CPTsdED <- round(sd(datamoded$`Cold Pressor Duration`), 2)
CPTsemED <- MTPTsdED / sqrt(nrow(datamoded))
CPTmeanCompleter <- round(mean(datamodcompleter$`Cold Pressor Duration`), 2)
CPTsdCompleter <- round(sd(datamodcompleter$`Cold Pressor Duration`), 2)
CPTsemCompleter <- MTPTsdCompleter / sqrt(nrow(datamoded))
barChartData <- data.frame(
    Measure = rep(c("PASAT", "MTPT", "BH", "CPT"), each = 2),
    Group = rep(c("ED", "Completer"), times = 4),
    Mean = c(PTTmeanED, PTTmeanCompleter, MTPTmeanED, MTPTmeanCompleter, BHmeanED, BHmeanCompleter, CPTmeanED, CPTmeanCompleter),
    SD = c(PTTsdED, PTTsdCompleter, MTPTsdED, MTPTsdCompleter, BHsdED, BHsdCompleter, CPTsdED, CPTsdCompleter),
    SEM = c(PTTsemED, PTTsemCompleter, MTPTsemED, MTPTsemCompleter, BHsemED, BHsemCompleter, CPTsemED, CPTsemCompleter))
dataSubset1 <- barChartData[barChartData$Measure %in% c("PASAT", "MTPT"), ]
dataSubset2 <- barChartData[barChartData$Measure %in% c("BH", "CPT"), ]
psychPlot <- ggplot(dataSubset1, aes(x = Measure, y = Mean, fill = Group)) +
    geom_bar(stat = "identity", position = position_dodge(), color = "black") +
    geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), position = position_dodge(.9), width = 0.25) +
    theme_minimal() +
    labs(title = "Comparison of Group Means", x = "", y = "Mean Persistence (Seconds)") +
    scale_fill_manual(values = c("ED" = "white", "Completer" = "gray"))
physiPlot <- ggplot(dataSubset2, aes(x = Measure, y = Mean, fill = Group)) +
    geom_bar(stat = "identity", position = position_dodge(), color = "black") +
    geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), position = position_dodge(.9), width = 0.25) +
    theme_minimal() +
    labs(title = "Comparison of Group Means", x = "", y = "Mean Persistence (Seconds)") +
    scale_fill_manual(values = c("ED" = "white", "Completer" = "gray"))
psychPlot
physiPlot


## --------------------------------------------------------------------------------------------------------------------------------------------------
#Let's fit the Cox-PH Model and use Breath Holding Duration, Cold Pressor Duration, PASAT Total Time,  and Mirror Tracing Total Time
#Twi step COX PH model
cox.mod <- coxph(Surv(`Days in Treatment`, drop30d) ~ `Breath Holding Duration`  + `Cold Pressor Duration` + `PASAT Total Time` + `Mirror Tracing Total Time`, data = datamod)
#and ask for the summary of the model
summary(cox.mod)
#Let's fit the Cox-PH Model and use PASAT Total Time and Mirror Tracing Total Time
cox.mod2 <- coxph(Surv(`Days in Treatment`, drop30d) ~ `PASAT Total Time` + `Mirror Tracing Total Time`, data = datamod)
#and ask for the summary of the model
summary(cox.mod2)
#Let's fit the Cox-PH Model and use Breath Holding Duration and Cold Pressor Duration
cox.mod3 <- coxph(Surv(`Days in Treatment`, drop30d) ~ `Breath Holding Duration`  + `Cold Pressor Duration`, data = datamod)
#and ask for the summary of the model
summary(cox.mod3)


## --------------------------------------------------------------------------------------------------------------------------------------------------
### CHECKING LINEARITY

# checking linearity (for the model that used NUM X's)
# using MARTINGALE residuals
plot(predict(cox.mod2), residuals(cox.mod2, type = "martingale"), xlab = "fitted values", ylab = "martingale residuals", main = "Residual Plot", las = 1)
# add a line at y = residual of 0
abline(h = 0)
# fit a smoother thru the points
lines(smooth.spline(predict(cox.mod2), residuals(cox.mod2, type = "martingale")), col = "red")

# and checking linearity using DEVIANCE residuals
plot(predict(cox.mod2), residuals(cox.mod2, type = "deviance"), xlab = "fitted values", ylab = "deviance residuals", main = "Residual Plot", las = 1)
# add a line at y = residual of 0
abline(h = 0)
# fit a smoother thru the points
lines(smooth.spline(predict(cox.mod2), residuals(cox.mod2, type = "deviance")), col = "red")

## --------------------------------------------------------------------------------------------------------------------------------------------------
### CHECKING PROPORTIONAL HAZARDS ASSUMPTION
## Do the groups seperately, not together
# Look more into Schoenfeld Test and describe in your report why this was done to test the Proportional Hazards assumption and why
# test for prop hazards using Schoenfeld test for PH
# This test has a H_0 (null hyp): HAZARDS are prop and an H_a(alt hyp): HAZARDS are NOT prop
# will return test for each X, and for overall model
cox.zph(cox.mod2)
# all of these variables have a p value higher than 5% so we have evidence that we're going to fail to reject the null hypothesis or that we can work with the hazards being proportional assumption
# tests IF coef for variable(X) changes over time...
# if it changes over time then we have non-prop hazards
# (HR changes over time)


## --------------------------------------------------------------------------------------------------------------------------------------------------
plot(cox.zph(cox.mod2)[1], xlab = "Transformed Time", ylab = "Scaled Schoenfeld Residuals", main = "Time-Varying Effect of PASAT Total Time")
plot(cox.zph(cox.mod2)[2], xlab = "Transformed Time", ylab = "Scaled Schoenfeld Residuals", main = "Time-Varying Effect of Mirror Tracing Total Time")

