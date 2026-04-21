requiredPackages = c("stargazer", "lmtest", "fBasics", "sandwich", 
                     "olsrr", "car", "sandwich", "MASS", "tseries",
                     "robust","corrplot", "foreign", "PerformanceAnalytics",
                     "haven", "ggplot2", "viridis", "vioplot", "devtools",
                     "ggcorrplot", "tidyverse", "dypr")
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) }

options(scipen=999)
Sys.setenv(LANG="EN")




DESCRIPTIVE STATISTICS

descriptive_stat <- model_data2 %>%
  summarise(across(where(is.numeric), list(
    N = ~sum(!is.na(.)),       
    Min = ~min(., na.rm = TRUE),  
    Median = ~median(., na.rm = TRUE),  
    Mean = ~mean(., na.rm = TRUE),  
    SD = ~sd(., na.rm = TRUE),  
    Max = ~max(., na.rm = TRUE)  
  ), .names = "{.col}_{.fn}")) %>%
  
  # Correctly reshape data: ensure variable names and statistics are separated
  pivot_longer(cols = everything(), 
               names_to = c("Variable", "Statistic"), 
               names_pattern = "(.*)_(.*)") %>%
  pivot_wider(names_from = Statistic, values_from = value)


descriptive_stat<- descriptive_stat[-c(1),]

library(kableExtra)

descriptive_stat %>%
  kbl(caption = "", 
      digits = 2, 
      align = "c", 
      row.names = FALSE) %>%  # Usunięcie numeracji wierszy
  kable_classic(full_width = F, html_font = "Cambria") 





CORRELATIONS

library(corrplot)

# Specify the variables you want to include
included_vars <- c("GDP_growth", "fertility", "GDPpc2015",  "inf_def","investment", "education", "gov_exp_reduced", 
                   "cc_total", "v2x_libdem", "tot2", "trade","life_exp")  

# Subset only the specified variables
subset_data <- model_data4[, included_vars]


# Ensure all variables are numeric
numeric_subset <- subset_data[, sapply(subset_data, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_subset, use = "complete.obs", method = "spearman")


testRes = cor.mtest(cor_matrix, conf.level = 0.95)
corrplot(cor_matrix, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE',col = COL2('BrBG'),tl.col = "black", diag=FALSE)










1ST MODELS, TEST    

test <- na.omit(model_data)

regression1 <- lm(GDP_growth ~ fertility + log(GDPpc) + gov_exp_red + inf_def + investment + life_exp + education + tot + trade + cc_total + landlocked , data = test)
summary(regression1)
resettest(regression1)
vif(regression1)

regression2 <- lm(GDP_growth ~ fertility + log(GDPpc) + gov_exp_red + inf_def + investment + life_exp + education + tot + trade + cc_basic + landlocked , data = test)
summary(regression2)

s1 <- stargazer(regression1, regression2, regression3, type="html", 
                out=".../reg_table_star.html") 

# scatterplot weight vs price
plot(price_euros~grams, data=laptops)

# compare two fits - linear and quadratic
fit_lin <- lm(price_euros~grams, data=laptops)

# polynomial can be done in a couple of ways:
fit_q1 = lm(price_euros~poly(grams,2, raw=T), data=laptops)
fit_q2 = lm(price_euros~grams + I(grams^2), data=laptops)
summary(fit_q1)
summary(fit_q2) # will be equal


coefficients(fit_q1)

plot(price_euros~grams, data=laptops, main="Regressions for price")
lines(laptops$grams, predict(fit_lin), col="red")
curve(predict(fit_q1,newdata=data.frame(grams=x)),
      add=T, col="darkgreen", lwd=2)
cols <- c("red", "darkgreen")
legend("topleft", legend=c("linear", "quadratic"), 
       col=cols, lty=1, lwd=1.4, bg="transparent", cex = 0.75)

# what if cubic?
fit_c <- lm(price_euros~poly(grams,3, raw=T), data=laptops)
summary(fit_c)

plot(price_euros~grams, data=laptops, main="Regressions for price")
lines(laptops$grams, predict(fit_lin), col="red")
curve(predict(fit_q1,newdata=data.frame(grams=x)),
      add=T, col="darkgreen", lwd=2)
curve(predict(fit_c,newdata=data.frame(grams=x)),
      add=T, col="blue", lwd=2)
cols <- c("red", "darkgreen", "blue")
legend("topleft", legend=c("linear", "quadratic", "cubic"), 
       col=cols, lty=1, lwd=1.4, bg="transparent", cex = 0.75)


--------------------------------------------------------------------------------------
  
  summary(model_data)
summary(model_data$GDP_growth)  
sum(is.na(model_data))  

# I. Graphical analysis

# Histogram 
hist(model_data$GDP_growth, col = "blue", xlab = "GDP_growth") # freq=T  

hist(model_data$GDP_growth, col = "blue", xlab = "GDP_growth", freq=F)
xfit <- seq(min(model_data$GDP_growth), max(model_data$GDP_growth), length=70)
yfit <- dnorm(xfit, mean=mean(model_data$GDP_growth), sd=sd(model_data$GDP_growth))
lines(xfit, yfit, col="red", lwd=2)  

#histogram density
ggplot(model_data, aes(x=GDP_growth)) + 
  geom_density(color="lightblue", fill="lightblue") +
  theme_classic() +
  labs(title="Density of GDP growth", x="", y="", fill="") +
  theme(plot.title = element_text(size=18, hjust = (0.5)),
        axis.text.x = element_text(size=8, vjust = 0.5)) + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(model_data$GDP_growth), 
                            sd = sd(model_data$GDP_growth)))

#scatterplot
plot(GDP_growth ~  cc_total, data = test, pch=19, col="blue", main="GDP growth vs compliance",
     xlab="cc-prop", ylab="GDP_growth")

plot(GDP_growth ~  cc_prop, data = test, pch=19, col="blue", main="GDP growth vs compliance",
     xlab="cc-prop", ylab="GDP_growth")

plot(GDPpc ~  cc_total, data = test, pch=19, col="blue", main="GDP growth vs compliance",
     xlab="cc-prop", ylab="GDP_growth")

plot(GDPpc ~  cc_prop, data = test, pch=19, col="blue", main="GDP growth vs compliance",
     xlab="cc-prop", ylab="GDP_growth")







SOME OTHER MODELS 


re1 <- lm(GDP_growth ~ fertility + log(GDPpc2015) + inf_def + investment + 
            education + tot2:trade + gov_exp_reduced + life_exp +
            log(cc_total+2) + v2x_libdem + tot2 + trade, data = model_data4)
summary(re1)

#education, life_exp , v2x_libdem  

re2 <- lm(GDP_growth ~ fertility + log(GDPpc2015) + inf_def + investment + 
            tot2:trade + gov_exp_reduced +
            log(cc_total+2) + tot2 + trade, data = model_data4)
summary(re2)

linearHypothesis(re2,c("fertility","inf_def ","log(cc_total + 2)","tot2","tot2:trade"))


re3 <- lm(GDP_growth ~ fertility + log(GDPpc2015) + investment + 
            tot2:trade + gov_exp_reduced +
            log(cc_total+2) + tot2 + trade, data = model_data4)
summary(re3)

linearHypothesis(re3,c("fertility","log(cc_total + 2)","tot2","tot2:trade"))

#BEST
re4 <- lm(GDP_growth ~ log(GDPpc2015) + investment + 
            tot2:trade + gov_exp_reduced +
            log(cc_total+2) + tot2 + trade, data = model_data4)
summary(re4)

linearHypothesis(re4,c("log(cc_total + 2)","tot2","tot2:trade"))



re5 <- lm(GDP_growth ~ log(GDPpc2015) + investment + 
            tot2:trade + gov_exp_reduced +
            tot2 + trade, data = model_data4)
summary(re5)

linearHypothesis(re5,c("tot2","tot2:trade"))

re6 <- lm(GDP_growth ~ log(GDPpc2015) + investment + 
            gov_exp_reduced +
            tot2 + trade, data = model_data4)
summary(re6)

robust_re6 <- vcovHC(re6, type = "HC3")

# Display the reression results with robust standard errors
robust_results <- coeftest(re6, vcov = robust_re6)
print(robust_results)


re7 <- lm(GDP_growth ~ log(GDPpc2015) + investment + 
            gov_exp_reduced +
            trade, data = model_data4)
summary(re7)


#1. LINAERITY
resettest(re4, power = 2:3, type = "fitted")


# 2. NORMALITY OF RESIDUALS
hist(re4$residuals, col = "blue", main = "Histogram of residuals", xlab = "Residuals", freq=F, ylim=c(0,0.5))
lines(density(re4$residuals), col="red", lwd=2)
# q-q plot 
plot(re4, which=2)

tseries::jarque.bera.test(re4$residuals) 
shapiro.test(re4$residuals)



# 3. Homoscedasticity

#  Residuals vs fitted 
plot(re4, which=1)

# or also which = 3
plot(re4, which=3)


bptest(re4, studentize=T)

# 6. Autocorrelation
# cross-sectional data, so rather there is not
# Durbin-Watson 
lmtest::dwtest(re4)









DETAILED TEST   

regression123 <- lm(GDP_growth ~ log(GDPpc) + inf_cpi + investment + 
                      trade + gov_exp + life_exp +
                      dj_right_property, data = model_data3)
summary(regression123)
vif(regression123)







REG WITH CC GROWTH


regr_gr1 <- lm(GDP_growth ~ log(GDPpc) + inf_def + investment + 
                 education + tot + trade + gov_exp + 
                 cc_total_growth + landlocked + v2x_libdem, data = model_data2)
summary(regr_gr1)
vif(regr_gr1)

plot(GDP_growth ~  cc_total_growth_yty, data = model_data2, pch=19, col="blue", main="GDP growth vs compliance",
     xlab="cc-prop", ylab="GDP_growth")

plot(GDP_growth ~  cc_prop_growth, data = model_data2, pch=19, col="blue", main="GDP growth vs compliance",
     xlab="cc-prop", ylab="GDP_growth")



PROBLEMS WITH DATA


## ANALYSIS

# Before saving some statistics to a file,
# we can save them inside a dataset

model_data4_subset<-model_data4[,c("Country_Name","Country_Code","GDP_growth","GDPpc2015","investment","tot2","trade","gov_exp_reduced","cc_total")]
model_data4_subset <- na.omit(model_data4_subset)

# residuals 
model_data4_subset$resids <- residuals(re4)

# leverage
# Leverage refers to the extent to which the coefficients 
# in the regression model would change if a particular observation 
# was removed from the dataset.
model_data4_subset$lev <- hatvalues(re4)

# standardized residuals
# Standardized residuals refer to the standardized difference 
# between a predicted value for an observation and the actual 
# value of the observation.
model_data4_subset$rstd <- rstandard(re4)

# Cook distance
model_data4_subset$cookd <- cooks.distance(re4)

# fitted values
model_data4_subset$yhat <- fitted(re4)

# For how many observations leverage is > 2k/n?
# length(re4$coefficients) -- # of parameters, k
# nrow(model_data4_subset) -- # obs., n
lev_threshold <- 2*(length(re4$coefficients)/nrow(model_data4_subset))
length(model_data4_subset$lev[model_data4_subset$lev > lev_threshold])
# 108 obs.


# For how many observations |stand.residuals| >2?
length(model_data4_subset$rstd[abs(model_data4_subset$rstd)>2])
# 53 obs.

# For how many observation Cook's distance is > 4/n?
cook <- 4/nrow(model_data4_subset)
length(model_data4_subset$cookd[model_data4_subset$cookd > cook])
#  observations
nontypical0 <- model_data4_subset[model_data4_subset$lev > lev_threshold & abs(model_data4_subset$rstd ) > 2 & model_data4_subset$cookd > cook, ]
view(nontypical0)

nontypical <- model_data4_subset[model_data4_subset$lev > lev_threshold & abs(model_data4_subset$rstd ) > 2 | model_data4_subset$lev > 0.2, ]
view(nontypical)

# Cook's distance plot for subsequent observations
# abline -- adds a threshold line
plot(re4, which=4, cook.level=(4/nrow(model_data4_subset)))
abline(h=4/nrow(model_data4_subset), lty=2, col= "red")

# or
ols_plot_cooksd_chart(re4)



# Leverage vs standardized residuals
plot(re4, which=5)
# If any point in this plot falls outside of Cook’s distance (the dashed lines),
# then it is considered to be an influential observation.

# We’re looking at how the spread of standardized residuals changes 
# as the leverage, or sensitivity of the fitted \hat{y}_i to a change in y_i, increases. 
# This can also be used to detect heteroskedasticity and non-linearity. 
# The spread of standardized residuals shouldn’t change as a function of leverage

### How to Handle Influential Observations:
# 1. Verify that the observation is not an error.
# 2. Attempt to fit another regression model (polynomial, logarithmic, ...)
# 3. Remove the influential observations.
### Sources: 
# https://boostedml.com/2019/03/linear-regression-plots-residuals-vs-leverage.html
# https://www.statology.org/residuals-vs-leverage-plot/

# or:
ols_plot_resid_lev(re4)



# This function creates a “bubble” plot of Studentized residuals 
# versus hat values, with the areas of the circles representing 
# the observations proportional to the value Cook's distance. 
# Vertical reference lines are drawn at twice and three times 
# the average hat value, horizontal reference lines at -2, 0, and 2 
# on the Studentized-residual scale.

# id.method="noteworthy" identifies a couple of suspicious observations
influencePlot(re4, id.method="noteworthy", 
              main="Leverage and residuals", 
              sub= "Circle size is proportional to Cook D value")

nontypical_numb <- model_data4[c(28,34,90,129), ]
View(nontypical_numb)
# leverage threshold = 0.01576182
# Cook's distance threshold = 0.003502627




ROBUST REGRESSIONS 



_o <- without outliers




re1_o <- lm(GDP_growth ~ fertility + log(GDPpc2015) + inf_def + investment + 
              education + tot2:trade + gov_exp_reduced + life_exp +
              log(cc_total+2) + v2x_libdem + tot2 + trade, data = model_data4_o)
summary(re1_o)
vif(re1_o)
#education, life_exp , v2x_libdem  

re2_o <- lm(GDP_growth ~ fertility + log(GDPpc2015) + inf_def + investment + 
              tot2:trade + gov_exp_reduced +
              log(cc_total+2) + tot2 + trade, data = model_data4_o)
summary(re2_o)

linearHypothesis(re2_o,c("fertility","inf_def ","log(cc_total + 2)","tot2","trade","tot2:trade"))


re3_o <- lm(GDP_growth ~ fertility + log(GDPpc2015) + investment + 
              tot2:trade + gov_exp_reduced +
              log(cc_total+2) + tot2 + trade, data = model_data4_o)
summary(re3_o)

linearHypothesis(re3_o,c("fertility","log(cc_total + 2)","tot2","trade","tot2:trade"))

#BEST
re4_o <- lm(GDP_growth ~ log(GDPpc2015) + investment + 
              tot2:trade + gov_exp_reduced +
              log(cc_total+2) + tot2 + trade, data = model_data4_o)
summary(re4_o)

linearHypothesis(re4_o,c("log(cc_total + 2)","tot2","trade","tot2:trade"))



re5_o <- lm(GDP_growth ~ log(GDPpc2015) + investment + 
              tot2:trade + gov_exp_reduced +
              tot2 + trade, data = model_data4_o)
summary(re5_o)

linearHypothesis(re5_o,c("tot2","trade","tot2:trade"))

re6_o <- lm(GDP_growth ~ log(GDPpc2015) + investment + 
              gov_exp_reduced +
              tot2 + trade, data = model_data4_o)
summary(re6_o)

linearHypothesis(re6_o,c("tot2","trade"))

re7_o <- lm(GDP_growth ~ log(GDPpc2015) + investment + 
              gov_exp_reduced +
              trade, data = model_data4_o)
summary(re7_o)



-----
  re8_o <- lm(GDP_growth ~ log(GDPpc2015) + investment + 
                gov_exp_reduced +
                log(cc_total+2) + trade, data = model_data4_o)
summary(re8_o)

--
  
  re9_o <- lm(GDP_growth ~ log(GDPpc2015) + investment + 
                gov_exp_reduced +
                log(cc_total+2), data = model_data4_o)
summary(re9_o)

----
  
  
  #1. LINAERITY
  resettest(re4_o, power = 2:3, type = "fitted")


# 2. NORMALITY OF RESIDUALS
hist(re4_o$residuals, col = "blue", main = "Histogram of residuals", xlab = "Residuals", freq=F, ylim=c(0,0.5))
lines(density(re4_o$residuals), col="red", lwd=2)
# q-q plot 
plot(re4_o, which=2)

tseries::jarque.bera.test(re4_o$residuals) 
shapiro.test(re4_o$residuals)



# 3. Homoscedasticity

#  Residuals vs fitted 
plot(re4_o, which=1)

# or also which = 3
plot(re4_o, which=3)


bptest(re4_o, studentize=T)

# 6. Autocorrelation
# cross-sectional data, so rather there is not
# Durbin-Watson 
lmtest::dwtest(re4_o)














REG WITH OTHER INSTITUTIONS


inst1 <- lm(GDP_growth ~ log(GDPpc2015) + investment + 
              tot2:trade + gov_exp_reduced +
              rule_of_law + tot2 + trade, data = model_data4_o)
summary(inst1)

inst2 <- lm(GDP_growth ~ log(GDPpc2015) + investment + 
              tot2:trade + gov_exp_reduced +
              ctrl_of_corr + tot2 + trade, data = model_data4_o)
summary(inst2)

inst3 <- lm(GDP_growth ~ log(GDPpc2015) + investment + 
              tot2:trade + gov_exp_reduced +
              gov_eff + tot2 + trade, data = model_data4_o)
summary(inst3)


inst4 <- lm(GDP_growth ~ log(GDPpc2015) + investment + 
              tot2:trade + gov_exp_reduced +
              pol_stab + tot2 + trade, data = model_data4_o)
summary(inst4)

inst5 <- lm(GDP_growth ~ log(GDPpc2015) + investment + 
              tot2:trade + gov_exp_reduced +
              reg_q + tot2 + trade, data = model_data4_o)
summary(inst5)


inst6 <- lm(GDP_growth ~ log(GDPpc2015) + investment + 
              tot2:trade + gov_exp_reduced +
              voice_and_acc + tot2 + trade, data = model_data4_o)
summary(inst6)



correlationTest(model_data4$GDP_growth, model_data4$cc_total, method = "spearman", 
                title = NULL, description = NULL)

plot(GDP_growth ~ cc_total, data = model_data4, pch=19, col="blue",
     xlab="cc_total", ylab="GDP_growth")

plot(rule_of_law ~ cc_total, data = model_data4, pch=19, col="blue",
     xlab="cc_total", ylab="rule_of_law")









DIAGNOSTICS 

# Model (0)
resettest(re2_o, power = 2:3, type = "fitted")
shapiro.test(re2_o$residuals)
bptest(re2_o, studentize=T)
lmtest::dwtest(re2_o)


# Model (1)
resettest(re4_o, power = 2:3, type = "fitted")
shapiro.test(re4_o$residuals)
bptest(re4_o, studentize=T)
lmtest::dwtest(re4_o)

# Load required packages
library(lmtest)
library(car)
library(kableExtra)
library(dplyr)

# Run tests and store results in a data frame
results <- data.frame(
  Model = c("1", "2","2lv","2prop","i1","i2","i3","i4","i5","i6"),
  RESET = c(
    resettest(re2_o, power = 2:3, type = "fitted")$p.value,
    resettest(re4_o, power = 2:3, type = "fitted")$p.value,
    resettest(re4_olv, power = 2:3, type = "fitted")$p.value,
    resettest(re4_oprop, power = 2:3, type = "fitted")$p.value,
    resettest(inst1, power = 2:3, type = "fitted")$p.value,
    resettest(inst2, power = 2:3, type = "fitted")$p.value,
    resettest(inst3, power = 2:3, type = "fitted")$p.value,
    resettest(inst4, power = 2:3, type = "fitted")$p.value,
    resettest(inst5, power = 2:3, type = "fitted")$p.value,
    resettest(inst6, power = 2:3, type = "fitted")$p.value
  ),
  Shapiro = c(
    shapiro.test(re2_o$residuals)$p.value,
    shapiro.test(re4_o$residuals)$p.value,
    shapiro.test(re4_olv$residuals)$p.value,
    shapiro.test(re4_oprop$residuals)$p.value,
    shapiro.test(inst1$residuals)$p.value,
    shapiro.test(inst2$residuals)$p.value,
    shapiro.test(inst3$residuals)$p.value,
    shapiro.test(inst4$residuals)$p.value,
    shapiro.test(inst5$residuals)$p.value,
    shapiro.test(inst6$residuals)$p.value
  ),
  `Breusch-Pagan` = c(
    bptest(re2_o, studentize = TRUE)$p.value,
    bptest(re4_o, studentize = TRUE)$p.value,
    bptest(re4_olv, studentize = TRUE)$p.value,
    bptest(re4_oprop, studentize = TRUE)$p.value,
    bptest(inst1, studentize = TRUE)$p.value,
    bptest(inst2, studentize = TRUE)$p.value,
    bptest(inst3, studentize = TRUE)$p.value,
    bptest(inst4, studentize = TRUE)$p.value,
    bptest(inst5, studentize = TRUE)$p.value,
    bptest(inst6, studentize = TRUE)$p.value
  ),
  `Durbin-Watson` = c(
    dwtest(re2_o)$p.value,
    dwtest(re4_o)$p.value,
    dwtest(re4_olv)$p.value,
    dwtest(re4_oprop)$p.value,
    dwtest(inst1)$p.value,
    dwtest(inst2)$p.value,
    dwtest(inst3)$p.value,
    dwtest(inst4)$p.value,
    dwtest(inst5)$p.value,
    dwtest(inst6)$p.value
  )
)

# Format p-values to 4 decimal places
results[, -1] <- round(results[, -1], 4)

# Rename columns for display
colnames(results) <- c("Model", "RESET", "Shapiro-Wilk", "Breusch-Pagan", "Durbin-Watson")

# Create a formatted and centered table with kableExtra
results %>%
  kbl(caption = "Regression Diagnostic Tests", digits = 4, align = "c") %>%  # Aligns columns to center
  add_header_above(c(" " = 1, "P-value" = 4)) %>%  # Adds "P-value" header above tests
  kable_classic(full_width = FALSE, html_font = "Cambria", position = "center")  # Centers the table











TABLES REGRESSIONS


------------------------------------------------------------------
  robust_se2 <- sqrt(diag(vcovHC(re2_o, type = "HC3")))

# Compute robust t-statistics for reg6
robust_t2 <- coef(re2_o) / robust_se2


robust_se4 <- sqrt(diag(vcovHC(re4_o, type = "HC3")))

# Compute robust t-statistics for reg6
robust_t4 <- coef(re4_o) / robust_se4

# Pass robust standard errors and t-statistics to stargazer
stargazer(re2_o, re4_o, type = "html",
          se = list(robust_se2, robust_se4),  
          t = list(robust_t2, robust_t4),    
          out = "reg_table1.html")


-----------------------------------------------------------------
  
  robust_sei1 <- sqrt(diag(vcovHC(inst1, type = "HC3")))

# Compute robust t-statistics for reg6
robust_ti1 <- coef(inst1) / robust_sei1

robust_sei2 <- sqrt(diag(vcovHC(inst2, type = "HC3")))

# Compute robust t-statistics for reg6
robust_ti2 <- coef(inst2) / robust_sei2

robust_sei3 <- sqrt(diag(vcovHC(inst3, type = "HC3")))

# Compute robust t-statistics for reg6
robust_ti3 <- coef(inst3) / robust_sei3

robust_sei4 <- sqrt(diag(vcovHC(inst4, type = "HC3")))

# Compute robust t-statistics for reg6
robust_ti4 <- coef(inst4) / robust_sei4

robust_sei5 <- sqrt(diag(vcovHC(inst5, type = "HC3")))

# Compute robust t-statistics for reg6
robust_ti5 <- coef(inst5) / robust_sei5

robust_sei6 <- sqrt(diag(vcovHC(inst6, type = "HC3")))

# Compute robust t-statistics for reg6
robust_ti6 <- coef(inst6) / robust_sei6

stargazer(inst1,inst2,inst3,inst4,inst5,inst6, type = "html",
          se = list(robust_sei1,robust_sei2,robust_sei3,robust_sei4,robust_sei5, robust_sei6), 
          t = list(robust_ti1,robust_ti2,robust_ti3,robust_ti4,robust_ti5, robust_ti6),    
          out = "reg_table_inst.html")

------------------------------------------------------------
  
  
  robust_sere4_olv <- sqrt(diag(vcovHC(re4_olv, type = "HC3")))

# Compute robust t-statistics for reg6
robust_tre4_olv <- coef(re4_olv) / robust_sere4lv


robust_sere4_oprop <- sqrt(diag(vcovHC(re4_oprop, type = "HC3")))

# Compute robust t-statistics for reg6
robust_tre4_oprop <- coef(re4_oprop) / robust_sere4prop


stargazer(re4_olv,re4_oprop, type = "html",
          se = list(robust_sere4_olv,robust_sere4_oprop),  
          t = list(robust_tre4_olv,robust_tre4_oprop),    
          out = "reg_table_lv&prop.html")



# 2 i 3 razem
stargazer(re2_o, re4_o,re4_olv,re4_oprop, type = "html",
          se = list(robust_se2, robust_se4,robust_sere4_olv,robust_sere4_oprop),  
          t = list(robust_t2, robust_t4,robust_tre4_olv,robust_tre4_oprop),    
          out = "reg_table2&3.html")




TABLES CORRELATIONS - total


# Załadowanie wymaganych pakietów
library(kableExtra)

# Funkcja do generowania tabeli wyników testu Spearmana
generate_spearman_table_from_test <- function(data, var1, var2) {
  
  # Automatyczne pobranie nazwy dataframe
  df_name <- deparse(substitute(data))
  
  # Wykonanie testu Spearmana
  test_result <- cor.test(data[[var1]], data[[var2]], method = "spearman")
  
  # Wyciągnięcie wyników testu
  rho <- test_result$estimate  # rho
  p_two_sided <- test_result$p.value  # p-value dla testu dwustronnego
  
  # Obliczenie p-value dla "less" i "greater"
  p_less <- test_result$p.value / 2  
  p_greater <- 1 - (test_result$p.value / 2)  
  
  # Przygotowanie danych do tabeli
  test_results <- data.frame(
    Value = c(rho, p_two_sided, p_less, p_greater)
  )
  
  # Ustawienie nazw kolumn na nazwę dataframe
  colnames(test_results) <- df_name
  rownames(test_results) <- c("rho", "Alternative Two-Sided", "Alternative Less", "Alternative Greater")
  
  # Transponowanie tabeli
  transposed_results <- t(test_results)
  
  return(transposed_results)  # Zwracamy transponowaną ramkę danych z wynikami
}

# Testowanie funkcji na przykładzie
corr_without_grouping <- generate_spearman_table_from_test(model_data4_o, "GDP_growth", "cc_total")
corr_rich <- generate_spearman_table_from_test(rich, "GDP_growth", "cc_total")
corr_poor <- generate_spearman_table_from_test(poor, "GDP_growth", "cc_total")
corr_africa <- generate_spearman_table_from_test(Africa, "GDP_growth", "cc_total")
corr_america <- generate_spearman_table_from_test(America, "GDP_growth", "cc_total")
corr_asia <- generate_spearman_table_from_test(Asia, "GDP_growth", "cc_total")
corr_europe <- generate_spearman_table_from_test(Europe, "GDP_growth", "cc_total")
corr_pacific <- generate_spearman_table_from_test(Pacific, "GDP_growth", "cc_total")
corr_fr <- generate_spearman_table_from_test(FR, "GDP_growth", "cc_total")
corr_so <- generate_spearman_table_from_test(SO, "GDP_growth", "cc_total")
corr_uk <- generate_spearman_table_from_test(UK, "GDP_growth", "cc_total")
corr_ge <- generate_spearman_table_from_test(GE, "GDP_growth", "cc_total")
corr_sc <- generate_spearman_table_from_test(SC, "GDP_growth", "cc_total")
corr_colonized <- generate_spearman_table_from_test(ever_colonized, "GDP_growth", "cc_total")
corr_not_colonized <- generate_spearman_table_from_test(never_colonized, "GDP_growth", "cc_total")



# Łączenie wyników testów w jedną ramkę danych
merged_results <- rbind(corr_without_grouping, corr_rich, corr_poor, corr_africa, corr_america, corr_asia, corr_europe,
                        corr_pacific, corr_fr, corr_so, corr_uk, corr_ge, corr_sc, corr_colonized, corr_not_colonized)

# Wyświetlanie połączonych wyników w formacie HTML

library(kableExtra)

merged_results %>%
  as.data.frame() %>% 
  mutate(across(everything(), ~ round(as.numeric(.), 4))) %>% 
  kbl(caption = "Spearman's rho Correlation Test Results:<br>GDP_growth vs cc_total<br>H0: rho = 0", 
      digits = 4, 
      align = "c", 
      row.names = TRUE) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  add_header_above(c(" " = 2, "P-value" = 3), bold = TRUE)  # Pogrubienie tylko P-value












TABLES CORRELATIONS - PROP

propcorr_without_grouping <- generate_spearman_table_from_test(model_data4_o, "GDP_growth", "cc_prop")
propcorr_rich <- generate_spearman_table_from_test(rich, "GDP_growth", "cc_prop")
propcorr_poor <- generate_spearman_table_from_test(poor, "GDP_growth", "cc_prop")
propcorr_africa <- generate_spearman_table_from_test(Africa, "GDP_growth", "cc_prop")
propcorr_america <- generate_spearman_table_from_test(America, "GDP_growth", "cc_prop")
propcorr_asia <- generate_spearman_table_from_test(Asia, "GDP_growth", "cc_prop")
propcorr_europe <- generate_spearman_table_from_test(Europe, "GDP_growth", "cc_prop")
propcorr_pacific <- generate_spearman_table_from_test(Pacific, "GDP_growth", "cc_prop")
propcorr_fr <- generate_spearman_table_from_test(FR, "GDP_growth", "cc_prop")
propcorr_so <- generate_spearman_table_from_test(SO, "GDP_growth", "cc_prop")
propcorr_uk <- generate_spearman_table_from_test(UK, "GDP_growth", "cc_prop")
propcorr_ge <- generate_spearman_table_from_test(GE, "GDP_growth", "cc_prop")
propcorr_sc <- generate_spearman_table_from_test(SC, "GDP_growth", "cc_prop")
propcorr_colonized <- generate_spearman_table_from_test(ever_colonized, "GDP_growth", "cc_prop")
propcorr_not_colonized <- generate_spearman_table_from_test(never_colonized, "GDP_growth", "cc_prop")



# Łączenie wyników testów w jedną ramkę danych
propmerged_results <- rbind(propcorr_without_grouping, propcorr_rich, propcorr_poor, propcorr_africa, propcorr_america, propcorr_asia, propcorr_europe,
                            propcorr_pacific, propcorr_fr, propcorr_so, propcorr_uk, propcorr_ge, propcorr_sc, propcorr_colonized, propcorr_not_colonized)

# Wyświetlanie połączonych wyników w formacie HTML

library(kableExtra)

propmerged_results %>%
  as.data.frame() %>% 
  mutate(across(everything(), ~ round(as.numeric(.), 4))) %>% 
  kbl(caption = "Spearman's rho Correlation Test Results:<br>GDP_growth vs cc_prop<br>H0: rho = 0", 
      digits = 4, 
      align = "c", 
      row.names = TRUE) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  add_header_above(c(" " = 2, "P-value" = 3), bold = TRUE)  # Pogrubienie tylko P-value














SOME PLOTS   

plot(GDP_growth ~ cc_total, data = model_data4, pch=19, col="blue",
     xlab="cc_total", ylab="GDP_growth")

plot(GDP_growth ~ log(fertility), data = model_data4, pch=19, col="blue",
     xlab="cc_total", ylab="GDP_growth")

plot(GDP_growth ~ log(GDPpc2015), data = model_data4, pch=19, col="blue",
     xlab="cc_total", ylab="GDP_growth")

plot(GDP_growth ~ log(inf_def+1), data = model_data4, pch=19, col="blue",
     xlab="cc_total", ylab="GDP_growth")

plot(GDP_growth ~ investment, data = model_data4, pch=19, col="blue",
     xlab="cc_total", ylab="GDP_growth")

plot(GDP_growth ~ gov_exp_reduced, data = model_data4, pch=19, col="blue",
     xlab="cc_total", ylab="GDP_growth")

plot(GDP_growth ~ log(cc_total+2), data = model_data4, pch=19, col="blue",
     xlab="cc_total", ylab="GDP_growth")

plot(GDP_growth ~ life_exp, data = model_data4, pch=19, col="blue",
     xlab="cc_total", ylab="GDP_growth")

plot(GDP_growth ~ education, data = model_data4, pch=19, col="blue",
     xlab="cc_total", ylab="GDP_growth")

plot(GDP_growth ~ v2x_libdem, data = model_data4, pch=19, col="blue",
     xlab="cc_total", ylab="GDP_growth")

plot(GDP_growth ~ log(tot), data = model_data4, pch=19, col="blue",
     xlab="cc_total", ylab="GDP_growth")
plot(GDP_growth ~ trade, data = model_data4, pch=19, col="blue",
     xlab="cc_total", ylab="GDP_growth")

plot(GDP_growth ~ log(tot), data = model_data4, pch=19, col="blue",
     xlab="cc_total", ylab="GDP_growth")

plot(GDP_growth ~ landlocked, data = model_data4, pch=19, col="blue",
     xlab="landlocked", ylab="GDP_growth")



# Make sure landlocked is a factor
model_data4$landlocked <- as.factor(model_data4$landlocked)

# Create the vertical boxplot
boxplot(model_data4$GDP_growth ~ model_data4$landlocked, 
        varwidth = TRUE, 
        main = "GDP Growth vs Landlockedness", 
        xlab = "Landlocked", 
        ylab = "GDP Growth")


boxplot(model_data5$GDP_growth ~ model_data5$landlocked, 
        varwidth = TRUE, 
        main = "GDP Growth vs Landlockedness", 
        xlab = "Landlocked", 
        ylab = "GDP Growth")












REGRESSIONS BY CONTINENT

Africa <- model_data4.1[model_data4.1$continent == "Africa", ]
America <- model_data4.1[model_data4.1$continent == "America", ]
Asia <- model_data4.1[model_data4.1$continent == "Asia", ]
Europe <- model_data4.1[model_data4.1$continent == "Europe", ]
Pacific <- model_data4.1[model_data4.1$continent == "Pacific", ]


regression_america1 <- lm(GDP_growth ~ log(GDPpc) + inf_cpi + investment  + tot:trade  + 
                            log(cc_total) , data = America)
summary(regression_america1)

-------------------------------------------
  
  regression_africa1 <- lm(GDP_growth ~ log(GDPpc) + inf_cpi + investment  + trade  + 
                             log(cc_total) , data = Africa)
summary(regression_africa1)

-------------------------------------------
  
  regression_asia1 <- lm(GDP_growth ~ log(GDPpc) + inf_cpi + investment  + tot:trade  + 
                           log(cc_total) , data = Asia)
summary(regression_asia1)

-------------------------------------------
  
  regression_europe1 <- lm(GDP_growth ~ log(GDPpc) + inf_cpi + investment  + trade  + 
                             log(cc_total) , data = Europe)
summary(regression_europe1)

------------------------------------------
  
  regression_pacific1 <- lm(GDP_growth ~ log(GDPpc) + inf_cpi + investment  + trade  + 
                              log(cc_total) , data = Pacific)
summary(regression_pacific1)


correlationTest(Africa$GDP_growth, Africa$cc_total, method = "spearman", 
                title = NULL, description = NULL)

correlationTest(America$GDP_growth, America$cc_total, method = "spearman", 
                title = NULL, description = NULL)

correlationTest(Asia$GDP_growth, Asia$cc_total, method = "spearman", 
                title = NULL, description = NULL)

correlationTest(Europe$GDP_growth, Europe$cc_total, method = "spearman", 
                title = NULL, description = NULL)

correlationTest(Pacific$GDP_growth, Pacific$cc_total, method = "spearman", 
                title = NULL, description = NULL)




REGRESSIONS BY LEGAL ORIGIN

FR <- model_data4.1[model_data4.1$legal_old_o == "fr", ]
SO <- model_data4.1[model_data4.1$legal_old_o == "so", ]
UK <- model_data4.1[model_data4.1$legal_old_o == "uk", ]
GE <- model_data4.1[model_data4.1$legal_old_o == "ge", ]
SC <- model_data4.1[model_data4.1$legal_old_o == "sc", ]

regression_fr1 <- lm(GDP_growth ~ log(GDPpc) + inf_cpi + investment  + tot:trade  + 
                       log(cc_total) , data = FR)
summary(regression_fr1)

regression_so1 <- lm(GDP_growth ~ log(GDPpc) + inf_cpi + investment  + tot:trade  + 
                       log(cc_total) , data = SO)
summary(regression_so1)

regression_uk1 <- lm(GDP_growth ~ log(GDPpc) + inf_cpi + investment  + tot:trade  + 
                       log(cc_total) , data = UK)
summary(regression_uk1)

regression_ge1 <- lm(GDP_growth ~ log(GDPpc) + inf_cpi + investment  + tot:trade  + 
                       log(cc_total) , data = GE)
summary(regression_ge1)

regression_sc1 <- lm(GDP_growth ~ log(GDPpc) + inf_cpi + investment  + tot:trade  + 
                       log(cc_total) , data = SC)
summary(regression_sc1)



correlationTest(FR$GDP_growth, FR$cc_total, method = "spearman", 
                title = NULL, description = NULL)

correlationTest(SO$GDP_growth, SO$cc_total, method = "spearman", 
                title = NULL, description = NULL)

correlationTest(UK$GDP_growth, UK$cc_total, method = "spearman", 
                title = NULL, description = NULL)

correlationTest(GE$GDP_growth, GE$cc_total, method = "spearman", 
                title = NULL, description = NULL)

correlationTest(SC$GDP_growth, SC$cc_total, method = "spearman", 
                title = NULL, description = NULL)













REGRESSIONS IF COLONIZED   

ever_colonized <- model_data4.1[model_data4.1$ever_colonized == 1, ]
never_colonized <- model_data4.1[model_data4.1$ever_colonized == 0, ]

regression_ever_colonized1 <- lm(GDP_growth ~ log(GDPpc) + inf_cpi + investment  + tot:trade  + 
                                   log(cc_total) , data = ever_colonized)
summary(regression_ever_colonized1)

regression_never_colonized1 <- lm(GDP_growth ~ log(GDPpc) + inf_cpi + investment  + tot:trade  + 
                                    log(cc_total) , data = never_colonized)
summary(regression_never_colonized1)


--------------------------------------------
  
  correlationTest(ever_colonized$GDP_growth, ever_colonized$cc_total, method = "spearman", 
                  title = NULL, description = NULL)

correlationTest(never_colonized$GDP_growth, never_colonized$cc_total, method = "spearman", 
                title = NULL, description = NULL)





SOME OTHER REGRESSIONS TO BE CHECKED

re4_olv<- lm(GDP_growth ~ log(GDPpc2015) + investment + 
               tot2:trade + gov_exp_reduced +
               log(cc_total_lv+2) + tot2 + trade, data = model_data4_o)
summary(re4_olv)

re4_oprop<- lm(GDP_growth ~ log(GDPpc2015) + investment + 
                 tot2:trade + gov_exp_reduced +
                 log(cc_prop+2) + tot2 + trade, data = model_data4_o)
summary(re4_oprop)

re4_oprop<- lm(GDP_growth ~ log(GDPpc2015) + investment + 
                 tot2:trade + gov_exp_reduced +
                 log(cc_prop+2) + tot2 + trade, data = model_data4_o)
summary(re4_oprop)







