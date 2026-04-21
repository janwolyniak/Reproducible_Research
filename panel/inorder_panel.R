requiredPackages = c("stargazer", "lmtest", "fBasics", "sandwich", 
                     "olsrr", "car", "sandwich", "MASS", "tseries",
                     "robust","corrplot", "foreign", "PerformanceAnalytics",
                     "haven", "ggplot2", "viridis", "vioplot", "devtools",
                     "ggcorrplot", "tidyverse", "purrr","plm")
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) }

options(scipen=999)
Sys.setenv(LANG="EN")


pdata <- pdata.frame(model_data, index=c("Country","Year"))
pdata2 <- pdata.frame(model_data2, index=c("Country","Year"))



Africa <- model_data2[model_data2$continent == "Africa", ]
America <- model_data2[model_data2$continent == "America", ]
Asia <- model_data2[model_data2$continent == "Asia", ]
Europe <- model_data2[model_data2$continent == "Europe", ]
Pacific <- model_data2[model_data2$continent == "Pacific", ]

FR <- model_data2[model_data2$legal_old_o == "fr", ]
SO <- model_data2[model_data2$legal_old_o == "so", ]
UK <- model_data2[model_data2$legal_old_o == "uk", ]
GE <- model_data2[model_data2$legal_old_o == "ge", ]
SC <- model_data2[model_data2$legal_old_o == "sc", ]




PLOT 

#EUROPE

ggplot(Europe, aes(x = Year)) +
  # Plot for GDP growth
  geom_line(aes(y = GDPgrowth, color = "GDP Growth"), size = 1) +
  # Plot for carbon emissions (cc_total)
  geom_line(aes(y = cc_total, color = "Constitutional Compliance"), size = 1) +
  facet_wrap(~ Country, scales = "free_y") +
  scale_color_manual(values = c("GDP Growth" = "blue", "Constitutional Compliance" = "red")) +
  labs(title = "GDP Growth and Constitutional Compliance Over Time",
       x = "Year",
       y = "GDP Growth (%)") +
  # Adding a secondary y-axis for the second variable
  scale_y_continuous(
    name = "GDP Growth (%)",
    sec.axis = sec_axis(~ ., name = "Constitutional Compliance (cc_total)")
  ) +
  theme_minimal()








CORRELATIONS 

library(corrplot)

# Specify the variables you want to include
included_vars <- c("GDP_growth", "fertility", "GDPpc2015",  "inflation","investment", "gov_exp_reduced", 
                   "cc_total", "tot_growth", "trade", "education", "life_exp")  

# Subset only the specified variables
subset_data <- model_data2[, included_vars]


# Ensure all variables are numeric
numeric_subset <- subset_data[, sapply(subset_data, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_subset, use = "complete.obs", method = "spearman")


testRes = cor.mtest(cor_matrix, conf.level = 0.95)
corrplot(cor_matrix, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE',col = COL2('BrBG'),tl.col = "black", diag=FALSE)










Panel Data Models in R




install.packages("plm")
library(plm)
small_plm$fertility <- as.numeric(as.character(small_plm$fertility))

# Set data as panel data
small_pdata <- small_pdata.frame(small_plm, index=c("Country","Year"))


# Descriptive statistics
summary(small_pdata$GDPgrowth)


# Pooled OLS estimator
pooling <- plm(GDPgrowth ~ log(cc_total+2)  + life_exp + education + fertility + log(lag(GDPpc2015, 2)) +
                 gov_exp_reduced + inflation + investment + tot_growth + trade , data=small_pdata, model= "pooling")
summary(pooling)

# Between estimator
between <- plm(GDPgrowth ~ log(cc_total+2)  + life_exp + education  + fertility + log(lag(GDPpc2015, 2)) +
                 gov_exp_reduced + inflation + investment + tot_growth + trade , data=small_pdata, model= "between")
summary(between)

# First differences estimator
firstdiff <- plm(GDPgrowth ~ log(cc_total+2)  + life_exp + education  + fertility + log(lag(GDPpc2015, 2)) +
                   gov_exp_reduced + inflation + investment + tot_growth + trade , data=small_pdata, model= "fd")
summary(firstdiff)

# Fixed effects or within estimator
fixed <- plm(GDPgrowth ~ log(cc_total+2)  + life_exp + education + fertility + log(lag(GDPpc2015, 2)) +
               gov_exp_reduced + inflation + investment + tot_growth + trade, data=small_pdata, model= "within")
summary(fixed)

# Random effects estimator
random <- plm(GDPgrowth ~ log(cc_total+2)  + life_exp + education  + fertility + log(lag(GDPpc2015, 2)) +
                gov_exp_reduced + inflation + investment + tot_growth + trade , data=small_pdata, model= "random")
summary(random)

# LM test for random effects versus OLS
plmtest(pooling)

# LM test for fixed effects versus OLS
pFtest(fixed, pooling)

# Hausman test for fixed versus random effects model
phtest(random, fixed)

vif(random)




DIAGNOSTICS


# 1. Serial Correlation Test = Autocorrelation (Wooldridge Test)
pbgtest(fixed)

# 2. Heteroskedasticity Test (Breusch-Pagan)
bptest(model)

# 3. Hausman Test (Fixed vs Random Effects)
phtest(fixed, random)

# 4. Cross-sectional Dependence Test (Pesaran CD)
pcdtest(model, test = "cd")

# 5. Panel Unit Root Test (Levin-Lin-Chu)
purtest(pdata$GDPgrowth, test = "levinlin")








----------------------


# Set data as panel data
small_pdata <- pdata.frame(small_plm, index=c("Country","Year"))


# Descriptive statistics
summary(small_pdata$GDPgrowth)


# Pooled OLS estimator
small_pooling <- plm(GDPgrowth ~ log(cc_total+2)  + fertility + log(lag(GDPpc2015, 2)) +
                       gov_exp_reduced + inflation + investment + tot_growth + trade+ tot_growth:trade  , data=small_pdata, model= "pooling")
summary(small_pooling)

# Between estimator
small_between <- plm(GDPgrowth ~ log(cc_total+2)  + fertility + log(lag(GDPpc2015, 2)) +
                       gov_exp_reduced + inflation + investment + tot_growth + trade+ tot_growth:trade  , data=small_pdata, model= "between")
summary(small_between)

# First differences estimator
small_firstdiff <- plm(GDPgrowth ~ log(cc_total+2)    + fertility + log(lag(GDPpc2015, 2)) +
                         gov_exp_reduced + inflation + investment + tot_growth + trade+ tot_growth:trade  , data=small_pdata, model= "fd")
summary(small_firstdiff)

# Fixed effects or within estimator
small_fixed <- plm(GDPgrowth ~ log(cc_total+2)  + fertility + log(lag(GDPpc2015, 2)) +
                     gov_exp_reduced + inflation + investment + tot_growth + trade+ tot_growth:trade , data=small_pdata, model= "within")
summary(small_fixed)

# Random effects estimator
small_random <- plm(GDPgrowth ~ log(cc_total+2)   + fertility + log(lag(GDPpc2015, 2)) +
                      gov_exp_reduced + inflation + investment + tot_growth + trade+ tot_growth:trade  , data=small_pdata, model= "random")
summary(small_random)

# LM test for random effects versus OLS
plmtest(small_pooling)

# LM test for fixed effects versus OLS
pFtest(small_fixed, small_pooling)

# Hausman test for fixed versus random effects model
phtest(small_random, small_fixed)

vif(small_random)






SPECIFICATION TESTS   


results_p <- data.frame(
  Model = c("2010-2019", "1990-2020"),
  `Lagrange Multiplier Test` = c(
    plmtest(small_pooling)$p.value,
    plmtest(pooling)$p.value
  ),
  `F Test for Individual and/or Time Effects` = c(
    pFtest(small_fixed, small_pooling)$p.value,
    pFtest(fixed, pooling)$p.value
  ),
  `Hausman Test` = c(
    phtest(small_random, small_fixed)$p.value,
    phtest(random, fixed)$p.value
  )
)

# Format p-values to 4 decimal places
results_p[, -1] <- round(results_p[, -1], 4)

# Rename columns for display
colnames(results_p) <- c("Model", "Lagrange Multiplier Test", "F Test for Individual and/or Time Effects", "Hausman Test")

# Create a formatted and centered table with kableExtra
library(kableExtra)

results_p %>%
  kbl(caption = "P-Values for Panel Regression Specification Tests", align = "c") %>%  # Aligns columns to center
  add_header_above(c(" " = 1, "P-value" = 3)) %>%  # Adds "P-value" header above tests
  kable_classic(full_width = FALSE, html_font = "Cambria", position = "center")  # Centers the table


results_p_t <- as.data.frame(t(results_p))

# Set first row as column names
colnames(results_p_t) <- results_p_t[1, ]
results_p_t <- results_p_t[-1, ]

# Make sure the values are numeric
results_p_t[, ] <- lapply(results_p_t, as.numeric)

# Add a 'Test' column
results_p_t <- cbind(Test = rownames(results_p_t), results_p_t)
rownames(results_p_t) <- NULL

# Now you can safely round
results_p_t[, -1] <- round(results_p_t[, -1], 4)

# Create a pretty table
results_p_t %>%
  kbl(caption = "P-Values for Panel Regression Specification Tests", align = "c") %>%
  add_header_above(c(" " = 1, "Model" = 2)) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria", position = "center")





DIAGNOSTICS TABLE  

results_p <- data.frame(
  Model = c("1p", "2p"),
  `Serial Correlation (Wooldridge) Test` = c(
    pbgtest(small_fixed)$p.value,
    pbgtest(model)$p.value
  ),
  `Heteroskedasticity Test (Breusch-Pagan)` = c(
    bptest(small_fixed)$p.value,
    bptest(model)$p.value
  ),
  `Cross-sectional Dependence Test (Pesaran CD)` = c(
    pcdtest(small_fixed, test = "cd")$p.value,
    pcdtest(model, test = "cd")$p.value
  )
)

# Format p-values to 4 decimal places
results_p[, -1] <- round(results_p[, -1], 4)

# Rename columns for display
colnames(results_p) <- c("Model","Wooldridge Test","Breusch-Pagan","Pesaran CD")

# Create a formatted and centered table with kableExtra
library(kableExtra)

results_p %>%
  kbl(caption = "P-Values for Panel Regression Diagnostic Tests", align = "c") %>%  # Aligns columns to center
  add_header_above(c(" " = 1, "P-value" = 3)) %>%  # Adds "P-value" header above tests
  kable_classic(full_width = FALSE, html_font = "Cambria", position = "center")  # Centers the table




results_p_t <- as.data.frame(t(results_p))

# Set first row as column names
colnames(results_p_t) <- results_p_t[1, ]
results_p_t <- results_p_t[-1, ]

# Make sure the values are numeric
results_p_t[, ] <- lapply(results_p_t, as.numeric)

# Add a 'Test' column
results_p_t <- cbind(Test = rownames(results_p_t), results_p_t)
rownames(results_p_t) <- NULL

# Now you can safely round
results_p_t[, -1] <- round(results_p_t[, -1], 4)

# Create a pretty table
results_p_t %>%
  kbl(caption = "P-Values for Panel Regression Diagnostic Tests", align = "c") %>%
  add_header_above(c(" " = 1, "Model" = 2)) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria", position = "center")











ROBUST MATRICES 

model<-fixed

#The Driscoll-Kraay estimator
coeftest(model, vcov = vcovSCC)

#Arellano estimator 
arellano_vcov <- vcovHC(model, method = "arellano")
coeftest(model, vcov = arellano_vcov)

#Double cluster
dc_se <- vcovDC(model, type = "HC1")
coeftest(model, vcov = dc_se)

# Clustered standard errors
clustered_se <- vcovHC(model, method = "arellano", type = "HC1", cluster = "time")
coeftest(model, vcov. = clustered_se)

---------------------------------------
  
  # Plot the residuals
  hist(resid(model), breaks = 30, main = "Residuals Distribution", col = "skyblue")
boxplot(resid(model), main = "Boxplot of Residuals")


plot(GDPgrowth ~ cc_total, data = model_data, pch=19, col="blue",
     xlab="cc_total", ylab="GDP_growth")





TABLE REGRESSIONS

# small_plm

# pure fixed

# The Driscoll-Kraay estimator

robust_se1 <- vcovSCC(model)
robust_se1 <- sqrt(diag(robust_se1))


stargazer(small_fixed, model, model, title = "Panel regression", 
          se = list(NULL,NULL, robust_se1),
          type = "html", out = "model_output.html")


stargazer(model, model, title = "Panel regression", 
          se = list(NULL, robust_se1),
          type = "html", out = "model_output.html")





ALTERNATIVE REGRESSIONS

fe_basic<- plm(GDPgrowth ~ log(cc_basic+2)  + fertility + log(lag(GDPpc2015, 2)) +
                 gov_exp_reduced + inflation + investment + tot_growth:trade+ tot_growth + trade , data=pdata, model= "within")
summary(fe_basic)

robust_se_basic <- vcovSCC(fe_basic)
robust_se_basic <- sqrt(diag(robust_se_basic))


fe_civil<- plm(GDPgrowth ~ log(cc_civil+2)  + fertility + log(lag(GDPpc2015, 2)) +
                 gov_exp_reduced + inflation + investment + tot_growth + trade+ tot_growth:trade , data=pdata, model= "within")
summary(fe_civil)

robust_se_civil <- vcovSCC(fe_civil)
robust_se_civil <- sqrt(diag(robust_se_civil))


fe_polit<- plm(GDPgrowth ~ log(cc_polit+2)  + fertility + log(lag(GDPpc2015, 2)) +
                 gov_exp_reduced + inflation + investment + tot_growth + trade+ tot_growth:trade , data=pdata, model= "within")
summary(fe_polit)

robust_se_polit <- vcovSCC(fe_polit)
robust_se_polit <- sqrt(diag(robust_se_polit))


fe_prop<- plm(GDPgrowth ~ log(cc_prop+2)  + fertility + log(lag(GDPpc2015, 2)) +
                gov_exp_reduced + inflation + investment + tot_growth + trade+ tot_growth:trade , data=pdata, model= "within")
summary(fe_prop)

robust_se_prop <- vcovSCC(fe_prop)
robust_se_prop <- sqrt(diag(robust_se_prop))



stargazer(fe_basic, fe_civil, fe_polit,fe_prop, title = "Panel regression", 
          se = list(robust_se_basic,robust_se_civil,robust_se_polit, robust_se_prop),
          type = "html", out = "model_output2.html")




