# Load libraries
if (!require("dplyr")) { install.packages("dplyr") }; library(dplyr)
if (!require("devtools")) { install.packages("devtools") }
if (!require("readr")) { install.packages("readr") }
if (!require("surveytools2")) { devtools::install_github("peterhurford/surveytools2") }; library(surveytools2)

# Load data
data <- readr::read_csv("reducetarian_data_share.csv")

# Make variables
data$howOftenEatMeat_chg <- data$howOftenEatMeat.1 - data$howOftenEatMeat.3

data$is_veg.1 <- data$FFQfreqTurkey.1 == 0 & data$FFQfreqPork.1 == 0 & data$FFQfreqChicken.1 == 0 & data$FFQfreqFish.1 == 0 & data$FFQfreqBeef.1 == 0
data$is_veg.3 <- data$FFQfreqTurkey.3 == 0 & data$FFQfreqPork.3 == 0 & data$FFQfreqChicken.3 == 0 & data$FFQfreqFish.3 == 0 & data$FFQfreqBeef.3 == 0
data$veg_chg <- data$is_veg.3 - data$is_veg.1

data$mt.1 <- data$FFQfreqTurkey.1 + data$FFQfreqPork.1 + data$FFQfreqChicken.1 + data$FFQfreqFish.1 + data$FFQfreqBeef.1
data$mt.3 <- data$FFQfreqTurkey.3 + data$FFQfreqPork.3 + data$FFQfreqChicken.3 + data$FFQfreqFish.3 + data$FFQfreqBeef.3
data$mt_chg <- data$mt.3 - data$mt.1

data$bogus.1 <- data$FFQfreqFruit.1 + data$FFQfreqNuts.1 + data$FFQfreqVegetables.1 + data$FFQfreqBeans.1 + data$FFQfreqGrains.1
data$bogus.3 <- data$FFQfreqFruit.3 + data$FFQfreqNuts.3 + data$FFQfreqVegetables.3 + data$FFQfreqBeans.3 + data$FFQfreqGrains.3
data$bogus_chg <- data$bogus.3 - data$bogus.1
data$vegetable_chg <- data$FFQfreqVegetables.3 - data$FFQfreqVegetables.1

# Analysis
data %>% ctable(FFQtotalSumMeat_chg, treatment)
# Adding missing grouping variables: `group`
# FFQtotalSumMeat_chg ### treatment
# # A tibble: 3 × 4
#     group       mean median       sd
#     <chr>      <dbl>  <dbl>    <dbl>
# 1 control  0.3034188      0 8.054944
# 2  reduce -0.6977077      0 8.595043
# 3     veg -0.7948895      0 9.412867


# Call:
# lm(formula = .)

# Residuals:
#     Min      1Q  Median      3Q     Max
# -32.803  -2.803   0.697   3.198  26.795

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)   0.3034     0.3289   0.923   0.3563
# yreduce      -1.0011     0.4658  -2.149   0.0317 *
# yveg         -1.0983     0.4615  -2.380   0.0174 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 8.713 on 2121 degrees of freedom
#   (113 observations deleted due to missingness)
# Multiple R-squared:  0.003232,  Adjusted R-squared:  0.002292
# F-statistic: 3.438 on 2 and 2121 DF,  p-value: 0.03229

data %>% ctable(howOftenEatMeat_chg, treatment)
# Adding missing grouping variables: `group`
# howOftenEatMeat_chg ### treatment
# # A tibble: 3 × 4
#     group        mean median        sd
#     <chr>       <dbl>  <int>     <dbl>
# 1 control -0.03518268      0 0.9084706
# 2  reduce  0.08119080      0 0.9774757
# 3     veg  0.01192053      0 0.9926071


# Call:
# lm(formula = .)

# Residuals:
#     Min      1Q  Median      3Q     Max
# -5.0812 -0.0812 -0.0119  0.0352  4.9881

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept) -0.03518    0.03533  -0.996   0.3195
# yreduce      0.11637    0.04997   2.329   0.0199 *
# yveg         0.04710    0.04970   0.948   0.3434
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.9605 on 2230 degrees of freedom
#   (4 observations deleted due to missingness)
# Multiple R-squared:  0.002456,  Adjusted R-squared:  0.001562
# F-statistic: 2.746 on 2 and 2230 DF,  p-value: 0.06443

data %>% filter(treatment == "control") %>% .$veg_chg %>% table
# -1   0   1
#  3 727   6
data %>% filter(treatment != "control") %>% .$veg_chg %>% table
# -1    0    1
#  6 1478    8


data %>% ctable(veg_chg > 0, treatment)
# > veg_chg 0 ### treatment

#         control reduce    veg
#   FALSE  0.9918 0.9973 0.9921
#   TRUE   0.0082 0.0027 0.0079


#         Pearson's Chi-squared test

# data:  x and y
# X-squared = 2.2503, df = 2, p-value = 0.3246


data %>% ctable(mt_chg < 0, treatment == "control")
# < mt_chg 0 ### == treatment control

#          FALSE   TRUE
#   FALSE 0.5755 0.6467
#   TRUE  0.4245 0.3533


#         Pearson's Chi-squared test with Yates' continuity correction

# data:  x and y
# X-squared = 9.9013, df = 1, p-value = 0.001652

data %>% ctable(bogus_chg < 0, treatment == "control")
# p-value: 0.7933

data %>% ctable(bogus_chg < 0, treatment == "control")
# < bogus_chg 0 ### == treatment control

#          FALSE   TRUE
#   FALSE 0.5755 0.5860
#   TRUE  0.4245 0.4140

data %>% ctable(vegetable_chg, treatment == "control")
# p-value: 0.1778


#         Pearson's Chi-squared test with Yates' continuity correction

# data:  x and y
# X-squared = 0.17827, df = 1, p-value = 0.6729


# Beef vs. chicken
data %>% filter(treatment == "control") %>% filter(FFQfreqBeef.3 < FFQfreqBeef.1) %>% { .$FFQfreqChicken.3 - .$FFQfreqChicken.1 } %>% mean(., na.rm = T)
# [1] -0.6649485
data %>% filter(treatment != "control") %>% filter(FFQfreqBeef.3 < FFQfreqBeef.1) %>% { .$FFQfreqChicken.3 - .$FFQfreqChicken.1 } %>% mean(., na.rm = T)
# [1] -2.200758
data %>% filter(treatment == "control") %>% filter(FFQfreqChicken.3 < FFQfreqChicken.1) %>% { .$FFQfreqBeef.3 - .$FFQfreqBeef.1 } %>% mean(., na.rm = T)
# [1] -1
data %>% filter(treatment != "control") %>% filter(FFQfreqChicken.3 < FFQfreqChicken.1) %>% { .$FFQfreqBeef.3 - .$FFQfreqBeef.1 } %>% mean(., na.rm = T)
# [1] -2.283673

# Product elimination analysis
data %>% filter(treatment == "control") %>% { (.$FFQfreqBeef.3 == 0 & .$FFQfreqBeef.1 > 0) } %>% table
# FALSE  TRUE
#   721    15
# 2.0380435%
data %>% filter(treatment != "control") %>% { (.$FFQfreqBeef.3 == 0 & .$FFQfreqBeef.1 > 0) } %>% table
# FALSE  TRUE
#  1450    40
# 2.684563758%

data %>% filter(treatment == "control") %>% { (.$FFQfreqChicken.3 == 0 & .$FFQfreqChicken.1 > 0) } %>% table
# FALSE  TRUE
#   726    10
# 1.358695652%
data %>% filter(treatment != "control") %>% { (.$FFQfreqChicken.3 == 0 & .$FFQfreqChicken.1 > 0) } %>% table
# FALSE  TRUE
#  1477    15
# 1.00536193%

data %>% filter(treatment == "control") %>% { (.$FFQfreqFish.3 == 0 & .$FFQfreqFish.1 > 0) } %>% table
# FALSE  TRUE
#   694    40
# 5.449591281%
data %>% filter(treatment != "control") %>% { (.$FFQfreqFish.3 == 0 & .$FFQfreqFish.1 > 0) } %>% table
# FALSE  TRUE
#  1412    80
# 5.361930295%

data %>% filter(treatment == "control") %>% { (.$FFQfreqEggs.3 == 0 & .$FFQfreqEggs.1 > 0) } %>% table
# FALSE  TRUE
#   724    11
# 1.496598639%
data %>% filter(treatment != "control") %>% { (.$FFQfreqEggs.3 == 0 & .$FFQfreqEggs.1 > 0) } %>% table
# FALSE  TRUE
#  1440    52
# 3.485254692%

data %>% filter(treatment == "control") %>% { (.$FFQfreqDairy.3 == 0 & .$FFQfreqDairy.1 > 0) } %>% table
# FALSE  TRUE
#   726     9
# 1.2244898%
data %>% filter(treatment != "control") %>% { (.$FFQfreqDairy.3 == 0 & .$FFQfreqDairy.1 > 0) } %>% table
# FALSE  TRUE
#  1471    15
# 0.605652759%


# Consumption analysis

data %>% filter(treatment == "control") %>% { .$FFQfreqTurkey.3 - .$FFQfreqTurkey.1 } %>% mean(., na.rm = T)
# [1] 0.07901907
data %>% filter(treatment != "control") %>% { .$FFQfreqTurkey.3 - .$FFQfreqTurkey.1 } %>% mean(., na.rm = T)
# [1] -0.1851107
data %>% filter(treatment == "control") %>% { .$FFQfreqPork.3 - .$FFQfreqPork.1 } %>% mean(., na.rm = T)
# [1] 0.1707483
data %>% filter(treatment != "control") %>% { .$FFQfreqPork.3 - .$FFQfreqPork.1 } %>% mean(., na.rm = T)
# [1] -0.3003356
data %>% filter(treatment == "control") %>% { .$FFQfreqChicken.3 - .$FFQfreqChicken.1 } %>% mean(., na.rm = T)
# [1] 0.09332425
data %>% filter(treatment != "control") %>% { .$FFQfreqChicken.3 - .$FFQfreqChicken.1 } %>% mean(., na.rm = T)
# [1] -0.262273
data %>% filter(treatment == "control") %>% { .$FFQfreqFish.3 - .$FFQfreqFish.1 } %>% mean(., na.rm = T)
# [1] 0.1532148
data %>% filter(treatment != "control") %>% { .$FFQfreqFish.3 - .$FFQfreqFish.1 } %>% mean(., na.rm = T)
# [1] 0.07550336
data %>% filter(treatment == "control") %>% { .$FFQfreqBeef.3 - .$FFQfreqBeef.1 } %>% mean(., na.rm = T)
# [1] 0.04774898
data %>% filter(treatment != "control") %>% { .$FFQfreqBeef.3 - .$FFQfreqBeef.1 } %>% mean(., na.rm = T)
# [1] -0.2234543
