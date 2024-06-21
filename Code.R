#-----Section 01-------------------------------------------

# set working directory
setwd(dirname(file.choose()))
getwd()

# read in data from csv file
data_set <- read.csv("u2555660_DS7006_CW2_dataset.csv", stringsAsFactors = FALSE)
head(data_set)
str(data_set)


library(psych)
library(rcompanion)

# rm(list=ls())

##########################################################
#-----Section 02-------------------------------------------


attach(data_set)

# descriptive statistics for each numerical variable to compare mean and median
summary(data_set)


################################################################################
################################################################################
####### CHECKING NORMALITY FOR THE DEPENDENT VARIABLE  #########################
################################################################################
################################################################################

boxplot(p_total_deaths, xlab = "p_total_deaths", main = "Boxplot", medcol = "red", 
        boxlty = 0, whisklty = 1, staplelwd = 4, outpch = 8)

outliers <- boxplot(p_total_deaths, plot = FALSE)$out
text(x = 1, y = outliers, labels = round(outliers, 2), pos = 4, col = "red")

# Create a data frame with row numbers and outliers
outliers_df <- data.frame(Row = which(p_total_deaths %in% outliers), Outlier = outliers)

num_outliers <- length(outliers)
print(num_outliers)

cat("Number of outliers =", num_outliers, "\n")
cat("List of outliers \n", outliers, "\n")
cat("Row numbers and outliers:\n")
cat(outliers_df$Row, ", ", outliers_df$Outlier, "\n")


qqnorm(p_total_deaths, xlab = "p_total_deaths", col = "purple", pch = 16)
qqline(p_total_deaths, col = 2)

# ?qqnorm

ks.test(p_total_deaths, "pnorm", mean(p_total_deaths), sd(p_total_deaths))

# shapiro.test(p_total_deaths)

hist(p_total_deaths)
plotNormalHistogram(p_total_deaths, main = "Histogram", xlab = "p_total_deaths", col = "lightblue")


# install.packages("goftest")
# library(goftest)

# ad_test <- ad.test(data_set$p_total_deaths)
# print(ad_test)


################################################################################
################################################################################
################### AGE THEME ##################################################
################################################################################
################################################################################


boxplot(p_toddlers, xlab = "p_toddlers")
boxplot(p_kids)
boxplot(p_teenagers)
# boxplot(p_young_adults)
boxplot(p_working_professionals)
boxplot(p_senior_citizens)



qqnorm(p_toddlers, xlab = "p_toddlers")
qqline(p_toddlers, col = 2)

qqnorm(p_kids, xlab="p_kids")
qqline(p_kids, col = 2)

qqnorm(p_teenagers, xlab="p_teenagers")
qqline(p_teenagers)

qqnorm(p_working_professionals, xlab="p_working_professionals")
qqline(p_working_professionals, col = 2)

qqnorm(p_senior_citizens, xlab="p_senior_citizens")
qqline(p_senior_citizens, col = 2)


ks.test(p_toddlers, "pnorm", mean(p_toddlers), sd(p_toddlers))
ks.test(p_kids, "pnorm", mean(p_kids), sd(p_kids))
ks.test(p_teenagers, "pnorm", mean(p_teenagers), sd(p_teenagers))
ks.test(p_working_professionals, "pnorm", mean(p_working_professionals), sd(p_working_professionals))
ks.test(p_senior_citizens, "pnorm", mean(p_senior_citizens), sd(p_senior_citizens))

data_set$log_p_teenagers <- log(data_set$p_teenagers)
boxplot(data_set$log_p_teenagers)
qqnorm(data_set$log_p_teenagers)
qqline(data_set$log_p_teenagers)
ks.test(data_set$log_p_teenagers, "pnorm", mean(data_set$log_p_teenagers), sd(data_set$log_p_teenagers))

data_set$log_p_working_professionals <- log(data_set$p_working_professionals)
boxplot(data_set$log_p_working_professionals)
qqnorm(data_set$log_p_working_professionals)
qqline(data_set$log_p_working_professionals, col = 2)
ks.test(data_set$log_p_working_professionals, "pnorm", mean(data_set$log_p_working_professionals), sd(data_set$log_p_working_professionals))



data_set$log_p_senior_citizens <- log(data_set$p_senior_citizens)
boxplot(data_set$log_p_senior_citizens)
qqnorm(data_set$log_p_senior_citizens)
qqline(data_set$log_p_senior_citizens, col = 2)
ks.test(data_set$log_p_senior_citizens, mean(data_set$log_p_senior_citizens), sd(data_set$log_p_senior_citizens))
plotNormalHistogram(data_set$log_p_senior_citizens)
plotNormalHistogram(data_set$p_senior_citizens)
# plotNormalHistogram(p_toddlers)

# p_toddler
# p_kids
# p_senior_citizens

######################################################
######################################################
################### GENDER THEME #####################
######################################################
######################################################

boxplot(p_males)
boxplot(p_females)

qqnorm(p_males, xlab = "p_males")
qqline(p_males, col = 2)

qqnorm(p_females, xlab = "p_females")
qqline(p_females, col = 2)

data_set$log_p_males <- log(data_set$p_males)
data_set$log_p_females <- log(data_set$p_females)

data_set$sqrt_p_males <- sqrt(data_set$p_males)
data_set$sqrt_p_females <- sqrt(data_set$p_females)

boxplot(data_set$log_p_males)
boxplot(data_set$log_p_females)

qqnorm(data_set$log_p_males, xlab = "p_males")
qqline(data_set$log_p_males, col = 2)

qqnorm(data_set$log_p_females, xlab = "p_females")
qqline(data_set$log_p_females, col = 2)

# describe(data_set$log_p_males)
# describe(data_set$sqrt_p_males)

# describe(log_p_females)
# describe(sqrt_p_females)

ks.test(data_set$log_p_males, "pnorm", mean(data_set$log_p_males), sd(data_set$log_p_males))
ks.test(data_set$log_p_females, "pnorm", mean(data_set$log_p_females), sd(data_set$log_p_females))

data_set$sqrt_p_males <- sqrt(data_set$p_males)
data_set$sqrt_p_females <- sqrt(data_set$p_females)

boxplot(data_set$sqrt_p_males)
boxplot(data_set$sqrt_p_females)

qqnorm(data_set$sqrt_p_males)
qqline(data_set$sqrt_p_males)

qqnorm(data_set$sqrt_p_females)
qqline(data_set$sqrt_p_females)

ks.test(data_set$sqrt_p_males, "pnorm", mean(data_set$sqrt_p_males), sd(data_set$sqrt_p_males))
ks.test(data_set$sqrt_p_females, "pnorm", mean(data_set$sqrt_p_females), sd(data_set$sqrt_p_females))



#########################################################################
#########################################################################
###################### ECOMOMY THEME ####################################
#########################################################################
#########################################################################

boxplot(p_econ_active)
boxplot(p_econ_inactive, xlab = "p_econ_inactive", main = "Boxplot", medcol = "red", 
        boxlty = 0, whisklty = 1, staplelwd = 4, outpch = 8)
qqnorm(p_econ_active)
qqline(p_econ_active, col = 2)
qqnorm(p_econ_inactive)
qqline(p_econ_inactive, col = 2)
ks.test(p_econ_active, "pnorm", mean(p_econ_active), sd(p_econ_active))
ks.test(p_econ_inactive, "pnorm", mean(p_econ_inactive), sd(p_econ_inactive))

boxplot(p_males_econ_active)
boxplot(p_males_econ_inactive)
qqnorm(p_males_econ_active, xlab = "p_males_econ_active")
qqline(p_males_econ_active, col = 2)
qqnorm(p_males_econ_inactive, xlab = "p_males_econ_inactive")
qqline(p_males_econ_inactive, col = 2)
ks.test(p_males_econ_active, "pnorm", mean(p_males_econ_active), sd(p_males_econ_active))
ks.test(p_males_econ_inactive, "pnorm", mean(p_males_econ_inactive), sd(p_males_econ_inactive))

boxplot(p_females_econ_active)
boxplot(p_females_econ_inactive)
qqnorm(p_females_econ_active, xlab = "p_females_econ_active")
qqline(p_females_econ_active, col = 2)
qqnorm(p_females_econ_inactive, xlab = "p_females_econ_inactive")
qqline(p_females_econ_inactive, col = 2)
ks.test(p_females_econ_active, "pnorm", mean(p_females_econ_active), sd(p_females_econ_active))
ks.test(p_females_econ_inactive, "pnorm", mean(p_females_econ_inactive), sd(p_females_econ_inactive))

plotNormalHistogram(data_set$p_econ_active)
plotNormalHistogram(data_set$p_econ_inactive)
plotNormalHistogram(data_set$p_males_econ_active)
plotNormalHistogram(data_set$p_males_econ_inactive)
plotNormalHistogram(data_set$p_females_econ_active)
plotNormalHistogram(data_set$p_females_econ_inactive)

# log_p_males_econ_active
data_set$log_p_males_econ_active <- log(data_set$p_males_econ_active)
boxplot(data_set$log_p_males_econ_active)
qqnorm(data_set$log_p_males_econ_active)
qqline(data_set$log_p_males_econ_active, col = 2)
ks.test(data_set$log_p_males_econ_active, "pnorm", mean(data_set$log_p_males_econ_active), sd(data_set$log_p_males_econ_active))
plotNormalHistogram(data_set$log_p_males_econ_active)


# log_p_males_econ_inactive
data_set$log_p_males_econ_inactive <- log(data_set$p_males_econ_inactive)
boxplot(data_set$log_p_males_econ_inactive)
qqnorm(data_set$log_p_males_econ_inactive)
qqline(data_set$log_p_males_econ_inactive, col = 2)
ks.test(data_set$log_p_males_econ_inactive, "pnorm", mean(data_set$log_p_males_econ_inactive), sd(data_set$log_p_males_econ_inactive))
plotNormalHistogram(data_set$log_p_males_econ_inactive)


# log_p_females_econ_active
data_set$log_p_females_econ_active <- log(data_set$p_females_econ_active)
boxplot(data_set$log_p_females_econ_active)
qqnorm(data_set$log_p_females_econ_active)
qqline(data_set$log_p_females_econ_active, col = 2)
ks.test(data_set$log_p_females_econ_active, "pnorm", mean(data_set$log_p_females_econ_active), sd(data_set$log_p_females_econ_active))
plotNormalHistogram(data_set$log_p_females_econ_active)


# log_p_females_econ_inactive
data_set$log_p_females_econ_inactive <- log(data_set$p_females_econ_inactive)
boxplot(data_set$log_p_females_econ_inactive)
qqnorm(data_set$log_p_females_econ_inactive)
qqline(data_set$log_p_females_econ_inactive, col = 2)
ks.test(data_set$log_p_females_econ_inactive, "pnorm", mean(data_set$log_p_females_econ_inactive), sd(data_set$log_p_females_econ_inactive))
plotNormalHistogram(data_set$log_p_females_econ_inactive)

# p_econ_active
# p_econ_inactive
# log_p_males_econ_active
# log_p_males_econ_inactive
# log_p_females_econ_active
# log_p_females_econ_inactive

################################################################################
################################################################################
################## HEALTH ######################################################
################################################################################
################################################################################

boxplot(p_very_good_health)
boxplot(p_good_health)
boxplot(p_fair_health)
boxplot(p_bad_health, xlab = "p_bad_health")
boxplot(p_very_bad_health)

qqnorm(p_very_good_health, xlab = "p_very_good_health")
qqline(p_very_good_health, col = 2)

qqnorm(p_good_health, xlab = "p_good_health")
qqline(p_good_health, col = 2)

qqnorm(p_fair_health, xlab = "p_fair_health")
qqline(p_fair_health, col = 2)

qqnorm(p_bad_health, xlab = "p_bad_health")
qqline(p_bad_health, col = 2)

qqnorm(p_very_bad_health, xlab = "p_very_bad_health")
qqline(p_very_bad_health, col = 2)

ks.test(p_very_good_health, "pnorm", mean(p_very_good_health), sd(p_very_good_health))
ks.test(p_good_health, "pnorm", mean(p_good_health), sd(p_good_health))
ks.test(p_fair_health, "pnorm", mean(p_fair_health), sd(p_fair_health))
ks.test(p_bad_health, "pnorm", mean(p_bad_health), sd(p_bad_health))
ks.test(p_very_bad_health, "pnorm", mean(p_very_bad_health), sd(p_very_bad_health))

plotNormalHistogram(data_set$p_very_good_health, xlab = "p_very_good_health")
plotNormalHistogram(data_set$p_good_health)
plotNormalHistogram(data_set$p_fair_health, xlab = "p_fair_health")
plotNormalHistogram(data_set$p_bad_health)
plotNormalHistogram(data_set$p_very_bad_health)


# log_p_very_good_health
data_set$log_p_very_good_health <- log(data_set$p_very_good_health)
boxplot(data_set$log_p_very_good_health)
qqnorm(data_set$log_p_very_good_health)
qqline(data_set$log_p_very_good_health, col = 2)
ks.test(data_set$log_p_very_good_health, mean(data_set$log_p_very_good_health), sd(data_set$log_p_very_good_health))
plotNormalHistogram(data_set$log_p_very_good_health)

# log_p_good_health
data_set$log_p_good_health <- log(data_set$p_good_health)
boxplot(data_set$log_p_good_health)
qqnorm(data_set$log_p_good_health)
qqline(data_set$log_p_good_health, col = 2)
ks.test(data_set$log_p_good_health, mean(data_set$log_p_good_health), sd(data_set$log_p_good_health))
plotNormalHistogram(data_set$log_p_good_health)


# log_p_fair_health
data_set$log_p_fair_health <- log(data_set$p_fair_health)
boxplot(data_set$log_p_fair_health)
qqnorm(data_set$log_p_fair_health)
qqline(data_set$log_p_fair_health, col = 2)
ks.test(data_set$log_p_fair_health, mean(data_set$log_p_fair_health), sd(data_set$log_p_fair_health))
plotNormalHistogram(data_set$log_p_fair_health)


# log_p_very_bad_health
data_set$log_p_very_bad_health <- log(data_set$p_very_bad_health)
boxplot(data_set$log_p_very_bad_health)
qqnorm(data_set$log_p_very_bad_health)
qqline(data_set$log_p_very_bad_health, col = 2)
ks.test(data_set$log_p_very_bad_health, mean(data_set$log_p_very_bad_health), sd(data_set$log_p_very_bad_health))
plotNormalHistogram(data_set$log_p_very_bad_health)



# log_p_bad_health
data_set$log_p_bad_health <- log(data_set$p_bad_health)
boxplot(data_set$log_p_bad_health)
qqnorm(data_set$log_p_bad_health)
qqline(data_set$log_p_bad_health, col = 2)
ks.test(data_set$log_p_bad_health, mean(data_set$log_p_bad_health), sd(data_set$log_p_bad_health))
plotNormalHistogram(data_set$log_p_bad_health)


# log_p_very_good_health
# log_p_good_health
# log_p_fair_health
# log_p_bad_health
# log_p_very_bad_health

################################################################################
################################################################################
############# DISTANCE TRAVELLED ###############################################
################################################################################
################################################################################

boxplot(p_distance_0to5kms, xlab = "p_distancce_0to5kms")
boxplot(p_distance_5to10kms)
# boxplot(p_distance_10to20kms)
# boxplot(p_distance_20to30kms)
# boxplot(p_distance_30to40kms)
boxplot(p_work_from_home)

qqnorm(p_distance_0to5kms, xlab = "p_distance_0to5kms")
qqline(p_distance_0to5kms, col = 2)

qqnorm(p_distance_5to10kms)
qqline(p_distance_5to10kms)

# qqnorm(p_distance_10to20kms, xlab = "p_distance_10to20kms")
# qqline(p_distance_10to20kms, col = 2)
# 
# qqnorm(p_distance_20to30kms, xlab = "p_distance_20to30kms")
# qqline(p_distance_20to30kms, col = 2)
# 
# qqnorm(p_distance_30to40kms, xlab = "p_distance_30to40kms")
# qqline(p_distance_30to40kms, col = 2)

qqnorm(p_work_from_home, xlab = "p_work_from_home")
qqline(p_work_from_home, col = 2)

ks.test(p_distance_0to5kms, "pnorm", mean(p_distance_0to5kms), sd(p_distance_0to5kms))
ks.test(p_distance_5to10kms, "pnorm", mean(p_distance_5to10kms), sd(p_distance_5to10kms))
# ks.test(p_distance_10to20kms, "pnorm", mean(p_distance_10to20kms), sd(p_distance_10to20kms))
# ks.test(p_distance_20to30kms, "pnorm", mean(p_distance_20to30kms), sd(p_distance_20to30kms))
# ks.test(p_distance_30to40kms, "pnorm", mean(p_distance_30to40kms), sd(p_distance_30to40kms))
ks.test(p_work_from_home, "pnorm", mean(p_work_from_home), sd(p_work_from_home))

plotNormalHistogram(data_set$p_work_from_home)

data_set$log_p_distance_0to5kms <- log(data_set$p_distance_0to5kms)
data_set$log_p_distance_5to10kms <- log(data_set$p_distance_5to10kms)
data_set$log_p_work_from_home <- log(data_set$p_work_from_home)

# data_set$log_p_distance_10to20kms <- log(data_set$p_distance_10to20kms)
# data_set$log_p_distance_30to40kms <- log(data_set$p_distance_30to40kms)

boxplot(data_set$log_p_distance_0to5kms)
boxplot(data_set$log_p_distance_5to10kms)
boxplot(data_set$log_p_work_from_home)

# boxplot(data_set$log_p_distance_10to20kms)
# boxplot(data_set$log_p_distance_30to40kms)


qqnorm(data_set$log_p_distance_0to5kms)
qqline(data_set$log_p_distance_0to5kms, col = 2)
qqnorm(data_set$log_p_distance_5to10kms)
qqline(data_set$log_p_distance_5to10kms, col = 2)
qqnorm(data_set$log_p_work_from_home)
qqline(data_set$log_p_work_from_home, col = 2)



# qqnorm(data_set$log_p_distance_10to20kms)
# qqline(data_set$log_p_distance_10to20kms, col = 2)



# Check for missing or infinite values
# any(is.na(data_set$log_p_distance_30to40kms))  # Check for missing values
# any(!is.finite(data_set$log_p_distance_30to40kms))  # Check for infinite values
# data_set <- data_set[is.finite(data_set$log_p_distance_30to40kms), ]
# qqnorm(data_set$log_p_distance_30to40kms)
# qqline(data_set$log_p_distance_30to40kms, col = 2)


ks.test(data_set$log_p_distance_0to5kms, "pnorm", mean(data_set$log_p_distance_0to5kms), sd(data_set$log_p_distance_0to5kms))
ks.test(data_set$log_p_distance_5to10kms, "pnorm", mean(data_set$log_p_distance_5to10kms), sd(data_set$log_p_distance_5to10kms))


# ks.test(data_set$log_p_distance_30to40kms, "pnorm", mean(data_set$log_p_distance_30to40kms), sd(data_set$log_p_distance_30to40kms))


plotNormalHistogram(data_set$p_distance_0to5kms)
plotNormalHistogram(data_set$log_p_distance_0to5kms)

plotNormalHistogram(data_set$p_distance_5to10kms)
plotNormalHistogram(data_set$log_p_distance_5to10kms)

plotNormalHistogram(data_set$p_work_from_home)
plotNormalHistogram(data_set$log_p_work_from_home)

# plotNormalHistogram(data_set$p_distance_10to20kms)

# log_p_distance_0to5kms
# log_p_distance_5to10kms
# log_p_work_from_home

################################################################################

################################################################################
################################################################################
################# CORRELATION TEST #############################################
################################################################################
################################################################################


# correlation matrix

# select a dependent variable and independent variables

data_set2 <- data.frame(
                        data_set$p_total_deaths,
                        data_set$p_toddlers, 
                        data_set$p_kids, 
                        data_set$p_senior_citizens,
                        data_set$p_econ_active, 
                        data_set$p_econ_inactive, 
                        data_set$log_p_males_econ_active, 
                        data_set$log_p_males_econ_inactive,
                        data_set$log_p_females_econ_active, 
                        data_set$log_p_females_econ_inactive,
                        data_set$log_p_very_good_health, 
                        data_set$log_p_good_health,
                        data_set$log_p_very_bad_health, 
                        data_set$log_p_bad_health, 
                        data_set$log_p_fair_health,
                        data_set$log_p_distance_0to5kms, 
                        data_set$log_p_distance_5to10kms,
                        data_set$log_p_work_from_home
                        )

colnames(data_set2) <- c("total_deaths", "toddlers", "kids", "senior_citizens", "econ_active", 
                         "econ_inactive", "males_econ_active", "males_econ_inactive", "females_econ_active",
                         "females_econ_inactive", "very_good_health", "good_health" ,"very_bad_health", "bad_health",
                         "fair_health", "distance_0to5kms", "distance_5to10kms",
                         "work_from_home")


cor.matrix <- cor(data_set2, use = "pairwise.complete.obs", method = "pearson")
round(cor.matrix, digits = 2)
cor.df <- as.data.frame(cor.matrix)
View(cor.df)

pairs.panels(data_set2, method = "pearson", hist.col = "grey", col = "blue", main = "pearson")

library(corrgram)
# corrgram works best with Pearson correlation
corrgram(data_set2, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="All variables")





################################################################################
#### from the correlation diagram, select only those independent variables with high correlation

# data_set3 <- data.frame(data_set$p_toddlers, data_set$p_kids, data_set$log_p_working_professionals, 
#                         data_set$p_senior_citizens,
#                         data_set$p_econ_active, data_set$p_econ_inactive, 
#                         data_set$p_males_econ_active, data_set$p_males_econ_inactive,
#                         data_set$p_females_econ_active, data_set$p_females_econ_inactive,
#                         data_set$p_very_good_health,
#                         data_set$p_very_bad_health, data_set$p_bad_health, data_set$p_fair_health,
#                         data_set$p_distance_0to5kms, data_set$p_work_from_home)
# 
# colnames(data_set3) <- c("toddlers", "kids", "woring_prof", "senior_citizens", "econ_active",
#                          "econ_inactive", "males_econ_active", "males_econ_inactive", 
#                          "females_econ_active", "females_econ_inactive", "very_good_health", 
#                          "very_bad_health", "bad_health" ,"fair_health", "distance_0to5kms", "wfh")
# 
# cor3.matrix <- cor(data_set3, use = "pairwise.complete.obs", method = "pearson")
# round(cor3.matrix, digits = 2)
# cor3.df <- as.data.frame(cor3.matrix)
# View(cor3.df)
# 
# pairs.panels(data_set3, method = "pearson", hist.col = "grey", col = "blue", main = "pearson")
# 
# # corrgram works best with Pearson correlation
# corrgram(data_set3, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
#          upper.panel=panel.pie, text.panel=panel.txt, main="Selected variables")

################################################################################
################################################################################
###############  RETAIN HIGH CORRELATION OF DEP VAR



# p_kids
# p_senior_citizens
# p_econ_active
# p_econ_inactive
# p_males_econ_active
# p_males_econ_inactive
# p_females_econ_active
# p_females_econ_inactive
# p_very_good_health
# p_very_bad_health
# p_bad_health
# p_fair_health
# p_distance_0to5kms
# p_work_from_home


data_set3 <- data.frame(
                          # data_set$p_total_deaths,
                          data_set$p_toddlers, 
                          data_set$p_kids, 
                          data_set$p_senior_citizens,
                          data_set$p_econ_active, 
                          data_set$p_econ_inactive, 
                          data_set$log_p_males_econ_active, 
                          data_set$log_p_males_econ_inactive,
                          data_set$log_p_females_econ_active, 
                          data_set$log_p_females_econ_inactive,
                          data_set$log_p_very_good_health, 
                          # data_set$log_p_good_health,
                          data_set$log_p_very_bad_health, 
                          data_set$log_p_bad_health, 
                          data_set$log_p_fair_health,
                          data_set$log_p_distance_0to5kms,
                          # data_set$log_p_distance_5to10kms,
                          data_set$log_p_work_from_home
                        )



# corrgram works best with Pearson correlation
corrgram(data_set3, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="Selected independent variables (retaining only high correaltion)")


pcor.test(data_set$p_total_deaths, data_set$p_kids, data_set$p_toddlers, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_toddlers, data_set$p_kids, method = "pearson")
# retain p_kids (reason - lowest p-value)


pcor.test(data_set$p_total_deaths, data_set$log_p_working_professionals, data_set$p_work_from_home)
pcor.test(data_set$p_total_deaths, data_set$p_work_from_home, data_set$log_p_working_professionals)

cor.test(data_set$p_total_deaths, data_set$log_p_working_professionals, method = "pearson")
cor.test(data_set$p_total_deaths, data_set$p_work_from_home, method = "pearson")

### Retain 


pcor.test(data_set$p_total_deaths, data_set$p_econ_active, data_set$p_males_econ_active, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_males_econ_active, data_set$p_econ_active, method = "pearson")
#### Retain 


pcor.test(data_set$p_total_deaths, data_set$p_females_econ_active, data_set$p_females_econ_inactive, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_females_econ_inactive, data_set$p_females_econ_active, method = "pearson")
#### Retain 


pcor.test(data_set$p_total_deaths, data_set$p_males_econ_active, data_set$p_females_econ_inactive, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_females_econ_inactive, data_set$p_males_econ_active, method = "pearson")
#### Retain 

pcor.test(data_set$p_total_deaths, data_set$p_very_bad_health, data_set$p_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_bad_health, data_set$p_very_bad_health, method = "pearson")
#### Retain 



################################################################################
############# perform partial correlation  #####################################
################################################################################

library(ppcor)

pcor.test(data_set$p_total_deaths, data_set$p_toddlers, data_set$p_kids, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_kids, data_set$p_toddlers, method = "pearson")
# p_kids

pcor.test(data_set$p_total_deaths, data_set$log_p_working_professionals, data_set$p_very_bad_health)
pcor.test(data_set$p_total_deaths, data_set$p_very_bad_health, data_set$log_p_working_professionals)
# retain p_very_bad_health since the p-value is greater compared to the other one


# pcor.test(data_set$p_total_deaths, data_set$log_p_working_professionals, data_set$p_bad_health)
# pcor.test(data_set$p_total_deaths, data_set$p_bad_health, data_set$log_p_working_professionals)

pcor.test(data_set$p_total_deaths, data_set$log_p_working_professionals, data_set$p_work_from_home)
pcor.test(data_set$p_total_deaths, data_set$p_work_from_home, data_set$log_p_working_professionals)
# retain log_p_working_professionals

pcor.test()


pcor.test(data_set$p_total_deaths, data_set$p_econ_inactive, data_set$p_econ_active, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_econ_active, data_set$p_econ_inactive, method = "pearson")
#p_econ_active

pcor.test(data_set$p_total_deaths, data_set$p_econ_inactive, data_set$p_work_from_home, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_work_from_home, data_set$p_econ_inactive, method = "pearson")
#p_work_from_home

pcor.test(data_set$p_total_deaths, data_set$p_econ_inactive, data_set$p_males_econ_active, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_males_econ_active, data_set$p_econ_inactive, method = "pearson")
#p_males_econ_active

pcor.test(data_set$p_total_deaths, data_set$p_econ_inactive, data_set$p_males_econ_inactive, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_males_econ_inactive, data_set$p_econ_inactive, method = "pearson")
#p_econ_inactive

pcor.test(data_set$p_total_deaths, data_set$p_econ_inactive, data_set$p_females_econ_active, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_females_econ_active, data_set$p_econ_inactive, method = "pearson")
#p_females_econ_active

pcor.test(data_set$p_total_deaths, data_set$p_econ_inactive, data_set$p_females_econ_inactive, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_females_econ_inactive, data_set$p_econ_inactive, method = "pearson")
#p_females_econ_inactive


pcor.test(data_set$p_total_deaths, data_set$p_econ_inactive, data_set$p_very_good_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_very_good_health, data_set$p_econ_inactive, method = "pearson")
#p_very_good_health

pcor.test(data_set$p_total_deaths, data_set$p_econ_inactive, data_set$p_very_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_very_bad_health, data_set$p_econ_inactive, method = "pearson")
#p_very_bad_health

pcor.test(data_set$p_total_deaths, data_set$p_econ_inactive, data_set$p_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_bad_health, data_set$p_econ_inactive, method = "pearson")            
#p_bad_health


pcor.test(data_set$p_total_deaths, data_set$p_econ_active, data_set$p_males_econ_active, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_males_econ_active, data_set$p_econ_active, method = "pearson")
#p_econ_active

pcor.test(data_set$p_total_deaths, data_set$p_econ_active, data_set$p_males_econ_inactive, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_males_econ_inactive, data_set$p_econ_active, method = "pearson")
#p_econ_active

pcor.test(data_set$p_total_deaths, data_set$p_econ_active, data_set$p_females_econ_active, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_females_econ_active, data_set$p_econ_active, method = "pearson") ##
# p_econ_active

pcor.test(data_set$p_total_deaths, data_set$p_econ_active, data_set$p_females_econ_inactive, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_females_econ_inactive, data_set$p_econ_active, method = "pearson")
# p_econ_active

pcor.test(data_set$p_total_deaths,data_set$p_econ_active, data_set$p_very_good_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_very_good_health, data_set$p_econ_active, method = "pearson") 
#p_very_good_health

pcor.test(data_set$p_total_deaths, data_set$p_econ_active, data_set$p_very_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_very_bad_health, data_set$p_econ_active, method = "pearson")
#p_econ_very_bad_health

pcor.test(data_set$p_total_deaths, data_set$p_econ_active, data_set$p_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_bad_health, data_set$p_econ_active, method = "pearson")
# p_econ_bad_health

pcor.test(data_set$p_total_deaths, data_set$p_working_professionals, data_set$senior_citizens, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$senior_citizens, data_set$p_working_professionals, method = "pearson")
#p_working_professionals

pcor.test(data_set$p_total_deaths, data_set$p_males_econ_active, data_set$p_males_econ_inactive, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_males_econ_inactive, data_set$p_males_econ_active, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_males_econ_active, data_set$p_females_econ_active, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_females_econ_active, data_set$p_males_econ_active, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_males_econ_active, data_set$p_females_econ_inactive, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_females_econ_inactive, data_set$p_males_econ_active, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_males_econ_active, data_set$p_very_good_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_very_good_health, data_set$p_males_econ_active, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_males_econ_active, data_set$p_very_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_very_bad_health, data_set$p_males_econ_active, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_males_econ_active, data_set$p_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_bad_health, data_set$p_males_econ_active, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_econ_active, data_set$p_males_econ_active, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_males_econ_active, data_set$p_econ_active, method = "pearson")
# p_econ_active

pcor.test(data_set$p_total_deaths, data_set$p_econ_active, data_set$p_males_econ_inactive, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_males_econ_inactive, data_set$p_econ_active, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_econ_active, data_set$p_females_econ_active, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_females_econ_active, data_set$p_econ_active, method = "pearson") ##
# p_econ_active

pcor.test(data_set$p_total_deaths, data_set$p_econ_active, data_set$p_females_econ_inactive, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_females_econ_inactive, data_set$p_econ_active, method = "pearson")

pcor.test(data_set$p_total_deaths,data_set$p_econ_active, data_set$p_very_good_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_very_good_health, data_set$p_econ_active, method = "pearson") 

pcor.test(data_set$p_total_deaths, data_set$p_econ_active, data_set$p_very_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_very_bad_health, data_set$p_econ_active, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_econ_active, data_set$p_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_bad_health, data_set$p_econ_active, method = "pearson")



pcor.test(data_set$p_total_deaths, data_set$p_males_econ_inactive, data_set$p_females_econ_active, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_females_econ_active, data_set$p_males_econ_inactive, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_males_econ_inactive, data_set$p_females_econ_inactive, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_females_econ_inactive, data_set$p_males_econ_inactive, method = "pearson")
# p_females_econ_inactive

pcor.test(data_set$p_total_deaths, data_set$p_males_econ_inactive, data_set$p_very_good_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_very_good_health, data_set$p_males_econ_inactive, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_males_econ_inactive, data_set$p_very_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_very_bad_health, data_set$p_males_econ_inactive, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_males_econ_inactive, data_set$p_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_bad_health, data_set$p_males_econ_inactive, method = "pearson")


pcor.test(data_set$p_total_deaths, data_set$p_females_econ_active, data_set$p_females_econ_inactive, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_females_econ_inactive, data_set$p_females_econ_active, method = "pearson")
# p_females_eco_inactive

pcor.test(data_set$p_total_deaths, data_set$p_females_econ_active, data_set$p_very_good_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_very_good_health, data_set$p_females_econ_active, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_females_econ_active, data_set$p_very_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_very_bad_health, data_set$p_females_econ_active, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_females_econ_active, data_set$p_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_bad_health, data_set$p_females_econ_active, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_females_econ_inactive, data_set$p_very_good_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_very_good_health, data_set$p_females_econ_inactive, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_females_econ_inactive, data_set$p_very_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_very_bad_health, data_set$p_females_econ_inactive, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_females_econ_inactive, data_set$p_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_bad_health, data_set$p_females_econ_inactive, method = "pearson")


pcor.test(data_set$p_total_deaths, data_set$p_very_good_health, data_set$p_very_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_very_bad_health, data_set$p_very_good_health, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_very_good_health, data_set$p_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_bad_health, data_set$p_very_good_health, method = "pearson")


pcor.test(data_set$p_total_deaths, data_set$p_very_bad_health, data_set$p_bad_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_bad_health, data_set$p_very_bad_health, method = "pearson")
# p_bad_health

pcor.test(data_set$p_total_deaths, data_set$p_very_bad_health, data_set$p_work_from_home, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_work_from_home, data_set$p_very_bad_health, method = "pearson")

pcor.test(data_set$p_total_deaths, data_set$p_bad_health, data_set$p_work_from_home, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_work_from_home, data_set$p_bad_health, method = "pearson")


pcor.test(data_set$p_total_deaths, data_set$p_bad_health, data_set$p_fair_health, method = "pearson")
pcor.test(data_set$p_total_deaths, data_set$p_fair_health, data_set$p_bad_health, method =  "pearson")

################################################################################
############  AFTER PCOR  ######################################################
################################################################################


# Retain
# p_kids
# p_bad_health


################################################################################
############################### After PCOR

temp_dataset <- data.frame(
                            # data_set$p_total_deaths,
                            # data_set$p_toddlers, 
                            data_set$p_kids, 
                            data_set$p_senior_citizens,
                            # data_set$p_econ_active,
                            data_set$p_econ_inactive, 
                            # data_set$log_p_males_econ_active, 
                            # data_set$log_p_males_econ_inactive,
                            # data_set$log_p_females_econ_active,
                            # data_set$log_p_females_econ_inactive,
                            # data_set$log_p_very_good_health, 
                            # data_set$log_p_good_health,
                            # data_set$log_p_very_bad_health, 
                            data_set$log_p_bad_health, 
                            # data_set$log_p_fair_health,
                            data_set$log_p_distance_0to5kms,
                            # data_set$log_p_distance_5to10kms,
                            data_set$log_p_work_from_home
                                                     )


# temp_dataset <- data.frame(
#                             data_set$p_kids,
#                             data_set$p_senior_citizens,
#                             # data_set$p_econ_active,
#                             data_set$p_econ_inactive,
#                             # data_set$p_males_econ_inactive,
#                             data_set$p_females_econ_inactive,
#                             # data_set$p_very_bad_health,
#                             data_set$p_bad_health,
#                             data_set$p_distance_0to5kms,
#                             data_set$p_work_from_home
# )



# corrgram works best with Pearson correlation
corrgram(temp_dataset, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="Selected variables for temp dataset (afer pcor)")


# library(psych)
KMO(cor(temp_dataset))
# print(cor(temp_dataset))

# MSA = 0.55
# KMO(cor(data_set_b))





################################################################################
################################################################################
############## FACTOR ANALYSIS #################################################
################################################################################
################################################################################



# Determine Number of Factors to Extract
library(nFactors)

# get eigenvalues: eigen() uses a correlation matrix
ev <- eigen(cor(temp_dataset))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="blue", xlab="variables", ylab = "ev_values")

# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", xlab="number of components", ylab ="cumulative proportion")


# Varimax Rotated Principal Components
# retaining 'nFactors' components
library(GPArotation)

# principal() uses a data frame or matrix of correlations
fit <- principal(temp_dataset, nfactors = 3, rotate="varimax")
fit


# RC1 - p_econ_inactive, log_p_bad_health, log_p_work_from_home -> p_econ_inactive
# RC2 - p_kids, p_senior_citizens -> p_kids
# RC3 - log_p_distance_0to5kms -> log_p_distance_0to5kms


################################################################################
################################################################################
################# LINEAR REGRESSION ############################################
################################################################################
################################################################################

library(car)

############## WITH ALL THE VARIABLES

model1 <- lm(data_set$p_total_deaths ~ data_set$p_toddlers + data_set$p_kids + 
             data_set$p_senior_citizens + 
              data_set$p_econ_active + data_set$p_econ_inactive + 
              data_set$log_p_males_econ_active + data_set$log_p_males_econ_inactive + 
              data_set$log_p_females_econ_active + data_set$log_p_females_econ_inactive + 
              data_set$log_p_very_good_health + data_set$log_p_good_health + data_set$log_p_fair_health +
              data_set$log_p_bad_health + data_set$log_p_very_bad_health + data_set$log_p_distance_0to5kms +
              data_set$log_p_distance_5to10kms + data_set$log_p_work_from_home)



summary(model1)

hist(model1$residuals)
rug(model1$residuals)
# consider normality of residuals
plot(model1$residuals ~ model1$fitted.values, xlab = "Model1", ylab = "residuals", main = "Actual vs Predicted")
ks.test(model1$residuals, "pnorm", mean(model1$residuals), sd(model1$residuals))


plot(model1,which=2, xlab = "Model 1")


sqrt(vif(model1)) > 2


########### VARIABLES AFTER CORRELATION (BEFORE PCOR)
############## AFTER RETAINING THE HIGH CORRELATIONS 

model2 <- lm(data_set$p_total_deaths ~ 
               # data_set$p_toddlers +
               data_set$p_kids +
               # data_set$p_working_professionals + 
             data_set$p_senior_citizens +
             data_set$p_econ_active + 
               data_set$p_econ_inactive + 
             data_set$log_p_males_econ_active + 
               data_set$log_p_males_econ_inactive +
             data_set$log_p_females_econ_active + 
               data_set$log_p_females_econ_inactive +
             data_set$log_p_very_good_health +
             data_set$log_p_very_bad_health + 
               data_set$log_p_bad_health + 
               data_set$log_p_fair_health +
             data_set$log_p_distance_0to5kms + 
               data_set$log_p_work_from_home)

summary(model2)


hist(model2$residuals)
rug(model2$residuals)
# consider normality of residuals
plot(model2$residuals ~ model2$fitted.values, xlab = "Model2", ylab = "residuals", main = "Actual vs Predicted")
ks.test(model2$residuals, "pnorm", mean(model2$residuals), sd(model2$residuals))



plot(model2,which=2, xlab = "Model 2")


sqrt(vif(model2)) > 2

####################################### BEFORE FACTOR ANALYSIS (AFTER PCOR)
# temp_dataset

model3 <- lm(data_set$p_total_deaths ~ 
               data_set$p_kids + 
               data_set$p_senior_citizens +
               data_set$p_econ_inactive +
               # data_set$p_males_econ_inactive +
               # data_set$log_p_females_econ_inactive + 
               data_set$log_p_bad_health + 
               data_set$log_p_distance_0to5kms +
               data_set$log_p_work_from_home
             )

summary(model3)


hist(model3$residuals)
rug(model3$residuals)
# consider normality of residuals
plot(model3$residuals ~ model3$fitted.values, xlab = "Model3", ylab = "residuals", main = "Actual vs Predicted")
ks.test(model3$residuals, "pnorm", mean(model3$residuals), sd(model3$residuals))


plot(model3,which=2, xlab = "Model 3")


sqrt(vif(model3)) > 2

################################ AFTER FACTOR ANALYSIS


model4 <- lm(data_set$p_total_deaths ~ 
               data_set$p_kids + 
               data_set$p_senior_citizens +
               data_set$p_econ_inactive +
               # data_set$p_males_econ_inactive +
               # data_set$log_p_females_econ_inactive + 
               data_set$log_p_bad_health + 
               data_set$log_p_distance_0to5kms +
               data_set$log_p_work_from_home
               )

summary(model4)




hist(model4$residuals)
rug(model4$residuals)
plotNormalHistogram(model4$residuals)
# consider normality of residuals
plot(model4$residuals ~ model4$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model4$residuals, "pnorm", mean(model4$residuals), sd(model4$residuals))

plot(model4,which=2)


sqrt(vif(model4)) > 2

################################################################################
################################################################################
######################## ANOVA TESTS ###########################################
################################################################################
################################################################################

# anova(model1, model2, test = "F")
# anova(model1, model3, test = "F")
# 
# anova(model2, model3, test = "F")

anova(model1, model2, model3, test = "F")








