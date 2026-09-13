################################################################################
#                                   Week 1                                     #
################################################################################

## Make a folder location so that you can load and save files:

#1. Replace my folder location with your location,
#   remember to use forward slashes, "/"
folder_location <- "C:/temp/python/"

#2. We'll paste "folder_location" with any file name, you can always
#   skip that and type the full location. Pasting them together is
#   not required, just a shortcut.


##############
## Get Data ##
##############
# 2 options

# 1. load with the ham package
#install.packages("ham")  #only need to run this command once
library(ham)

# 2. load CSV data (upload directly from a csv formatted file)
write.csv(hosprog, row.names = FALSE,
          file= paste0(folder_location, "hosprog.csv") )
hosprog <- read.csv( paste0(folder_location, "hosprog.csv") )


###################
## Distributions ##
###################

# Summary statistics #

# We usually review the average (or mean) values
mean(hosprog$los, na.rm=TRUE)
# na.rm=TRUE lets us get results when we have missing data
median(hosprog$los, na.rm=TRUE)

# In addition to describing the "center", we also want to discuss the "spread"
sd(hosprog$los, na.rm=TRUE)  #standard deviation

# We can get more info about the distribution with the quartiles and mean
summary(hosprog$los)


## Graphs ##

hist(hosprog$los)  # This is a histogram

# R plots have a variety of display options like changing the main title
hist(hosprog$los, main = "Histogram of Hospital Length of Stay")

# Adding a density curve
hist(hosprog$los, main = "Histogram with a Density Curve", prob=TRUE)
lines(density(hosprog$los), col = "red", lwd = 2)  # Line color & width

# The density curve gives us something similar to what is called the
# "probability distributiob function" (PDF)

# A real PDF is what we use to calculate probabilities, significance
# tests, and cumulative distribution functions (CDF). These are the
# basis of many important statistical properties.

###############
## Normality ##
###############

# There are a couple of options when assessing "normality" or
# how normal the distribution looks. In other words, how close
# to a bell curve it looks like.

#1. Visual inspection: This graph shows a positive-skew with
#   the "outliers" pulling the distribution to the right.
#   A negative-skew would be in the opposite direction.
#2. Kolmogrov-Smirnov normality test. This is not a commonly
#   used test but tests how different it is from the bell-shape.
#3. Q-Q plots: This is a way to see how the actual distribution
#   matches a theoretical normal distribution.


#1. Visual inspection

# -- DESCRIBE SKEWNESS AND KURTOSIS HERE
hist(hosprog$los, main = "Histogram with a Normal Curve", prob=TRUE)
#lines(density(hosprog$los), col = "red", lwd = 2)  # Line color & width
curve(dnorm(x, mean = mean(hosprog$los), sd = sd(hosprog$los)),
      from = 0, to = 14, add = TRUE, col = "blue", lwd = 2,
      main = "Normal Distribution")

# We clearly see a positive-skew or right-tailed distribution
#with outliers pulled to the right.
# It is less obvious but there is some higher levels of kurtosis
# because some of the histogram bars are clumped around the center
# giving a slightly more "tall and thin" appearance (i.e., tree vs bush).


#2. Kolmogrov-Smirnov normality test
# We will have small values so setting an optional scientific
# notation level so I get all the digits
options(scipen=20)
#Significantly different from normal shape
ks.test(hosprog$los, "pnorm", mean = 0, sd = 1)

# I can set it to a more reasonable number
options(scipen=5)


#3. Q-Q plot
qqnorm(hosprog$los, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
#Adds a 45 degree reference line for an expected normal distribution
qqline(hosprog$los, col = "red", lwd = 2) # Adds the 45-degree reference line

# The flatness on the left side (more horizontal) suggests more data in
# that part of the distribution. The steeper increase on the right side
# (more vertical) suggests more of a tail on the right or positively
# skewed data. In the middle, the data falls along the theoretical
# quantiles so it looks more normal.


###################################
## Correlations and Scatterplots ##
###################################

# These are all Pearson correlations but there are other
# correlations with methodological advantages (e.g., Spearman)
# and have slightly different formulas.
# Pearson correlations are the most widely used correlations.

#1. Very strong positive correlation between LOS and cost
cor.test(hosprog$los, hosprog$cost, use = "complete.obs",
         method = "pearson" )

# Let's take a look
plot(hosprog$los, hosprog$cost )

# "pearson" is the default correlation so we can drop it from
# the code use = "complete.obs" is to handle missing data

#2. Strong negative correlation between LOS and survey results
cor.test(hosprog$los, hosprog$survey, use = "complete.obs" )
plot(hosprog$los, hosprog$survey )

#3. Weak negative correlation between LOS and age but statistically significant
cor.test(hosprog$los, hosprog$age, use = "complete.obs" )
plot(hosprog$los, hosprog$age )

# You can do point-biseral correlations which are correlations
# between continuous and binary variables.

#4. Moderate positive correlation between LOS and 30-day readmissions
cor.test(hosprog$los, hosprog$rdm30, use = "complete.obs" )
plot(hosprog$los, hosprog$rdm30 )
# We see that when it comes to patients with readmissions,
# they had much longer lengths of stay

## Make a graph with all 4 graphs in 1 #

# Change the graph settings
par(mfrow= c(2,2))

#Run the scatter plots and use the correlation tests in the text
#Storing the results as an "object" so I can use later
#1.
cor1 <- cor.test(hosprog$los, hosprog$cost, use = "complete.obs",
                 method = "pearson")
plot(hosprog$los, hosprog$cost, main= "LOS x Cost (strong +)",
     xlab= "LOS", ylab= "Cost", col="slateblue",
     sub= paste("Correlation= ", round(cor1$estimate, 3), #paste text &
                "p= ", round(cor1$p.value, 3)) )  # numbers for subtitle
#2.
cor2 <- cor.test(hosprog$los, hosprog$survey, use = "complete.obs",
                 method = "pearson" )
plot(hosprog$los, hosprog$survey, main= "LOS x Survey (strong -)",
     xlab= "LOS", ylab= "Survey", col="slateblue",
     sub= paste("Correlation= ", round(cor2$estimate, 3), #paste text &
                "p= ", round(cor2$p.value, 3)) )  # numbers for subtitle
#3.
cor3 <- cor.test(hosprog$los, hosprog$age, use = "complete.obs",
                 method = "pearson" )
plot(hosprog$los, hosprog$age, main= "LOS x Age (weak -)",
     xlab= "LOS", ylab= "Age", col="slateblue",
     sub= paste("Correlation= ", round(cor3$estimate, 3), #paste text &
                "p= ", round(cor3$p.value, 3)) )  # numbers for subtitle
#4.
cor4 <- cor.test(hosprog$los, hosprog$rdm30, use = "complete.obs",
                 method = "pearson" )
plot(hosprog$los, hosprog$rdm30, main= "LOS x Readmission (moderate +)",
     xlab= "LOS", ylab= "Readmission", col="slateblue",
     sub= paste("Correlation= ", round(cor4$estimate, 3), #paste text &
                "p= ", round(cor4$p.value, 3)) )  # numbers for subtitle

# Return the graph settings back to normal
par(mfrow= c(1, 1))



########################
## Correlation matrix ##
########################
hos_cor <- cor(hosprog[, c("survey", "los","cost","rdm30","death30","female","age")],
               use = "complete.obs", method = "pearson")
print(hos_cor, digits=2)
# digits=2 is the number of significant digits, not rounding digits

