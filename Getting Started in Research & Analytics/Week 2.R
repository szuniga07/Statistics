################################################################################
#                                   Week 2                                     #
################################################################################

## Make a folder location so that you can load and save files:

#1. Replace my folder location with your location, 
#   remember to use forward slashes, "/" 
folder_location <- "G:/Steve/Stats with Steve/Course/Getting started in research/Data/"

#2. We'll paste "folder_location" with any file name, you can always 
#   skip that and type the full location. Pasting them together is 
#   not required, just a shortcut.

#if using the ham package, run the line below
library(ham)

##############
## Get Data ##
##############

titanic3 <- read.csv(paste0(folder_location, "titanic3.csv"))
los2 <- read.csv(paste0(folder_location, "los2.csv"))
hosprog <- read.csv( paste0(folder_location, "hosprog.csv") )


###############
## Inference ##
###############

## Example ##

# Get average ages and confidence intervals by passenger classes
gr1 <- group(x="pclass", y="age", data=titanic3, dist="t")
#View results
gr1$Group.CI$adf_numeric # prints results

# Create a graph of point estimates and confidence intervals
# the x11 and par add graphing options to view better

  x11(width=16, height=10) #makes a special graph window
  par(mar=c(4.2, 4.5, 3.5, 4))
  plot(x=gr1, y="group", overall=TRUE, oband=TRUE, 
       lwd=4, cex=2, cex.main=2, cex.lab=2, cex.axis=1.5 ) # graph display
  dev.off()  #This closes the special graph window


## Mean vs Median ##
  par(mfrow=c(1,2))
  #Hospital A's mean, median, standard deviation
  hos_A <- summary(los2$LOS[los2$Hospital=="A"])
  hos_B <- summary(los2$LOS[los2$Hospital=="B"])
  A_sd <- sd(los2$LOS[los2$Hospital=="A"])
  B_sd <- sd(los2$LOS[los2$Hospital=="B"])
  
  hist(los2$LOS[los2$Hospital=="A"],
       main= "Hospital A", xlab="LOS", col="red",
       sub=paste("Mean=", round(hos_A[4], 1), 
                 "Median=", round(hos_A[3], 1),
                 "SD=", round(A_sd, 1)) )
  hist(los2$LOS[los2$Hospital=="B"],
       main= "Hospital B", xlab="LOS", col="blue", 
       sub=paste("Mean=", round(hos_B[4], 1), 
                 "Median=", round(hos_B[3], 1),
                 "SD=", round(B_sd, 1)) )
  par(mfrow=c(1,1))  #return display back to normal
  
  # Examine 2 hospital's trend over time for LOS
gr2 <- group(x="Hospital", y="LOS", z="Month", data=los2, dist="t")
plot(x=gr2, y="time", gcol=c("red","blue"), lwd=6 )


########################
## Distribution tests ##
########################

## t-test 
t.test(age ~ survived, data=titanic3) # p=0.08

## ANOVA test
anova_test <- aov(age ~ pclass, data=titanic3) # p < 0.001
summary(anova_test)

## Example of distributions that are significantly different p= 0.03
t.test(cost ~ program, data=hosprog)

int_mean <- mean(hosprog$cost[hosprog$program == 1])
ctl_mean <- mean(hosprog$cost[hosprog$program == 0])
int_mean
ctl_mean

hist(hosprog$cost, prob=T, xlim=c(0,30000), 
     main="Histogram of Cost: Int= $9,013, Ctl= $9,640 (p= 0.03)")
#This adds density lines to show the distributions
lines(density(hosprog$cost[hosprog$program == 1]), 
      col = "blue", lwd = 3) 
lines(density(hosprog$cost[hosprog$program == 0]), 
      col = "red", lwd = 3)
legend("topright", legend=c("Intervention", "Controls" ), 
       lty=1, lwd=1, col= c("blue", "red"), bty="n")

## Chi-square test
surv_table <- table(titanic3$survived, titanic3$sex) # 2x2 table
# Run the Chi-Square test
chisq.test(surv_table) # significant results

## t-test vs Mann U test
# t-test 
t.test(LOS ~ Hospital, data=los2)

# Mann-Whitney test for non-normal distributions (non-parametric test)
wilcox.test(LOS ~ Hospital, data=los2)

