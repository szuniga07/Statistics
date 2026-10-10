################################################################################
#                                   Week 5                                     #
################################################################################

## Make a folder location so that you can load and save files:

#1. Replace my folder location with your location,
#   remember to use forward slashes, "/"
folder_location = "your_location"

#2. We'll paste "folder_location" with any file name, you can always
#   skip that and type the full location. Pasting them together is
#   not required, just a shortcut.


############
## Set up ##
############
#only need to install once, remove far left '#' to install
#install.packages("ham")
library(ham)


##############
## Get Data ##
##############

# 1. load CSV data (upload directly from a csv formatted file)
support3 <- read.csv( paste0(folder_location, "support3.csv") )
titanic3 <- read.csv( paste0(folder_location, "titanic3.csv") )
titanic3$survived <- ifelse(titanic3$survived == "Alive", 1, 0)

# Will create subsets below because of how packages/libraries handle
# missing data. See documentation to find your best option.


#######################
## Linear Regression ##
#######################
# simple linear regression model
model0 <- lm(slos ~ age, data=support3)
# Summary of the model results
summary(model0)

# trend line
plot(support3$age, support3$slos, pch=15, cex=.5, main="Regression of LOS on Age",
xlab="Age", ylab="LOS", cex.main=3, col="gray", xlim=c(18,100), ylim=c(3,40))
abline(model0, col="red", lwd=8)
abline(h=mean(support3$slos), col="gold", lwd=2)
abline(v=mean(support3$age), col="gold", lwd=2)

# object for the multiple linear regression model
model1 <- lm(slos ~ dzgroup + age + sex + meanbp + wblc + hrt + temp, data=support3)
# Summary of the model results
summary(model1)


#########################
## Logistic Regression ##
#########################

# object for the multiple linear regression model
lmodel1 <- glm(survived ~ age + sex + pclass + sibsp, data=titanic3)
# Summary of the model results
summary(lmodel1)


################################################################################
#                            ham package examples                              #
################################################################################
library(ham)

#####################
## Support Example ##
#####################

# multiple linear regression model
m01 <- assess(slos ~ dzgroup + age + sex + meanbp + wblc + hrt + temp,
              data=support3, regression="ols")
summary(m01$model)


# Variable importance
#Consider using these graphical parameters
par(mar=c(4.2, 2, 3.5, .55))
par(oma = c(0, 0, 0, 3))
plot(importance(m01$model), color="blue", lcolor="orange", cex=1.75)
par(mar=c(5.1, 4.1, 4.1, 2.1))
par(oma = c(0, 0, 0, 0))

# Graph the model effects
plot( review(m01$model), abbrv =5, lwd=3, cex.axis=1.5, pt.cex= 2)

# Graph the model effects, increasing continuous predictor values to understand more
plot( review(m01$model, increase=c(age= 10, wblc=10, hrt=25, temp=2)),
      abbrv =5, lwd=3, cex.axis=1.5, pt.cex= 2)

# partial prediction plots
par(mfrow=c(2, 2))
plot(m01, y= "age", col= 'purple', cex.main= "2", main="LOS by Age", cex.axis=2)
plot(m01, y= "hrt", col= 'purple', cex.main= "2", main="LOS by Heart", cex.axis=2)
plot(m01, y= "temp", col= 'purple', cex.main= "2", main="LOS by Temp", cex.axis=2)
plot(m01, y= "dzgroup", col= 'forestgreen', cex.main= "2", main="LOS by Disease", lwd=5, cex=2, tgt=17.9)
par(mfrow=c(1, 1))

# interpret results
interpret(m01)$model


#####################
## Titanic Example ##
#####################

lm01 <- assess(survived ~ age *sex+pclass+ sibsp,
               data = titanic3, regression="logistic")

summary(lm01$model)

# Variable importance
#Consider using these graphical parameters
par(mar=c(4.2, 2, 3.5, .55))
par(oma = c(0, 0, 0, 3))
plot(importance(lm01$model), color="blue", lcolor="orange", cex=1.75)
par(mar=c(5.1, 4.1, 4.1, 2.1))
par(oma = c(0, 0, 0, 0))

# Model summary
plot(x=review(lm01$model, increase=c(age=10)), xlim=c(0, 1.01),
     lwd=8, color="cyan", pcol="salmon", pt.cex=3)

# Review of passenger class
plot(lm01, y=c("pclass", "plogis"), add.legend="bottomleft", cex.legend= 2, lwd=6,
     cex=3, col= 'purple', cex.main= "2", main="Titanic Survival by Passenger Class")

# Reviewing the interaction
plot(lm01, y=c("age", "plogis", "sex"), add.legend="bottomleft", cex.legend= 2,
     tgt=.5, tgtcol="black", col= c('aquamarine',  'tomato'),
     cex.main= "2", main="Titanic Survival by Age per Gender")

# Get interpretations of results
interpret(lm01)$model

