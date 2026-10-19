################################################################################
#                                   Week 6                                     #
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
library(MatchIt)

##############
## Get Data ##
##############

# 1. load CSV data (upload directly from a csv formatted file)
support3 <- read.csv( paste0(folder_location, "support3.csv") )


############################
## Propensity score model ##
############################

# Estimate the propensity score (probability of treatment)
propensity_score <- glm(program ~ age + sex + num.co + dzgroup,
                data = support3,
                family = binomial(link = "logit"))

# Extract propensity scores and add them back to the data
support3$ps <- predict(propensity_score, type = "response")

##############
## Matching ##
##############

# Matching
matched_data <- matchit(program ~ age + sex + num.co + dzgroup,
                        data = support3,
                        method = "nearest",
                        distance = "mahalanobis",
                        replace = TRUE)

# Get a summary of the matches and a check on balance
summary(matched_data)

# Create a new data frame of just the matched data
msupport3 <- match.data(matched_data)


##############
## Analyses ##
##############

# Standard regression
model0 <- assess(charges ~ program , data=support3, regression="ols")
# Matched analysis--same results with an independent t-test
match1 <- assess(charges ~ program , data=msupport3, regression="ols")
# Covariate adjustment with the propensity score
mps <- assess(charges ~ program , data=support3, intervention = "program",
               regression="ols", propensity=c("age", "sex", "num.co", "dzgroup"),
               newdata=TRUE)
# Using inverse probability weights based on the propensity score
mipw <- assess(charges ~ program, data=support3, intervention = "program",
               regression="ols", propensity=c("age", "sex", "num.co", "dzgroup"),
               newdata=TRUE, weights="ipw")
support4 <- mipw$newdata
# Using normalized inverse probability weights based on the propensity score
mnipw <- assess(charges ~ program, data=support3, intervention = "program",
               regression="ols", propensity=c("age", "sex", "num.co", "dzgroup"),
               newdata=TRUE, weights="nipw")
# Using Average Treatment Effect on the Treated weights based on the propensity score
matt <- assess(charges ~ program, data=support3, intervention = "program",
               regression="ols", propensity=c("age", "sex", "num.co", "dzgroup"),
               newdata=TRUE, weights="att")


##############
## Results  ##
##############
#Compare 6 patient propensity scores and weights
id6 <- c(5,9105,3,9104, 6, 9102)
signif(head(mipw$newdata[id6, c("program","pscore","ipw","nipw", "att")]), 3)

#Compare weight scores
head(mipw$newdata)
tail(mipw$newdata)

#Create a summary table of matching results
program_effects <-  round(c(model0$model[[1]][2], match1$model[[1]][2],
                      mps$model[[1]][2], mipw$model[[1]][2],
                      mnipw$model[[1]][2], matt$model[[1]][2]), 0)
matching_results <- data.frame("Savings"=program_effects, "p.value"="p<0.001")
#Sample size
matching_results$N <- c(9105, 6471,9105,9105,9105,9105)
rownames(matching_results) <- c("None", "Matched","Propensity", "IPW", "NIPW", "ATT")

matching_results

