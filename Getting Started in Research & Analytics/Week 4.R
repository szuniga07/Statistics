sz################################################################################
#                                   Week 4                                     #
################################################################################

## Make a folder location so that you can load and save files:

#1. Replace my folder location with your location,
#   remember to use forward slashes, "/"
folder_location <- "your_location"

#2. We'll paste "folder_location" with any file name, you can always
#   skip that and type the full location. Pasting them together is
#   not required, just a shortcut.


############
## Set up ##
############
#only need to install once, remove far left '#' to install
#install.packages("rms")
#install.packages(c("psych", "GPArotation"))

library(rms)  #for the cluster analysis section
library(psych)
library(ham)
library(GPArotation)


##############
## Get Data ##
##############

# 1. load CSV data (upload directly from a csv formatted file)
support3 <- read.csv( paste0(folder_location, "support3.csv") )
ebp <- read.csv( paste0(folder_location, "ebp.csv") )

# Will create subsets below because of how packages/libraries handle
# missing data. See documentation to find your best option.


############
## Recode ##
############
# Convert string responses to numeric variables for EBP data
#Create new variables
ebp$q0002r <- NA
ebp$q0006r <-  NA
ebp$q0008r <-  NA
ebp$q0013r <-  NA
ebp$q0015r <-  NA
ebp$q0026r <-  NA
ebp$q0027r <-  NA
ebp$q0028r <-  NA
# Recode values
ebp[, "q0002r"][ebp$q0002 == 'Strongly Agree'] <- 5
ebp[, "q0002r"][ebp$q0002 == 'Agree'] <- 4
ebp[, "q0002r"][ebp$q0002 == 'Uncertain'] <- 3
ebp[, "q0002r"][ebp$q0002 == 'Disagree'] <- 2
ebp[, "q0002r"][ebp$q0002 == 'Strongly Disagree'] <- 1

ebp[, "q0006r"][ebp$q0006 == 'Strongly Agree'] <- 5
ebp[, "q0006r"][ebp$q0006 == 'Agree'] <- 4
ebp[, "q0006r"][ebp$q0006 == 'Uncertain'] <- 3
ebp[, "q0006r"][ebp$q0006 == 'Disagree'] <- 2
ebp[, "q0006r"][ebp$q0006 == 'Strongly Disagree'] <- 1

ebp[, "q0008r"][ebp$q0008 == 'Strongly Agree'] <- 5
ebp[, "q0008r"][ebp$q0008 == 'Agree'] <- 4
ebp[, "q0008r"][ebp$q0008 == 'Uncertain'] <- 3
ebp[, "q0008r"][ebp$q0008 == 'Disagree'] <- 2
ebp[, "q0008r"][ebp$q0008 == 'Strongly Disagree'] <- 1

ebp[, "q0013r"][ebp$q0013 == 'Strongly Agree'] <- 5
ebp[, "q0013r"][ebp$q0013 == 'Agree'] <- 4
ebp[, "q0013r"][ebp$q0013 == 'Uncertain'] <- 3
ebp[, "q0013r"][ebp$q0013 == 'Disagree'] <- 2
ebp[, "q0013r"][ebp$q0013 == 'Strongly Disagree'] <- 1

ebp[, "q0015r"][ebp$q0015 == 'Strongly Agree'] <- 5
ebp[, "q0015r"][ebp$q0015 == 'Agree'] <- 4
ebp[, "q0015r"][ebp$q0015 == 'Uncertain'] <- 3
ebp[, "q0015r"][ebp$q0015 == 'Disagree'] <- 2
ebp[, "q0015r"][ebp$q0015 == 'Strongly Disagree'] <- 1

ebp[, "q0026r"][ebp$q0026 == 'Strongly Agree'] <- 5
ebp[, "q0026r"][ebp$q0026 == 'Agree'] <- 4
ebp[, "q0026r"][ebp$q0026 == 'Uncertain'] <- 3
ebp[, "q0026r"][ebp$q0026 == 'Disagree'] <- 2
ebp[, "q0026r"][ebp$q0026 == 'Strongly Disagree'] <- 1

ebp[, "q0027r"][ebp$q0027 == 'Strongly Agree'] <- 5
ebp[, "q0027r"][ebp$q0027 == 'Agree'] <- 4
ebp[, "q0027r"][ebp$q0027 == 'Uncertain'] <- 3
ebp[, "q0027r"][ebp$q0027 == 'Disagree'] <- 2
ebp[, "q0027r"][ebp$q0027 == 'Strongly Disagree'] <- 1

ebp[, "q0028r"][ebp$q0028 == 'Strongly Agree'] <- 5
ebp[, "q0028r"][ebp$q0028 == 'Agree'] <- 4
ebp[, "q0028r"][ebp$q0028 == 'Uncertain'] <- 3
ebp[, "q0028r"][ebp$q0028 == 'Disagree'] <- 2
ebp[, "q0028r"][ebp$q0028 == 'Strongly Disagree'] <- 1

## Cronbach's alpha with the ham pacakge

Factor2 <- alpha(items=c("q0026r", "q0027r", "q0028r"), data=ebp)
Factor2
interpret(Factor2)


#####################
## Cluster Analysis##
#####################

#create gender variable
support3$gender_1 <- ifelse(support3$sex == "female", 1, 0 )

# Conduct cluster analysis
cluster_anlys <- varclus( ~wblc + gender_1 + meanbp + age + hrt, data= support3)

## Dendogram ##
plot(cluster_anlys)


######################
## Cronbach's alpha ##
######################

alpha(items=c("q0026r","q0027r","q0028r"), data=ebp)

# Get interpretations
interpret(alpha(items=c("q0026r","q0027r","q0028r"), data=ebp))


#####################
## Factor Analysis ##
#####################

# Run Exploratory Factor Analysis (EFA)
# Extracts 2 factors using Principal Axis factoring and Varimax rotation
fa_1 <- fa(ebp[, c("q0002r","q0006r","q0008r","q0013r","q0015r","q0026r","q0027r","q0028r")],
           nfactors = 2, rotate = "varimax", fm = "pa")

# View the factor loadings, 0.4 correlated threshold, and summary
print(fa_1, cut = 0.4, sort = TRUE)

