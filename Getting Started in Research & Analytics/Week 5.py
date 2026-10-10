################################################################################
#                                   Week 5                                     #
################################################################################
# Set up libraries
# interactive session
#%%

# only need to run pip commands once and add # comment after
#pip install pandas
#pip install numpy
#pip install scikit-learn
#pip install pingouin
#pip install factor-analyzer

import pandas as pd
import numpy as np
import statsmodels.stats.power as smp
import statsmodels.formula.api as smf
import statsmodels.api as sm

## Make a folder location so that you can load and save files:

#1. Replace my folder location with your location, 
#   remember to use forward slashes, "/" 
folder_location = "your_location"

#2. We'll paste "folder_location" with any file name, you can always 
#   skip that and type the full location. Pasting them together is 
#   not required, just a shortcut.

##############
## Get Data ##
##############

# 1. load CSV data (upload directly from a csv formatted file)
support3 = pd.read_csv( folder_location +  "support3.csv") 
support3.head()
titanic3 = pd.read_csv( folder_location + "titanic3.csv") 
titanic3.head()

# Will create subsets below because of how packages/libraries handle
# missing data. See documentation to find your best option.


#######################
## Linear Regression ##
#######################
# simple linear regression model
model0 = smf.ols("slos ~ age", data=support3).fit()
# Summary of the model results
print(model0.summary())

# object for the multiple linear regression model
model1 = smf.ols("slos ~ dzgroup + age + sex + meanbp + wblc + hrt + temp", data=support3).fit()
# Summary of the model results
print(model1.summary())


#########################
## Logistic Regression ##
#########################

# object for the multiple logistic regression model
lmodel1 = smf.glm('survived ~ age + sex + pclass', data=titanic3, family=sm.families.Binomial()).fit()
# Summary of the model results
print(lmodel1.summary())

