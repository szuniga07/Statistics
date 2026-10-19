################################################################################
#                                   Week 6                                     #
################################################################################
# interactive session
#%%
# Set up libraries

# only need to run pip commands once and add # comment after
#pip install pandas
#pip install numpy
#pip install scikit-learn
#pip install pingouin
#pip install factor-analyzer
#pip install psmpy

import pandas as pd
import numpy as np
import statsmodels.stats.power as smp
import statsmodels.formula.api as smf
import statsmodels.api as sm
from sklearn.linear_model import LogisticRegression
# psmpy
from psmpy import PsmPy
from psmpy.functions import cohenD
from psmpy.plotting import *
sns.set(rc={'figure.figsize':(10,8)}, font_scale = 1.3)

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

# Will create subsets below because of how packages/libraries handle
# missing data. See documentation to find your best option.


############################
## Propensity score model ##
############################

# Rename the column to run in the model
support3 = support3.rename(columns={'num.co': 'num_co'})
support3.head()

# object for the logistic regression model
propensity_score = smf.glm('program ~ age + sex + num_co + dzgroup', 
                           data=support3, family=sm.families.Binomial()).fit()
# Summary of the model results
print(propensity_score.summary())

# Extract propensity scores and add them back to the data
support3['ps'] = propensity_score.predict(support3)


##############
## Matching ##
##############
# Make dataset with main predictors
sup3 = support3[[ 'program','age', 'num_co', 'sex', 'dzgroup','id']] 
sup3.head()

# Initialize the PsmPy class
psm = PsmPy(sup3, treatment='program', indx='id', exclude = ['sex', 'dzgroup'])
# Calculate the propensity score
psm.logistic_ps(balance = True)
# Perform K Nearest Neighbor matching
psm.kdtree_matched(matcher='propensity_logit', replacement=False, caliper=None, drop_unmatched=True)
# Create a matched dataset
sup3_match = psm.df_matched
# Remove cases that were not matched
msupport3 = support3[support3['id'].isin(sup3_match['id'])]
# Number of rows and columns of matched data frame
print(msupport3.shape)

##############
## Analyses ##
##############

# Standard regression
model0 = smf.ols("charges ~ program", data=support3).fit()
print(model0.summary2(float_format="%.4f"))
# Matched analysis--same results with an independent t-test
match1 = smf.ols("charges ~ program", data=msupport3).fit()
print(match1.summary2(float_format="%.4f"))
# Covariate adjustment with the propensity score
mps = smf.ols("charges ~ program + ps", data=support3).fit()
print(mps.summary2(float_format="%.4f"))

# Create the IPW weight
support3['ipw'] = np.where(
    support3['program'] == 1, 1 / support3['ps'], 
    1 / (1 - support3['ps'])
)
# Using inverse probability weights based on the propensity score
mipw = smf.wls("charges ~ program ", data=support3, 
               weights=support3["ipw"]).fit()
print(mipw.summary2(float_format="%.4f"))

## The Matched data regression (i.e., ind. samp. t-test) amd 
## IPW weight had results closer to a basic model with little
## impact from the propensity score. The logistic regression
## used in the propensity model and matching options can be
## modified for improved results.

## The covariate adjustment with the propensity score provides
## the most conservative results. These may be most useful but
## adjusting the models above could provide model improvement.

# %%
