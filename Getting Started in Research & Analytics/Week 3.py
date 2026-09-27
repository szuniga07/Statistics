################################################################################
#                                   Week 3                                     #
################################################################################
# Set up libraries

#pip install pandas
import pandas as pd
#pip install numpy
import numpy as np
#pip install statsmodels
import statsmodels.stats.power as smp

## Make a folder location so that you can load and save files:

#1. Replace my folder location with your location, 
#   remember to use forward slashes, "/" 
folder_location = "C:/Users/szuni/Documents/kp/Education/Getting started in research/"
folder_location

#2. We'll paste "folder_location" with any file name, you can always 
#   skip that and type the full location. Pasting them together is 
#   not required, just a shortcut.

##############
## Get Data ##
##############

# 1. load CSV data (upload directly from a csv formatted file)
lbw = pd.read_csv(folder_location + "lbw.csv")
lbw.head()

#####################
## Power analysis  ##
#####################

############
## t-test ##
############

## Sample Size  independent t-test ##

# Calculate Cohen's d (Effect Size) Effect/pooled SD
effect_size = 300 / 700
# Initialize the power analysis object
power_analysis = smp.TTestIndPower()
# Power Analysis
required_n = power_analysis.solve_power(
    effect_size=effect_size, 
    alpha= 0.05, 
    power= 0.80, 
    ratio=1.0,            # 1.0 means equal group sizes
    alternative='two-sided'
)
#Required sample size per group, round up
print(required_n)


## Power Level independent t-test ##

# Calculate Cohen's d (Effect Size) Effect/pooled SD
effect_size = 300 / 700
# get the harmonic mean for unequal group sizes
round((2 * (74 * 115)) / (74 + 115))
# Initialize the power analysis object
power_analysis = smp.TTestIndPower()
# Power Analysis
required_n = power_analysis.solve_power(
    effect_size=effect_size, 
    alpha= 0.05, 
    nobs1= 90, 
    ratio=1.0,            # 1.0 means equal group sizes
    alternative='two-sided'
)
# Power level 
print(required_n)


######################
## Chi-square test  ##
######################

###############
## Example 1 ##
###############

# proportion of smokers and non-smokers moms with low birth weight babies
# 0.41 and 0.25

# Get sample size

# Calculate Cohen's h (effect size for two independent proportions)
cohen_h = 2 * (np.arcsin(np.sqrt(0.41)) - np.arcsin(np.sqrt(0.25)))

# Power analysis
analysis = smp.NormalIndPower()
required_n = analysis.solve_power(
    effect_size= cohen_h, 
    alpha= 0.05, 
    power= 0.80, 
    alternative= 'two-sided'
)

# required sample size per group
print(required_n)


###############
## Example 2 ##
###############

# proportion of smokers and non-smokers moms with low birth weight babies
# 0.41 and 0.25

# get the harmonic mean for unequal group sizes
round((2 * (74 * 115)) / (74 + 115))

# Get power level

# Calculate Cohen's h (effect size for two independent proportions)
cohen_h = 2 * (np.arcsin(np.sqrt(0.41)) - np.arcsin(np.sqrt(0.25)))

# Power analysis
analysis = smp.NormalIndPower()
required_n = analysis.solve_power(
    effect_size= abs(cohen_h), 
    alpha= 0.05, 
    nobs1= 90, 
    alternative= 'two-sided'
)

# required sample size per group
print(required_n)

