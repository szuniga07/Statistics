################################################################################
#                                   Week 2                                     #
################################################################################
# Set up libraries
import statistics

#pip install pandas
import pandas as pd

#pip install numpy
import numpy as np

#pip install matplotlib
import matplotlib.pyplot as plt

#pip install seaborn
import seaborn as sns

#pip install scipy
from scipy import stats
from scipy.stats import f_oneway
from scipy.stats import median_test
from scipy.stats import chi2_contingency


## Make a folder location so that you can load and save files:

#1. Replace my folder location with your location, 
#   remember to use forward slashes, "/" 
folder_location = "G:/Steve/Stats with Steve/Course/Getting started in research/Data/"

#2. We'll paste "folder_location" with any file name, you can always 
#   skip that and type the full location. Pasting them together is 
#   not required, just a shortcut.


##############
## Get Data ##
##############

# 1. load CSV data (upload directly from a csv formatted file)
titanic3 = pd.read_csv(folder_location + "titanic3.csv")
los2 = pd.read_csv(folder_location + "los2.csv")
hosprog = pd.read_csv(folder_location + "hosprog.csv")

########################
## Distribution tests ##
########################

# subset the data
surv1 = titanic3.loc[titanic3['survived'] == 'Alive', 'age']
surv0 = titanic3.loc[titanic3['survived'] == 'Dead', 'age']

# t-test
t_stat, p_value = stats.ttest_ind(surv1, surv0, equal_var=True, nan_policy='omit')

# Results
print(f"T-statistic: {t_stat:.4f}")
print(f"P-value: {p_value:.4f}")
# No significant differences: p = 0.0727 

## ANOVA test
pclass1 = titanic3.loc[titanic3['pclass'] == '1st', 'age']
pclass2 = titanic3.loc[titanic3['pclass'] == '2nd', 'age']
pclass3 = titanic3.loc[titanic3['pclass'] == '3rd', 'age']
# run ANOVA
f_stat, p_val = f_oneway(pclass1, pclass2, pclass3, nan_policy='omit')
# Results
print(f"F-Statistic: {f_stat:.4f}")
print(f"p-value: {p_val:.4f}")  # p < 0.001

## Example of distributions that are significantly different p= 0.03
# subset the data
cost1 = hosprog.loc[hosprog['program'] == 1, 'cost']
cost0 = hosprog.loc[hosprog['program'] == 0, 'cost']

np.mean(cost1)  #intervention mean cost 
np.mean(cost0)  #control group mean cost

# t-test
t_cost, p_val_cost = stats.ttest_ind(cost1, cost0, equal_var=True, nan_policy='omit')

# Results
print(f"P-value: {p_val_cost:.4f}")

# Histogram showing both group's distributions
sns.histplot(data=hosprog, x='cost', hue='program',
             kde=True, common_norm=False, line_kws={"linewidth": 7})
plt.title("Histogram of Cost: Int= $9,013, Ctl= $9,640 (p= 0.03)")


## Chi-square test

# 2x2 table
surv_table = pd.crosstab(titanic3['survived'], titanic3['sex'])

# Chi-Square Test
chi2_stat, p_value, dof, expected_freq = chi2_contingency(surv_table)

# significant results
print(f"Chi-Square Statistic: {chi2_stat:.4f}")
print(f"P-value: {p_value:.4f}") # p < 0.001


#################
##  Mean test  ##
#################

# subset the data
hosA = los2.loc[los2['Hospital'] == 'A', 'LOS']
hosB = los2.loc[los2['Hospital'] == 'B', 'LOS']

# t-test
t_stat, p_value = stats.ttest_ind(hosA, hosB, equal_var=True)

# Results
print(f"T-statistic: {t_stat:.4f}")
print(f"P-value: {p_value:.4f}")
# No significant differences: p = 0.857 

#########################
##  Mann-Whitney test  ##
#########################

# Results
u_statistic, p_value = stats.mannwhitneyu(hosA, hosB, alternative='two-sided')

print(f"U-Statistic: {u_statistic:.4f}")
print(f"P-Value: {p_value:.4f}")
# No significant differences: p = 0.975 

