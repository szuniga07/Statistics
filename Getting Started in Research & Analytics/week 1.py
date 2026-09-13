################################################################################

#                                   Week 1                                     #
################################################################################
# Set up libraries
import statistics

#pip install pandas
import pandas as pd

#pip install matplotlib
import matplotlib.pyplot as plt

#pip install seaborn
import seaborn as sns

#pip install scipy
from scipy.stats import norm
from scipy.stats import kstest
import scipy.stats as stats

#pip install numpy
import numpy as np

## Make a folder location so that you can load and save files:

#1. Replace my folder location with your location, 
#   remember to use forward slashes, "/" 
folder_location = "C:/temp/python/"

#2. We'll paste "folder_location" with any file name, you can always 
#   skip that and type the full location. Pasting them together is 
#   not required, just a shortcut.


##############
## Get Data ##
##############

# 1. load CSV data (upload directly from a csv formatted file)
hosprog = pd.read_csv(folder_location + "hosprog.csv")

# View the first 5 rows
print(hosprog.head())


###################
## Distributions ##
###################

# Summary statistics #

# We usually review the average (or mean) values
hosprog["los"].mean()   
# The Median. Python automatically deals missing data
hosprog["los"].median()   

# In addition to describing the "center", we also want to discuss the "spread"  
statistics.stdev(hosprog["los"])

# We can get more info about the distribution with the quartiles and mean
print(hosprog.describe())


## Graphs ##

# Histogram
# %%
hosprog["los"].hist()
#plt.show()

# Adding a density curve, highlight both lines and run
sns.histplot(data=hosprog, x='los', kde=True, stat="density")
plt.title('Histogram with a Density Curve')

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

# Get normal distribution parameters
# Hightlight A-E, press shift + Enter to get normal curve 
los_mean = hosprog["los"].mean()   
los_sd = statistics.stdev(hosprog["los"])
#A.  normal distribution data based on mean and SD
norm_data = np.random.normal(loc=los_mean, scale=los_sd, size= 720)

#B. Fit the normal distribution parameters (mean and standard deviation) to the data
mu, std = norm.fit(norm_data)

#C. Plot the histogram with density scale so it matches the PDF curve
ax = sns.histplot(hosprog["los"], stat="density", kde=False)

#D. Generate points for the normal curve over the range of the plot
x = np.linspace(norm_data.min(), norm_data.max(), 100)
p = norm.pdf(x, mu, std)

#E. Overlay the fitted normal curve line
plt.plot(x, p, 'r-', linewidth=2)


# We clearly see a positive-skew or right-tailed distribution
#with outliers pulled to the right.
# It is less obvious but there is some higher levels of kurtosis
# because some of the histogram bars are clumped around the center
# giving a slightly more "tall and thin" appearance (i.e., tree vs bush).


#2. Kolmogrov-Smirnov normality test
stat, p_value = kstest(hosprog["los"], 'norm')
print(f"Statistic: {stat}, p-value: {p_value}") 
#Significantly different from a normal distribution


#3. Q-Q plot
stats.probplot(hosprog["los"], dist="norm", plot=plt)
plt.title("Normal Q-Q Plot")
plt.show()

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
r_coef, p_value = stats.pearsonr(hosprog['los'], hosprog['cost'])
print(f"Pearson Coefficient: {r_coef}, P-value: {p_value}")

# Let's take a look
plt.scatter(hosprog['los'], hosprog['cost'])

# "pearson" is the default correlation so we can drop it from 
# the code use = "complete.obs" is to handle missing data

#2. Strong negative correlation between LOS and survey results
r_coef, p_value = stats.pearsonr(hosprog['los'], hosprog['survey'])
print(f"Pearson Coefficient: {r_coef}, P-value: {p_value}")
plt.scatter(hosprog['los'], hosprog['survey'])


#3. Weak negative correlation between LOS and age   but statistically significant
r_coef, p_value = stats.pearsonr(hosprog['los'], hosprog['age'])
print(f"Pearson Coefficient: {r_coef}, P-value: {p_value}")
plt.scatter( hosprog['age'], hosprog['los'],)

# You can do point-biseral correlations which are correlations 
# between continuous and binary variables.

#4. Moderate positive correlation between LOS and 30-day readmissions
r_coef, p_value = stats.pearsonr(hosprog['los'], hosprog['rdm30'])
print(f"Pearson Coefficient: {r_coef}, P-value: {p_value}")
plt.scatter(hosprog['los'], hosprog['rdm30'])

# We see that when it comes to patients with readmissions, 
# they had much longer lengths of stay


########################
## Correlation matrix ##
########################
main_vars = ["survey", "los","cost","rdm30","death30","female","age"]

hos_cor = hosprog[main_vars].corr()
print(hos_cor.round(3))
