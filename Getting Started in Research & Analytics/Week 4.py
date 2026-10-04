################################################################################
#                                   Week 4                                     #
################################################################################
# Set up libraries
#%%
# only need to run pip commands once and add # comment after
#pip install pandas
#pip install numpy
#pip install scikit-learn
#pip install pingouin
#pip install factor-analyzer
#pip install -U factor-analyzer

import pandas as pd
import numpy as np
import statsmodels.stats.power as smp
import matplotlib.pyplot as plt
import pingouin as pg
from scipy.cluster.hierarchy import linkage, dendrogram, fcluster
from scipy.spatial.distance import squareform
from sklearn.cluster import AgglomerativeClustering
from sklearn.datasets import make_blobs
from factor_analyzer import FactorAnalyzer
from factor_analyzer.factor_analyzer import calculate_bartlett_sphericity, calculate_kmo

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
ebp = pd.read_csv( folder_location + "ebp.csv") 
ebp.head()

# Will create subsets below because of how packages/libraries handle
# missing data. See documentation to find your best option.

############
## Recode ##
############

# Create data map
mapping = {'Strongly Disagree': 1, 'Disagree': 2, 'Uncertain': 3, 'Agree': 4, 'Strongly Agree': 5}

# Replace 'NA' string with NaN, then map text to numbers
ebp['q0002r'] = ebp['q0002'].replace('NA', np.nan).map(mapping)
ebp['q0006r'] = ebp['q0006'].replace('NA', np.nan).map(mapping)
ebp['q0008r'] = ebp['q0008'].replace('NA', np.nan).map(mapping)
ebp['q0013r'] = ebp['q0013'].replace('NA', np.nan).map(mapping)
ebp['q0015r'] = ebp['q0015'].replace('NA', np.nan).map(mapping)
ebp['q0026r'] = ebp['q0026'].replace('NA', np.nan).map(mapping)
ebp['q0027r'] = ebp['q0027'].replace('NA', np.nan).map(mapping)
ebp['q0028r'] = ebp['q0028'].replace('NA', np.nan).map(mapping)


#####################
## Cluster Analysis##
#####################

#create gender variable
support3['gender_1'] = (support3['sex'] == 'female').astype(int)

# Conduct cluster analysis
# get subset
cols = ['wblc' , 'gender_1' , 'meanbp' , 'age' , 'hrt']
df = pd.DataFrame(data=support3, columns=cols)
#Remove Nans
df = df.dropna()
df[0:].describe()	

# Calculate the absolute correlation matrix
corr_matrix = df.corr().abs()

# Convert correlation to a distance matrix (0 = identical, 1 = orthogonal)
dist_matrix = 1 - corr_matrix

# Condense the distance matrix into a 1D array
condensed_dist = squareform(dist_matrix)

# Compute hierarchical clustering using Ward's linkage method
# Ward's method minimizes variance within the variable clusters
Z = linkage(condensed_dist, method='ward')

# Create the Dendrogram -- run al plt lines at the same time
plt.figure(figsize=(10, 6))
dendrogram(
    Z, 
    labels=df.columns, 
    orientation='left',  # Left orientation makes long variable names easy to read
    leaf_font_size=12
)
plt.title("Hierarchical Clustering Dendrogram of Variables")
plt.xlabel("Distance")
plt.show()


######################
## Cronbach's alpha ##
######################

# get subset
alpha_cols = ["q0026r", "q0027r", "q0028r"]
alpha_df = pd.DataFrame(data=ebp, columns=alpha_cols)
#Remove Nans
alpha_df = alpha_df.dropna()
alpha_df.head()

# Compute Cronbach's alpha
alpha = pg.cronbach_alpha(data=alpha_df)

print(f"Cronbach's Alpha: {alpha}")


#####################
## Factor Analysis ##
#####################

# get subset
fa_cols = ["q0002r","q0006r","q0008r","q0013r","q0015r","q0026r", "q0027r", "q0028r"]
fa_df = pd.DataFrame(data=ebp, columns=fa_cols)
#Remove Nans
fa_df = fa_df.dropna()
fa_df.head()

# Run Exploratory Factor Analysis (EFA)

# Initialize and fit the Factor Analysis model
# Specify the number of expected factors and choose 'varimax' rotation
n_factors = 2
fa = FactorAnalyzer(n_factors=n_factors, rotation="varimax")
fa.fit(fa_df)

# Get Rotated Factor Loadings
# Loadings show how much each variable contributes to a factor
loadings = pd.DataFrame(
    fa.loadings_, 
    columns=[f"Factor_{i+1}" for i in range(n_factors)], 
    index=fa_df.columns
)
print("--- MAIN RESULTS ---")
print("--- Rotated Factor Loadings ---")
print(loadings.round(3))
print("\n")


# Get Variance Explained by each factor
# Returns: SS Loadings, Proportion Variance, Cumulative Variance
factor_variance = fa.get_factor_variance()
variance_df = pd.DataFrame(
    factor_variance,
    index=['SS Loadings', 'Proportion Var', 'Cumulative Var'],
    columns=[f"Factor_{i+1}" for i in range(n_factors)]
)
print("--- Variance Summary ---")
print(variance_df.round(3))

# %%
