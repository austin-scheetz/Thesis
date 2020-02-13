# Austin Scheetz

# =========================================================================== #
#               Load modules and change working directory                     #
# =========================================================================== #

import pandas as pd
import os

os.chdir("C:/Users/austi/Documents/Yale/Thesis/Data")

# =========================================================================== #
#                                  Analysis                                   #
# =========================================================================== #

# Load in data
pastoralist = pd.read_csv("Pastoralist_geocleaned.csv")

pastoralist.job.unique()

# First, the unemployed
pastoralist['job'].fillna('none', inplace = True)

# Ranch jobs
pastoralist.loc[pastoralist['job'].str.contains('rder'), 'job'] = 'ranch'
pastoralist.loc[pastoralist['job'].str.contains('manager'), 'job'] = 'ranch'
pastoralist.loc[pastoralist['job'].str.contains('security'), 'job'] = 'ranch'
pastoralist.loc[pastoralist['job'].str.contains('guide'), 'job'] = 'ranch'

# Everyone else
pastoralist.loc[~pastoralist['job'].str.contains('ranch|none'), 'job'] = 'other'

pastoralist.job.value_counts()

# =========================================================================== #
#                                  Exporting                                  #
# =========================================================================== #

pastoralist.to_csv("cleaned_job.csv", index = False)
