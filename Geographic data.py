# Austin Scheetz

# =========================================================================== #
#                       Load modules and input filepaths                      #
# =========================================================================== #
import os
import sys
import pandas as pd

# Plug in the relevant working directory for your machine
os.chdir("C:/Users/austi/Documents/Yale/Thesis/Data")

# Plug in the filepath where packages are stored on your machine
sys.path.append("C:\\Users\\austi\\AppData\\Local\\Programs\\Python\\Python37-32\\Lib\\site-packages")


# =========================================================================== #
#                           Loading in data                                   #
# =========================================================================== #

# I want to generate a new simple dataset which records the ranch names where my respondents take their animals in a wet vs dry year.

# Loading in dataset
Pastoralist = pd.read_csv("corrected_heifer_master.csv")

Pastoralist.ranches_wet.unique()
Pastoralist.ranches_dry.unique()
Pastoralist.village.unique()

# =========================================================================== #
#                      Data cleaning the village variable                     #
# =========================================================================== #

# This variable records the village each respondent is from

Pastoralist['village'].fillna('Il Motiok', inplace = True)
Pastoralist['village'].replace('naserian', 'Il Motiok', inplace = True)
Pastoralist.loc[Pastoralist['village'].str.contains('motiok'), 'village'] = 'Il Motiok'
Pastoralist.loc[Pastoralist['village'].str.contains('larubai'),'village'] = 'Il Motiok'

Pastoralist['village'].replace('morijo', 'Morijo', inplace = True)
Pastoralist['village'].replace('loisaba', 'Morijo', inplace = True)

Pastoralist['village'].replace('endana', 'Endana', inplace = True)

Pastoralist['village'].replace('p $d', 'P&D', inplace = True)
Pastoralist['village'].replace('p&d', 'P&D', inplace = True)
Pastoralist.loc[Pastoralist['village'].str.contains('primary'), 'village'] = 'P&D'
Pastoralist.loc[Pastoralist['village'].str.contains('mugie'),'village'] = 'P&D' 

Pastoralist['village'].replace('koija', 'Koija', inplace = True)
Pastoralist['village'].replace('k6', 'Koija', inplace = True)

Pastoralist['village'].replace('lobarishoreki', 'Labarishereki', inplace = True)

Pastoralist.loc[Pastoralist['village'].str.contains('tangi'), 'village'] = 'Tangi Nyeusi'

Pastoralist['village'].replace('tiamamut', 'Tiamamut', inplace = True)

Pastoralist['village'].replace('sanaguri', 'Sanaguri', inplace = True)

Pastoralist.village.unique()

# =========================================================================== #
#                       Data cleaning the wet year info                       #
# =========================================================================== #

# This variable records which tracts of land pastoralists take their cattle in a wet year

# Pastoralists keeping cows on group ranches
Pastoralist['ranches_wet'].replace('koija', 'Koija', inplace = True)

Pastoralist['ranches_wet'].replace('lobarishoreki', 'Labarishereki', inplace = True)

Pastoralist['ranches_wet'].replace('p&d', 'P&D', inplace = True)

Pastoralist['ranches_wet'].replace('morijo', 'Morijo', inplace = True)
Pastoralist['ranches_wet'].replace('mo5', 'Morijo', inplace = True)

Pastoralist['ranches_wet'].replace('tiamamut', 'Tiamamut', inplace = True)

Pastoralist['ranches_wet'].replace('endana', 'Endana', inplace = True)

Pastoralist['ranches_wet'].replace('sanaguri', 'Sanaguri', inplace = True)

Pastoralist.loc[Pastoralist['ranches_wet'].str.contains('group'), 'ranches_wet'] = 'Il Motiok'
Pastoralist.loc[Pastoralist['ranches_wet'].str.contains('motiok'), 'ranches_wet'] = 'Il Motiok'

Pastoralist.loc[Pastoralist['ranches_wet'].str.contains('tangi'), 'ranches_wet'] = 'Tangi Nyeusi'

# Look at new data after cleaning
Pastoralist.ranches_wet.unique()

# =========================================================================== #
#                       Data cleaning the dry year info                       #
# =========================================================================== #

# This variable records which tracts of land pastoralists take their cattle in a dry year

# Pastoralists keeping cows on group ranches
Pastoralist['ranches_dry'].replace('koija', 'Koija', inplace = True)

Pastoralist['ranches_dry'].replace('endana', 'Endana', inplace = True)

Pastoralist.loc[Pastoralist['ranches_dry'].str.contains('motiok'), 'ranches_dry'] = 'Il Motiok'

# Pastoralist putting cows on private ranches
Pastoralist['ranches_dry'].replace('mpala', 'Mpala', inplace = True)
Pastoralist['ranches_dry'].replace('mpala 350', 'Mpala', inplace = True)

Pastoralist['ranches_dry'].replace('mugie', 'Mugie', inplace = True)

Pastoralist['ranches_dry'].replace('loisaba', 'Loisaba', inplace = True)

Pastoralist['ranches_dry'].replace('oljogi', 'Ol Jogi', inplace = True)

Pastoralist['ranches_dry'].replace('olpajeta', 'Ol Pejeta', inplace = True)

Pastoralist['ranches_dry'].replace('elkarama', 'El Karama', inplace = True)

Pastoralist['ranches_dry'].replace('loisaba, mugie', 'Loisaba, Mugie', inplace = True)
Pastoralist['ranches_dry'].replace('.loisaba, mugie', 'Loisaba, Mugie', inplace = True)
Pastoralist['ranches_dry'].replace('loisaba mugie', 'Loisaba, Mugie', inplace = True)
Pastoralist['ranches_dry'].replace('loisaba , mugie', 'Loisaba, Mugie', inplace = True)
Pastoralist['ranches_dry'].replace('loisaba,mugie', 'Loisaba, Mugie', inplace = True)

Pastoralist['ranches_dry'].replace('mpala, loisaba', 'Mpala, Loisaba', inplace = True)
Pastoralist['ranches_dry'].replace('mpala,loisaba', 'Mpala, Loisaba', inplace = True)
Pastoralist['ranches_dry'].replace('mpala  loisaba', 'Mpala, Loisaba', inplace = True)
Pastoralist['ranches_dry'].replace('loisaba, mpala', 'Mpala, Loisaba', inplace = True)

Pastoralist['ranches_dry'].replace('mpala,karisia', 'Mpala, Karisia', inplace = True)
Pastoralist['ranches_dry'].replace('mpala ,karisia', 'Mpala, Karisia', inplace = True)

Pastoralist['ranches_dry'].replace('mpala, segera', 'Mpala, Segera', inplace = True)

Pastoralist['ranches_dry'].replace('olpajeta, a.d.c', 'Ol Pejeta, ADC', inplace = True)
Pastoralist['ranches_dry'].replace('olpajeta , a.d.c', 'Ol Pejeta, ADC', inplace = True)
Pastoralist['ranches_dry'].replace('a.d.c, olpajeta', 'Ol Pejeta, ADC', inplace = True)
Pastoralist['ranches_dry'].replace('a.d.c,olpajeta', 'Ol Pejeta, ADC', inplace = True)

Pastoralist['ranches_dry'].replace('soitanyiro  ranch', 'Soita Nyiro', inplace = True)

Pastoralist['ranches_dry'].replace('segera 550', 'Segera', inplace = True)
Pastoralist['ranches_dry'].replace('segera', 'Segera', inplace = True)

Pastoralist['ranches_dry'].replace('rumuruti', 'Rumuruti', inplace = True)

Pastoralist['ranches_dry'].replace('karisia', 'Karisia', inplace = True)

Pastoralist['ranches_dry'].replace('olmaran', 'Ol Moran', inplace = True)

Pastoralist['ranches_dry'].replace('loisaba, olmalo', 'Loisaba, Ol Malo', inplace = True)

# Look at new data after cleaning
Pastoralist.ranches_dry.unique()

# Export cleaned data as a .csv to be used later in R
Pastoralist.to_csv("Pastoralist_geocleaned.csv", index = False)

# =========================================================================== #
#                       Where are my respondents from?                        #
# =========================================================================== #

# Jogging my memory
Pastoralist.village.unique()

# Counting the number of respondents from each village

# This step creates a Python dictionary that matches village name with number of people living there
village = {'Sanaguri'      : len(Pastoralist[Pastoralist['village'].str.contains('Sanaguri')]),
           'Il Motiok'     : len(Pastoralist[Pastoralist['village'].str.contains('Motiok')]),
           'Koija'         : len(Pastoralist[Pastoralist['village'].str.contains('Koija')]),
           'P&D'           : len(Pastoralist[Pastoralist['village'].str.contains('P&D')]),
           'Labarishereki' : len(Pastoralist[Pastoralist['village'].str.contains('Labarishereki')]),
           'Tiamamut'      : len(Pastoralist[Pastoralist['village'].str.contains('Tiamamut')]),
           'Morijo'        : len(Pastoralist[Pastoralist['village'].str.contains('Morijo')]),
           'Tangi Nyeusi'  : len(Pastoralist[Pastoralist['village'].str.contains('Tangi')]),
           'Endana'        : len(Pastoralist[Pastoralist['village'].str.contains('Endana')])}

# Inputting to dataframe
village_map = pd.DataFrame(list(village.items()), columns = ['village', 'num_people'])

# =========================================================================== #
#                    Where people put cows in the wet season                  #
# =========================================================================== #

# Jogging my memory
Pastoralist.ranches_wet.unique()

# Counting the number of respondents putting cattle on each ranch/group ranch
# Note: some people put cattle in more than one place

# This step creates a Python dictionary that matches land parcel name with number of people using it
wet = {'Sanaguri'      : len(Pastoralist[Pastoralist['ranches_wet'].str.contains('Sanaguri')]),
       'Il Motiok'     : len(Pastoralist[Pastoralist['ranches_wet'].str.contains('Motiok')]),
       'Koija'         : len(Pastoralist[Pastoralist['ranches_wet'].str.contains('Koija')]),
       'P&D'           : len(Pastoralist[Pastoralist['ranches_wet'].str.contains('P&D')]),
       'Labarishereki' : len(Pastoralist[Pastoralist['ranches_wet'].str.contains('Labarishereki')]),
       'Tiamamut'      : len(Pastoralist[Pastoralist['ranches_wet'].str.contains('Tiamamut')]),
       'Morijo'        : len(Pastoralist[Pastoralist['ranches_wet'].str.contains('Morijo')]),
       'Tangi Nyeusi'  : len(Pastoralist[Pastoralist['ranches_wet'].str.contains('Tangi')]),
       'Endana'        : len(Pastoralist[Pastoralist['ranches_wet'].str.contains('Endana')])
       }

# Inputting to dataframe
wet_map = pd.DataFrame(list(wet.items()), columns = ['parcel', 'num_people'])

# =========================================================================== #
#                    Where people put cows in the dry season                  #
# =========================================================================== #

# Jogging my memory
Pastoralist.ranches_dry.unique()

# Counting the number of respondents putting cattle on each ranch/group ranch
# Note: some people put cattle in more than one place

# This step creates a Python dictionary that matches land parcel name with number of people using it
dry = {'Sanaguri'      : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Sanaguri')]),
       'Il Motiok'     : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Motiok')]),
       'Koija'         : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Koija')]),
       'P&D'           : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('P&D')]),
       'Labarishereki' : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Labarishereki')]),
       'Tiamamut'      : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Tiamamut')]),
       'Morijo'        : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Morijo')]),
       'Tangi Nyeusi'  : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Tangi')]),
       'Endana'        : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Endana')]),
       'Mpala'         : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Mpala')]),
       'Mugie'         : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Mugie')]),
       'Loisaba'       : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Loisaba')]),
       'Karisia'       : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Karisia')]),
       'Ol Pejeta'     : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Ol Pejeta')]),
       'ADC'           : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('ADC')]),
       'El Karama'     : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('El Karama')]),
       'Segera'        : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Segera')]),
       'Ol Moran'      : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Ol Moran')]),
       'Ol Malo'       : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Ol Malo')]),
       'Ol Jogi'       : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Ol Jogi')]),
       'Soita Nyiro'   : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Soita')]),
       'Rumuruti'      : len(Pastoralist[Pastoralist['ranches_dry'].str.contains('Rumuruti')])
       }

# Inputting to dataframe
dry_map = pd.DataFrame(list(dry.items()), columns = ['parcel', 'num_people'])

# =========================================================================== #
#                 Exporting these to .csv files for use later                 #
# =========================================================================== #

village_map.to_csv("village_map.csv", index = False)
wet_map.to_csv("wet_map.csv", index = False)
dry_map.to_csv("dry_map.csv", index = False)