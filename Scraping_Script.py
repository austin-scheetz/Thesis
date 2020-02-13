# Austin Scheetz

# BEFORE RUNNING THIS CODE:
# Download the Chrome driver from https://chromedriver.chromium.org/downloads
# Download Python 3.7 and install the selenium module
# Note the filepath where selenium and ChromeDriver installed

# =========================================================================== #
#                       Load modules and input filepaths                      #
# =========================================================================== #
# I use two major packages here: selenium and pandas
# Selenium allows me to automate a web browser and scrape information from it
# Pandas allows me to manipulate R-esque dataframes
# I also load in a few functions from the selenium package
#   - This is a convention in Python that allows me to shorten my script later on
import os
import sys

# Plug in the filepath where selenium is stored on your machine
sys.path.append()

# Plug in the relevant working directory for your machine
os.chdir()

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import StaleElementReferenceException
from selenium.common.exceptions import ElementClickInterceptedException
from selenium.common.exceptions import NoSuchElementException
import pandas as pd

# Plug in the filepath where the Chrome driver is stored on your machine
driver = webdriver.Chrome()
# This will initialize an automated Chrome browser

# Plug in the URL of the server's login page and interviews page
login = ""
inter = ""

# Plug in username and password
usern = ""
passw = ""

# Enter the xpath of the question of interest
quest = ""

# =========================================================================== #
#            Opening automated browser and logging into the server            #
# =========================================================================== #

# Navigate in browser to my private server
driver.get(login)

# Logging in
# First I must locate the username and password elements so selenium knows where to input information
# The find_element_by... notation tells Python to look for certain HTML attributes on the page in the automated browser
username = driver.find_element_by_name("UserName")
password = driver.find_element_by_name("Password")

username.send_keys(usern) # entering in my username
password.send_keys(passw) # entering in my password

driver.find_element_by_tag_name("BUTTON").click() # clicking the "submit" button

# Navigating to the interviews page which lists all 301 interviews in a table
driver.get(inter)

# =========================================================================== #
#               Now running a for loop to gather review links                 #
# =========================================================================== #

# This web table has 16 pages, so I need to scrape each of the 16 pages for links to individual interviews
# To do this, I use a 'for' loop

# Initializing lists that will house interview IDs (for tracking) and links
link_list = list()
ID_list = list()

# Scrape the first page in the web table for links
WebDriverWait(driver, 10).until(EC.presence_of_all_elements_located((By.XPATH,"//a[contains(@href, '/Interview/Review')]")))
Page = driver.find_elements(By.XPATH,"//a[contains(@href, '/Interview/Review')]")
for EachRow in Page:
        link = EachRow.get_attribute("href") # 'href' is the html attribute where the interview link is stored
        link_list.append(link)
        ID = EachRow.get_attribute("text") # This is simply an interview ID that I can use later to merge in with the master dataset
        ID_list.append(ID)

# Click "Next" a bunch of times to operate on all the other pages of the web table
while True: # This line simply tells Python to keep doing this forever until it satisfies a stopping condition that I supplied (except ... :)
    try:
        # These "WebDriverWait" functions tell the script to wait until the elements are loaded before trying to get information from them
        WebDriverWait(driver, 10, ignored_exceptions = (NoSuchElementException,StaleElementReferenceException,)).until(EC.element_to_be_clickable((By.ID, "DataTables_Table_0_next")))
        driver.find_element_by_id("DataTables_Table_0_next").click()
        
        # Scrape each row of the table for the respondent ID and the link to their interview
        # I add another layer of 'try ... except' because it helps avoid weird loading issues. Gives the script a second chance if it sees that stale element error message
        try:
            WebDriverWait(driver, 10, ignored_exceptions = (NoSuchElementException,StaleElementReferenceException,)).until(EC.presence_of_all_elements_located((By.XPATH,"//a[contains(@href, '/Interview/Review')]")))
            Page = driver.find_elements(By.XPATH,"//a[contains(@href, '/Interview/Review')]")
            for EachRow in Page:
                     link = EachRow.get_attribute("href")
                     link_list.append(link)
                     ID = EachRow.get_attribute("text")
                     ID_list.append(ID)
        except StaleElementReferenceException:
            WebDriverWait(driver, 10, ignored_exceptions = (NoSuchElementException,StaleElementReferenceException,)).until(EC.presence_of_all_elements_located((By.XPATH,"//a[contains(@href, '/Interview/Review')]")))
            Page = driver.find_elements(By.XPATH,"//a[contains(@href, '/Interview/Review')]")
            for EachRow in Page:
                     link = EachRow.get_attribute("href")
                     link_list.append(link)
                     ID = EachRow.get_attribute("text")
                     ID_list.append(ID)
    # Here I tell Python to look at this part of the code if I get the 'ElementClickInterceptedException' error message, which occurs on the last page of the table
    except ElementClickInterceptedException:
        print("All done!")
        del link
        del ID
        del Page
        break # this is the key line that tells the loop to stop

# =========================================================================== #
#                Getting question and answer for each interview               #
# =========================================================================== #

###      NOTE: This section of my script takes about 30 minutes to run      ###
        
# I will now be navigating to each interview URL individually, scraping information, storing it, and moving to the next. 301 times.

# Setting up a pandas dataframe to input information
data_df = pd.DataFrame()

# This tells Python to pair the ID's with the links in the other list in corresponding position, and loop through that pair (while recording a numeric index)
for index,interview in enumerate(zip(ID_list,link_list)):
    # Navigate to interview link
    driver.get(interview[1])
        
    # Navigate to the heifer tradeoff question
    WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH,quest)))
    heifer_question = driver.find_element(By.XPATH,quest).get_attribute("href")
    driver.get(heifer_question)
    
    # Scrape the page for the questions the respondent saw, and their answers
    WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.CLASS_NAME,"question-editor")))
    Interview = driver.find_elements(By.XPATH,"//div[contains(@id,'loc_') and not(contains(@class,'static-text'))]//h5") # I'm excluding some introductory text at the beginning explaining the question
    Answer = driver.find_elements(By.XPATH,"//input[@type='text']")
    
    #Put information in a python dictionary and then append to pandas dataframe
    data = {"Respondent_ID" : interview[0],
            "Question" : [question.text for question in Interview],
            "Answer" : [item.get_attribute("value") for item in Answer]}
    data_df = data_df.append(pd.DataFrame(data))
    
    # To track progress: it should print a new number as it loops, all the way up to 301
    print(index)

del index
del heifer_question
del data
del interview
del Answer
del Interview
driver.quit()

# Now, I have a subset of my dataset that correctly matches question and answer for this section of my survey

# Saving this intermediate output to a .csv
data_df.to_csv("intermediate_heifer.csv", index = False)

# This concludes the web scraping portion of this script

# =========================================================================== #
#                 Ensuring compatibility with my master data                  #
# =========================================================================== #

# Re-reading in this dataset: simply de-comment the line below to get here without scraping again
# data_df = pd.read_csv("intermediate_heifer.csv")

past = data_df.copy()

# First, translating the original Swahili into the names of the English-language variables found in the master data
past = past.replace("Miezi ngapi ya kukupea stima bila malipo kwako", "heifer_electricity")
past = past.replace("Simu ya rununu", "heifer_smartphone")
past = past.replace("Pesa ngapi", "heifer_cash")
past = past.replace("Kilo ya maindi", "heifer_maize")
past = past.replace("Mbuzi au kondoo", "heifer_goat")
past = past.replace("Ndume", "heifer_steer")
past = past.replace("Idadi ya miezi ya kulisha ngombe kwenye shamba la karibu", "heifer_mpala")
past = past.replace("Kadi ya simu", "heifer_sim")
past = past.replace("Mihula ngapi ya shule ya msingi ata mlipia mvulana wako?", "heifer_school_son")
past = past.replace("Mihula ngapi ya shule ya msingi ata mlipia msichana wako?", "heifer_school_daughter")

# Performing a 'long to wide' transformation
past = past.pivot(index = "Respondent_ID", columns = "Question", values = "Answer")

# For clarity, setting ID as a column (instead of an index) and moving it to be the first column
past['Respondent_ID'] = past.index
past = past[['Respondent_ID', 'heifer_cash', 'heifer_electricity', 'heifer_goat', 'heifer_maize','heifer_mpala', 'heifer_school_daughter', 'heifer_school_son', 'heifer_sim','heifer_smartphone', 'heifer_steer']]

# Saving this output as a .csv
past.to_csv('heifer_question.csv', index = False)

# =========================================================================== #
#                         Merging with master dataset                         #
# =========================================================================== #

# Re-reading in this dataset: simply de-comment the line below to get here without scraping again
# past = pd.read_csv("heifer_question.csv")

# Reading in the master dataset
pastoralist = pd.read_csv("pastoralist.csv")

# Removing the original messed up columns from the master dataset
pastoralist = pastoralist.drop(['heifer_cash', 'heifer_electricity', 'heifer_goat', 'heifer_maize','heifer_mpala', 'heifer_school_daughter', 'heifer_school_son', 'heifer_sim','heifer_smartphone', 'heifer_steer','v_cash', 'v_electricity', 'v_goat', 'v_maize','v_mpala', 'v_sim','v_smartphone', 'v_steer'], 1)

# Merging the two dataframes
Pastoralist = pastoralist.merge(past, on = "Respondent_ID")

# Removing a couple extra variables to clean up for export back into R
Pastoralist = Pastoralist.drop(['Unnamed: 0','interview__id','sssys_irnd','has__errors', 'interview__status'], 1)

# Writing a new csv for my corrected dataset!
Pastoralist.to_csv("corrected_heifer_master.csv", index = False)