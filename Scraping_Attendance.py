#!/usr/bin/env python
# coding: utf-8

# In[ ]:


# from selenium.webdriver.support.ui import Select
from selenium import webdriver
from bs4 import BeautifulSoup
import pandas as pd
import numpy as np
import os
import csv

## Change according to required folder
parent_dir = "C:/Users/Sayyam/Desktop/School_attendance"
os.chdir(parent_dir)

## Make sure webdriver is installed according to chrome version  
driver = webdriver.Chrome()

#The required website
driver.get("http://edustud.nic.in/mis/student/student/frmStudentAttendanceReport.aspx?Type=DesiredSchool")

## Getting names of All schools for school name sub-folders
html = driver.page_source
soup = BeautifulSoup(html)
school_name= soup.find("select", {"id": "ddlType"}).text
li = list(school_name.split("\n"))     

## Extracting data

## Selecting School from dropdown
sselect = Select(driver.find_element_by_name('ddlType'))
for index in range(1,len(sselect.options)):
    sselect = Select(driver.find_element_by_name('ddlType'))
    sselect.select_by_index(index)
    ## Only for naming subfoder correctly
    x = 1+index
    school= li[x]
    
    path = os.path.join(parent_dir,school)
    os.mkdir(path)
    
    ## Changing folder according to school
    os.chdir(path)
    
    ##Selecting year 
    for year in (2014,2015,2016,2017,2018, 2019):
        inputElement = driver.find_element_by_id("txtYear")
        inputElement.clear()
        inputElement.send_keys(year)
        
        ## For selecting Month
        mselect = Select(driver.find_element_by_name('ddlMonth'))
        for month in range(1,len(mselect.options)):
            mselect = Select(driver.find_element_by_name('ddlMonth'))
            k=mselect.select_by_index(month)
            ## For selecting day
            dselect = Select(driver.find_element_by_name('ddlDay'))
            ##Adjusting according to month
            if month == 2:
                dat= 29
            elif month in (1,3,5,7,8,10,12):
                dat = 32
            elif month in (4,6,9,11):
                dat = 31 
            for date in range(1,dat):
                dselect = Select(driver.find_element_by_name('ddlDay'))
                dselect.select_by_index(date)
                ## Click next to print table 
                driver.find_element_by_id('btnNext').click()
                try:
                    html = driver.page_source
                    soup = BeautifulSoup(html)
                    ## Finding the table
                    table_html = soup.find("table", {"id": "AttTable"})
                    ## storing table as a pandas dataframe 
                    table= pd.read_html(str(table_html), header = 0)
                    # Excel name
                    file = [str(month), str(date), str(year)]
                    file = "_".join(file)
                    ls = table
                    df = pd.DataFrame(ls[0])
                    ## Saving as a csv file
                    df.to_csv("{}.csv".format(file))
                ##Correcting for days no observation is observed and no table is found.    
                except:
                        pass


# In[ ]:




