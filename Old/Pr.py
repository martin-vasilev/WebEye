# -*- coding: utf-8 -*-
"""
Created on Tue Jun 11 08:50:44 2024

@author: Martin
"""

from selenium import webdriver

driver = webdriver.Chrome() # using Edge as Chrome doesn't work with the website on my laptop..
#driver.maximize_window() # go full screen
driver.get('https://prolific.com/l') # open url window

chrome_options = webdriver.ChromeOptions()
chrome_options.add_argument("--disable-popup-blocking")
