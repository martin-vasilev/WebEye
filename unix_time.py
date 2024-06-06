# -*- coding: utf-8 -*-
"""
Created on Mon Jun  3 17:23:25 2024

@author: Martin
"""

import datetime

unix_timestamp = int(datetime.datetime.timestamp(datetime.datetime.now())*1000)
print(unix_timestamp)



