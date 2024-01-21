#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct 16 14:55:59 2023

@author: lukenash1
"""

import pandas as pd
import math

refs = pd.DataFrame(pd.read_excel("/Users/lukenash1/Documents/OrniStats/Public/Rare Bird Data - Main.xlsx",sheet_name="Location Co-ordinates"))
refs.set_index("Site",inplace=True)
inputData = pd.DataFrame(pd.read_excel("/Users/lukenash1/Documents/OrniStats/Public/Rare Bird Data - Main.xlsx",sheet_name="Dual Import"))
outputData = None

def midpoint(df):
    listTheta1 = []
    listTheta2 = []
    listLambda1 = []
    listLambda2 = []
    global outputData
    
    for locA in df['Start']:
        theta1 = math.radians(refs.at[locA, 'Lat'])
        listTheta1.append(theta1)
        lambda1 = math.radians(refs.at[locA,'Long'])
        listLambda1.append(lambda1)
    
    for locB in df['End']:
        theta2 = math.radians(refs.at[locB,'Lat'])
        listTheta2.append(theta2)
        lambda2 = math.radians(refs.at[locB,'Long'])
        listLambda2.append(lambda2)
    
    newColumns = {'Theta1': listTheta1, 'Theta2' : listTheta2, 'Lambda1' : listLambda1, 'Lambda2' : listLambda2}
    df = pd.concat([df, pd.DataFrame(newColumns)], axis=1)
    df['deltaLong'] = df['Lambda2'] - df['Lambda1']
    df['constBX'] = df['Theta2'].apply(math.cos) * df['deltaLong'].apply(math.cos)
    df['constBY'] = df['Theta2'].apply(math.cos) * df['deltaLong'].apply(math.sin)
    df['midLat'] = df.apply(
        lambda row: math.degrees(math.atan2(
            math.sin(row['Theta1'])+ math.sin(row['Theta2']),
            math.sqrt((math.cos(row['Theta1'])+row['constBX'])**2 + (row['constBY']**2))
        )),
        axis = 1
    )
    
    df['midLong'] = df.apply(
        lambda row : math.degrees(row['Lambda1'] + math.atan2(row['constBY'], math.cos(row['Theta1'])+ row['constBX'])),
        axis = 1
    )
    
    df['midLat'] = round(df['midLat'],3)
    df['midLong'] = round(df['midLong'],3)
    outputData = df[['Start','End','midLat','midLong']]

midpoint(inputData)
outputData.to_csv('/Users/lukenash1/Documents/OrniStats/Private/2022 2 MP Output.csv',index = False)
    
    
    