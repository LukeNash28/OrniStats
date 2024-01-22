#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 24 13:55:51 2023

@author: lukenash1
"""

import pandas as pd
from geopy.distance import geodesic
from geopy.point import Point
from geographiclib.geodesic import Geodesic

refs = pd.DataFrame(pd.read_excel("/Users/lukenash1/Documents/OrniStats/Public/Rare Bird Data - Main.xlsx",sheet_name = "Location Co-ordinates"))
refs.set_index("Site",inplace=True)
inputData = pd.DataFrame(pd.read_excel("/Users/lukenash1/Documents/OrniStats/Public/Rare Bird Data - Main.xlsx",sheet_name = "Polygon Import"))
outputData = None

def midpoint_vincenty(df):
    global refs
    global outputData
    inputDict = {}
    
    for col in df.columns:
        inputList = []
        
        for i, value in df[col].items():
            if pd.notna(value) == True:
                inputLoc = df.at[i, col]
                inputLat = refs.at[inputLoc, 'Lat']
                inputLong = refs.at[inputLoc, 'Long']
                inputCoord = (inputLat, inputLong)
                inputList.append(inputCoord)
        
        inputDict[col] = inputList
    
    #Test code onwards here
    for i in inputDict:
        pointsList = inputDict[i]
        distance = geodesic(Point(pointsList[0]),Point(pointsList[1])).km
        bearing = Geodesic.WGS84.Inverse((pointsList[0][0]),(pointsList[0][1]),(pointsList[1][0]),(pointsList[1][1]))['azi1']
        midpoint = geodesic(Point(pointsList[0]),Point(pointsList[1])).destination(Point(pointsList[0]),bearing = bearing,distance = distance/2)
    
        for point in pointsList[2:]:
            iterDistance = geodesic(midpoint, Point(point)).km
            iterBearing = Geodesic.WGS84.Inverse(midpoint.latitude,midpoint.longitude,point[0],point[1])['azi1']
            midpoint = geodesic(midpoint,Point(point)).destination(midpoint, bearing = iterBearing, distance=iterDistance/2)
        
        inputDict[i] = round(midpoint.latitude, 3), round(midpoint.longitude, 3)
    
    outputData = pd.DataFrame(inputDict)
    outputData = outputData.T

midpoint_vincenty(inputData)


outputData.to_csv('/Users/lukenash1/Documents/OrniStats/Private/2022 3+ MP Output.csv',index = True)
