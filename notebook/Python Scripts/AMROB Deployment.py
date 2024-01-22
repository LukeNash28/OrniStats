#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 19:23:17 2023

@author: lukenash1
"""

import pandas as pd
import numpy as np
import folium
import datetime
from folium.plugins import MarkerCluster

data = pd.read_excel("/Users/lukenash1/Documents/OrniStats/Public/Rare Bird Data - Main.xlsx", sheet_name = "Accepted Records")
AMROB_data = data[data['Qualifier'] == 'AMROB']

LocationsRef = pd.read_excel("/Users/lukenash1/Documents/OrniStats/Public/Rare Bird Data - Main.xlsx", sheet_name = "Location Co-ordinates")
LocationsRef.set_index("site", inplace=True)

discDates = []
deptDates = []

for i, date in AMROB_data["Date of Discovery"].items():
    if pd.notna(date) == True:
        datestr = date.strftime("%d/%m/%Y")
        discDates.append(datestr)
    else:
        AMROB_data.loc[i, "Month of Discovery"] = datestr
        discDates.append(datestr)

for i, date in AMROB_data["Date of Departure"].items():
    if pd.notna(date) == True:
        datestr = date.strftime("%d/%m/%Y")
        deptDates.append(datestr)
    else:
        AMROB_data.loc[i, "Month of Departure"] = datestr
        deptDates.append(datestr)

AMROB_data["Discovery Date String"] = discDates
AMROB_data["Departure Date String"] = deptDates

markerLocations = {}
markerLabels = {}

for i, value in AMROB_data["Helper"].items():
    site = AMROB_data.at[i, "Site"]
    inputLat = LocationsRef.at[site, 'lat']
    inputLong = LocationsRef.at[site, 'long']
    inputCoords = [inputLat, inputLong]
    markerLocations[value] = inputCoords
    area = AMROB_data.at[i, "Recording Area"]
    discDate = AMROB_data.at[i, "Discovery Date String"]
    deptDate = AMROB_data.at[i, "Departure Date String"]
    markerLabels[value] = f"{site}, {area}, {discDate} to {deptDate}"

m = folium.Map(location = [55.00, -5.25], tiles = "Esri world_imagery", zoom_start = 5)

cluster = MarkerCluster(color = "red").add_to(m)

for key, item in markerLocations.items():
    folium.Marker(location = markerLocations[key], 
                  popup = markerLabels[key], 
                  icon = folium.Icon(color = "red", icon = "binoculars", prefix = "fa"),).add_to(cluster)

m
