############################## Script for calculating drought index for the Hawkesbury region   ###############
############################## Created by: Mingkai Jiang                                        ###############
############################## First created on: 2017-09-01                                     ###############
############################## Modified on: 2017-09-01                                          ###############
##############################################################################################################
##############################################################################################################
##############################################################################################################
#### Prepare all functions and coordinate files
### clear workspace
rm(list=ls())

### Calls script containing all necessary functions
source("R/prepare_R.R")

##############################################################################################################
DatFile <- "IDCJAC0009_067021_1800_Data.csv"

##############################################################################################################
#### Missing Check and Gap filling

### Step 1: Check for missing data
Missing_check(DatFile, sourceDir = "Data", destDir = "Analyses/Gap_Filled")

##############################################################################################################
#### Compute rainfall indices

### Step 1:
### Compute coefficient of variation 
CoefVar(sourceDir = "Analyses/Gap_Filled", destDir = "Analyses/CoefVar")

### Step 2: 
### Calculate seasonal 1D prcp and save into corresponding directory
RX1S(sourceDir = "data/ghcnd_gap_filled", destDir = "data/indices/rx1s")

### Step 3:
##Calculate seasonal 5D prcp and save into corresponding directory
RX5S(sourceDir = "data/ghcnd_gap_filled", destDir = "data/indices/rx5s")

### Step 4: 
### Calculate threshold based indices, R10, R20, R95P, R99P,PRCPTOT at seasonal timestep
ThrIndS(sourceDir = "data/ghcnd_gap_filled", destDir = "data/indices/ThrIndS")

### Step 5:
### Calculate prcp/# of wet days over each season and save into corresponding directory
SDIIS(sourceDir = "data/ghcnd_gap_filled", destDir = "data/indices/SDIIS")

### Step 6: 
### Calculate consecutive days indices
consecutive_day_indices(final_station_DF, 
                        sourceDir = "data/ghcnd_gap_filled", destDir = "data/indices/CDS")

##############################################################################################################
#### Calculate whole year range predictability
R05PS_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")


R01PS_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")

## Calculate dry consecutive predictability
consec_dry_pred(sourceDir = "data/indices/CDS", destDir = "data/predictability")

