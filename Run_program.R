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
### Calculate threshold based dry index
## number of days within a season when prcp < 5th & < 1st percentile
ThrIndS(DatFile, sourceDir = "Analyses/Gap_Filled", destDir = "Analyses/ThrIndS")

### Step 2:
### Calculate consecutive dry days indices
consecutive_day_annual(DatFile,
                        sourceDir = "Analyses/Gap_Filled", destDir = "Analyses/Consecutive_Dry_Annual")

consecutive_day_decadal(DatFile,
                       sourceDir = "Analyses/Gap_Filled", destDir = "Analyses/Consecutive_Dry_Decadal")


##############################################################################################################
#### Wavelet analyses

##############################################################################################################
#### Calculate whole year range predictability
R05PS_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")


R01PS_pred(sourceDir = "data/indices/ThrIndS", destDir = "data/predictability")

## Calculate dry consecutive predictability
consec_dry_pred(sourceDir = "data/indices/CDS", destDir = "data/predictability")

