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

consecutive_day_decadal_moving(DatFile,
                               sourceDir = "Analyses/Gap_Filled", destDir = "Analyses/Consecutive_Dry_Decadal_Moving")

consecutive_day_decadal(DatFile,
                        sourceDir = "Analyses/Gap_Filled", destDir = "Analyses/Consecutive_Dry_Decadal")

consecutive_day_whole(DatFile,
                      sourceDir = "Analyses/Gap_Filled", destDir = "Analyses/Consecutive_Dry_Whole")


##############################################################################################################
#### Wavelet analyses

wavelet_analysis(DatFile,
                 sourceDir = "Analyses/Consecutive_Dry_Annual", destDir = "Analyses/Wavelet_Analysis")


### wavelet analyses may not be ideal for inferring consecutive dry days,
### because wavelet analyses produces a frequency correlation plot.
### The y-axis is period - the recurrence periodicity of the x-axis.
### But the x-axis does not have a value attached to it - what we calculated was
### number of consecutive dry days within each year or
### max number of consecutive dry days within each year/decade. 


##############################################################################################################
#### Calculate predictability for all drought indices
R05PS_pred(DatFile, sourceDir = "Analyses/ThrIndS", destDir = "Analyses/R05PS")

R01PS_pred(DatFile, sourceDir = "Analyses/ThrIndS", destDir = "Analyses/R01PS")

## Calculate dry consecutive predictability
consec_dry_pred_annual(DatFile, sourceDir = "Analyses/Consecutive_Dry_Annual", destDir = "Analyses/CDP")



#### Notes:
### predictability is not straightforward for making inference of the recurrence interval of droughts
### it might make sense just to calculate the likelihood of occurrence of consecutive dry days > 20 days/season etc.

##############################################################################################################
#### Generate future summer precipitation based on previous statistics
#### Criteria:
#### 1. Pick years when annual prec is 650 - 750 mm/yr
#### 2. Calculate the consecutive dry and wet day indices for summer only, for those years
#### 3. Get an all-time averages for one consecutive dry and wet day estimates for summer
#### 4. Summer is defined as Dec, Jan and Feb
#### 5. Create randomly a total of 60 rainfall days in summer (out of 90 days)
#### 6. The total amount of rainfall = 300 mm
#### 7. Out of the 60 rainfall events, 30 are small (1 - 5 mm)
####                                   20 are medium (5 - 30 mm)
####                                   10 are large (30 - 80 mm)
#### 8. Let the consecutive dry and wet days in this new dataset following the distribution of historic records

year_2018_rainfall_generation(DatFile, 
                              sourceDir = "Analyses/Gap_Filled", 
                              destDir = "Analyses/Year_2018_rainfall")



##############################################################################################################
#### End

