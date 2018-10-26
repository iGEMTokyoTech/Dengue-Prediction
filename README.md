# Dengue-Prediction

## Requirements
In R console, type below commands.
```
install.packages("openxlsx")
install.packages("rEDM")
```

## Command

```
Rscript DenguePrediction.R /path/to/ConfigFile
```

Result files are produced in `./plot`

## Configure File
```config.R
### setting of subject data
subject_data <- "/Users/ryogamisu/iGEM2018/Dengue-Prediction/denguevirus_infected_in_Bankok.xlsx"
subject_sheetname <- "Processed"
subject_rows = 1:28 # A numeric vector specifying which rows in the subject data file to read. If NULL, all rows are read.

### setting of climate data
climate_data <- "/Users/ryogamisu/iGEM2018/Dengue-Prediction/Climate_BANGKOK.xlsx"
climate_sheetname <- "Sheet1"
climate_rows = 1:68

### setting of colnamaes in your subject_data
Time <- "Year"
DEN1_subject <- "DEN1_Total"
DEN2_subject <- "DEN2_Total"
DEN3_subject <- "DEN3_Total"
DEN4_subject <- "DEN4_Total"
DEN_total <- "DEN_Total"

# DEN1_1st_subject <- "DEN1_1"
# DEN1_2nd_subject <- "DEN1_2"
# DEN2_1st_subject <- "DEN2_1"
```
