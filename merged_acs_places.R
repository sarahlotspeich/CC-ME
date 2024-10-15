# Read in PLACES data
places <- read.csv("~/Dropbox (Wake Forest University)/8 - STUDENTS/Darcy-Concentration-Curves/PLACES_2024.csv")

## Subset to only rows for "Visits to doctor for routine checkup within the past year among adults" 
places <- subset(x = places, 
                subset = Measure == "Visits to doctor for routine checkup within the past year among adults")
## Subset to only rows for age-adjusted prevalence (rather than crude)
places <- subset(x = places, 
                subset = Data_Value_Type == "Age-adjusted prevalence")

# Keep only relevant columns
places <- places[, c("StateAbbr", "LocationName", "Data_Value", "Low_Confidence_Limit", "High_Confidence_Limit", "LocationID", "TotalPop18plus")]

# Rename columns to be more descriptive 
colnames(places) <- c("State", "County", "Routine_Appt", "LB_Routine_Appt", "UB_Routine_Appt", "LocationID", "TotalPop18plus")

# Add column for margin of error 
places$MoE_Routine_Appt = places$UB_Routine_Appt - places$Routine_Appt

# Add leading zeros back to LocationID, which should be the 5-digit County FIPS code 
library(stringr) ## Run first: install.packages("stringr")
places$LocationID <- str_pad(string = places$LocationID, 
                             width = 5, 
                             side = "left", 
                             pad = "0")

# *** Note to Darcy: I'm going to do the following with pipes, but you can follow the code above to do it without!) *** 
# Load ACS data
library(tidycensus)
acs <- get_acs(geography = "county", ## at the county level (whole US)
               table = "B19013", ## take variable B19013 (median household income in the last 12 mo)
               year = 2022, ## use 2022 ACS
               cache_table = TRUE) ## load this table in case we want it again (just saves time)

# Delete "variable" column, since we only pulled one variable (so we don't need it)
acs <- acs |> 
  select(-variable)

# Add columns with the LB/UB of the 90% CI 
## Define critical value from a N(0,1) distribution for a 90% confidence interval
Z <- qnorm(p = 1 - (0.1/2), 
           mean = 0, 
           sd = 1) 
acs <- acs |> 
  mutate(LB_Med_Income = estimate - Z * moe, 
         LU_Med_Income = estimate + Z * moe)

# Also, remove the "NAME" column from the ACS since we can merge on GEOID and use the name from the PLACES data
acs <- acs |> 
  select(-NAME)

# Rename "estimate" column to be more descriptive 
acs = acs |> 
  rename(Med_Income = estimate, 
         MoE_Med_Income = moe)

# Merge PLACES and ACS data 
places_acs <- places |> 
  left_join(y = acs, 
            by = join_by(LocationID == GEOID))

# Save the merged data
places_acs |> 
  write.csv("~/Dropbox (Wake Forest University)/8 - STUDENTS/Darcy-Concentration-Curves/merged_acs_places.csv", 
            row.names = FALSE)
