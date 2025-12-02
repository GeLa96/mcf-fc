##### MCF Assignement 5
##### Question 4

### Load necessary libraries
librarian::shelf(readxl, tidyverse, lubridate, ggplot2)

### Define the path to the Excel file
ea_mpd_path <- "assignment5/data/Adjusted_Dataset_EA-MPD.xlsx"
# excel_sheets(ea_mpd_path)

### load press conference data
## 6 press conferences from 2014 to 2015
ea_pc <- read_excel(ea_mpd_path, sheet = "Press Conference Window (Q4)")
glimpse(ea_pc)

### change date format
ea_pc <- ea_pc %>%
    mutate(Date = as.Date(date))

glimpse(ea_pc)

### Summaries the cahnge in basis points for 5Y and 10Y yields by country (20Y not included since only available for Germany)
### Summaries for all selected dates
ea_pc %>%
    summarise(
        DE5Y_total  = sum(DE5Y, na.rm = TRUE),
        DE10Y_total = sum(DE10Y, na.rm = TRUE),
        FR5Y_total  = sum(FR5Y, na.rm = TRUE),
        FR10Y_total = sum(FR10Y, na.rm = TRUE),
        ES5Y_total  = sum(ES5Y, na.rm = TRUE),
        ES10Y_total = sum(ES10Y, na.rm = TRUE),
        IT5Y_total  = sum(IT5Y, na.rm = TRUE),
        IT10Y_total = sum(IT10Y, na.rm = TRUE)
    ) %>%
    print(width = Inf)


##### Question 5

### load press conference data
## 5 related press conferences dates from 2022 to 2023
ea_pc_22_23 <- read_excel(ea_mpd_path, sheet = "Press Conference Window (Q5)")

### change date format
ea_pc_22_23 <- ea_pc_22_23 %>%
    mutate(Date = as.Date(date))

### Summaries the cahnge in basis points for 5Y and 10Y yields by country
### Summaries for all selected dates
ea_pc_22_23 %>%
    summarise(
        DE5Y_total  = sum(DE5Y, na.rm = TRUE),
        DE10Y_total = sum(DE10Y, na.rm = TRUE),
        FR5Y_total  = sum(FR5Y, na.rm = TRUE),
        FR10Y_total = sum(FR10Y, na.rm = TRUE),
        ES5Y_total  = sum(ES5Y, na.rm = TRUE),
        ES10Y_total = sum(ES10Y, na.rm = TRUE),
        IT5Y_total  = sum(IT5Y, na.rm = TRUE),
        IT10Y_total = sum(IT10Y, na.rm = TRUE)
    ) %>%
    print(width = Inf)

#### Since it is expected, that the yields increase, but they dont, further investigation is needed
### Check individual events
ea_pc_22_23 %>%
    select(Date, DE5Y, DE10Y, FR5Y, FR10Y, ES5Y, ES10Y, IT5Y, IT10Y)
# Only for 2022-12-15 there is a positive change.

### Calculating simple averages for 10Y yields to see the average change during the selected events
ea_pc_22_23 %>%
    summarise(
        DE10Y_avg = mean(DE10Y, na.rm = TRUE),
        FR10Y_avg = mean(FR10Y, na.rm = TRUE),
        ES10Y_avg = mean(ES10Y, na.rm = TRUE),
        IT10Y_avg = mean(IT10Y, na.rm = TRUE)
    )
