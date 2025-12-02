library(tidyverse)
library(janitor)
library(readr)
library(dplyr)
library(zoo)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(seasonal)
library(TTR)

###### TASK a) 
##### EURO AREA
## Load data 
# inflation measures
E_pi <- read.csv("EA_pi.csv")
E_unemp <- read.csv("EA_unemp.csv")
E_energy <- read.csv("EA_energy.csv")
# forecast 
PSF <- read.csv("PSF.csv")
# import 
IMP <- read.csv("IMP.csv")

## trim data 
E_pi <- E_pi %>%
  filter(DATE >= as.Date("2000-03-31"))
E_unemp <- E_unemp %>%
  filter(DATE >= as.Date("2000-03-31"))
E_energy <- E_energy %>%
  filter(DATE >= as.Date("2000-03-31"))
PSF <- PSF %>%
  filter(DATE >= as.Date("2000-03-31"))
IMP <- IMP %>%
  filter(DATE >= as.Date("2000-03-31"))

## creating a data frame
E_pi <- E_pi %>% select(-1) %>% mutate(
  TIME.PERIOD = gsub("Q", "-Q", TIME.PERIOD),        # make it "1997-Q1"
  TIME.PERIOD = as.yearqtr(TIME.PERIOD, format = "%Y-Q%q"),  # convert to yearqtr class
  TIME.PERIOD = as.Date(TIME.PERIOD)                 # turns it into first day of quarter
)
E_energy <- E_energy %>% select(-1) %>% mutate(
  TIME.PERIOD = gsub("Q", "-Q", TIME.PERIOD),        
  TIME.PERIOD = as.yearqtr(TIME.PERIOD, format = "%Y-Q%q"),  
  TIME.PERIOD = as.Date(TIME.PERIOD)                 
)
E_unemp <- E_unemp %>% select(-1) %>% mutate(
  TIME.PERIOD = gsub("Q", "-Q", TIME.PERIOD),        
  TIME.PERIOD = as.yearqtr(TIME.PERIOD, format = "%Y-Q%q"),  
  TIME.PERIOD = as.Date(TIME.PERIOD)                 
)
PSF <- PSF %>% select(-1) %>% mutate(
  TIME.PERIOD = gsub("Q", "-Q", TIME.PERIOD),        
  TIME.PERIOD = as.yearqtr(TIME.PERIOD, format = "%Y-Q%q"),  
  TIME.PERIOD = as.Date(TIME.PERIOD)                 
)
IMP <- IMP %>% select(-1) %>% mutate(
  TIME.PERIOD = gsub("Q", "-Q", TIME.PERIOD),        
  TIME.PERIOD = as.yearqtr(TIME.PERIOD, format = "%Y-Q%q"),  
  TIME.PERIOD = as.Date(TIME.PERIOD)                 
)

## Joining data 
EA <- E_pi %>%
  left_join(E_energy %>% select(TIME.PERIOD, energy), by = "TIME.PERIOD") %>%
  left_join(E_unemp %>% select(TIME.PERIOD, unemp),  by = "TIME.PERIOD") %>%
  left_join(PSF %>% select(TIME.PERIOD, pi_e),  by = "TIME.PERIOD") %>%
  left_join(IMP %>% select(TIME.PERIOD, imp),  by = "TIME.PERIOD") 
glimpse(EA)
## adding gap
n_win = 4
EA <- EA %>% arrange(TIME.PERIOD) %>% mutate( 
  unemp_sma = SMA(unemp, n = n_win), 
  gap = unemp - unemp_sma                    
  )
## adding dummy variable 
EA <- EA %>%
  mutate(
    COVID = if_else(TIME.PERIOD >= as.Date("2020-01-01"), 1, 0)
  )
## change TIME.PERIOD
EA <- EA %>%
  mutate(
    TIME.PERIOD = as.yearqtr(TIME.PERIOD),           # Convert to quarterly class
    TIME.PERIOD = format(TIME.PERIOD, "%YQ%q")       # Format like 2000Q1, 2000Q2, etc.
  )
## adding lags  # lags for one quarter or one year 
EA <- EA %>%
  arrange(TIME.PERIOD) %>%  
  mutate(
    pi_lag1      = lag(pi, 4),
    pi_lag2      = lag(pi, 8),
    energy_lag1  = lag(energy, 4),
    imp_lag1     = lag(imp, 4)
  )
## trim the data: 2010 Q1 to 2024 Q1 ; excluding 2020 Q1 and Q2 
EA_model <- EA %>%
  filter(
    TIME.PERIOD >= "2010Q1",     # keep from Q1 2010
    TIME.PERIOD <= "2024Q1",     # keep up to Q1 2024
    !(TIME.PERIOD %in% c("2020Q1", "2020Q2"))  # drop Q1 and Q2 2020
  )
# Model
EA_lm <- lm(pi ~ gap + I(gap*COVID) + COVID + pi_e + pi_lag1 + pi_lag2 + energy_lag1 + imp_lag1, data = EA_model)
summary(EA_model)

###### AUSTRIA ######
A_pi <- read.csv("A_pi.csv")
A_unemp <- read.csv("A_unemp.csv")
A_energy <- read.csv("A_energy.csv")
## creating data frame
A_pi <- A_pi %>% select(-1) %>% mutate(
  TIME.PERIOD = gsub("Q", "-Q", TIME.PERIOD),        
  TIME.PERIOD = as.yearqtr(TIME.PERIOD, format = "%Y-Q%q"),  
  TIME.PERIOD = as.Date(TIME.PERIOD)                 
)
A_energy <- A_energy %>% select(-1) %>% mutate(
  TIME.PERIOD = gsub("Q", "-Q", TIME.PERIOD),        
  TIME.PERIOD = as.yearqtr(TIME.PERIOD, format = "%Y-Q%q"),  
  TIME.PERIOD = as.Date(TIME.PERIOD)                 
)
A_unemp <- A_unemp %>% select(-1) %>% mutate(
  TIME.PERIOD = gsub("Q", "-Q", TIME.PERIOD),        
  TIME.PERIOD = as.yearqtr(TIME.PERIOD, format = "%Y-Q%q"),  
  TIME.PERIOD = as.Date(TIME.PERIOD)                 
)
A <- A_pi %>%
  left_join(E_energy %>% select(TIME.PERIOD, energy), by = "TIME.PERIOD") %>%
  left_join(E_unemp %>% select(TIME.PERIOD, unemp),  by = "TIME.PERIOD") %>%
  left_join(PSF %>% select(TIME.PERIOD, pi_e),  by = "TIME.PERIOD") %>%
  left_join(IMP %>% select(TIME.PERIOD, imp),  by = "TIME.PERIOD") 
## adding gap
n_win = 4
A <- A %>% arrange(TIME.PERIOD) %>% mutate( 
  unemp_sma = SMA(unemp, n = n_win), 
  gap = unemp - unemp_sma                    
)
## adding dummy variable 
A <- A %>%
  mutate(
    COVID = if_else(TIME.PERIOD >= as.Date("2020-01-01"), 1, 0)
  )
## change TIME.PERIOD
A <- A %>%
  mutate(
    TIME.PERIOD = as.yearqtr(TIME.PERIOD),           # Convert to quarterly class
    TIME.PERIOD = format(TIME.PERIOD, "%YQ%q")       # Format like 2000Q1, 2000Q2, etc.
  )
## adding lags  # lags for one quarter or one year 
A <- A %>%
  arrange(TIME.PERIOD) %>%  
  mutate(
    pi_lag1      = lag(pi, 4),
    pi_lag2      = lag(pi, 8),
    energy_lag1  = lag(energy, 4),
    imp_lag1     = lag(imp, 4)
  )
## trim the data: 2010 Q1 to 2024 Q1 ; excluding 2020 Q1 and Q2 
A_model <- A %>%
  filter(
    TIME.PERIOD >= "2010Q1",     # keep from Q1 2010
    TIME.PERIOD <= "2024Q1",     # keep up to Q1 2024
    !(TIME.PERIOD %in% c("2020Q1", "2020Q2"))  # drop Q1 and Q2 2020
  )
# Model
A_lm <- lm(pi ~ gap + I(gap*COVID) + COVID + pi_e + pi_lag1 + pi_lag2 + energy_lag1 + imp_lag1, data = A_model)
summary(A_model)

###### GERMANY ######
G_pi <- read.csv("G_pi.csv")
G_unemp <- read.csv("G_unemp.csv")
G_energy <- read.csv("G_energy.csv")
## creating data frame
G_pi <- G_pi %>% select(-1) %>% mutate(
  TIME.PERIOD = gsub("Q", "-Q", TIME.PERIOD),        
  TIME.PERIOD = as.yearqtr(TIME.PERIOD, format = "%Y-Q%q"),  
  TIME.PERIOD = as.Date(TIME.PERIOD)                 
)
G_energy <- G_energy %>% select(-1) %>% mutate(
  TIME.PERIOD = gsub("Q", "-Q", TIME.PERIOD),        
  TIME.PERIOD = as.yearqtr(TIME.PERIOD, format = "%Y-Q%q"),  
  TIME.PERIOD = as.Date(TIME.PERIOD)                 
)
G_unemp <- G_unemp %>% select(-1) %>% mutate(
  TIME.PERIOD = gsub("Q", "-Q", TIME.PERIOD),        
  TIME.PERIOD = as.yearqtr(TIME.PERIOD, format = "%Y-Q%q"),  
  TIME.PERIOD = as.Date(TIME.PERIOD)                 
)
G <- G_pi %>%
  left_join(E_energy %>% select(TIME.PERIOD, energy), by = "TIME.PERIOD") %>%
  left_join(E_unemp %>% select(TIME.PERIOD, unemp),  by = "TIME.PERIOD") %>%
  left_join(PSF %>% select(TIME.PERIOD, pi_e),  by = "TIME.PERIOD") %>%
  left_join(IMP %>% select(TIME.PERIOD, imp),  by = "TIME.PERIOD") 
## adding gap
n_win = 4
G <- G %>% arrange(TIME.PERIOD) %>% mutate( 
  unemp_sma = SMA(unemp, n = n_win), 
  gap = unemp - unemp_sma                    
)
## adding dummy variable 
G <- G %>%
  mutate(
    COVID = if_else(TIME.PERIOD >= as.Date("2020-01-01"), 1, 0)
  )
## change TIME.PERIOD
G <- G %>%
  mutate(
    TIME.PERIOD = as.yearqtr(TIME.PERIOD),           # Convert to quarterly class
    TIME.PERIOD = format(TIME.PERIOD, "%YQ%q")       # Format like 2000Q1, 2000Q2, etc.
  )
## adding lags  # lags for one quarter or one year 
G <- G %>%
  arrange(TIME.PERIOD) %>%  
  mutate(
    pi_lag1      = lag(pi, 4),
    pi_lag2      = lag(pi, 8),
    energy_lag1  = lag(energy, 4),
    imp_lag1     = lag(imp, 4)
  )
## trim the data: 2010 Q1 to 2024 Q1 ; excluding 2020 Q1 and Q2 
G_model <- G %>%
  filter(
    TIME.PERIOD >= "2010Q1",     # keep from Q1 2010
    TIME.PERIOD <= "2024Q1",     # keep up to Q1 2024
    !(TIME.PERIOD %in% c("2020Q1", "2020Q2"))  # drop Q1 and Q2 2020
  )
# Model
G_lm <- lm(pi ~ gap + I(gap*COVID) + COVID + pi_e + pi_lag1 + pi_lag2 + energy_lag1 + imp_lag1, data = G_model)
summary(G_model)

###### TASK b) 
#### 1 variation quadratic gap
E_qm <- lm(pi ~ gap + I(gap^2) + I(gap*COVID) + COVID + pi_e + pi_lag1 + pi_lag2 + energy_lag1 + imp_lag1, data = E_model)
summary(E_qm)
A_qm <- lm(pi ~ gap + I(gap^2) + I(gap*COVID) + COVID + pi_e + pi_lag1 + pi_lag2 + energy_lag1 + imp_lag1, data = A_model)
summary(A_qm)
G_qm <- lm(pi ~ gap + I(gap^2) + I(gap*COVID) + COVID + pi_e + pi_lag1 + pi_lag2 + energy_lag1 + imp_lag1, data = G_model)
summary(G_qm)
#### 2 variation War in Ukrain
# Define dummy variable for war in Ukrain
EA_model <- EA_model %>%
  mutate(
    WAR = if_else(TIME.PERIOD >= "2022Q1", 1, 0)
  )
A_model <- A_model %>%
  mutate(
    WAR = if_else(TIME.PERIOD >= "2022Q1", 1, 0)
  )
G_model <- G_model %>%
  mutate(
    WAR = if_else(TIME.PERIOD >= "2022Q1", 1, 0)
  )
# Regression with WAR and its interaction:
EA_lm_war <- lm(pi ~ gap + I(gap * COVID) + I(gap * WAR) +
                    COVID + WAR + pi_e + pi_lag1 + pi_lag2 + energy_lag1 + imp_lag1,
                  data = EA_model)
summary(EA_lm_war)
A_lm_war <- lm(pi ~ gap + I(gap * COVID) + I(gap * WAR) +
                    COVID + WAR + pi_e + pi_lag1 + pi_lag2 + energy_lag1 + imp_lag1,
                  data = A_model)
summary(A_lm_war)
G_lm_war <- lm(pi ~ gap + I(gap * COVID) + I(gap * WAR) +
                    COVID + WAR + pi_e + pi_lag1 + pi_lag2 + energy_lag1 + imp_lag1,
                  data = G_model)
summary(G_lm_war)

##### Task c) 
A_anpi <- read.csv("A_anpi.csv")
G_anpi <- read.csv("G_anpi.csv")

