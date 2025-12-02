rm(list = ls())

setwd("/Users/gerhardlaimer/Desktop/mcf-fc/assignment1")

if (!require(librarian)) install.packages("librarian")
library(librarian)

shelf(tidyverse, janitor, zoo, gridExtra, seasonal, TTR, 
      mFilter, stargazer
      )

###### Create helper functions for 
# change date to the beginning of the quarter
begin_quarter <- function(df) {
  df %>%
    filter(DATE >= "2000-03-31") %>%
    select(-1) %>%                                          # drop first column
    mutate(
      TIME.PERIOD = gsub("Q", "-Q", TIME.PERIOD), 
      TIME.PERIOD = as.yearqtr(TIME.PERIOD, format = "%Y-Q%q"),
      TIME.PERIOD = as.Date(TIME.PERIOD)
    ) 
}

# prepare data for linear model
prep_for_model <- function(df,
                           COVID_start = as.Date("2020-01-01"),
                           trim_start = "2010Q1",
                           trim_end   = "2024Q1",
                           drop_q     = c("2020Q1", "2020Q2"),
                           win = 4) {
  df %>%
    arrange(TIME.PERIOD) %>%
    mutate(
      unemp_sma = SMA(unemp, n = win), # simple moving average
      gap       = unemp - unemp_sma,
      COVID     = if_else(TIME.PERIOD >= COVID_start , 1, 0), # dummy COVID
      TIME.PERIOD = as.yearqtr(TIME.PERIOD),
      TIME.PERIOD = format(TIME.PERIOD, "%YQ%q")
    ) %>%
    arrange(TIME.PERIOD) %>%
    mutate(
      pi_lag1      = lag(pi, 4),
      pi_lag2      = lag(pi, 8),
      energy_lag1  = lag(energy, 4),
      imp_lag1     = lag(imp, 4)
    ) %>%
    filter(
      TIME.PERIOD >= trim_start,
      TIME.PERIOD <= trim_end,
      !(TIME.PERIOD %in% drop_q)
    )
}

### Create the base model 
lm_PC <- function(df) { lm(pi ~ gap + I(gap * COVID) + COVID + pi_e + 
                        pi_lag1 + pi_lag2 + energy_lag1 + imp_lag1, 
                        data = df)
                       }

######### TASK a) Baseline Model for EA, AUT, GER

### EURO AREA
E_pi     <- read.csv("data/EA_pi.csv")
E_unemp  <- read.csv("data/EA_unemp.csv")
E_energy <- read.csv("data/EA_energy.csv")
PSF      <- read.csv("data/PSF.csv")
IMP      <- read.csv("data/IMP.csv")

# Standardized quarter parsing
E_pi     <- begin_quarter(E_pi)
E_unemp  <- begin_quarter(E_unemp)
E_energy <- begin_quarter(E_energy)
PSF      <- begin_quarter(PSF)
IMP      <- begin_quarter(IMP)

# Join
EA <- E_pi %>%
  left_join(E_energy %>% select(TIME.PERIOD, energy), by = "TIME.PERIOD") %>%
  left_join(E_unemp  %>% select(TIME.PERIOD, unemp),  by = "TIME.PERIOD") %>%
  left_join(PSF      %>% select(TIME.PERIOD, pi_e),   by = "TIME.PERIOD") %>%
  left_join(IMP      %>% select(TIME.PERIOD, imp),    by = "TIME.PERIOD")

EA_model <- prep_for_model(EA)
EA_lm    <- lm_PC(EA_model)
summary(EA_lm)


### AUSTRIA
A_pi     <- read.csv("data/A_pi.csv")
A_unemp  <- read.csv("data/A_unemp.csv")
A_energy <- read.csv("data/A_energy.csv")

A_pi     <- begin_quarter(A_pi)
A_unemp  <- begin_quarter(A_unemp)
A_energy <- begin_quarter(A_energy)

A <- A_pi %>%
  left_join(A_energy %>% select(TIME.PERIOD, energy), by = "TIME.PERIOD") %>%
  left_join(A_unemp  %>% select(TIME.PERIOD, unemp),  by = "TIME.PERIOD") %>%
  left_join(PSF      %>% select(TIME.PERIOD, pi_e),   by = "TIME.PERIOD") %>%
  left_join(IMP      %>% select(TIME.PERIOD, imp),    by = "TIME.PERIOD")

A_model <- prep_for_model(A)
A_lm    <- lm_PC(A_model)
summary(A_lm)


### GERMANY
G_pi     <- read.csv("data/G_pi.csv")
G_unemp  <- read.csv("data/G_unemp.csv")
G_energy <- read.csv("data/G_energy.csv")

G_pi     <- begin_quarter(G_pi)
G_unemp  <- begin_quarter(G_unemp)
G_energy <- begin_quarter(G_energy)

G <- G_pi %>%
  left_join(G_energy %>% select(TIME.PERIOD, energy), by = "TIME.PERIOD") %>%
  left_join(G_unemp  %>% select(TIME.PERIOD, unemp),  by = "TIME.PERIOD") %>%
  left_join(PSF      %>% select(TIME.PERIOD, pi_e),   by = "TIME.PERIOD") %>%
  left_join(IMP      %>% select(TIME.PERIOD, imp),    by = "TIME.PERIOD")

G_model <- prep_for_model(G)
G_lm    <- lm_PC(G_model)
summary(G_lm)


##### Stargazer to Latex
stargazer(EA_lm, A_lm, G_lm,
          type = "latex",
          title = "Extended Phillips Curve Estimates",
          out = "tables/Phillips_Table.tex",
          column.labels = c("Euro Area", "Austria", "Germany"),
          dep.var.labels = "Inflation (pi)",
          covariate.labels = c( "Gap", "Gap × COVID",
                                "COVID ", "Expected Inflation",
                                "Lag(pi, 4Q)", "Lag(pi, 8Q)",
                                "Energy Price Lag", "Import Price Lag"),
          omit.stat = c("f"),
          digits = 3)


######### TASK b) Create two variations of the base model 
### Quadratic Model: GAP x GAP
qm_gap_PC <- function(df) {lm(pi ~ gap + I(gap^2) + I(gap*COVID) + COVID + 
                              pi_e + pi_lag1 + pi_lag2 + energy_lag1 + imp_lag1, 
                              data = df)
                           }

EA_qm_gap <- qm_gap_PC(EA_model)
A_qm_gap  <- qm_gap_PC(A_model)
G_qm_gap  <- qm_gap_PC(G_model)


### Latex table for Gap x Gap model 
stargazer(
  EA_qm_gap, A_qm_gap, G_qm_gap,
  type = "latex",
  title = "Extended Phillips Curve with Quadratic Term",
  out = "tables/Phillips_Table_QUAD.tex",
  column.labels = c("Euro Area", "Austria", "Germany"),
  dep.var.labels = "Inflation (pi)",
  covariate.labels = c( "Gap", "GAP \\times GAP", "Gap \\times COVID",
                        "COVID ", "Expected Inflation", "Lag(pi, 4Q)",
                        "Lag(pi, 8Q)", "Energy Price Lag","Import Price Lag"),
  omit.stat = c("f"),
  digits = 3,
  label = "tab:phillips_quad",
  no.space = TRUE
)


### Linear Model: dummy variable for War in Ukraine
lm_war_PC <- function(df) {
  df <- df %>%
    mutate( 
      WAR = if_else(TIME.PERIOD >= "2022Q1", 1, 0)
      )
  lm(pi ~ gap + I(gap * COVID) + I(gap * WAR) + COVID + WAR + pi_e + 
     pi_lag1 + pi_lag2 + energy_lag1 + imp_lag1,
     data = df
  )
}

EA_lm_war <- lm_war_PC(EA_model)
A_lm_war  <- lm_war_PC(A_model)
G_lm_war  <- lm_war_PC(G_model)


## Latex table for dummy WAR model 
stargazer(
  EA_lm_war, A_lm_war, G_lm_war,
  type = "latex",
  title = "Extended Phillips Curve with COVID and WAR",
  out = "tables/Phillips_Table_War.tex",
  column.labels = c("Euro Area", "Austria", "Germany"),
  dep.var.labels = "Inflation (pi)",
  covariate.labels = c( "Unemployment Gap", "Gap \\times COVID", "Gap \\times WAR",
                        "COVID ", "WAR ", "Expected Inflation", "Lag(pi, 4Q)",
                        "Lag(pi, 8Q)", "Energy Price Lag", "Import Price Lag"),
  omit.stat = c("f"),
  digits = 3,
  label = "tab:phillips_war",
  no.space = TRUE
)

