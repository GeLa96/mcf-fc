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
library(mFilter)
library(stargazer)
#install.packages("mFilter")
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
### HP filter
#EA <- EA %>%
#  arrange(TIME.PERIOD) %>%
#  mutate(
#    gap = hpfilter(unemp, type = "lambda", freq = 1600)$cycle
#  )

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
summary(EA_lm)

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
summary(A_lm)

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
summary(G_lm)

##### Stargazer to Latex
stargazer(EA_lm, A_lm, G_lm,
          type = "latex",
          title = "Extended Phillips Curve Estimates",
          out = "Phillips_Table.tex",
          column.labels = c("Euro Area", "Austria", "Germany"),
          dep.var.labels = "Inflation (pi)",
          covariate.labels = c("Gap", "Gap × COVID",
                               "COVID ", "Expected Inflation",
                               "Lag(pi, 4Q)", "Lag(pi, 8Q)",
                               "Energy Price Lag", "Import Price Lag"),
          omit.stat = c("f"),
          digits = 3)

###### TASK b) 
#### 1 variation quadratic gap
EA_qm <- lm(pi ~ gap + I(gap^2) + I(gap*COVID) + COVID + pi_e + pi_lag1 + pi_lag2 + energy_lag1 + imp_lag1, data = EA_model)
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

##### Stargazer to Latex
## exponential term
stargazer(
  EA_qm, A_qm, G_qm,
  type = "latex",
  title = "Extended Phillips Curve with Quadratic Term",
  out = "Phillips_Table_QUAD.tex",
  column.labels = c("Euro Area", "Austria", "Germany"),
  dep.var.labels = "Inflation (pi)",
  covariate.labels = c(
    "Gap",
    "GAP \\times GAP",      # <-- NEW: Quadratic term
    "Gap \\times COVID",
    "COVID ",
    "Expected Inflation",
    "Lag(pi, 4Q)",
    "Lag(pi, 8Q)",
    "Energy Price Lag",
    "Import Price Lag"
  ),
  omit.stat = c("f"),
  digits = 3,
  label = "tab:phillips_quad",
  no.space = TRUE
)

## dummy War
stargazer(
  EA_lm_war, A_lm_war, G_lm_war,
  type = "latex",
  title = "Extended Phillips Curve with COVID and WAR",
  out = "Phillips_Table_War.tex",
  column.labels = c("Euro Area", "Austria", "Germany"),
  dep.var.labels = "Inflation (pi)",
  covariate.labels = c(
    "Unemployment Gap",
    "Gap \\times COVID",
    "Gap \\times WAR",
    "COVID ",
    "WAR ",
    "Expected Inflation",
    "Lag(pi, 4Q)",
    "Lag(pi, 8Q)",
    "Energy Price Lag",
    "Import Price Lag"
  ),
  omit.stat = c("f"),
  digits = 3,
  label = "tab:phillips_war",
  no.space = TRUE
)



##### Task c) 
A_anpi <- read.csv("A_anpi.csv")
G_anpi <- read.csv("G_anpi.csv")
RATE   <- read.csv("MROR.csv")

AUT_HICP_annual <- A_anpi %>%
  transmute(year = as.integer(TIME.PERIOD), pi_annual = as.numeric(pi)) %>%
  arrange(year)

GER_HICP_annual <- G_anpi %>%
  transmute(year = as.integer(TIME.PERIOD), pi_annual = as.numeric(pi)) %>%
  arrange(year)

main_rate_median <- RATE %>%
  transmute(year = as.integer(TIME.PERIOD), mror_median = as.numeric(rate)) %>%
  arrange(year)

## Function to detect annual jump
detect_annual_jump <- function(df){
  df %>%
    mutate(change = pi_annual - dplyr::lag(pi_annual)) %>%
    filter(!is.na(change), change > 2) %>%
    slice(1) %>%
    pull(year)
}

# Panel with optional manual anchor
make_country_panels <- function(hicp_df, rate_median_df, country_name, anchor_year = NULL) {
  # if no manual anchor provided, detect after 2020 (your original logic)
  if (is.null(anchor_year)) {
    anchor_year <- hicp_df %>%
      arrange(year) %>%
      mutate(change = pi_annual - dplyr::lag(pi_annual)) %>%
      filter(year >= 2020, change > 2) %>%
      slice(1) %>% pull(year)
  }
  if (length(anchor_year) == 0 || is.na(anchor_year)) {
    stop(paste("No anchor year available for", country_name))
  }
  
## Normalizes Period t = -1 
  infl_panel <- hicp_df %>%
    mutate(t = year - anchor_year) %>%
    filter(dplyr::between(t, -5, 5)) %>%
    mutate(
      base_pi = {
        v <- pi_annual[t == -1]
        if (length(v) == 0 || is.na(v[1])) NA_real_ else v[1]
      },
      # if base missing (e.g., series starts at anchor), fallback to subtract first available value
      base_pi = ifelse(is.na(base_pi), first(pi_annual), base_pi),
      pi_dev  = pi_annual - base_pi
    )
  
# Compute real = nominal - inflation (absolute),
  yrs <- tibble(year = (anchor_year - 5):(anchor_year + 5)) %>% mutate(t = year - anchor_year)
  
  rate_joined <- yrs %>%
    left_join(rate_median_df, by = "year") %>%
    left_join(hicp_df,        by = "year") %>%
    arrange(year) %>%
    mutate(
      mror_median = zoo::na.locf(mror_median, na.rm = FALSE),
      mror_median = zoo::na.locf(mror_median, fromLast = TRUE),
      real_median_rate = mror_median - pi_annual
    )
  
  base_nominal_2021 <- rate_joined %>% filter(year == 2021) %>% pull(mror_median) %>% .[1]
  if (length(base_nominal_2021) == 0 || is.na(base_nominal_2021)) {
    stop(paste("Cannot find/construct 2021 median rate for", country_name))
  }
  
  rate_panel <- rate_joined %>%
    filter(dplyr::between(t, -5, 5)) %>%
    transmute(
      t,
      nominal_dev      = mror_median - base_nominal_2021,  
      real_median_rate = real_median_rate                  
    )
  
  list(infl_panel = infl_panel, rate_panel = rate_panel, name = country_name, anchor_year = anchor_year)
}

# Manual anchors 
AT_panels <- make_country_panels(AUT_HICP_annual, main_rate_median, "Austria",  anchor_year = 2022)
DE_panels <- make_country_panels(GER_HICP_annual, main_rate_median, "Germany", anchor_year = 2021)

# Plots 
plot_infl <- function(panel, name, anchor_year) {
  ggplot(panel, aes(x = t, y = pi_dev)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_line(linewidth = 1.2) +
    scale_x_continuous(breaks = -5:5) +
    labs(
      title = paste0(name, " – Inflation deviation (t−1=0, anchor=", anchor_year, ")"),
      x = "Years relative to anchor (t)",
      y = "Deviation (pp)"
    ) +
    theme_minimal()
}

plot_rate <- function(panel, name, anchor_year) {
  ggplot(panel, aes(x = t)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_line(aes(y = nominal_dev,      color = "Nominal  Rate (2021=0)"), linewidth = 1.2) +
    geom_line(aes(y = real_median_rate, color = "Real Rate (absolute)"),  linewidth = 1.2) +
    scale_x_continuous(breaks = -5:5) +
    labs(
      title = paste0(name, " – Nominal Rate (2021=0) vs  Real Rate (anchor=", anchor_year, ")"),
      x = "Years relative to anchor (t)",
      y = "Rate (%)",
      color = ""
    ) +
    theme_minimal()
}

# Render
AT_infl_plot <- plot_infl(AT_panels$infl_panel, "Austria",  AT_panels$anchor_year)
AT_rate_plot <- plot_rate(AT_panels$rate_panel, "Austria",  AT_panels$anchor_year)
DE_infl_plot <- plot_infl(DE_panels$infl_panel, "Germany", DE_panels$anchor_year)
DE_rate_plot <- plot_rate(DE_panels$rate_panel, "Germany", DE_panels$anchor_year)

AT_infl_plot; AT_rate_plot; DE_infl_plot; DE_rate_plot

# Save 
ggsave(AT_infl_plot, filename = "Inflation Austria.png", height = 5, width = 8, bg = "white")
ggsave(AT_rate_plot, filename = "Interest Rates Austria.png", height = 5, width = 8, bg = "white")
ggsave(DE_infl_plot, filename = "Inflation Germany.png", height = 5, width = 8, bg = "white")
ggsave(DE_rate_plot, filename = "Interest Rate Germany.png", height = 5, width = 8, bg = "white")






