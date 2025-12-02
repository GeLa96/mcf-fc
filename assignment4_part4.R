library(dplyr)
library(tidyr)
library(ggplot2)

## load data
df <- read.csv2("data_assign4.csv", header = TRUE, sep = ";", dec = ",")
View(df)

# Extract metadata
meta_country <- as.character(df[1, ]) # Country names
meta_series <- as.character(df[2, ]) # Series type (Interest, HICP, GDP)

# Remove metadata rows
data <- df[-c(1, 2), ]
# View(data)

# Build clean column names: "Country||Series"
names(data) <- paste0(meta_country, "||", meta_series)

# Fix Year column
names(data)[1] <- "Year"
data$Year <- as.integer(data$Year)

# Convert to long format
df_long <- data %>%
    pivot_longer(
        cols      = -Year,
        names_to  = c("Country", "Series"),
        names_sep = "\\|\\|",
        values_to = "value"
    ) %>%
    mutate(
        value = as.numeric(gsub(",", ".", value, fixed = TRUE))
    ) %>%
    # Map the descriptive Series names to i / hicp / gdp
    mutate(
        TIME_PERIOD = case_when(
            Series == "Interest rates, Short term nominal (ISN)" ~ "i",
            grepl("^HICP - Overall index", Series) ~ "hicp",
            grepl("^Gross domestic product at market prices", Series) ~ "gdp",
            TRUE ~ NA_character_
        )
    ) %>%
    # Keep only the three series we care about
    filter(!is.na(TIME_PERIOD))

# Keep only the true series (drop duplicate EU interest rates, long-term rates, etc.)
df_long <- df_long %>%
    filter(
        (TIME_PERIOD == "i" & Series == "Interest rates, Short term nominal (ISN)") |
            (TIME_PERIOD == "hicp" & grepl("^HICP - Overall index", Series)) |
            (TIME_PERIOD == "gdp" & grepl("^Gross domestic product at market prices", Series))
    )

# From here on, use `TIME PERIOD` instead of `Series`
df_tidy <- df_long %>%
    mutate(
        Country = ifelse(
            grepl("^EU", Country, ignore.case = TRUE),
            Country,
            gsub("\\s*\\(.*\\)", "", Country)
        )
    )
# View(df_tidy)

df_tidy <- df_tidy %>%
    mutate(
        Country = case_when(
            # 1) European Union -> EU
            grepl("^European Union", Country, ignore.case = TRUE) ~ "EU",

            # 2) EU, EU(…) etc. -> EU  (but NOT "Euro area")
            grepl("^EU\\b", Country, ignore.case = TRUE) ~ "EU",

            # 3) Euro area (…) or Euro area 17/19/20 (…) -> keep "Euro area", "Euro area 17", etc.
            grepl("^Euro area", Country, ignore.case = TRUE) ~
                trimws(gsub("\\s*\\(.*\\)", "", Country)),

            # 4) Czech Republic -> Czechia
            Country == "Czech Republic" ~ "Czechia",

            # 5) All other countries: drop parentheses like "(AT)"
            TRUE ~ trimws(gsub("\\s*\\(.*\\)", "", Country))
        )
    ) %>%
    group_by(Year, Country, `TIME_PERIOD`) %>%
    summarise(
        value = mean(value, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(Country, Year, `TIME_PERIOD`)

View(df_tidy)

# Pivot to wide format: one row per Year–Country, columns i, hicp, gdp
df_panel <- df_tidy %>%
    pivot_wider(
        id_cols = c(Year, Country),
        names_from = `TIME_PERIOD`,
        values_from = value
    ) %>%
    arrange(Country, Year) # within each country: 2000, 2001, 2002, ...

# glimpse(df_panel)
# View(df_panel)
# print(df_panel)


## helper function for missing values
fix_na_seq <- function(x) {
    n <- length(x)
    if (n <= 1) {
        return(x)
    }

    i <- 1
    while (i <= n) {
        if (is.na(x[i])) {
            # find end of this NA-run
            j <- i
            while (j + 1 <= n && is.na(x[j + 1])) {
                j <- j + 1
            }
            run_len <- j - i + 1

            if (run_len == 1) {
                # Case: single NA

                if (i == 1 && !is.na(x[i + 1])) {
                    # leading single NA -> take next value
                    x[i] <- x[i + 1]
                } else if (i == n && !is.na(x[i - 1])) {
                    # trailing single NA -> take previous value
                    x[i] <- x[i - 1]
                } else if (i > 1 && i < n && !is.na(x[i - 1]) && !is.na(x[i + 1])) {
                    # interior single NA -> average of neighbors
                    x[i] <- (x[i - 1] + x[i + 1]) / 2
                }
                # else: one side is NA too -> leave as NA
            } else {
                # run_len > 1 -> leave all these NAs unchanged
            }

            i <- j + 1
        } else {
            i <- i + 1
        }
    }
    x
}
### Apply missing value fixing per Country
group_by(Country) %>%
    arrange(Year, .by_group = TRUE) %>%
    mutate(
        i    = fix_na_seq(i),
        hicp = fix_na_seq(hicp),
        gdp  = fix_na_seq(gdp)
    ) %>%
    ungroup()
View(df_panel)

## add column real interest rate
df_panel <- df_panel %>%
    mutate(
        r = i - hicp
    )
# View(df_panel)

# create a new country in column Countrey that uses gdp from Euro area 20 and i and hicp from Euro area
# 1. Take Euro area i, hicp, r
ea <- df_panel %>%
    filter(Country == "Euro area") %>%
    select(Year, i, hicp, r)

# 2. Take Euro area 20 gdp
ea20 <- df_panel %>%
    filter(Country == "Euro area 20") %>%
    select(Year, gdp)

# 3. Join them and build synthetic Euro area 20 country
ea_combined <- ea %>%
    inner_join(ea20, by = "Year") %>%
    transmute(
        Year,
        Country = "Euro area 20 (EA i & hicp, EA20 gdp)",
        i,
        hicp,
        gdp,
        r
    )

df_panel <- bind_rows(df_panel, ea_combined)
# View(df_panel)
# Quick check:
df_panel %>%
    filter(grepl("Euro area 20", Country)) %>%
    arrange(Year) %>%
    head(10)

#### task 4a)
## Exclude the aggregate entities from the eurozone average calculation
ez_exclude <- c(
    "EU",
    "Euro area",
    "Euro area 17",
    "Euro area 19",
    "Euro area 20",
    "Euro area 20 (EA i & hicp, EA20 gdp)"
)

## for the member countries calculate the average of r, hicp, gdp for each year
ez_avg <- df_panel %>%
    filter(!Country %in% ez_exclude) %>% # only member countries
    group_by(Year) %>%
    summarise(
        ez_r    = mean(r, na.rm = TRUE),
        ez_hicp = mean(hicp, na.rm = TRUE),
        ez_gdp  = mean(gdp, na.rm = TRUE),
        .groups = "drop"
    )
## differnces of each country to eurozone average
df_div <- df_panel %>%
    left_join(ez_avg, by = "Year") %>%
    mutate(
        diff_r    = r - ez_r,
        diff_hicp = hicp - ez_hicp,
        diff_gdp  = gdp - ez_gdp
    )
print(df_div)

## plot
plot_data <- df_div %>%
    filter(!Country %in% ez_exclude) %>%
    select(Year, Country, diff_r, diff_hicp, diff_gdp) %>%
    pivot_longer(starts_with("diff_"), names_to = "var", values_to = "value")

ggplot(plot_data, aes(x = Year, y = value, group = Country, colour = Country)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(alpha = 0.7) +
    facet_wrap(~var, scales = "free_y", ncol = 1) +
    theme_minimal() +
    labs(x = "Year", y = "Differential vs EZ average")
