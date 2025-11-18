library(dplyr)
library(tidyr)

df <- read.csv2("data_assign4_adjusted.csv", header = TRUE, sep = ";", dec = ",", check.names = FALSE)
# View(df)
## Country names are in the column names
meta_country <- names(df)

# First data row contains the variable type: "i", "hicp", "gdp", ...
meta_var <- as.character(df[1, ])

# data excluding first row
data <- df[-1, ]

# fix column names combining country and variable
names(data) <- paste0(meta_country, "_", meta_var)
names(data)[1] <- "Year" # first column is the Year column

# converts Year to integer
data$Year <- as.integer(as.character(data$Year))

# Replace commas with dots and convert to numeric for all other columns
data[, -1] <- lapply(data[, -1], function(x) as.numeric(gsub(",", ".", as.character(x))))

# Pivot to long tidy form
df_long <- data %>%
    pivot_longer(
        cols = -Year,
        names_to = c("Country", "Variable"),
        names_sep = "_",
        values_to = "value"
    )

# Clean and pivot wider to get one row per Year–Country
df_tidy <- df_long %>%
    # Keep only i, hicp, gdp and drop any empty Variable names
    filter(
        Variable %in% c("i", "hicp", "gdp"),
        Variable != ""
    ) %>%
    # If there are duplicates per Year–Country–Variable, average them
    group_by(Year, Country, Variable) %>%
    summarise(
        value = mean(value, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_wider(
        id_cols = c(Year, Country),
        names_from = Variable,
        values_from = value
    ) %>%
    arrange(Country, Year)

View(df_tidy)


### 4a) Compute real interest rate and differentials vs Eurozone average
## add real interest rate column
df_panel <- df_tidy %>%
    mutate(r = i - hicp)

## Compute Eurozone averages and differentials
# exclude aggregate Eurozone entries from average calculation
ez_exclude <- c("EU", "European Union", "Euro area", "Euro area 17", "Euro area 19", "Euro area 20")

ez_avg <- df_panel %>%
    filter(!Country %in% ez_exclude) %>%
    group_by(Year) %>%
    summarise(
        ez_r    = mean(r, na.rm = TRUE),
        ez_hicp = mean(hicp, na.rm = TRUE),
        ez_gdp  = mean(gdp, na.rm = TRUE),
        .groups = "drop"
    )

df_div <- df_panel %>%
    left_join(ez_avg, by = "Year") %>%
    mutate(
        diff_r    = r - ez_r,
        diff_hicp = hicp - ez_hicp,
        diff_gdp  = gdp - ez_gdp
    )

### Plot 4a)
plot_data <- df_div %>%
    filter(!Country %in% ez_exclude) %>%
    select(Year, Country, diff_r, diff_hicp, diff_gdp) %>%
    pivot_longer(starts_with("diff_"), names_to = "var", values_to = "value") %>%
    filter(is.finite(value))

ggplot(plot_data, aes(x = Year, y = value, group = Country, colour = Country)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(alpha = 0.7) +
    facet_wrap(~var, scales = "free_y", ncol = 1) +
    theme_minimal() +
    labs(x = "Year", y = "Differential vs EZ average") +
    guides(colour = guide_legend(ncol = 1))

ggsave("Assignment4/plot_4a.pdf", plot = last_plot(), width = 8, height = 10)


### 4b) Analyze Quantitatively
# leads for inflation and gdp
df_leads <- df_div %>%
    filter(!Country %in% ez_exclude) %>% # only member states
    group_by(Country) %>%
    arrange(Year, .by_group = TRUE) %>%
    mutate(
        diff_hicp_lead1 = lead(diff_hicp, 1),
        diff_hicp_lead2 = lead(diff_hicp, 2),
        diff_gdp_lead1  = lead(diff_gdp, 1),
        diff_gdp_lead2  = lead(diff_gdp, 2)
    ) %>%
    ungroup()

## differential correlation between interest rate with future inflation and gdp
cors <- df_leads %>%
    summarise(
        corr_r_hicp0 = cor(diff_r, diff_hicp, use = "complete.obs"),
        corr_r_hicp1 = cor(diff_r, diff_hicp_lead1, use = "complete.obs"),
        corr_r_hicp2 = cor(diff_r, diff_hicp_lead2, use = "complete.obs"),
        corr_r_gdp0  = cor(diff_r, diff_gdp, use = "complete.obs"),
        corr_r_gdp1  = cor(diff_r, diff_gdp_lead1, use = "complete.obs"),
        corr_r_gdp2  = cor(diff_r, diff_gdp_lead2, use = "complete.obs")
    )
print(cors)


## local projections
library(purrr)
library(broom)

lp_results <- map_df(0:2, function(h) {
    df_h <- df_leads %>%
        mutate(
            y_hicp = dplyr::lead(diff_hicp, h),
            y_gdp  = dplyr::lead(diff_gdp, h)
        )

    # inflation regression
    reg_hicp <- lm(y_hicp ~ diff_r, data = df_h)
    # growth regression
    reg_gdp <- lm(y_gdp ~ diff_r, data = df_h)

    tibble(
        horizon = h,
        beta_hicp = coef(reg_hicp)[2],
        beta_gdp = coef(reg_gdp)[2]
    )
})
print(lp_results)

library(ggplot2)

lp_results_long <- lp_results %>%
    pivot_longer(cols = starts_with("beta"), names_to = "series", values_to = "beta")

pl_4b <- ggplot(lp_results_long, aes(x = horizon, y = beta, color = series)) +
    geom_line(linewidth = 1.2) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(
        title = "Response of Inflation and GDP Differentials to Real Rate Differentials",
        y = "Local Projection Estimate",
        x = "Horizon (years)"
    )

ggsave("Assignment4/plot_4b_local_projections.pdf",
    plot = pl_4b,
    width = 8, height = 5
)
