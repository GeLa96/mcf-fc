# install.packages("readxl")
library(readxl)
library(ggplot2)
library(broom)
library(stargazer)

# Read the Excel file and specify the sheets to read
monthly <- read_excel("ecb.CES_data_2024_monthly.august.xlsx", sheet = "ecb.CES_data_2024_monthly.en")
background <- read_excel("ecb.CES_data_2024_monthly.august.xlsx", sheet = "ecb.CES_data_background.en")
# print(dim(monthly)); print(dim(background))
# print(head(names(monthly), 20))
# print(head(names(background), 20))

merged <- merge(monthly, background, by = c(
  "a0010", # Respondent ID
  "a0020"
)) # Country code

dim(merged) # the two key columns are 'a0010' and 'a0020' are not duplicated, therefore we get 162 columns.
head(names(merged), 20)

# Filter for Austria (country code "AT")
austria_data <- subset(merged, a0020 == "AT")
dim(austria_data)
head(names(austria_data), 20)

### a) One year ahead inflation expectations 'c1120'

## pattern 1)  Expectations on Education ISCED 'b210_prec'

# Check the distribution of education levels of respondents; 144 low, 474 medium, 325 high and no NAs.
table(austria_data$b2100_prec, useNA = "ifany")
# Calculate the average one year ahead inflation expectations by education level
aggregate(c1120 ~ b2100_prec, data = austria_data, FUN = mean, na.rm = TRUE)
# On Average for low the expected inflation is 6.18 %, for medium 4.17% and for high 4.49% one year ahead.
# This is in line with waht the paper argues, that lower educated people have higher inflation expectations.
p_edu <- ggplot(austria_data, aes(
  x = factor(b2100_prec,
    levels = 1:3,
    labels = c("Low", "Middle", "High")
  ),
  y = c1120
)) +
  geom_boxplot(fill = "red") +
  labs(
    x = "Education Level (ISCED)",
    y = "One Year Ahead Inflation Expectations (c1120)",
    title = "Inflation Expectations by Education Level in Austria (CES 2024)"
  ) +
  theme_minimal()
ggsave("education_plot_austria.png", plot = p_edu, width = 7, height = 5, dpi = 300)
# the plot also visuallieses that there are extreme outliers in all education groups, but the median is higher for low education.

# Regression where low education is the baseline category and we include medium and high as dummies.
# This tells the same story as the aggregate function above.
model_edu <- lm(c1120 ~ factor(b2100_prec), data = austria_data)
summary(model_edu)

## pattern 2) extrapolation of expectations based on past prices in general – open end  "c1020" from scale -100 to 100.
cor_extrap <- cor(austria_data$c1020, austria_data$c1120, use = "complete.obs")
cor_extrap
# gives a positive correlation of about 0.53, this shows that repondents who pelceive higher past inflation also expect higher future inflation.

# Visualization: scatterplot with regression line
p_extr <- ggplot(austria_data, aes(x = c1020, y = c1120)) +
  geom_point(alpha = 0.3, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", lwd = 0.8) +
  labs(
    x = "Perceived past 12-month inflation (%) (C1020)",
    y = "Expected 1-year-ahead inflation (%) (C1120)",
    title = "Austria: Extrapolation pattern (perceived vs. expected inflation)"
  ) +
  theme_minimal()
ggsave("extrapolation_plot_austria.png", plot = p_extr, width = 7, height = 5, dpi = 300)


### b) Calculate the diffrence between the one-year ahead inflation expectation.

aut_hicp <- 4.1 # Eurostat, Source:  https://ec.europa.eu/eurostat/databrowser/view/prc_hicp_manr/default/table?lang=en

aut_exp_hicp <- mean(austria_data$c1120, na.rm = TRUE)
print(aut_exp_hicp)

# Mean error for expected inflatio and actual inflation
mean_error <- aut_exp_hicp - aut_hicp
print(mean_error)

# expected error
expected_error <- austria_data$c1120 - aut_hicp

## Regression to explain the error.
# variables:
edu <- factor(austria_data$b2100_prec) # Education level # categorical variable
extrap <- austria_data$c1020 # Extrapolation of past inflation
gender <- factor(austria_data$a1020_prec) # Gender
age <- factor(austria_data$a1010_age_prec) # Age group # categorical variable
finknow <- factor(austria_data$b5040) # Financial knowledge risk diversification
trust <- factor(austria_data$c8011_1) # Trust in ECB
income <- factor(austria_data$b7040) # Income level
# creats three bins for trust

# check if all variables have the same length
lengths_list <- sapply(list(
  expected_error = expected_error,
  edu = edu,
  extrap = extrap,
  gender = gender,
  age = age,
  finknow = finknow,
  trust = trust,
  income = income
), length)
print(lengths_list)


# build a data frame for regression with same rows for all variables
df_error <- data.frame(expected_error, edu, extrap, gender, age, finknow, trust, income)
colSums(is.na(df_error)) # check for NAs in each column

df_error$trust_group <- cut(as.numeric(as.character(df_error$trust)),
  breaks = c(-1, 3, 7, 10),
  labels = c("Low", "Medium", "High")
)

# Regression model
model_error <- lm(expected_error ~ edu + extrap + gender + age + trust_group + income, data = df_error)
summary(model_error)

# Check overall model fit
summary(model_error)$r.squared
summary(model_error)$adj.r.squared

# Plot residuals
plot(model_error, which = 1) # Residuals vs Fitted
hist(resid(model_error), main = "Residual distribution", xlab = "Residuals")

# setting baselines
df_error$edu <- relevel(df_error$edu, ref = levels(df_error$edu)[1]) # "Low"
df_error$gender <- relevel(df_error$gender, ref = levels(df_error$gender)[1]) # e.g., "Male"
df_error$age <- relevel(df_error$age, ref = levels(df_error$age)[1]) # youngest bin
df_error$trust_group <- relevel(df_error$trust_group, ref = "Low")
df_error$income <- relevel(df_error$income, ref = levels(df_error$income)[1]) # lowest bin

model_error1 <- lm(expected_error ~ edu + extrap + gender + age + trust_group + income,
  data = df_error, na.action = na.omit
)

# 2) Nice labels in the order stargazer prints the coefficients
cov_labels <- c(
  "Education: Middle", # accoording to ISCED
  "Education: High",
  "Extrapolation (past inflation)",
  "Gender: Female", # base male
  "Age: 35--49",
  "Age: 50--70",
  "Age: 71+",
  "Trust (Medium)", # trust in ECB
  "Trust (High)",
  "Income: 2nd Quantile",
  "Income: 3rd Quantile",
  "Income: 4th Quantile",
  "Income: 5th Quantile",
  "Liquidity: Sufficient" # sufficient liquidity C7010 # base insufficient
)

# Creating a variation of the model with the variable sufficient liquidity C7010
# Variation of the model with sufficient liquidity C7010
liquidity <- factor(austria_data$c7010)
length(liquidity)

model_error2 <- lm(expected_error ~ edu + extrap + gender + age + trust_group + income + liquidity,
  data = df_error, na.action = na.omit
)

# 3d variation housing type B3300_prec
housing_type <- factor(austria_data$b3300_prec)
length(housing_type)

model_error3 <- lm(expected_error ~ edu + extrap + gender + age + trust_group + income + housing_type,
  data = df_error, na.action = na.omit
)

# 3) Export LaTeX with stargazer
stargazer(
  model_error1, model_error2, model_error3,
  type = "latex",
  title = "Determinants of Inflation Expectation Errors (Austria, Aug 2024 → Aug 2025)",
  label = "tab:exp_error",
  dep.var.labels = "Expectation error (pp)",
  covariate.labels = cov_labels,
  digits = 3,
  align = TRUE,
  star.cutoffs = c(0.1, 0.05, 0.01),
  keep.stat = c("n", "rsq", "adj.rsq"),
  omit.stat = c("f"),
  notes = "Baseline categories: Education = Low; Gender = Male; Age = 18-34; Trust = Low; Income = 1st Quantile.",
  notes.append = FALSE,
  out = "inflation_expectation_error_austria.tex"
)


## c) Difficulties assassing future inflation.


## Expectation of prices in general next 12 months - open - ended "c1120"
c1120_all <- merged$c1120
# count for specifice values
values_count <- c(-10, -5, 0, 5, 10, 15, 20)
counts <- sapply(values_count, function(x) sum(c1120_all == x, na.rm = TRUE))
c1120_all_counts <- data.frame(Value = values_count, Count = counts)
print(c1120_all_counts)

total_focal <- sum(c1120_all_counts$Count, na.rm = TRUE)
total_valid <- sum(!is.na(c1120_all))

# the ratio of these answers is 32 percent which is quite high.
ratio <- total_focal / total_valid
print(ratio)


ratio <- length(c1120_all_counts) / length(c1120_all)
print(ratio)


# histograms
p1 <- ggplot(merged, aes(x = c1120)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black", alpha = 0.8) +
  # geom_density(aes(y = after_stat(count) * 2), color = "red", linewidth = 0.6, adjust = 1) + # Overlay density scaled to histogram
  labs(
    x = "One Year ahead Inflation Expectations (in percent)",
    y = "Survey Responses",
    title = "Histogram of One Year ahead Inflation Expectations (CES 2024)"
  ) +
  theme_minimal()
ggsave("histogram_1year_inflation_expectations.png", plot = p1, width = 7, height = 5, dpi = 300)
print(p1)


## Expecations of prices in general 3 years ahead - open - ended "c1220"
c1220_all <- merged$c1220

values_count <- c(-10, -5, 0, 5, 10, 15, 20)
counts <- sapply(values_count, function(x) sum(c1220_all == x, na.rm = TRUE))
c1220_all_counts <- data.frame(Value = values_count, Count = counts)
print(c1120_all_counts)

total_focal <- sum(c1220_all_counts$Count, na.rm = TRUE)
total_valid <- sum(!is.na(c1220_all))

# the ratio of these answers is ca. 34 percent which is quite high.
ratio <- total_focal / total_valid
print(ratio)

# histograms
p2 <- ggplot(merged, aes(x = c1220)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black", alpha = 0.8) +
  # geom_density(aes(y = after_stat(count) * 2), color = "red", linewidth = 0.6, adjust = 1) + # Overlay density scaled to histogram
  labs(
    x = "Three Year ahead Inflation Expectations (in percent)",
    y = "Survey Responses",
    title = "Histogram of Three Year ahead Inflation Expectations (CES 2024)"
  ) +
  theme_minimal()
ggsave("histogram_3year_inflation_expectations.png", plot = p1, width = 7, height = 5, dpi = 300)
print(p2)


## density plot for both one and three year expectstions
ggplot() +
  geom_density(aes(x = c1120_all, color = "1 Year Ahead"), size = 0.4) +
  geom_density(aes(x = c1220_all, color = "3 Years Ahead"), size = 0.4) +
  geom_vline(xintercept = c(0, 5, 10, 15), linetype = "dashed", color = "grey40", linewidth = 0.2) +
  annotate("text",
    x = c(0, 5, 10, 15), y = -0.005, # adjust y for position
    label = c("0", "5", "10", "15"),
    angle = 90, vjust = -0.5, size = 2.5, color = "grey40"
  ) +
  labs(
    x = "Inflation Expectations (%)",
    y = "Density",
    title = "Density of Inflation Expectations: 1 Year vs. 3 Years Ahead (CES 2024)"
  ) +
  scale_color_manual(name = "Horizon", values = c("1 Year Ahead" = "blue", "3 Years Ahead" = "green")) +
  theme_minimal()
ggsave("density_inflation_expectations.png", width = 7, height = 5, dpi = 300)
