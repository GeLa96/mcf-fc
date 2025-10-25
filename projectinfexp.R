# install.packages("readxl")
library(readxl)
library(ggplot2)

# Read the Excel file and specify the sheets to read
monthly    <- read_excel("ecb.CES_data_2024_monthly.august.xlsx", sheet = "ecb.CES_data_2024_monthly.en")
background <- read_excel("ecb.CES_data_2024_monthly.august.xlsx", sheet = "ecb.CES_data_background.en")
#print(dim(monthly)); print(dim(background))
#print(head(names(monthly), 20))
#print(head(names(background), 20))

merged <- merge(monthly, background, by = c('a0010',        # Respondent ID
                                            'a0020'))    # Country code

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
p_edu <-ggplot(austria_data, aes(x = factor(b2100_prec,
                                    levels = 1:3,
                                    labels = c('Low','Middle','High')), 
                         y = c1120)) +
  geom_boxplot(fill = 'red') +
  labs(x = "Education Level (ISCED)", 
       y = "One Year Ahead Inflation Expectations (c1120)",
       title = "Inflation Expectations by Education Level in Austria (CES 2024)") +
  theme_minimal()
ggsave("education_plot_austria.png",plot = p_edu, width = 7, height = 5, dpi = 300)
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
  labs(x = "Perceived past 12-month inflation (%) (C1020)",
       y = "Expected 1-year-ahead inflation (%) (C1120)",
       title = "Austria: Extrapolation pattern (perceived vs. expected inflation)") +
  theme_minimal()
ggsave("extrapolation_plot_austria.png",plot = p_extr, width = 7, height = 5, dpi = 300)
