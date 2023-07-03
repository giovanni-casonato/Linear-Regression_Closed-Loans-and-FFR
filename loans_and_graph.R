library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
############################################################################
        #CREATE loans_month_data DATAFRAME WITH ALL CLOSED LOANS BY MONTH

# Read the data from the Excel file
loan_data <- read_excel("C:/Users/giovanni.casonato/Desktop/Main Files/Closed_Loans_2000-2023.xlsx")

# Convert the date column to a proper date format
loan_data$date <- as.Date(loan_data$date)

# Extract the month from the date column and keep it in date format
loan_data$month <- as.Date(cut(loan_data$date, "month"))
loan_data <- loan_data %>%
  arrange(date)

# Group the data by month
grouped_data <- loan_data %>%
  group_by(month) %>%
  summarize(num_loans = n())
  
  
# Create a data frame with all months
all_months <- data.frame(month = seq(as.Date("2000-01-01"),
                                     as.Date("2023-05-01"), by = "month"))


# Merge the loan_counts data frame with all_months and replace missing values with 0
loans_month_data <- all_months %>%
  left_join(grouped_data, by = "month") %>%
  replace_na(list(num_loans = 0))

################################################################################
                  # Linear Regression (FFR and closed loans)

ffr_data <- read_excel("C:/Users/giovanni.casonato/Desktop/Main Files/FEDFUNDS.xls")
ffr_data$month <- as.Date(ffr_data$month, format = "%Y-%m-%d")

# Merge ffr_data with loans_month_data
loans_ffr_data <- merge(loans_month_data, ffr_data, by = "month")

# Take the Log and sqrt of the Data to get a normal distribution
# Take the log transformation of num_loans
loans_ffr_data$log_num_loans <- log(loans_ffr_data$num_loans)
# Take the sqrt transformation of num_loans
loans_ffr_data$sqrt_num_loans <- sqrt(loans_ffr_data$num_loans)


# Subset rows for 2018-2023
loans_ffr_2018_to_2023 <- loans_ffr_data[217:281,]
# Subset rows for 2000-2010
loans_ffr_2000_to_2010 <- loans_ffr_data[1:132,]


# Perform linear regression
lm_2018_to_2023_results <- lm(num_loans ~ ffr, data = loans_ffr_2018_to_2023)

# Print the regression summary
summary(lm_2018_to_2023_results)

# Create a scatter plot of the data points
ggplot(loans_ffr_2018_to_2023, aes(x = ffr, y = num_loans)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "FFR", y = "# Loans") +
  theme_minimal()

################################################################################
                  # PLOT THE loans_month_data DATAFRAME

# Define the recession periods
recession_periods <- data.frame(
  start = as.Date(c("2001-03-01", "2007-12-01", "2020-02-01")),
  end = as.Date(c("2001-11-01", "2009-06-30", "2020-04-01"))
)

# Years displayed on the graph
selected_years <- c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2023)

# Plot the Graph
ggplot() +
  geom_rect(
    data = recession_periods,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill = "gray", alpha = 0.7
  ) +
  geom_line(data = loans_ffr_data, aes(x = month, y = num_loans), color = "steelblue", size = 1) +
  geom_line(data = loans_ffr_data, aes(x = month, y = ffr * 10), color = "red", size = 1) +
  labs(x = "Year", y = "Number of Loans", title = "Closed Loans and FFR - Monthly") +
  theme_minimal() +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / 10, name = "FFR %")
  ) +
  scale_x_date(
    date_labels = "%Y",
    breaks = as.Date(paste0(selected_years, "-01-01"))
  )







