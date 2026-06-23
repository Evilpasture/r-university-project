library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)

# 1. Load and immediately drop unnecessary columns
df <- fromJSON("hotel-hue-reviews.json") %>%
    select(-title, -comment) %>%
    mutate(
        # Convert dates and handle potential parsing errors
        visit_date = my(visit_date),

        # 2. Hard filter: remove weird dates and invalid star ratings
        # We define 'valid' as dates between 2000 and 2026, and stars 1-5
    ) %>%
    filter(
        visit_date >= as.Date("2000-01-01") & visit_date <= as.Date("2026-12-31"),
        star >= 1 & star <= 5
    ) %>%
    mutate(
        year = year(visit_date),
        month = month(visit_date),
        event_date = as.Date("2026-01-01"),
        period = if_else(visit_date >= event_date, "After", "Before")
    )



# 3. Quick sanity check on the cleaned data
print(summary(df$star))
print(range(df$visit_date))

# 4. Now perform your clean analysis
time_analysis <- df %>%
    summarise(
        volume = n(),
        average_rating = mean(star),
        .by = c(year, month)
    ) %>%
    arrange(year, month)

# 5. Cleaned visualization
ggplot(time_analysis, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = average_rating)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_smooth(method = "loess", color = "red", linetype = "dashed", se = FALSE) +
    labs(
        title = "Cleaned Average Rating Trend",
        x = "Date", y = "Average Star Rating"
    ) +
    theme_minimal()
