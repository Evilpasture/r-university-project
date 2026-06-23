library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)

# 1. Load data and extract Hotel Names using Regex
# The regex (?<=Reviews-)[^/-]+ looks for text between "Reviews-" and the next hyphen/slash
df <- fromJSON("hotel-hue-reviews.json") %>%
    mutate(hotel_name = str_extract(url, "(?<=Reviews-)[^/-]+")) %>%
    select(-title, -comment, -reviewer_url) %>% # Drop noise
    mutate(
        visit_date = my(visit_date)
    )

# 2. Hard filter: remove weird dates and invalid star ratings
df <- df %>%
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

# 3. Sanity check
print(summary(df$star))
print(range(df$visit_date))

# 4. Perform analysis by Hotel and Date
time_analysis <- df %>%
    summarise(
        volume = n(),
        average_rating = mean(star),
        .by = c(hotel_name, year, month)
    ) %>%
    arrange(hotel_name, year, month)

# 5. Cleaned visualization
# We use facets to see if different hotels show different trends
ggplot(time_analysis, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = average_rating)) +
    geom_line(color = "steelblue") +
    geom_smooth(method = "loess", color = "red", linetype = "dashed", se = FALSE) +
    facet_wrap(~hotel_name) +
    labs(
        title = "Average Rating Trend by Hotel",
        subtitle = "Faceted view of performance over time",
        x = "Date", y = "Average Star Rating"
    ) +
    theme_minimal()
