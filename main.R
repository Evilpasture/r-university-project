library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)

# 1. Load and Transform Data
df_original <- fromJSON("hotel-hue-reviews.json")

# --- Diagnostic Summary ---
na_percentage <- sum(is.na(df_original)) / (nrow(df_original) * ncol(df_original)) * 100

cat(sprintf("Numbers of NA: %d\n", sum(is.na(df_original))))
cat(sprintf("Percentage of missing data: %.2f%%\n", na_percentage))
cat(sprintf("Total number of duplicate rows: %d\n", sum(duplicated(df_original))))
cat(sprintf("Number of errors or incorrect data format/values: %d\n", sum(!complete.cases(df_original))))
# --------------------------

# Process data
# Change name and process strings
df <- df_original |>
    mutate(
        hotel_name = str_extract(url, "(?<=Reviews-)[^/-]+"),
        reviewer_name = str_extract(reviewer_url, "[^/]+$")
    ) |>
    # ROBUST ID GENERATION: Using group_indices() for global unique IDs
    # mutate(reviewer_id = group_indices(pick(), reviewer_name)) |>
    select(-title, -reviewer_url)


# 2. Hard filter
df_deduplicated <- df |>
    distinct() # Remove duplicate rows
# Cast and split and create columns
df_clean <- df_deduplicated |>
    # filter(
    #     visit_date >= as.Date("2000-01-01") & visit_date <= as.Date("2026-12-31"),
    #     star >= 1 & star <= 5
    # ) |>
    mutate(visit_date = as.Date(visit_date)) |>
    mutate(
        year = year(visit_date),
        month = month(visit_date),
        rating = as.numeric(star),
        rating_group = case_when(
            rating >= 4.5 ~ "Excellent",
            rating >= 4.0 ~ "Good",
            rating >= 3.0 ~ "Average",
            TRUE ~ "Low"
        )
    )

# 3. Export
write.csv(df_clean, "cleaned_hotel_data.csv", row.names = FALSE)

# 4. Analysis
time_analysis <- df_clean |>
    summarise(
        volume = n(),
        average_rating = mean(rating),
        .by = c(hotel_name, year, month)
    ) |>
    arrange(hotel_name, year, month)

# 5. Visualization
ggplot(time_analysis, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = average_rating)) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    geom_smooth(method = "loess", color = "red", linetype = "dashed", se = FALSE) +
    facet_wrap(~hotel_name, scales = "free_y") +
    labs(
        title = "Average Rating Trend by Hotel",
        subtitle = "Faceted analysis of performance over time (2000-2026)",
        x = "Date", y = "Average Star Rating"
    ) +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold", size = 10))

# Calculate counts per hotel and rating group
rating_counts <- df_clean |>
    group_by(hotel_name, rating_group) |>
    summarise(count = n(), .groups = "drop")

# Set factor levels to ensure the order of groups (optional but recommended)
rating_counts$rating_group <- factor(rating_counts$rating_group,
    levels = c("Excellent", "Good", "Average", "Low")
)

# Generate the plot
ggplot(rating_counts, aes(x = rating_group, y = count, fill = rating_group)) +
    geom_col() +
    facet_wrap(~hotel_name, scales = "free_y") +
    labs(
        title = "Distribution of Review Ratings by Hotel",
        subtitle = "Count of reviews categorized by rating group",
        x = "Rating Group",
        y = "Number of Reviews"
    ) +
    theme_minimal() +
    theme(
        strip.text = element_text(face = "bold", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_fill_brewer(palette = "Set3")
