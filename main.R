library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)

# Helper function to print data status
peek <- function(df, step_name) {
    cat(sprintf("\n--- %s ---\n", step_name))
    cat(sprintf("Dimensions: %d rows x %d columns\n", nrow(df), ncol(df)))
    print(head(df, 3))
}

# --- 1. Load and Transform ---
raw_data <- fromJSON("hotel-hue-reviews.json")
peek(raw_data, "1a. Initial Data Load")

df <- raw_data |>
    mutate(
        hotel_name = str_extract(url, "(?<=Reviews-)[^/-]+") |> str_replace_all("_", " "),
        province = str_to_title(str_trim(province)),
        trip_type_clean = case_when(
            str_detect(trip_type, "(?i)couple") ~ "Couple",
            str_detect(trip_type, "(?i)family") ~ "Family",
            str_detect(trip_type, "(?i)solo|business") ~ "Solo/Business",
            str_detect(trip_type, "(?i)friend") ~ "Friends",
            TRUE ~ "Other"
        ),
        language_clean = case_when(
            str_detect(language, "(?i)en|english") ~ "English",
            str_detect(language, "(?i)vi|viet|vn") ~ "Vietnamese",
            str_detect(language, "(?i)fr|french") ~ "French",
            TRUE ~ "Other/International"
        ),
        comment_length = nchar(comment),
        reviewer_id = str_extract(reviewer_url, "[^/]+$"),
        visit_date = my(visit_date),
        rating = as.numeric(star)
    ) |>
    select(-reviewer_url) |>
    distinct(url, reviewer_id, .keep_all = TRUE)

peek(df, "1b. After Normalization and Deduplication")

# --- 2. Feature Engineering ---
df_clean <- df |>
    mutate(
        year = year(visit_date),
        rating_group = case_when(
            rating >= 4.5 ~ "Excellent",
            rating >= 4.0 ~ "Good",
            rating >= 3.0 ~ "Average",
            TRUE ~ "Low"
        )
    )

peek(df_clean, "2. After Feature Engineering")

# --- 3. Diagnostic Summary ---
cat("\n--- Data Quality Report ---\n")
cat(sprintf("Total Records: %d\n", nrow(df_clean)))
cat(sprintf("Languages identified: %s\n", paste(unique(df_clean$language_clean), collapse = ", ")))

# --- 4. Visualizations ---
ggplot(df_clean, aes(x = hotel_name, fill = language_clean)) +
    geom_bar(position = "dodge") +
    labs(title = "Review Volume by Language", x = "Hotel", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df_clean, aes(x = language_clean, y = rating, fill = language_clean)) +
    geom_violin(alpha = 0.6) +
    labs(title = "Rating Distribution by Language", x = "Language", y = "Star Rating") +
    theme_minimal()

# --- 5. Export ---
write.csv(df_clean, "cleaned_hotel_data.csv", row.names = FALSE)
cat("\nProcess complete. Data exported to 'cleaned_hotel_data.csv'.\n")
