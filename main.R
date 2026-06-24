library(dplyr)
library(lubridate)
library(countrycode)
library(jsonlite)
library(lubridate)
library(stringr)

peek <- function(df, step_name) {
    cat(sprintf("\n--- %s ---\n", step_name))
    cat(sprintf("Dimensions: %d rows x %d columns\n", nrow(df), ncol(df)))
    print(head(df, 3))
}

lang_map <- c(
    "English" = "English", "EN" = "English",
    "Vietnamese" = "Vietnamese", "VI" = "Vietnamese",
    "DA" = "Danish", "NO" = "Norwegian", "FI" = "Finnish",
    "CS" = "Czech", "RO" = "Romanian", "SL" = "Slovenian",
    "SK" = "Slovak", "TL" = "Tagalog", "HE" = "Hebrew",
    "EL" = "Greek", "SO" = "Somali", "AF" = "Afrikaans",
    "CY" = "Welsh",
    "French" = "French", "Japanese" = "Japanese", "Dutch" = "Dutch",
    "Russian" = "Russian", "German" = "German", "Italian" = "Italian",
    "Polish" = "Polish", "Chinese (Sim.)" = "Chinese",
    "Chinese (Trad.)" = "Chinese", "Korean" = "Korean",
    "Portuguese" = "Portuguese", "Spanish" = "Spanish",
    "Thai" = "Thai", "Hungarian" = "Hungarian", "Turkish" = "Turkish",
    "Swedish" = "Swedish", "Indonesian" = "Indonesian"
)

raw_data <- fromJSON("hotel-hue-reviews.json")

cleaned_language_data <- raw_data |>
    distinct(reviewer_url, visit_date, .keep_all = TRUE) |>
    mutate(
        # This keeps the original value if not found,
        # but you can change .default to "Other"
        language = recode(language, !!!lang_map, .default = "Other")
    )

# Check the results
peek(cleaned_language_data, "Transformation Complete")

cleaned_trip_data <- cleaned_language_data |>
    mutate(
        trip_type_normalized = case_when(
            str_detect(trip_type, "couple|Couple") ~ "Couple",
            str_detect(trip_type, "family|Family") ~ "Family",
            str_detect(trip_type, "solo|Solo") ~ "Solo",
            str_detect(trip_type, "business|Business") ~ "Business",
            str_detect(trip_type, "friend|Friend") ~ "Friends",
            TRUE ~ "Unknown" # This handles the empty strings ("")
        ),
        trip_type_normalized = as.factor(trip_type_normalized)
    )

peek(cleaned_trip_data, "Trip Type Normalization Complete")


cleaned_trip_data <- cleaned_trip_data |>
    mutate(
        # 1. Extract 3-letter month OR full month name, followed by 4 digits
        # 2. This regex captures: "June 2026", "Jun 2026", "Oct 2015" (ignoring trailing text)
        clean_date_str = str_extract(visit_date, "(?i)[A-Z][a-z]+\\s+\\d{4}|[A-Z][a-z]{2}\\s+\\d{4}"),

        # 3. Now parse the cleaned string
        visit_date = my(clean_date_str),

        # 4. Clean up the helper column and ensure stars are numeric
        star = as.numeric(star)
    ) |>
    select(-clean_date_str) # Remove helper column


# Check the results
peek(cleaned_trip_data, "Final Data Castings Complete")
