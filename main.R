library(dplyr)
library(lubridate)
library(countrycode)
library(jsonlite)
library(lubridate)
library(stringr)

# Check current locale
Sys.getlocale("LC_TIME")

# Set temporarily to English
Sys.setlocale("LC_TIME", "en_US.UTF-8")
# Note: On Windows this might be "English_United States.1252"

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

# View the raw strings that are failing to parse
unique(raw_data$visit_date[is.na(cleaned_trip_data$visit_date)])

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
        clean_date_str = str_extract(
            visit_date,
            "(?i)(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)\\s+\\d{4}(?=\\D|$)"
        ),
        be_year = as.integer(str_extract(clean_date_str, "\\d{4}")),
        clean_date_str = case_when(
            !is.na(clean_date_str) & !is.na(be_year) & be_year > 2100 ~
                str_replace(clean_date_str, as.character(be_year), as.character(be_year - 543)),
            TRUE ~ clean_date_str
        ),
        visit_date = my(clean_date_str),
        star = as.numeric(star)
    ) |>
    select(-clean_date_str, -be_year)


# Check the results
peek(cleaned_trip_data, "Final Data Castings Complete")
