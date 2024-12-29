library(readr)

data <- readr::read_csv("新北市義勇消防總隊接受捐贈_export.csv")

glimpse(`新北市義勇消防總隊接受捐贈`)

library(readr)
library(dplyr)
library(lubridate)

# Read the list of yyymmroc from a CSV file
#The file '新北市義勇消防總隊接受捐贈_export.csv' has a column named 'yyymmroc'
data <- read_csv("新北市義勇消防總隊接受捐贈_export.csv")

# Parse the yyymmroc values
parsed_data <- data %>%
  mutate(
    minguo_year = floor(yyymmroc / 100),      # Extract Minguo year
    western_year = minguo_year + 1911,       # Convert to Gregorian year
    month = yyymmroc %% 100,                 # Extract the month
    western_date = ymd(paste(western_year, month, "01", sep = "-"))  # Combine year-month-day
  )

# View the parsed data
parsed_data

# 'donation' is the column containing donation items
data$donation <- as.factor(data$donation)
data$donation <- factor(data$donation, 
                             levels = c("災情勘查車", "空氣呼吸器背架及氣瓶", "救護外套", "機能外套", "個人裝備器材", "災情勘查車2輛"))
str(data)
summary(data$donation)

data <- data |>
  dplyr::mutate(
    donor = factor(
      donor, 
      levels = unique(donor),
      labels = paste0("donor ", seq_along(unique(donor)))
    )
  )

data <- data |>
  dplyr::mutate(
    donation = dplyr::recode(
      donation,
      `災情勘查車` = "Disaster Survey Vehicle",
      `災情勘查車2輛` = "2 Disaster Investigation Vehicles",
      `個人裝備器材` = "Personalized Equipment",
      `機能外套` = "Functional Jacket",
      `空氣呼吸器背架及氣瓶` = "Air Respirator Backpack and Cylinder",
      `救護外套` = "Rescue Jacket"
    )
  )

# Parse the yyymmroc values
data <- data %>%
  mutate(
    minguo_year = floor(yyymmroc / 100),      # Extract Minguo year
    western_year = minguo_year + 1911,       # Convert to Gregorian year
    month = yyymmroc %% 100,                 # Extract the month
    western_date = ymd(paste(western_year, month, "01", sep = "-"))  # Combine year-month-day
  )

# View the parsed data
parsed_data

summary_classes <- data |>
  purrr::map_chr(class) |>
  tibble::enframe(name = "variable", value = "class") |>
  dplyr::count(class, name = "count") |>
  dplyr::arrange(desc(count))


data <- data |>
  dplyr::mutate(
    western_year = lubridate::year(western_date),
    western_month = lubridate::month(western_date, label = FALSE)
  ) |>
  dplyr::select(-western_date)




