#Loading Packages
library(tidyverse)
library(lubridate)

#Data Import
covid <- read_csv("covid_data.csv")

#1.DATA CLEANING
#Parsing date
covid$date <- mdy(covid$date)

#ordering data
covid <- covid |> arrange(region, date)

#Check missing data
colSums(is.na(covid))

#fill missing cumulative and recovered columns
covid <- covid |> 
  group_by(region) |> 
  fill(accum_cases, recovered, .direction = "downup") |> 
  ungroup()

#calculate missing active cases (confirmed minus recovered)
covid <- covid |> 
  mutate(active_cases = if_else(is.na(active_cases), accum_cases - recovered, active_cases))

#calculate missing new cases
covid <- covid |> 
  mutate(new_case = if_else(is.na(new_case), accum_cases - lag(accum_cases, default = first(accum_cases)), new_case)) |> 
  ungroup()

#save cleaned data
write_csv(covid, "covid.csv")

#2.Analysis and Visualization
#loading packages
library(ggplot2)

#clean <- covid |> 
# mutate(ymd(date))

# National totals (aggregate across regions)
national_summary <- covid |> 
  group_by(date) |> 
  summarise(
    total_confirmed = sum(accum_cases, na.rm = TRUE),
    total_recovered = sum(recovered, na.rm = TRUE),
    total_active_cases = sum(active_cases, na.rm = TRUE),
    total_new_cases = sum(new_case, na.rm = TRUE),
    .groups = "drop"
  )


# Plot 1: National cumulative cases over time
ggplot(national_summary, aes(date, total_confirmed)) +
  geom_line(color = "red", linewidth = 1) +
  labs(title = "National Cumulative COVID-19 Cases in Ghana (June–August 2020)",
       x = "Date", y = "Cumulative Confirmed Cases") +
  theme_bw()

# Plot 2: New cases (national)
ggplot(national_summary, aes(date, total_new_cases)) +
  geom_col(fill = "orange") +
  labs(title = "Daily New Cases (National)", x = "Date", y = "New Cases") +
  theme_bw()

# Plot 3: Latest update showing top 5 regions by cumulative cases 
latest_update <- covid |>
  filter(date == max(date))

top_5_regions <- latest_update |> 
  arrange(desc(accum_cases)) |> 
  slice_head(n=5) |> 
  pull(region)

print(top_5_regions)

covid |> 
  filter(region %in% top_5_regions) |> 
  ggplot(aes(date, accum_cases, color = region)) +
  geom_line(linewidth = 1) +
  labs(title = "Cumulative Cases: Top 5 Regions",
       x = "Date", y = "Cumulative Cases") +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  theme(plot.background = element_rect(fill = 'grey'))


# Recovery rate based on latest update
latest_update |> 
  mutate(recovery_rate = recovered / accum_cases * 100) |> 
  arrange(desc(recovery_rate)) |> 
  slice_head(n = 10) |> 
  ggplot(aes(reorder(region, recovery_rate), recovery_rate)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = "Recovery Rate by Region (Latest Date)", y = "% Recovered", x = NULL) +
  theme_minimal()
