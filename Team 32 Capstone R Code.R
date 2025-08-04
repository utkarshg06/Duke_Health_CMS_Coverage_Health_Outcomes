### Team 32 ###
### Capstone Project R Code ###
### Non-Communicable Diseases Analysis ###



library(dplyr)      # Load the dplyr package for data manipulation
library(readr)      # Load the readr package to read CSV files

# Define the folder location on your computer where all 16 data files are stored
folder_path <- "/Users/RichardXie/Desktop/Capstone Database"

# Create a list of all file names — each file represents a piece of the full dataset
file_names <- paste0("IHME-GBD_2021_DATA-3afca3c4-", 1:16, ".csv")

# Combine the folder path and file names to get the full path for each file
file_paths <- file.path(folder_path, file_names)

# Read all 16 CSV files and combine them into one large dataset called 'gbd_full_data'
gbd_full_data <- file_paths %>%
  lapply(read_csv) %>%
  bind_rows()

# (Optional) Convert key columns into the appropriate format to make analysis easier:
# - Numbers as numeric
# - Categories like gender, age, disease type, and state as factors
gbd_full_data <- gbd_full_data %>%
  mutate(
    year = as.numeric(year),
    measure_name = as.factor(measure_name),
    age_name = as.factor(age_name),
    sex_name = as.factor(sex_name),
    cause_name = as.factor(cause_name),
    location_name = as.factor(location_name)
  )

# Quickly view the structure of the dataset (column types and a few example values)
glimpse(gbd_full_data)


##############################################################################################################


library(dplyr)      # Load dplyr for data manipulation
library(ggplot2)    # Load ggplot2 for creating visualizations

# Group the data by age group and calculate the total disease burden for each age
# This combines data across all diseases, years, and sexes
age_distribution <- gbd_full_data %>%
  group_by(age_name) %>%
  summarise(total_val = sum(val, na.rm = TRUE)) %>%
  arrange(desc(total_val))  # Sort from highest to lowest burden

# Print the summarized table to check the results
print(age_distribution)

# Create a horizontal bar chart showing which age groups carry the most total disease burden
ggplot(age_distribution, aes(x = reorder(age_name, total_val), y = total_val)) +
  geom_col(fill = "steelblue") +  # Draw bars
  coord_flip() +                  # Flip the axes so age groups are on the y-axis
  labs(
    title = "Total Disease Burden by Age Group (Summed Over Sex)",
    x = "Age Group",
    y = "Total Value"
  ) +
  theme_minimal()  # Use a clean, minimal chart style

# This code summarizes and visualizes which age groups have the highest overall burden of non-communicable diseases 
# across all years, sexes, and conditions.

##############################################################################################################

library(dplyr)  # Load the dplyr package for data manipulation

# Add a new column to indicate whether each year is before or after 2014
# This allows us to compare data from before vs. after Medicaid expansion
gbd_full_data <- gbd_full_data %>%
  mutate(Period = ifelse(year <= 2014, "Pre", "Post"))

# Define a function to run a t-test for each disease under a given health measure
run_ttest_by_measure <- function(df, measure_label) {
  df_filtered <- df %>% filter(measure_name == measure_label)  # Filter by one measure (e.g., DALYs)
  
  results <- df_filtered %>%
    group_by(Disease = cause_name) %>%  # Group by disease
    summarise(
      t_result = list(t.test(val ~ Period, data = cur_data(), var.equal = FALSE)),  # Run t-test comparing Pre vs Post
      .groups = "drop"
    ) %>%
    mutate(
      Measure = measure_label,
      Mean_Pre = sapply(t_result, function(x) x$estimate[1]),     # Extract mean before 2014
      Mean_Post = sapply(t_result, function(x) x$estimate[2]),    # Extract mean after 2014
      t_statistic = sapply(t_result, function(x) x$statistic),    # Get t-test statistic
      p_value = sapply(t_result, function(x) x$p.value)           # Get p-value
    ) %>%
    select(Disease, Measure, Mean_Pre, Mean_Post, t_statistic, p_value) %>%  # Select useful output columns
    filter(!is.na(p_value) & p_value < 0.05) %>%  # Keep only statistically significant results
    arrange(p_value)  # Sort by p-value (smallest = most significant)
  
  return(results)
}

# Run the t-test function for each health measure
ttest_deaths      <- run_ttest_by_measure(gbd_full_data, "Deaths")
ttest_dalys       <- run_ttest_by_measure(gbd_full_data, "DALYs (Disability-Adjusted Life Years)")
ttest_incidence   <- run_ttest_by_measure(gbd_full_data, "Incidence")
ttest_prevalence  <- run_ttest_by_measure(gbd_full_data, "Prevalence")

# Combine the significant results from all four measures into one table
ttest_all_measures <- bind_rows(ttest_deaths, ttest_dalys, ttest_incidence, ttest_prevalence)

# Print all the results to the console
print(ttest_all_measures, n = Inf)

# Save the results to a CSV file for documentation or future use
write.csv(ttest_all_measures, "significant_ttest_results.csv", row.names = FALSE)

# This code identifies which diseases show statistically significant changes before vs. after 2014, helping us select key conditions for further analysis.

# Filter the dataset where Mean_Pre is greater than or equal to Mean_Post
ttest_filtered <- ttest_all_measures %>%
  filter(Mean_Pre >= Mean_Post)

print(ttest_filtered, n = Inf)

# Save the filtered result to a CSV
write.csv(ttest_filtered, "significant_ttest_results_filtered.csv", row.names = FALSE)

# Filter and rank 'Deaths' measure with thresholds
ttest_deaths_filtered <- ttest_deaths %>%
  filter(Mean_Pre > 10, Mean_Post > 10, Mean_Post <= Mean_Pre) %>%
  mutate(Diff = Mean_Pre - Mean_Post) %>%
  arrange(desc(Diff))


# View the result
print(ttest_deaths_filtered)

# Optionally save to CSV
write.csv(ttest_deaths_filtered, "filtered_deaths_ttest_by_diff.csv", row.names = FALSE)

# Filter and rank 'DALYs' measure with Mean_Pre > Mean_Post
ttest_dalys_filtered <- ttest_dalys %>%
  filter(Mean_Pre > Mean_Post,
         !(Mean_Pre > 0 & Mean_Pre < 1),
         !(Mean_Post > 0 & Mean_Post < 1)) %>%
  mutate(Diff = Mean_Pre - Mean_Post) %>%
  arrange(desc(Diff))

# View the result
print(ttest_dalys_filtered, n = Inf)

# Optionally save to CSV
write.csv(ttest_dalys_filtered, "filtered_dalys_ttest_by_diff.csv", row.names = FALSE)

# Filter incidence results for meaningful increases and thresholds
ttest_incidence_filtered <- ttest_incidence %>%
  filter(Mean_Post > Mean_Pre) %>%                      # Keep only increasing incidence
  filter(!(Mean_Pre > 0 & Mean_Pre < 1)) %>%            # Remove Mean_Pre between 0 and 1
  filter(!(Mean_Post > 0 & Mean_Post < 1)) %>%          # Remove Mean_Post between 0 and 1
  filter(!(Mean_Pre < 9 & Mean_Post < 9)) %>%           # Remove rows where both are < 9
  mutate(Diff = Mean_Post - Mean_Pre) %>%
  arrange(desc(Diff))  # Rank by greatest increase

# View result
print(ttest_incidence_filtered, n = Inf)

# Optionally save to CSV
write.csv(ttest_incidence_filtered, "filtered_incidence_ttest_by_diff.csv", row.names = FALSE)

# Filter for decreased Prevalence and remove very low values
ttest_prevalence_filtered <- ttest_prevalence %>%
  filter(Mean_Pre > 1, Mean_Post > 1) %>%                     # remove low-prevalence diseases
  filter(Mean_Post < Mean_Pre) %>%                           # keep only decreasing prevalence
  mutate(Diff = Mean_Pre - Mean_Post) %>%                    # compute difference
  arrange(desc(Diff))                                        # sort by largest decrease

# View the filtered results
print(ttest_prevalence_filtered, n = Inf)

# Optionally save to CSV
write.csv(ttest_prevalence_filtered, "filtered_prevalence_ttest_by_diff.csv", row.names = FALSE)

# Ensure filtered datasets exist (this assumes you’ve already created these earlier)
# Find diseases common to both filtered Deaths and DALYs results
common_diseases <- intersect(
  intersect(unique(ttest_deaths_filtered$Disease),
            unique(ttest_dalys_filtered$Disease)),
  unique(ttest_prevalence_filtered$Disease)
)
print(common_diseases)

# EDA #
##############################################################################################################

# Trend of Average Deaths #

# Load required libraries
library(ggplot2)
library(dplyr)

# Vector of 10 selected diseases
selected_diseases <- c(
  "Substance use disorders",
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter dataset
trend_data <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases, measure_name == "Deaths") %>%
  group_by(year, cause_name) %>%
  summarise(Average_Death = mean(val, na.rm = TRUE), .groups = "drop")

# Plot the line graph
ggplot(trend_data, aes(x = year, y = Average_Death, color = cause_name)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Trend of Average Deaths (2010–2019)",
    x = "Year",
    y = "Average Deaths",
    color = "Disease"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2010, 2019, by = 1))


# EDA #
##############################################################################################################

# Comparison of Pre vs Post Medicaid Expansion - Death #

library(ggplot2)
library(dplyr)
library(tidyr)

# Your 10 selected diseases
selected_deaths <- c(
  "Substance use disorders",
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter and sort by Diff = Post - Pre
plot_data <- ttest_deaths_filtered %>%
  filter(Disease %in% selected_deaths) %>%
  mutate(Diff = Mean_Post - Mean_Pre) %>%
  arrange(desc(Diff)) %>%
  mutate(Disease = factor(Disease, levels = unique(Disease)))  # lock in sorted order

# Convert to long format for plotting
deaths_long <- plot_data %>%
  select(Disease, Mean_Pre, Mean_Post) %>%
  pivot_longer(cols = starts_with("Mean"), names_to = "Period", values_to = "Mean") %>%
  mutate(Period = ifelse(Period == "Mean_Pre", "Pre-Expansion", "Post-Expansion"))

# Plot
ggplot(deaths_long, aes(x = Mean, y = Disease, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Deaths Comparison: Pre vs Post Medicaid Expansion\n(Sorted by Diff: Post - Pre)",
    x = "Average Number of Deaths",
    y = "Disease"
  ) +
  scale_fill_manual(values = c("Pre-Expansion" = "tomato", "Post-Expansion" = "steelblue")) +
  theme_minimal()


##############################################################################################################

# Load necessary libraries
library(dplyr)
library(usmap)
library(ggplot2)

# Filter for the 10 selected diseases
selected_diseases <- c(
  "Substance use disorders",
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter dataset for "Deaths" measure and selected diseases
state_death_summary <- gbd_full_data %>%
  filter(measure_name == "Deaths", cause_name %in% selected_diseases) %>%
  group_by(state = location_name) %>%
  summarise(Total_Deaths = sum(val, na.rm = TRUE)) %>%
  ungroup()

# Create state-level map
plot_usmap(data = state_death_summary, values = "Total_Deaths", color = "white") +
  scale_fill_continuous(
    low = "lightyellow", high = "darkred", name = "Total Deaths", label = scales::comma
  ) +
  labs(title = "Total Deaths for Selected Diseases by State") +
  theme(legend.position = "right")

##############################################################################################################

# Top 15 States by Average DALYs, Deaths, Prevalence, Incidence

library(dplyr)
library(ggplot2)

# Define the 10 selected diseases
selected_diseases <- c(
  "Substance use disorders", "Drug use disorders", "Opioid use disorders",
  "Diabetes and kidney diseases", "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias", "Alcohol use disorders",
  "Chronic kidney disease due to hypertension", "Uterine cancer"
)

# Filter to 10 diseases and DALYs
filtered_dalys <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases,
         measure_name == "DALYs (Disability-Adjusted Life Years)")

# Aggregate and plot
filtered_dalys %>%
  group_by(location_name) %>%
  summarise(Average_DALYs = mean(val, na.rm = TRUE)) %>%
  arrange(desc(Average_DALYs)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(location_name, Average_DALYs), y = Average_DALYs)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 15 States by Average DALYs",
    x = "State",
    y = "Average DALYs"
  ) +
  theme_minimal()

##############################################################################################################

# Top 15 State by Avg Incidence

# Filter to 10 diseases and Incidence
filtered_incidence <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases,
         measure_name == "Incidence")

# Aggregate and plot
filtered_incidence %>%
  group_by(location_name) %>%
  summarise(Average_Incidence = mean(val, na.rm = TRUE)) %>%
  arrange(desc(Average_Incidence)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(location_name, Average_Incidence), y = Average_Incidence)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 15 States by Average Incidence",
    x = "State",
    y = "Average Incidence"
  ) +
  theme_minimal(base_size = 14)

##############################################################################################################

# Filter to 10 diseases and Prevalence
filtered_prevalence <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases,
         measure_name == "Prevalence")

# Aggregate and plot
filtered_prevalence %>%
  group_by(location_name) %>%
  summarise(Average_Prevalence = mean(val, na.rm = TRUE)) %>%
  arrange(desc(Average_Prevalence)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(location_name, Average_Prevalence), y = Average_Prevalence)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 15 States by Average Prevalence",
    x = "State",
    y = "Average Prevalence"
  ) +
  theme_minimal(base_size = 14)


##############################################################################################################

# Yearly Trend of DALYs

# Use only diseases that showed statistically significant decreases
filtered_data <- gbd_full_data %>%
  filter(
    cause_name %in% c("Substance use disorders", "Drug use disorders", "Opioid use disorders", 
                      "Diabetes and kidney diseases", "Chronic kidney disease", 
                      "Chronic kidney disease due to diabetes mellitus type 2", 
                      "Alzheimer's disease and other dementias", "Alcohol use disorders", 
                      "Chronic kidney disease due to hypertension", "Uterine cancer"),
    measure_name %in% c("Deaths", "DALYs (Disability-Adjusted Life Years)")
  )

# Then plot by year and measure
yearly_trend <- filtered_data %>%
  group_by(measure_name, year) %>%
  summarise(avg_val = mean(val, na.rm = TRUE), .groups = "drop")

# Same plotting loop as your friend’s
for (m in unique(yearly_trend$measure_name)) {
  subset <- yearly_trend %>% filter(measure_name == m)
  
  p <- ggplot(subset, aes(x = year, y = avg_val)) +
    geom_line(color = "#1f77b4", linewidth = 1) +
    geom_point(color = "#1f77b4", size = 2) +
    labs(
      title = paste("Yearly Trend of", m),
      x = "Year", y = "Average Value"
    ) +
    scale_x_continuous(breaks = seq(min(subset$year), max(subset$year), by = 1)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11)
    )
  
  print(p)
}

##############################################################################################################

library(dplyr)      # Load dplyr for data manipulation
library(ggplot2)    # Load ggplot2 for creating charts

# Define the list of 10 non-communicable diseases selected for analysis
selected_diseases <- c(
  "Substance use disorders", 
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter the dataset to include only DALYs and the selected diseases
# Then calculate the average DALYs for each year
dalys_trend <- gbd_full_data %>%
  filter(measure_name == "DALYs (Disability-Adjusted Life Years)",
         cause_name %in% selected_diseases) %>%
  group_by(year) %>%
  summarise(avg_dalys = mean(val, na.rm = TRUE), .groups = "drop")

# Create a line chart showing how average DALYs changed from 2010 to 2019
ggplot(dalys_trend, aes(x = year, y = avg_dalys)) +
  geom_line(color = "#1f77b4", linewidth = 1.2) +  # Draw the trend line
  geom_point(color = "#1f77b4", size = 2) +        # Add points for each year
  labs(
    title = "Yearly Trend of DALYs",
    x = "Year",
    y = "Average DALYs"
  ) +
  scale_x_continuous(breaks = seq(min(dalys_trend$year), max(dalys_trend$year), by = 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )

# This code visualizes how the average disease burden (measured in DALYs) changed over time for the 10 selected non-communicable diseases.

##############################################################################################################

library(dplyr)      # Load dplyr for data manipulation
library(ggplot2)    # Load ggplot2 for creating charts

# Define the list of 10 non-communicable diseases selected for this analysis
selected_diseases <- c(
  "Substance use disorders", 
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter the dataset to include only Prevalence values for the selected diseases
# Then calculate the average prevalence for each year
prevalence_trend <- gbd_full_data %>%
  filter(measure_name == "Prevalence",
         cause_name %in% selected_diseases) %>%
  group_by(year) %>%
  summarise(avg_prevalence = mean(val, na.rm = TRUE), .groups = "drop")

# Create a line chart to visualize how average prevalence has changed over time
ggplot(prevalence_trend, aes(x = year, y = avg_prevalence)) +
  geom_line(color = "steelblue", linewidth = 1.2) +  # Draw the trend line
  geom_point(color = "steelblue", size = 2) +        # Add data points for each year
  labs(
    title = "Yearly Trend of Prevalence",
    x = "Year",
    y = "Average Prevalence"
  ) +
  scale_x_continuous(breaks = seq(min(prevalence_trend$year), max(prevalence_trend$year), by = 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )

# This code shows how the average number of people living with selected chronic diseases has changed from 2010 to 2019.

##############################################################################################################

library(dplyr)
library(ggplot2)

# Define the 10 selected diseases
selected_diseases <- c(
  "Substance use disorders",
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Calculate average DALYs per year across the 10 diseases
dalys_trend <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases,
         measure_name == "DALYs (Disability-Adjusted Life Years)") %>%
  group_by(year, cause_name) %>%
  summarise(avg_dalys_per_disease = mean(val, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  summarise(average_dalys = mean(avg_dalys_per_disease, na.rm = TRUE))

# Plotting
ggplot(dalys_trend, aes(x = year, y = average_dalys)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Yearly Trend of DALYs",
    x = "Year",
    y = "Average DALYs"
  ) +
  scale_x_continuous(breaks = seq(min(dalys_trend$year), max(dalys_trend$year), by = 1)) +
  theme_minimal()

##############################################################################################################

library(dplyr)
library(ggplot2)

# Define the 10 selected diseases
selected_diseases <- c(
  "Substance use disorders",
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter and summarize for average deaths per year
deaths_trend <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases, measure_name == "Deaths") %>%
  group_by(year) %>%
  summarise(Average_Deaths = mean(val, na.rm = TRUE), .groups = "drop")

# Plot the trend
ggplot(deaths_trend, aes(x = year, y = Average_Deaths)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkblue") +
  labs(
    title = "Yearly Trend of Deaths",
    x = "Year",
    y = "Average Deaths"
  ) +
  scale_x_continuous(breaks = seq(min(deaths_trend$year), max(deaths_trend$year), by = 1)) +
  theme_minimal()

##############################################################################################################

library(dplyr)
library(ggplot2)

# Define the 10 selected diseases
selected_diseases <- c(
  "Substance use disorders",
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter and summarize for average prevalence per year
prevalence_trend <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases, measure_name == "Prevalence") %>%
  group_by(year) %>%
  summarise(Average_Prevalence = mean(val, na.rm = TRUE), .groups = "drop")

# Plot the trend
ggplot(prevalence_trend, aes(x = year, y = Average_Prevalence)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkblue") +
  labs(
    title = "Yearly Trend of Prevalence",
    x = "Year",
    y = "Average Prevalence"
  ) +
  scale_x_continuous(breaks = seq(min(prevalence_trend$year), max(prevalence_trend$year), by = 1)) +
  theme_minimal()

##############################################################################################################

library(dplyr)      # Load dplyr for data processing
library(ggplot2)    # Load ggplot2 for creating the visualization

# Define the list of 10 selected non-communicable diseases
selected_diseases <- c(
  "Substance use disorders",
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter the dataset to include only DALYs for the selected diseases
# Then calculate the average DALYs separately for males and females
dalys_by_sex <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases, measure_name == "DALYs (Disability-Adjusted Life Years)") %>%
  group_by(sex_name) %>%
  summarise(Average_DALYs = mean(val, na.rm = TRUE), .groups = "drop")

# Create a bar chart comparing average DALYs by sex
ggplot(dalys_by_sex, aes(x = sex_name, y = Average_DALYs, fill = sex_name)) +
  geom_bar(stat = "identity") +  # Draw bars based on average DALYs
  labs(
    title = "DALYs by Sex",
    x = "Sex",
    y = "Average DALYs"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")  # Use a soft color palette for readability

# This code compares the average disease burden (in DALYs) between males and females across the 10 selected chronic conditions.

##############################################################################################################
library(dplyr)      # Load dplyr for filtering and summarizing data
library(ggplot2)    # Load ggplot2 for creating the bar chart

# Define the list of 10 selected non-communicable diseases
selected_diseases <- c(
  "Substance use disorders",
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter the dataset to include only Deaths for the selected diseases
# Then calculate the average number of deaths separately for males and females
deaths_by_sex <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases, measure_name == "Deaths") %>%
  group_by(sex_name) %>%
  summarise(Average_Deaths = mean(val, na.rm = TRUE), .groups = "drop")

# Create a bar chart to compare average deaths between males and females
ggplot(deaths_by_sex, aes(x = sex_name, y = Average_Deaths, fill = sex_name)) +
  geom_bar(stat = "identity") +  # Draw bars based on average death values
  labs(
    title = "Deaths by Sex",
    x = "Sex",
    y = "Average Deaths"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")  # Use a gentle color palette for clarity

# This code shows whether males or females experienced more average deaths across the 10 selected chronic diseases.


##############################################################################################################

library(dplyr)      # Load dplyr for data filtering and summarization
library(ggplot2)    # Load ggplot2 for creating the visualization

# Define the list of 10 non-communicable diseases we’re focusing on
selected_diseases <- c(
  "Substance use disorders",
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter the data to include only Prevalence values for these diseases
# Then calculate the average prevalence for each sex (male/female)
prevalence_by_sex <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases, measure_name == "Prevalence") %>%
  group_by(sex_name) %>%
  summarise(Average_Prevalence = mean(val, na.rm = TRUE), .groups = "drop")

# Create a bar chart to compare the average prevalence between males and females
ggplot(prevalence_by_sex, aes(x = sex_name, y = Average_Prevalence, fill = sex_name)) +
  geom_bar(stat = "identity") +  # Draw bars for each sex
  labs(
    title = "Prevalence by Sex",
    x = "Sex",
    y = "Average Prevalence"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")  # Use a color palette for visual clarity

# This code compares how common the selected diseases are between males and females based on average prevalence.

##############################################################################################################

library(dplyr)      # Load dplyr to filter and summarize data
library(ggplot2)    # Load ggplot2 to create the bar chart

# Define the list of 10 key non-communicable diseases we are analyzing
selected_diseases <- c(
  "Substance use disorders",
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter the data to include only incidence values for these diseases
# Then calculate the average incidence for each sex group (male/female)
incidence_by_sex <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases, measure_name == "Incidence") %>%
  group_by(sex_name) %>%
  summarise(Average_Incidence = mean(val, na.rm = TRUE), .groups = "drop")

# Create a bar chart to compare how many new cases occur on average for each sex
ggplot(incidence_by_sex, aes(x = sex_name, y = Average_Incidence, fill = sex_name)) +
  geom_bar(stat = "identity") +  # Draw bars to represent average incidence
  labs(
    title = "Incidence by Sex",
    x = "Sex",
    y = "Average Incidence"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")  # Apply a soft color scheme for clarity

# This code compares the average number of new disease cases per year between males and females across the 10 selected chronic conditions.

##############################################################################################################

library(dplyr)      # Load dplyr for data filtering and summarization
library(ggplot2)    # Load ggplot2 for creating the bar chart

# Define the list of 10 selected non-communicable diseases for analysis
selected_diseases <- c(
  "Substance use disorders", 
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter the dataset to include only DALYs for the selected diseases
# Then calculate the average DALYs for each age group
dalys_by_age <- gbd_full_data %>%
  filter(measure_name == "DALYs (Disability-Adjusted Life Years)",
         cause_name %in% selected_diseases) %>%
  group_by(age_name) %>%
  summarise(avg_dalys = mean(val, na.rm = TRUE), .groups = "drop")

# Create a horizontal bar chart showing average DALYs by age group
ggplot(dalys_by_age, aes(x = avg_dalys, y = reorder(age_name, avg_dalys), fill = age_name)) +
  geom_bar(stat = "identity") +  # Draw bars based on DALY values
  labs(
    title = "DALYs by Age Group",
    x = "Average DALYs",
    y = "Age Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.position = "none"  # Legend not needed since age groups are already labeled
  )

# This code shows which age groups experience the greatest burden of disease (DALYs) for the selected chronic conditions.

##############################################################################################################

library(dplyr)
library(ggplot2)

# Selected diseases
selected_diseases <- c(
  "Substance use disorders", 
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter and compute average Deaths by age group
deaths_by_age <- gbd_full_data %>%
  filter(measure_name == "Deaths",
         cause_name %in% selected_diseases) %>%
  group_by(age_name) %>%
  summarise(avg_deaths = mean(val, na.rm = TRUE), .groups = "drop")

# Plot
ggplot(deaths_by_age, aes(x = avg_deaths, y = reorder(age_name, avg_deaths), fill = age_name)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Deaths by Age Group",
    x = "Average Deaths",
    y = "Age Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.position = "none"
  )


##############################################################################################################


library(dplyr)
library(ggplot2)

# Selected diseases
selected_diseases <- c(
  "Substance use disorders", 
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter and compute average Prevalence by age group
prevalence_by_age <- gbd_full_data %>%
  filter(measure_name == "Prevalence",
         cause_name %in% selected_diseases) %>%
  group_by(age_name) %>%
  summarise(avg_prevalence = mean(val, na.rm = TRUE), .groups = "drop")

# Plot
ggplot(prevalence_by_age, aes(x = avg_prevalence, y = reorder(age_name, avg_prevalence), fill = age_name)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Prevalence by Age Group",
    x = "Average Prevalence",
    y = "Age Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.position = "none"
  )

##############################################################################################################

library(dplyr)
library(ggplot2)

# Selected diseases
selected_diseases <- c(
  "Substance use disorders", 
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter and compute average Incidence by age group
incidence_by_age <- gbd_full_data %>%
  filter(measure_name == "Incidence",
         cause_name %in% selected_diseases) %>%
  group_by(age_name) %>%
  summarise(avg_incidence = mean(val, na.rm = TRUE), .groups = "drop")

# Plot
ggplot(incidence_by_age, aes(x = avg_incidence, y = reorder(age_name, avg_incidence), fill = age_name)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Incidence by Age Group",
    x = "Average Incidence",
    y = "Age Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.position = "none"
  )

##############################################################################################################


library(dplyr)
library(ggplot2)

# Selected diseases
selected_diseases <- c(
  "Substance use disorders", 
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter the dataset
dalys_top10 <- gbd_full_data %>%
  filter(measure_name == "DALYs (Disability-Adjusted Life Years)",
         cause_name %in% selected_diseases) %>%
  group_by(cause_name) %>%
  summarise(avg_val = mean(val, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_val)) %>%
  slice_head(n = 10)

# Plot
ggplot(dalys_top10, aes(x = avg_val, y = reorder(cause_name, avg_val))) +
  geom_col(fill = "skyblue") +
  labs(
    title = "Top 10 Causes for DALYs",
    x = "Average DALYs",
    y = "Cause"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )


##############################################################################################################



library(dplyr)
library(ggplot2)

# Selected diseases
selected_diseases <- c(
  "Substance use disorders", 
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter the dataset
deaths_top10 <- gbd_full_data %>%
  filter(measure_name == "Deaths",
         cause_name %in% selected_diseases) %>%
  group_by(cause_name) %>%
  summarise(avg_val = mean(val, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_val)) %>%
  slice_head(n = 10)

# Plot
ggplot(deaths_top10, aes(x = avg_val, y = reorder(cause_name, avg_val))) +
  geom_col(fill = "skyblue") +
  labs(
    title = "Top 10 Causes for Deaths",
    x = "Average Deaths",
    y = "Cause"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )



##############################################################################################################


library(dplyr)
library(ggplot2)

# Filter for Prevalence and the 10 selected diseases
prevalence_top10 <- gbd_full_data %>%
  filter(measure_name == "Prevalence",
         cause_name %in% selected_diseases) %>%
  group_by(cause_name) %>%
  summarise(avg_val = mean(val, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_val)) %>%
  slice_head(n = 10)

# Plot
ggplot(prevalence_top10, aes(x = avg_val, y = reorder(cause_name, avg_val))) +
  geom_col(fill = "skyblue") +
  labs(
    title = "Top 10 Causes for Prevalence",
    x = "Average Prevalence",
    y = "Cause"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )

##############################################################################################################

library(dplyr)
library(ggplot2)

# Filter for Incidence and the 10 selected diseases
incidence_top10 <- gbd_full_data %>%
  filter(measure_name == "Incidence",
         cause_name %in% selected_diseases) %>%
  group_by(cause_name) %>%
  summarise(avg_val = mean(val, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_val)) %>%
  slice_head(n = 10)

# Plot
ggplot(incidence_top10, aes(x = avg_val, y = reorder(cause_name, avg_val))) +
  geom_col(fill = "skyblue") +
  labs(
    title = "Top 10 Causes for Incidence",
    x = "Average Incidence",
    y = "Cause"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )

##############################################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

# Define the measures to plot
selected_measures <- c("Deaths", "DALYs (Disability-Adjusted Life Years)", "Prevalence", "Incidence")

# Loop for each selected measure
for (m in selected_measures) {
  heatmap_data <- gbd_full_data %>%
    filter(measure_name == m, cause_name %in% selected_diseases) %>%
    group_by(age_name, cause_name) %>%
    summarise(mean_val = mean(val, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = cause_name, values_from = mean_val)
  
  # Convert to matrix
  rownames_mat <- heatmap_data$age_name
  mat <- as.matrix(heatmap_data[, -1])
  rownames(mat) <- rownames_mat
  
  # Reshape for ggplot
  heatmap_df <- reshape2::melt(mat)
  names(heatmap_df) <- c("Age", "Cause", "Mean_Value")
  
  # Plot
  p <- ggplot(heatmap_df, aes(x = Cause, y = Age, fill = Mean_Value)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
    labs(
      title = paste("Heatmap of", m, "by Age and Disease"),
      x = "Disease", y = "Age Group"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(size = 14, face = "bold")
    )
  
  print(p)
}

##############################################################################################################


library(dplyr)
library(ggplot2)
library(tidyr)

# Replace this with your actual list of 10 selected diseases
selected_diseases <- c(
  "Substance use disorders",
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter original data
df_filtered <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases)

# Function to plot Pre vs Post Medicaid Expansion comparison for a measure
plot_pre_post_comparison <- function(df, measure) {
  df_summary <- df %>%
    filter(measure_name == measure) %>%
    mutate(Period = ifelse(year <= 2014, "Pre-Expansion", "Post-Expansion")) %>%
    group_by(cause_name, Period) %>%
    summarise(avg_val = mean(val, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Period, values_from = avg_val) %>%
    mutate(Diff = `Post-Expansion` - `Pre-Expansion`) %>%
    arrange(desc(Diff))
  
  # Long format for plotting
  df_long <- df_summary %>%
    pivot_longer(cols = c("Pre-Expansion", "Post-Expansion"), names_to = "Period", values_to = "Average") %>%
    mutate(Period = factor(Period, levels = c("Post-Expansion", "Pre-Expansion")))
  
  # Set factor levels to show largest difference at top
  df_long$cause_name <- factor(df_long$cause_name, levels = rev(df_summary$cause_name))
  
  # Plot
  ggplot(df_long, aes(x = Average, y = cause_name, fill = Period)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("Post-Expansion" = "#1f77b4", "Pre-Expansion" = "tomato")) +
    labs(
      title = paste0(measure, " Comparison: Pre vs Post Medicaid Expansion"),
      x = paste("Average Number of", measure),
      y = "Disease"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "right"
    )
}

# Plot for DALYs
plot_pre_post_comparison(df_filtered, "DALYs (Disability-Adjusted Life Years)")

# Plot for Prevalence
plot_pre_post_comparison(df_filtered, "Prevalence")

plot_pre_post_comparison(df_filtered, "Incidence")


##############################################################################################################


library(dplyr)
library(ggplot2)
library(tidyr)

# 10 selected diseases
selected_diseases <- c(
  "Substance use disorders",
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter full dataset
df_filtered <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases)

# Plot comparison function for Deaths
plot_pre_post_deaths <- function(df) {
  df_summary <- df %>%
    filter(measure_name == "Deaths") %>%
    mutate(Period = ifelse(year <= 2014, "Pre-Expansion", "Post-Expansion")) %>%
    group_by(cause_name, Period) %>%
    summarise(avg_val = mean(val, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Period, values_from = avg_val) %>%
    mutate(Diff = `Post-Expansion` - `Pre-Expansion`) %>%
    arrange(desc(Diff))
  
  df_long <- df_summary %>%
    pivot_longer(cols = c("Pre-Expansion", "Post-Expansion"), names_to = "Period", values_to = "Average") %>%
    mutate(Period = factor(Period, levels = c("Post-Expansion", "Pre-Expansion")))
  
  df_long$cause_name <- factor(df_long$cause_name, levels = rev(df_summary$cause_name))
  
  ggplot(df_long, aes(x = Average, y = cause_name, fill = Period)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("Post-Expansion" = "#1f77b4", "Pre-Expansion" = "#ff7f0e")) +
    labs(
      title = "Deaths Comparison: Pre vs Post Medicaid Expansion\n(Sorted by Diff: Post - Pre)",
      x = "Average Number of Deaths",
      y = "Disease"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "right"
    )
}

# Plot it
plot_pre_post_deaths(df_filtered)


##############################################################################################################

plot_pre_post_deaths <- function(df) {
  df_summary <- df %>%
    filter(measure_name == "Deaths") %>%
    mutate(Period = ifelse(year <= 2014, "Pre-Expansion", "Post-Expansion")) %>%
    group_by(cause_name, Period) %>%
    summarise(avg_val = mean(val, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Period, values_from = avg_val) %>%
    # Print just to confirm
    mutate(Diff = `Pre-Expansion` - `Post-Expansion`) %>%
    arrange(desc(Diff))
  
  # Check if any Post is higher than Pre
  print(df_summary)
  
  # Convert to long format for plotting
  df_long <- df_summary %>%
    pivot_longer(cols = c("Pre-Expansion", "Post-Expansion"),
                 names_to = "Period",
                 values_to = "Average") %>%
    mutate(Period = factor(Period, levels = c("Post-Expansion", "Pre-Expansion")))
  
  # Maintain the correct order in plot
  df_long$cause_name <- factor(df_long$cause_name, levels = df_summary$cause_name)
  
  # Plot
  ggplot(df_long, aes(x = Average, y = cause_name, fill = Period)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("Post-Expansion" = "#1f77b4", "Pre-Expansion" = "#ff7f0e")) +
    labs(
      title = "Deaths Comparison: Pre vs Post Medicaid Expansion\n(Sorted by Diff: Pre - Post)",
      x = "Average Number of Deaths",
      y = "Disease"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "right"
    )
}

##############################################################################################################


# Selected diseases (same list)
selected_diseases <- c(
  "Substance use disorders",
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Filter DALYs data
plot_dalys <- ttest_dalys_filtered %>%
  filter(Disease %in% selected_diseases) %>%
  mutate(Diff = Mean_Post - Mean_Pre) %>%
  arrange(desc(Diff)) %>%
  mutate(Disease = factor(Disease, levels = unique(Disease)))

# Long format for plotting
dalys_long <- plot_dalys %>%
  select(Disease, Mean_Pre, Mean_Post) %>%
  pivot_longer(cols = starts_with("Mean"), names_to = "Period", values_to = "Mean") %>%
  mutate(Period = ifelse(Period == "Mean_Pre", "Pre-Expansion", "Post-Expansion"))

# Plot
ggplot(dalys_long, aes(x = Mean, y = Disease, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "DALYs Comparison: Pre vs Post Medicaid Expansion\n(Sorted by Diff: Post - Pre)",
    x = "Average DALYs",
    y = "Disease"
  ) +
  scale_fill_manual(values = c("Pre-Expansion" = "tomato", "Post-Expansion" = "steelblue")) +
  theme_minimal()


##############################################################################################################


library(dplyr)
library(tidyr)
library(ggplot2)

# Selected diseases
selected_diseases <- c(
  "Substance use disorders",
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Clean and prepare data
plot_incidence <- ttest_incidence_filtered %>%
  filter(Disease %in% selected_diseases) %>%
  mutate(Diff = Mean_Post - Mean_Pre) %>%
  arrange(desc(Diff)) %>%
  mutate(Disease = factor(Disease, levels = unique(Disease)))

# Convert to long format
incidence_long <- plot_incidence %>%
  select(Disease, Mean_Pre, Mean_Post) %>%
  pivot_longer(cols = c(Mean_Pre, Mean_Post), names_to = "Period", values_to = "Mean") %>%
  mutate(Period = recode(Period,
                         "Mean_Pre" = "Pre-Expansion",
                         "Mean_Post" = "Post-Expansion"))

# Plot
ggplot(incidence_long, aes(x = Mean, y = Disease, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Incidence Comparison: Pre vs Post Medicaid Expansion\n(Sorted by Diff: Post - Pre)",
    x = "Average Incidence",
    y = "Disease"
  ) +
  scale_fill_manual(values = c("Pre-Expansion" = "tomato", "Post-Expansion" = "steelblue")) +
  theme_minimal()


##############################################################################################################


library(ggplot2)
library(dplyr)
library(tidyr)

# Step 1: Define the 10 selected diseases
selected_prevalence <- c(
  "Substance use disorders",
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Step 2: Filter and sort by Mean_Post (descending)
plot_data <- ttest_prevalence_filtered %>%
  filter(Disease %in% selected_prevalence) %>%
  arrange(desc(Mean_Post)) %>%
  mutate(Disease = factor(Disease, levels = unique(Disease)))  # preserve order

# Step 3: Convert to long format for plotting
prevalence_long <- plot_data %>%
  select(Disease, Mean_Pre, Mean_Post) %>%
  pivot_longer(cols = starts_with("Mean"), names_to = "Period", values_to = "Mean") %>%
  mutate(Period = ifelse(Period == "Mean_Pre", "Pre-Expansion", "Post-Expansion"))

# Step 4: Plot
ggplot(prevalence_long, aes(x = Mean, y = Disease, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Prevalence Comparison: Pre vs Post Medicaid Expansion",
    x = "Average Prevalence",
    y = "Disease"
  ) +
  scale_fill_manual(values = c("Pre-Expansion" = "tomato", "Post-Expansion" = "steelblue")) +
  theme_minimal()


##############################################################################################################
##############################################################################################################
##############################################################################################################

# DiD 


# Medicaid expansion years by state
expansion_year_dict <- c(
  "Alaska" = 2015, "Arizona" = 2014, "Arkansas" = 2014, "California" = 2014,
  "Colorado" = 2014, "Connecticut" = 2014, "Delaware" = 2014, "District of Columbia" = 2014,
  "Hawaii" = 2014, "Idaho" = 2020, "Illinois" = 2014, "Indiana" = 2015, "Iowa" = 2014,
  "Kentucky" = 2014, "Louisiana" = 2016, "Maine" = 2019, "Maryland" = 2014,
  "Massachusetts" = 2014, "Michigan" = 2014, "Minnesota" = 2014, "Missouri" = 2021,
  "Montana" = 2016, "Nebraska" = 2020, "Nevada" = 2014, "New Hampshire" = 2014,
  "New Jersey" = 2014, "New Mexico" = 2014, "New York" = 2014, "North Carolina" = 2023,
  "North Dakota" = 2014, "Ohio" = 2014, "Oklahoma" = 2021, "Oregon" = 2014,
  "Pennsylvania" = 2015, "Rhode Island" = 2014, "South Dakota" = 2023, "Utah" = 2020,
  "Vermont" = 2014, "Virginia" = 2019, "Washington" = 2014, "West Virginia" = 2014
)

library(dplyr)

df_early <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases) %>%  # Limit to your 10 diseases
  mutate(
    expansion_year = recode(location_name, !!!expansion_year_dict, .default = NA_real_),
    
    expansion_group = case_when(
      expansion_year == 2014 ~ "Early",
      expansion_year > 2014 ~ "Later",
      is.na(expansion_year) ~ "Never",
      TRUE ~ "Never"
    ),
    
    post = case_when(
      expansion_group == "Never" ~ ifelse(year >= 2014, 1, 0),
      expansion_group == "Early" ~ ifelse(year >= 2014, 1, 0),
      TRUE ~ NA_real_
    )
  ) %>%
  filter(expansion_group %in% c("Early", "Never"))

##############################################################################################################
### Run DiD analysis for Early group vs Never ###
##############################################################################################################


library(dplyr)
library(broom)

# Specify the three measures to analyze
measures_to_analyze <- c("Deaths", "DALYs (Disability-Adjusted Life Years)", "Prevalence")

# Loop results container
early_results <- list()

# Loop through each measure and disease
for (measure in measures_to_analyze) {
  for (cause in selected_diseases) {
    
    df_did <- df_early %>%
      filter(
        measure_name == measure,
        cause_name == cause
      ) %>%
      drop_na(post, val) %>%
      mutate(
        treat_group = as.numeric(expansion_group == "Early"),
        sex_name = as.factor(sex_name),
        age_name = as.factor(age_name)
      )
    
    cat("🔍", measure, "|", cause,
        "| obs:", nrow(df_did),
        "| treat_group:", n_distinct(df_did$treat_group),
        "| post:", n_distinct(df_did$post),
        "| sex:", n_distinct(df_did$sex_name),
        "| age:", n_distinct(df_did$age_name), "\n")
    
    # Skip if there's insufficient variation
    if (nrow(df_did) < 20 ||
        n_distinct(df_did$treat_group) < 2 ||
        n_distinct(df_did$post) < 2 ||
        n_distinct(df_did$sex_name) < 2 ||
        n_distinct(df_did$age_name) < 2) next
    
    # Run DiD OLS
    model <- lm(val ~ treat_group * post + sex_name + age_name, data = df_did)
    
    # Extract interaction term
    coef_table <- tidy(model) %>%
      filter(term == "treat_group:post") %>%
      mutate(
        Group = "Early",
        Measure = measure,
        Disease = cause
      )
    
    # Append result
    early_results[[length(early_results) + 1]] <- coef_table
  }
}

# Combine all results into a dataframe
early_did_output <- bind_rows(early_results)

# Display full results
print(early_did_output)

# Filter for statistically significant results
significant_results_initial <- early_did_output %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

print(significant_results_initial)

# Save to CSV
write.csv(significant_results_initial, "significant_results_initial.csv", row.names = FALSE)

##############################################################################################################
### Run DiD analysis for Early group vs Never (State Level) 
##############################################################################################################


library(fixest)
library(dplyr)
library(broom)

# Only run for selected 10 diseases
measures_to_analyze <- c("Deaths", "DALYs (Disability-Adjusted Life Years)", "Prevalence")
early_results <- list()

for (measure in measures_to_analyze) {
  for (cause in selected_diseases) {
    
    # Filter dataset for one (measure × disease) pair
    df_did <- df_early %>%
      filter(
        measure_name == measure,
        cause_name == cause
      ) %>%
      drop_na(post, val) %>%
      mutate(
        treat_group = as.numeric(expansion_group == "Early"),
        sex_name = as.factor(sex_name),
        age_name = as.factor(age_name)
      )
    
    # Print diagnostics
    cat("🔍", measure, "|", cause,
        "| obs:", nrow(df_did),
        "| treat_group:", n_distinct(df_did$treat_group),
        "| post:", n_distinct(df_did$post),
        "| sex_name:", n_distinct(df_did$sex_name),
        "| age_name:", n_distinct(df_did$age_name), "\n")
    
    # Skip groups with insufficient variation
    if (nrow(df_did) < 20 ||
        n_distinct(df_did$treat_group) < 2 ||
        n_distinct(df_did$post) < 2 ||
        n_distinct(df_did$sex_name) < 2 ||
        n_distinct(df_did$age_name) < 2) next
    
    # Run DiD model with fixed effects (state)
    model <- feols(val ~ post:treat_group + sex_name + age_name | location_name, data = df_did)
    
    # Extract interaction coefficient
    coef_table <- tidy(model) %>%
      filter(term == "post:treat_group") %>%
      mutate(
        Group = "Early",
        Measure = measure,
        Disease = cause
      )
    
    # Store result
    early_results[[length(early_results) + 1]] <- coef_table
  }
}

# Combine all results into a single table
early_did_output <- bind_rows(early_results)

# Show full results
print(early_did_output)

# Filter for statistically significant estimates
significant_results_state <- early_did_output %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

print(significant_results_state, n =Inf)

# Save to CSV
write.csv(significant_results_state, "significant_results_state.csv", row.names = FALSE)

##############################################################################################################
### Run DiD analysis for Early group vs Never (Year Level) 
##############################################################################################################


library(fixest)
library(dplyr)
library(broom)

# Define measures and initialize results
measures_to_analyze <- c("Deaths", "DALYs (Disability-Adjusted Life Years)", "Prevalence")
early_results <- list()

# Loop through measures and diseases
for (measure in measures_to_analyze) {
  for (cause in selected_diseases) {
    
    # Filter data for current measure and disease
    df_did <- df_early %>%
      filter(
        measure_name == measure,
        cause_name == cause
      ) %>%
      drop_na(post, val) %>%
      mutate(
        treat_group = as.numeric(expansion_group == "Early"),
        sex_name = as.factor(sex_name),
        age_name = as.factor(age_name)
      )
    
    # Diagnostic output
    cat("🔍", measure, "|", cause,
        "| obs:", nrow(df_did),
        "| treat_group:", n_distinct(df_did$treat_group),
        "| post:", n_distinct(df_did$post),
        "| sex_name:", n_distinct(df_did$sex_name),
        "| age_name:", n_distinct(df_did$age_name), "\n")
    
    # Skip if not enough data
    if (nrow(df_did) < 20 ||
        n_distinct(df_did$treat_group) < 2 ||
        n_distinct(df_did$post) < 2 ||
        n_distinct(df_did$sex_name) < 2 ||
        n_distinct(df_did$age_name) < 2) next
    
    # Run DiD regression with year fixed effect
    model <- feols(val ~ post:treat_group + sex_name + age_name | year, data = df_did)
    
    # Extract DiD effect
    coef_table <- tidy(model) %>%
      filter(term == "post:treat_group") %>%
      mutate(
        Group = "Early",
        Measure = measure,
        Disease = cause
      )
    
    early_results[[length(early_results) + 1]] <- coef_table
  }
}

# Combine all results
early_did_output <- bind_rows(early_results)
print(early_did_output)

# Show significant ones
significant_results_year <- early_did_output %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

print(significant_results_year, n=Inf)

# Save to CSV
write.csv(significant_results_year, "significant_results_state.csv", row.names = FALSE)


##############################################################################################################
### Run DiD analysis for Mid group vs Never ###
##############################################################################################################


library(dplyr)
library(broom)

# Define measures and initialize results list
measures_to_analyze <- c("Deaths", "DALYs (Disability-Adjusted Life Years)", "Prevalence")
mid_results <- list()

# Prepare dataset for Mid vs Never
df_mid <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases) %>%
  mutate(
    expansion_year = recode(location_name, !!!expansion_year_dict, .default = NA_real_),
    expansion_group = case_when(
      expansion_year == 2014 ~ "Early",
      expansion_year %in% c(2015, 2016) ~ "Mid",
      expansion_year > 2016 ~ "Late",
      is.na(expansion_year) ~ "Never",
      TRUE ~ "Never"
    ),
    post = case_when(
      expansion_group == "Mid" ~ ifelse(year >= expansion_year, 1, 0),
      expansion_group == "Never" ~ ifelse(year >= 2014, 1, 0),
      TRUE ~ NA_real_
    )
  ) %>%
  filter(expansion_group %in% c("Mid", "Never"))

# Run DiD loop
for (measure in measures_to_analyze) {
  for (cause in selected_diseases) {
    
    df_did <- df_mid %>%
      filter(
        measure_name == measure,
        cause_name == cause
      ) %>%
      drop_na(post, val) %>%
      mutate(
        treat_group = as.numeric(expansion_group == "Mid"),
        sex_name = as.factor(sex_name),
        age_name = as.factor(age_name)
      )
    
    cat("🔍", measure, "|", cause,
        "| obs:", nrow(df_did),
        "| treat_group:", n_distinct(df_did$treat_group),
        "| post:", n_distinct(df_did$post),
        "| sex:", n_distinct(df_did$sex_name),
        "| age:", n_distinct(df_did$age_name), "\n")
    
    if (nrow(df_did) < 20 ||
        n_distinct(df_did$treat_group) < 2 ||
        n_distinct(df_did$post) < 2 ||
        n_distinct(df_did$sex_name) < 2 ||
        n_distinct(df_did$age_name) < 2) next
    
    model <- lm(val ~ treat_group * post + sex_name + age_name, data = df_did)
    
    coef_table <- tidy(model) %>%
      filter(term == "treat_group:post") %>%
      mutate(
        Group = "Mid",
        Measure = measure,
        Disease = cause
      )
    
    mid_results[[length(mid_results) + 1]] <- coef_table
  }
}

# Combine results
mid_did_output <- bind_rows(mid_results)

# Filter significant results
significant_results_mid <- mid_did_output %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

# Save results
print(significant_results_mid, n = Inf)
write.csv(significant_results_mid, "significant_results_initial_mid.csv", row.names = FALSE)


##############################################################################################################
### Run DiD analysis for Mid group vs Never (Year Level) ###
##############################################################################################################


library(fixest)
library(dplyr)
library(broom)

# Define measures and initialize result list
measures_to_analyze <- c("Deaths", "DALYs (Disability-Adjusted Life Years)", "Prevalence")
mid_results <- list()

# Reuse df_mid from previous step, or re-prepare if needed
# (Assumes df_mid is already filtered for Mid vs Never and includes `post` and `treat_group`)

for (measure in measures_to_analyze) {
  for (cause in selected_diseases) {
    
    df_did <- df_mid %>%
      filter(
        measure_name == measure,
        cause_name == cause
      ) %>%
      drop_na(post, val) %>%
      mutate(
        treat_group = as.numeric(expansion_group == "Mid"),
        sex_name = as.factor(sex_name),
        age_name = as.factor(age_name)
      )
    
    cat("🔍", measure, "|", cause,
        "| obs:", nrow(df_did),
        "| treat_group:", n_distinct(df_did$treat_group),
        "| post:", n_distinct(df_did$post),
        "| sex:", n_distinct(df_did$sex_name),
        "| age:", n_distinct(df_did$age_name), "\n")
    
    if (nrow(df_did) < 20 ||
        n_distinct(df_did$treat_group) < 2 ||
        n_distinct(df_did$post) < 2 ||
        n_distinct(df_did$sex_name) < 2 ||
        n_distinct(df_did$age_name) < 2) next
    
    # Run DiD with year fixed effects
    model <- feols(val ~ post:treat_group + sex_name + age_name | year, data = df_did)
    
    coef_table <- tidy(model) %>%
      filter(term == "post:treat_group") %>%
      mutate(
        Group = "Mid",
        Measure = measure,
        Disease = cause
      )
    
    mid_results[[length(mid_results) + 1]] <- coef_table
  }
}

# Combine all results
mid_did_output <- bind_rows(mid_results)

# Filter for significant ones
significant_results_mid_year <- mid_did_output %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

# Save to CSV
print(significant_results_mid_year, n = Inf)
write.csv(significant_results_mid_year, "significant_results_mid_year.csv", row.names = FALSE)


##############################################################################################################
### Run DiD analysis for Mid group vs Never (State Level) ###
##############################################################################################################

library(fixest)
library(dplyr)
library(broom)

# Define measures and initialize result container
measures_to_analyze <- c("Deaths", "DALYs (Disability-Adjusted Life Years)", "Prevalence")
mid_results <- list()

# Use df_mid from earlier setup, already filtered for Mid vs Never
for (measure in measures_to_analyze) {
  for (cause in selected_diseases) {
    
    df_did <- df_mid %>%
      filter(
        measure_name == measure,
        cause_name == cause
      ) %>%
      drop_na(post, val) %>%
      mutate(
        treat_group = as.numeric(expansion_group == "Mid"),
        sex_name = as.factor(sex_name),
        age_name = as.factor(age_name)
      )
    
    cat("🔍", measure, "|", cause,
        "| obs:", nrow(df_did),
        "| treat_group:", n_distinct(df_did$treat_group),
        "| post:", n_distinct(df_did$post),
        "| sex:", n_distinct(df_did$sex_name),
        "| age:", n_distinct(df_did$age_name), "\n")
    
    if (nrow(df_did) < 20 ||
        n_distinct(df_did$treat_group) < 2 ||
        n_distinct(df_did$post) < 2 ||
        n_distinct(df_did$sex_name) < 2 ||
        n_distinct(df_did$age_name) < 2) next
    
    # Run DiD with state fixed effects
    model <- feols(val ~ post:treat_group + sex_name + age_name | location_name, data = df_did)
    
    coef_table <- tidy(model) %>%
      filter(term == "post:treat_group") %>%
      mutate(
        Group = "Mid",
        Measure = measure,
        Disease = cause
      )
    
    mid_results[[length(mid_results) + 1]] <- coef_table
  }
}

# Combine all results
mid_did_output <- bind_rows(mid_results)

# Filter for statistically significant results
significant_results_mid_state <- mid_did_output %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

# Save to CSV
print(significant_results_mid_state, n = Inf)
write.csv(significant_results_mid_state, "significant_results_mid_state.csv", row.names = FALSE)

##############################################################################################################
### Run DiD analysis for Late group vs Never ###
##############################################################################################################


library(dplyr)
library(broom)

# Define measures and initialize results
measures_to_analyze <- c("Deaths", "DALYs (Disability-Adjusted Life Years)", "Prevalence")
late_results <- list()

# Prepare dataset for Late vs Never
df_late <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases) %>%
  mutate(
    expansion_year = recode(location_name, !!!expansion_year_dict, .default = NA_real_),
    expansion_group = case_when(
      expansion_year == 2014 ~ "Early",
      expansion_year %in% c(2015, 2016) ~ "Mid",
      expansion_year > 2016 ~ "Late",
      is.na(expansion_year) ~ "Never",
      TRUE ~ "Never"
    ),
    post = case_when(
      expansion_group == "Late" ~ ifelse(year >= expansion_year, 1, 0),
      expansion_group == "Never" ~ ifelse(year >= 2014, 1, 0),
      TRUE ~ NA_real_
    )
  ) %>%
  filter(expansion_group %in% c("Late", "Never"))

# Run DiD for each (measure × disease) combination
for (measure in measures_to_analyze) {
  for (cause in selected_diseases) {
    
    df_did <- df_late %>%
      filter(
        measure_name == measure,
        cause_name == cause
      ) %>%
      drop_na(post, val) %>%
      mutate(
        treat_group = as.numeric(expansion_group == "Late"),
        sex_name = as.factor(sex_name),
        age_name = as.factor(age_name)
      )
    
    cat("🔍", measure, "|", cause,
        "| obs:", nrow(df_did),
        "| treat_group:", n_distinct(df_did$treat_group),
        "| post:", n_distinct(df_did$post),
        "| sex:", n_distinct(df_did$sex_name),
        "| age:", n_distinct(df_did$age_name), "\n")
    
    if (nrow(df_did) < 20 ||
        n_distinct(df_did$treat_group) < 2 ||
        n_distinct(df_did$post) < 2 ||
        n_distinct(df_did$sex_name) < 2 ||
        n_distinct(df_did$age_name) < 2) next
    
    model <- lm(val ~ treat_group * post + sex_name + age_name, data = df_did)
    
    coef_table <- tidy(model) %>%
      filter(term == "treat_group:post") %>%
      mutate(
        Group = "Late",
        Measure = measure,
        Disease = cause
      )
    
    late_results[[length(late_results) + 1]] <- coef_table
  }
}

# Combine and filter significant results
late_did_output <- bind_rows(late_results)

significant_results_late <- late_did_output %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

# Save results
print(significant_results_late, n = Inf)
write.csv(significant_results_late, "significant_results_initial_late.csv", row.names = FALSE)


##############################################################################################################
### Run DiD analysis for Late group vs Never (Year Level) ###
##############################################################################################################

library(fixest)
library(dplyr)
library(broom)

# Define measures and initialize results list
measures_to_analyze <- c("Deaths", "DALYs (Disability-Adjusted Life Years)", "Prevalence")
late_results <- list()

# Use df_late from the previous step (Late vs Never data already prepared)
for (measure in measures_to_analyze) {
  for (cause in selected_diseases) {
    
    df_did <- df_late %>%
      filter(
        measure_name == measure,
        cause_name == cause
      ) %>%
      drop_na(post, val) %>%
      mutate(
        treat_group = as.numeric(expansion_group == "Late"),
        sex_name = as.factor(sex_name),
        age_name = as.factor(age_name)
      )
    
    cat("🔍", measure, "|", cause,
        "| obs:", nrow(df_did),
        "| treat_group:", n_distinct(df_did$treat_group),
        "| post:", n_distinct(df_did$post),
        "| sex:", n_distinct(df_did$sex_name),
        "| age:", n_distinct(df_did$age_name), "\n")
    
    if (nrow(df_did) < 20 ||
        n_distinct(df_did$treat_group) < 2 ||
        n_distinct(df_did$post) < 2 ||
        n_distinct(df_did$sex_name) < 2 ||
        n_distinct(df_did$age_name) < 2) next
    
    # Run DiD model with year fixed effects
    model <- feols(val ~ post:treat_group + sex_name + age_name | year, data = df_did)
    
    coef_table <- tidy(model) %>%
      filter(term == "post:treat_group") %>%
      mutate(
        Group = "Late",
        Measure = measure,
        Disease = cause
      )
    
    late_results[[length(late_results) + 1]] <- coef_table
  }
}

# Combine results
late_did_output <- bind_rows(late_results)

# Filter for significant results
significant_results_late_year <- late_did_output %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

# Save output
print(significant_results_late_year, n = Inf)
write.csv(significant_results_late_year, "significant_results_late_year.csv", row.names = FALSE)


##############################################################################################################
### Run DiD analysis for Late group vs Never (State Level) ###
##############################################################################################################


library(fixest)
library(dplyr)
library(broom)

# Define measures and initialize result list
measures_to_analyze <- c("Deaths", "DALYs (Disability-Adjusted Life Years)", "Prevalence")
late_results <- list()

# Use df_late prepared earlier (filtered for Late and Never states, with 'post' defined)
for (measure in measures_to_analyze) {
  for (cause in selected_diseases) {
    
    df_did <- df_late %>%
      filter(
        measure_name == measure,
        cause_name == cause
      ) %>%
      drop_na(post, val) %>%
      mutate(
        treat_group = as.numeric(expansion_group == "Late"),
        sex_name = as.factor(sex_name),
        age_name = as.factor(age_name)
      )
    
    cat("🔍", measure, "|", cause,
        "| obs:", nrow(df_did),
        "| treat_group:", n_distinct(df_did$treat_group),
        "| post:", n_distinct(df_did$post),
        "| sex:", n_distinct(df_did$sex_name),
        "| age:", n_distinct(df_did$age_name), "\n")
    
    if (nrow(df_did) < 20 ||
        n_distinct(df_did$treat_group) < 2 ||
        n_distinct(df_did$post) < 2 ||
        n_distinct(df_did$sex_name) < 2 ||
        n_distinct(df_did$age_name) < 2) next
    
    # Run DiD model with state fixed effects
    model <- feols(val ~ post:treat_group + sex_name + age_name | location_name, data = df_did)
    
    coef_table <- tidy(model) %>%
      filter(term == "post:treat_group") %>%
      mutate(
        Group = "Late",
        Measure = measure,
        Disease = cause
      )
    
    late_results[[length(late_results) + 1]] <- coef_table
  }
}

# Combine results
late_did_output <- bind_rows(late_results)

# Filter significant ones
significant_results_late_state <- late_did_output %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

# Save to CSV
print(significant_results_late_state, n = Inf)
write.csv(significant_results_late_state, "significant_results_late_state.csv", row.names = FALSE)


##############################################################################################################


# DiD Updated

### Early vs Never DiD

library(dplyr)
library(broom)

selected_diseases <- c(
  "Substance use disorders",
  "Drug use disorders",
  "Opioid use disorders",
  "Diabetes and kidney diseases",
  "Chronic kidney disease",
  "Chronic kidney disease due to diabetes mellitus type 2",
  "Alzheimer's disease and other dementias",
  "Alcohol use disorders",
  "Chronic kidney disease due to hypertension",
  "Uterine cancer"
)

# Medicaid expansion years by state
expansion_year_dict <- c(
  "Alaska" = 2015, "Arizona" = 2014, "Arkansas" = 2014, "California" = 2014,
  "Colorado" = 2014, "Connecticut" = 2014, "Delaware" = 2014, "District of Columbia" = 2014,
  "Hawaii" = 2014, "Idaho" = 2020, "Illinois" = 2014, "Indiana" = 2015, "Iowa" = 2014,
  "Kentucky" = 2014, "Louisiana" = 2016, "Maine" = 2019, "Maryland" = 2014,
  "Massachusetts" = 2014, "Michigan" = 2014, "Minnesota" = 2014, "Missouri" = 2021,
  "Montana" = 2016, "Nebraska" = 2020, "Nevada" = 2014, "New Hampshire" = 2014,
  "New Jersey" = 2014, "New Mexico" = 2014, "New York" = 2014, "North Carolina" = 2023,
  "North Dakota" = 2014, "Ohio" = 2014, "Oklahoma" = 2021, "Oregon" = 2014,
  "Pennsylvania" = 2015, "Rhode Island" = 2014, "South Dakota" = 2023, "Utah" = 2020,
  "Vermont" = 2014, "Virginia" = 2019, "Washington" = 2014, "West Virginia" = 2014
)

# Step 1: Create df_early (filtered for selected diseases and recoded for DiD)
df_early <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases) %>%
  mutate(
    expansion_year = recode(location_name, !!!expansion_year_dict, .default = NA_real_),
    expansion_group = case_when(
      expansion_year == 2014 ~ "Early",
      expansion_year > 2014 ~ "Later",
      is.na(expansion_year) ~ "Never",
      TRUE ~ "Never"
    ),
    post = case_when(
      expansion_group == "Early" ~ ifelse(year >= 2014, 1, 0),
      expansion_group == "Never" ~ ifelse(year >= 2014, 1, 0),
      TRUE ~ NA_real_
    )
  ) %>%
  filter(expansion_group %in% c("Early", "Never"))


# Step 2: Run DiD for 4 measures
measures_to_analyze <- c("Deaths", "DALYs (Disability-Adjusted Life Years)", "Prevalence", "Incidence")
early_results <- list()

for (measure in measures_to_analyze) {
  for (cause in selected_diseases) {
    
    df_did <- df_early %>%
      filter(measure_name == measure, cause_name == cause) %>%
      drop_na(post, val) %>%
      mutate(
        treat_group = as.numeric(expansion_group == "Early"),
        sex_name = as.factor(sex_name),
        age_name = as.factor(age_name)
      )
    
    cat("🔍", measure, "|", cause,
        "| obs:", nrow(df_did),
        "| treat_group:", n_distinct(df_did$treat_group),
        "| post:", n_distinct(df_did$post),
        "| sex:", n_distinct(df_did$sex_name),
        "| age:", n_distinct(df_did$age_name), "\n")
    
    if (nrow(df_did) < 20 ||
        n_distinct(df_did$treat_group) < 2 ||
        n_distinct(df_did$post) < 2 ||
        n_distinct(df_did$sex_name) < 2 ||
        n_distinct(df_did$age_name) < 2) next
    
    # OLS DiD
    model <- lm(val ~ treat_group * post + sex_name + age_name, data = df_did)
    
    # Extract interaction effect
    coef_table <- tidy(model) %>%
      filter(term == "treat_group:post") %>%
      mutate(
        Group = "Early",
        Measure = measure,
        Disease = cause
      )
    
    early_results[[length(early_results) + 1]] <- coef_table
  }
}

# Step 3: Combine & export
early_did_output <- bind_rows(early_results)

# Significant results
significant_results <- early_did_output %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

# Print & Save
print(significant_results)
write.csv(significant_results, "significant_did_results_all_measures.csv", row.names = FALSE)

##############################################################################################################

### Graph
library(ggplot2)
library(dplyr)
library(forcats)
library(stringr)

# Format results
plot_data <- early_did_output %>%
  mutate(
    Measure = case_when(
      Measure == "DALYs (Disability-Adjusted Life Years)" ~ "DALYs",
      TRUE ~ Measure
    ),
    Effect_Type = case_when(
      p.value < 0.05 & estimate < 0 ~ "Significant.Negative",
      TRUE ~ "Non-Significant"
    ),
    Effect_Type = factor(Effect_Type, levels = c("Significant.Negative", "Non-Significant")),
    Disease = fct_reorder(Disease, estimate)
  )

# Plot
ggplot(plot_data, aes(x = estimate, y = Disease, color = Effect_Type)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2) +
  facet_wrap(~ Measure, scales = "free_x") +
  scale_color_manual(
    values = c("Significant.Negative" = "#1f78b4", "Non-Significant" = "grey50")
  ) +
  labs(
    title = "DiD Estimates by Disease",
    subtitle = "Early vs Never Expansion States (4 Measures Separated)",
    x = "Estimated Treatment Effect",
    y = NULL,
    color = "Effect Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )


### DiD 4 Measure Combined Graph
library(ggplot2)
library(dplyr)
library(forcats)

# Clean and label
early_did_output_cleaned <- early_did_output %>%
  mutate(
    Measure = case_when(
      Measure == "DALYs (Disability-Adjusted Life Years)" ~ "DALYs",
      TRUE ~ Measure
    ),
    Effect_Type = case_when(
      p.value < 0.05 & estimate > 0 ~ "Significant.Positive",
      p.value < 0.05 & estimate < 0 ~ "Significant.Negative",
      TRUE ~ "Non-Significant"
    ),
    Effect_Type = factor(
      Effect_Type,
      levels = c("Significant.Positive", "Significant.Negative", "Non-Significant")
    )
  )

# Plot
ggplot(early_did_output_cleaned, aes(
  x = estimate,
  y = fct_reorder(Disease, estimate),
  color = Effect_Type
)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(size = 3, position = position_dodge(width = 0.7)) +
  geom_errorbarh(
    aes(xmin = estimate - std.error, xmax = estimate + std.error),
    height = 0.2,
    position = position_dodge(width = 0.7)
  ) +
  scale_color_manual(
    values = c(
      "Significant.Positive" = "#33a02c",
      "Significant.Negative" = "#1f78b4",
      "Non-Significant" = "gray70"
    )
  ) +
  labs(
    title = "DiD Estimates by Disease",
    subtitle = "Early vs Never Expansion States (All Measures Combined)",
    x = "Estimated Treatment Effect",
    y = NULL,
    color = "Effect Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(size = 9)
  )

##############################################################################################################
#Fixed State: Early vs Never
##############################################################################################################

library(fixest)
library(dplyr)
library(broom)


# Include all 4 measures
measures_to_analyze <- c("Deaths", "DALYs (Disability-Adjusted Life Years)", "Prevalence", "Incidence")
early_results <- list()

for (measure in measures_to_analyze) {
  for (cause in selected_diseases) {
    
    df_did <- df_early %>%
      filter(
        measure_name == measure,
        cause_name == cause
      ) %>%
      drop_na(post, val) %>%
      mutate(
        treat_group = as.numeric(expansion_group == "Early"),
        sex_name = as.factor(sex_name),
        age_name = as.factor(age_name)
      )
    
    cat("🔍", measure, "|", cause,
        "| obs:", nrow(df_did),
        "| treat_group:", n_distinct(df_did$treat_group),
        "| post:", n_distinct(df_did$post),
        "| sex_name:", n_distinct(df_did$sex_name),
        "| age_name:", n_distinct(df_did$age_name), "\n")
    
    if (nrow(df_did) < 20 ||
        n_distinct(df_did$treat_group) < 2 ||
        n_distinct(df_did$post) < 2 ||
        n_distinct(df_did$sex_name) < 2 ||
        n_distinct(df_did$age_name) < 2) next
    
    model <- feols(val ~ post:treat_group + sex_name + age_name | location_name, data = df_did)
    
    coef_table <- tidy(model) %>%
      filter(term == "post:treat_group") %>%
      mutate(
        Group = "Early",
        Measure = measure,
        Disease = cause
      )
    
    early_results[[length(early_results) + 1]] <- coef_table
  }
}

early_did_output <- bind_rows(early_results)

# Label effect types
early_did_output <- early_did_output %>%
  mutate(
    Measure = case_when(
      Measure == "DALYs (Disability-Adjusted Life Years)" ~ "DALYs",
      TRUE ~ Measure
    ),
    Effect_Type = case_when(
      p.value < 0.05 & estimate > 0 ~ "Significant.Positive",
      p.value < 0.05 & estimate < 0 ~ "Significant.Negative",
      TRUE ~ "Non-Significant"
    ),
    Effect_Type = factor(
      Effect_Type,
      levels = c("Significant.Positive", "Significant.Negative", "Non-Significant")
    )
  )

# 🔹 Save & print
significant_results_state <- early_did_output %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

# Save both significant and all results
write.csv(significant_results_state, "significant_results_state.csv", row.names = FALSE)
write.csv(early_did_output, "all_results_state.csv", row.names = FALSE)

# Print for review
print(significant_results_state, n = Inf)

##############################################################################################################
### 4 Measure Combined Graph Fixed State: Early vs Never
##############################################################################################################

library(ggplot2)
library(dplyr)

# Make sure your result data is named correctly
df_plot <- early_did_output %>%
  mutate(
    Measure = case_when(
      Measure == "DALYs (Disability-Adjusted Life Years)" ~ "DALYs",
      TRUE ~ Measure
    ),
    Effect_Type = case_when(
      p.value < 0.05 & estimate > 0 ~ "Significant.Positive",
      p.value < 0.05 & estimate < 0 ~ "Significant.Negative",
      TRUE ~ "Non-Significant"
    ),
    Effect_Type = factor(
      Effect_Type,
      levels = c("Significant.Positive", "Significant.Negative", "Non-Significant")
    )
  )

# Plot
ggplot(df_plot, aes(x = estimate, y = Disease, color = Effect_Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
  scale_color_manual(
    values = c(
      "Significant.Positive" = "#FFC107",   # Yellow
      "Significant.Negative" = "#0072B2",   # Blue
      "Non-Significant" = "gray30"          # Dark gray
    )
  ) +
  labs(
    title = "DiD Estimates by Disease",
    subtitle = "Early vs Never Expansion States (4 Measures Combined)",
    x = "Estimated Treatment Effect",
    y = "Disease",
    color = "Effect Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom"
  )


##############################################################################################################
### 4 Measure Seperated Graph Fixed State: Early vs Never
##############################################################################################################

library(ggplot2)
library(dplyr)
library(patchwork)

# Ensure formatting
early_did_output <- early_did_output %>%
  mutate(
    Measure = case_when(
      Measure == "DALYs (Disability-Adjusted Life Years)" ~ "DALYs",
      TRUE ~ Measure
    ),
    Disease = factor(Disease, levels = rev(unique(Disease))),
    Effect_Type = case_when(
      p.value < 0.05 & estimate > 0 ~ "Significant.Positive",
      p.value < 0.05 & estimate < 0 ~ "Significant.Negative",
      TRUE ~ "Non-Significant"
    )
  )

# Define consistent colors
effect_colors <- c(
  "Significant.Positive" = "gold",
  "Significant.Negative" = "steelblue",
  "Non-Significant" = "darkgray"
)

# Create one plot per measure
plot_did_measure <- function(measure_name) {
  df <- early_did_output %>% filter(Measure == measure_name)
  
  ggplot(df, aes(x = estimate, y = Disease, color = Effect_Type)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    scale_color_manual(values = effect_colors) +
    labs(title = measure_name, x = "Estimated Treatment Effect", y = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 8),
      legend.position = "none"
    )
}

# Combine plots
p1 <- plot_did_measure("DALYs")
p2 <- plot_did_measure("Deaths")
p3 <- plot_did_measure("Incidence")
p4 <- plot_did_measure("Prevalence")

# Arrange in 2x2 grid with shared legend
combined_plot <- (p1 + p2) / (p3 + p4) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Display
combined_plot

##############################################################################################################
#### Fixed Year: Mid vs Never
##############################################################################################################

library(fixest)
library(dplyr)
library(broom)

# Define expansion year for states
expansion_year_dict <- c(
  "Alaska" = 2015, "Arizona" = 2014, "Arkansas" = 2014, "California" = 2014,
  "Colorado" = 2014, "Connecticut" = 2014, "Delaware" = 2014, "District of Columbia" = 2014,
  "Hawaii" = 2014, "Idaho" = 2020, "Illinois" = 2014, "Indiana" = 2015, "Iowa" = 2014,
  "Kentucky" = 2014, "Louisiana" = 2016, "Maine" = 2019, "Maryland" = 2014,
  "Massachusetts" = 2014, "Michigan" = 2014, "Minnesota" = 2014, "Missouri" = 2021,
  "Montana" = 2016, "Nebraska" = 2020, "Nevada" = 2014, "New Hampshire" = 2014,
  "New Jersey" = 2014, "New Mexico" = 2014, "New York" = 2014, "North Carolina" = 2023,
  "North Dakota" = 2014, "Ohio" = 2014, "Oklahoma" = 2021, "Oregon" = 2014,
  "Pennsylvania" = 2015, "Rhode Island" = 2014, "South Dakota" = 2023, "Utah" = 2020,
  "Vermont" = 2014, "Virginia" = 2019, "Washington" = 2014, "West Virginia" = 2014
)

# Create df_mid with Mid and Never states
df_mid <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases) %>%
  mutate(
    expansion_year = recode(location_name, !!!expansion_year_dict, .default = NA_real_),
    expansion_group = case_when(
      expansion_year %in% c(2015, 2016) ~ "Mid",
      is.na(expansion_year) ~ "Never",
      TRUE ~ "Other"
    ),
    post = case_when(
      expansion_group == "Mid" ~ ifelse(year >= expansion_year, 1, 0),
      expansion_group == "Never" ~ ifelse(year >= 2014, 1, 0),
      TRUE ~ NA_real_
    )
  ) %>%
  filter(expansion_group %in% c("Mid", "Never"))

# Measures to analyze
measures_to_analyze <- c("Deaths", "DALYs (Disability-Adjusted Life Years)", "Prevalence", "Incidence")
mid_results <- list()

# Loop through each measure and disease
for (measure in measures_to_analyze) {
  for (cause in selected_diseases) {
    
    df_did <- df_mid %>%
      filter(
        measure_name == measure,
        cause_name == cause
      ) %>%
      drop_na(post, val) %>%
      mutate(
        treat_group = as.numeric(expansion_group == "Mid"),
        sex_name = as.factor(sex_name),
        age_name = as.factor(age_name)
      )
    
    cat("🔍", measure, "|", cause,
        "| obs:", nrow(df_did),
        "| treat_group:", n_distinct(df_did$treat_group),
        "| post:", n_distinct(df_did$post),
        "| sex_name:", n_distinct(df_did$sex_name),
        "| age_name:", n_distinct(df_did$age_name), "\n")
    
    if (nrow(df_did) < 20 ||
        n_distinct(df_did$treat_group) < 2 ||
        n_distinct(df_did$post) < 2 ||
        n_distinct(df_did$sex_name) < 2 ||
        n_distinct(df_did$age_name) < 2) next
    
    model <- feols(val ~ post:treat_group + sex_name + age_name | location_name, data = df_did)
    
    coef_table <- tidy(model) %>%
      filter(term == "post:treat_group") %>%
      mutate(
        Group = "Mid",
        Measure = measure,
        Disease = cause
      )
    
    mid_results[[length(mid_results) + 1]] <- coef_table
  }
}

# Combine and label effects
mid_did_output <- bind_rows(mid_results) %>%
  mutate(
    Measure = ifelse(Measure == "DALYs (Disability-Adjusted Life Years)", "DALYs", Measure),
    Effect_Type = case_when(
      p.value < 0.05 & estimate > 0 ~ "Significant.Positive",
      p.value < 0.05 & estimate < 0 ~ "Significant.Negative",
      TRUE ~ "Non-Significant"
    ),
    Effect_Type = factor(
      Effect_Type,
      levels = c("Significant.Positive", "Significant.Negative", "Non-Significant")
    )
  )

# Save significant results
significant_results_mid <- mid_did_output %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

write.csv(significant_results_mid, "significant_results_mid.csv", row.names = FALSE)
print(significant_results_mid, n = Inf)



library(ggplot2)
library(dplyr)

# Color map
effect_colors <- c(
  "Significant.Positive" = "gold",
  "Significant.Negative" = "steelblue",
  "Non-Significant" = "darkgray"
)

# Combined plot (all measures, one axis)
ggplot(mid_did_output, aes(x = estimate, y = Disease, color = Effect_Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = effect_colors) +
  facet_wrap(~Measure, scales = "free_x") +
  labs(
    title = "Difference-in-Differences Results (Mid vs Never, State FE)",
    x = "Estimated Treatment Effect",
    y = "Disease",
    color = "Effect Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 9),
    strip.text = element_text(size = 12)
  )


##############################################################################################################

library(ggplot2)
library(dplyr)

# Clean and classify
mid_did_output <- mid_did_output %>%
  mutate(
    Measure = case_when(
      Measure == "DALYs (Disability-Adjusted Life Years)" ~ "DALYs",
      TRUE ~ Measure
    ),
    Disease_Label = paste(Disease, "-", Measure),  # Combine labels
    Effect_Type = case_when(
      p.value < 0.05 & estimate > 0 ~ "Significant.Positive",
      p.value < 0.05 & estimate < 0 ~ "Significant.Negative",
      TRUE ~ "Non-Significant"
    ),
    Effect_Type = factor(Effect_Type, levels = c("Significant.Positive", "Significant.Negative", "Non-Significant")),
    Disease_Label = factor(Disease_Label, levels = rev(unique(Disease_Label)))
  )

# Define color palette
effect_colors <- c(
  "Significant.Positive" = "gold",
  "Significant.Negative" = "steelblue",
  "Non-Significant" = "darkgray"
)

# Plot: Combined All Measures into One Panel
ggplot(mid_did_output, aes(x = estimate, y = Disease_Label, color = Effect_Type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = effect_colors) +
  labs(
    title = "Fixed Effect (State) DiD Estimates by Disease",
    subtitle = "Mid vs Never Expansion States (4 Measures Combined)",
    x = "Estimated Treatment Effect",
    y = "Disease - Measure",
    color = "Effect Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom"
  )

##############################################################################################################
#### Fixed Year: Late vs Never
##############################################################################################################

library(fixest)
library(dplyr)
library(broom)

# Define Late expansion states
df_late <- gbd_full_data %>%
  filter(cause_name %in% selected_diseases) %>%
  mutate(
    expansion_year = recode(location_name, !!!expansion_year_dict, .default = NA_real_),
    expansion_group = case_when(
      expansion_year %in% c(2017, 2018, 2019) ~ "Late",
      is.na(expansion_year) ~ "Never",
      TRUE ~ "Other"  # filter out Early and Mid
    ),
    post = case_when(
      expansion_group == "Never" ~ ifelse(year >= 2017, 1, 0),
      expansion_group == "Late" ~ ifelse(year >= 2017, 1, 0),
      TRUE ~ NA_real_
    )
  ) %>%
  filter(expansion_group %in% c("Late", "Never"))

# DiD analysis: Late vs Never (Fixed State)
measures_to_analyze <- c("Deaths", "DALYs (Disability-Adjusted Life Years)", "Prevalence", "Incidence")
late_results <- list()

for (measure in measures_to_analyze) {
  for (cause in selected_diseases) {
    
    df_did <- df_late %>%
      filter(
        measure_name == measure,
        cause_name == cause
      ) %>%
      drop_na(post, val) %>%
      mutate(
        treat_group = as.numeric(expansion_group == "Late"),
        sex_name = as.factor(sex_name),
        age_name = as.factor(age_name)
      )
    
    cat("🔍", measure, "|", cause,
        "| obs:", nrow(df_did),
        "| treat_group:", n_distinct(df_did$treat_group),
        "| post:", n_distinct(df_did$post),
        "| sex_name:", n_distinct(df_did$sex_name),
        "| age_name:", n_distinct(df_did$age_name), "\n")
    
    if (nrow(df_did) < 20 ||
        n_distinct(df_did$treat_group) < 2 ||
        n_distinct(df_did$post) < 2 ||
        n_distinct(df_did$sex_name) < 2 ||
        n_distinct(df_did$age_name) < 2) next
    
    model <- feols(val ~ post:treat_group + sex_name + age_name | location_name, data = df_did)
    
    coef_table <- tidy(model) %>%
      filter(term == "post:treat_group") %>%
      mutate(
        Group = "Late",
        Measure = measure,
        Disease = cause
      )
    
    late_results[[length(late_results) + 1]] <- coef_table
  }
}

late_did_output <- bind_rows(late_results)

# Categorize effect types
late_did_output <- late_did_output %>%
  mutate(
    Measure = case_when(
      Measure == "DALYs (Disability-Adjusted Life Years)" ~ "DALYs",
      TRUE ~ Measure
    ),
    Effect_Type = case_when(
      p.value < 0.05 & estimate > 0 ~ "Significant.Positive",
      p.value < 0.05 & estimate < 0 ~ "Significant.Negative",
      TRUE ~ "Non-Significant"
    ),
    Effect_Type = factor(
      Effect_Type,
      levels = c("Significant.Positive", "Significant.Negative", "Non-Significant")
    )
  )

# Save and print
significant_results_late <- late_did_output %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

write.csv(significant_results_late, "significant_results_late.csv", row.names = FALSE)
print(significant_results_late, n = Inf)



##############################################################################################################


library(ggplot2)

plot_data <- late_did_output %>%
  filter(Disease %in% selected_diseases) %>%
  mutate(
    Disease = factor(Disease, levels = rev(selected_diseases)),
    Measure = ifelse(Measure == "DALYs (Disability-Adjusted Life Years)", "DALYs", Measure)
  )

ggplot(plot_data, aes(x = estimate, y = Disease, color = Effect_Type, shape = Measure)) +
  geom_point(size = 3, position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(xmin = estimate - std.error, xmax = estimate + std.error),
                width = 0.3, position = position_dodge(width = 0.7)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c(
    "Significant.Positive" = "gold",
    "Significant.Negative" = "steelblue",
    "Non-Significant" = "darkgray"
  )) +
  labs(
    title = "DiD Effects (Late vs Never, State FE)",
    x = "Treatment Effect (Estimate)", y = "Disease"
  ) +
  theme_minimal()

##############################################################################################################

library(patchwork)

plot_by_measure <- function(m) {
  df <- late_did_output %>%
    filter(Measure == m, Disease %in% selected_diseases) %>%
    mutate(Disease = factor(Disease, levels = rev(selected_diseases)))
  
  ggplot(df, aes(x = estimate, y = Disease, color = Effect_Type)) +
    geom_point(size = 3) +
    geom_errorbar(aes(xmin = estimate - std.error, xmax = estimate + std.error), width = 0.3) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c(
      "Significant.Positive" = "gold",
      "Significant.Negative" = "steelblue",
      "Non-Significant" = "darkgray"
    )) +
    labs(title = m, x = "Treatment Effect", y = "Disease") +
    theme_minimal()
}

plot_by_measure("Deaths") + plot_by_measure("DALYs") +
  plot_by_measure("Prevalence") + plot_by_measure("Incidence")





