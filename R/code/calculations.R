# Load required libraries
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(tibble)
library(here)  

# Define the base path to the "data" folder using here::here()
base_path <- here("data")

# Define file names
raw_data <- "raw_data.xlsx"
mineral_data <- "mineral_data.xlsx"

# Define file paths
raw_data_path <- file.path(base_path, raw_data)
mineral_data_path <- file.path(base_path, mineral_data)

# Function to check if a file exists and load it
load_excel_file <- function(file_path, sheet = NULL) {
  if (file.exists(file_path)) {
    if (is.null(sheet)) {
      data <- read_excel(file_path)
    } else {
      data <- read_excel(file_path, sheet = sheet)
    }
    cat("File loaded successfully:", file_path, "\n")
    return(data)
  } else {
    cat("File not found at:", file_path, "\n")
    return(NULL)
  }
}

# Load raw_data.xlsx (contains only Cobalt data)
baseline_df <- load_excel_file(raw_data_path)
if (is.null(baseline_df)) stop("Cannot proceed without raw_data.xlsx")

# Load mineral_data.xlsx (only the "Co" sheet)
baseline_co_df <- load_excel_file(mineral_data_path, sheet = "Co")
if (is.null(baseline_co_df)) stop("Cannot proceed without mineral_data.xlsx Co sheet")

# Split the "Country" column into production, refining, and manufacturing countries
# Note: separate() automatically drops the original "Country" column by default
baseline_df <- baseline_df %>%
  separate(Country, into = c("production_country", "refining_country", "manufacturing_country"), 
           sep = "_", fill = "right", extra = "drop") %>%
  mutate(across(c(production_country, refining_country, manufacturing_country), 
                ~ ifelse(. == "", NA, .)))

# Replace country abbreviations with full names
baseline_df <- baseline_df %>%
  mutate(production_country = recode(production_country, 
                                     "SK" = "South Korea", 
                                     "UK" = "United Kingdom", 
                                     "SouthAfrica" = "South Africa", 
                                     "USA" = "United States"),
         refining_country = recode(refining_country, 
                                   "SK" = "South Korea", 
                                   "UK" = "United Kingdom", 
                                   "SouthAfrica" = "South Africa", 
                                   "USA" = "United States"),
         manufacturing_country = recode(manufacturing_country, 
                                        "SK" = "South Korea", 
                                        "UK" = "United Kingdom", 
                                        "SouthAfrica" = "South Africa", 
                                        "USA" = "United States"))

# Filter the data for Cobalt (this step is technically unnecessary if raw_data.xlsx contains only Co, but kept for robustness)
baseline_co_raw_df <- baseline_df %>%
  filter(Mineral == "Co")

# Reorder the columns to prioritize country columns
column_order <- c("production_country", "refining_country", "manufacturing_country", 
                  setdiff(names(baseline_df), c("production_country", "refining_country", "manufacturing_country")))
baseline_df <- baseline_df %>%
  select(all_of(column_order))

# Define emission factors based on different sources
emission_factors <- list(
  ipcc = list(sea = 0.047, land = 0.1005, rail = 0.0726),
  ecoinvent = list(sea = 0.0105, land = 0.154, rail = 0.0615),
  greet = list(sea = 0.025, land = 0.105, rail = 0.065)
)

# Merge the Cobalt data from mineral_data.xlsx with the filtered raw_data.xlsx
baseline_co_complete_df <- inner_join(baseline_co_df, baseline_co_raw_df, by = "Chain")

# Print column names after the join to verify the columns
cat("Column names after inner_join():\n")
print(names(baseline_co_complete_df))

# Fill missing values in specific columns with 0
baseline_co_complete_df <- baseline_co_complete_df %>%
  mutate(Cathode = replace(Cathode, is.na(Cathode), 0),
         distance_1 = replace(distance_1, is.na(distance_1), 0),
         distance_2 = replace(distance_2, is.na(distance_2), 0),
         sea_distance_km = replace(sea_distance_km, is.na(sea_distance_km), 0))

# Calculate the total weight of Cobalt in each supply chain (Mining + Refining + Cathode)
baseline_co_complete_df <- baseline_co_complete_df %>%
  mutate(weight_mineral_loc = Mining + Refining + Cathode)

# Calculate the total weight of Cobalt mined across all supply chains
baseline_co_total_weight <- sum(baseline_co_complete_df$Mining, na.rm = TRUE)

# Compute the weighted percentage for Cobalt (weight of Cobalt in this supply chain / total Cobalt weight)
baseline_co_complete_df <- baseline_co_complete_df %>%
  mutate(`weighted %` = weight_mineral_loc / baseline_co_total_weight)

# Define the contribution per ton for Cobalt
co_contribution_per_ton <- 0.0000128

# Calculate the contribution of each supply chain to the final product (in tons)
baseline_co_complete_df <- baseline_co_complete_df %>%
  mutate(`Contribution (tons)` = `weighted %` * co_contribution_per_ton)

# Define transport types and emission sources
transport_modes <- c("truck", "rail")
emission_sources <- c("ecoinvent", "greet", "ipcc")

# Function to calculate emissions for a given transport mode and emission source
calculate_emissions <- function(df, mode, source, distance_col, emission_col) {
  transport_factor <- if (mode == "truck") emission_factors[[source]]$land else emission_factors[[source]]$rail
  df <- df %>%
    mutate(!!emission_col := ifelse(get(paste0("transport_type_", sub("distance_", "", distance_col))) == mode,
                                    get(distance_col) * transport_factor * `Contribution (tons)`,
                                    0))
  return(df)
}

# Loop through each emission source and transport mode to calculate emissions for land/rail transport
for (source in emission_sources) {
  for (mode in transport_modes) {
    # Compute emissions for transport_type_1
    baseline_co_complete_df <- calculate_emissions(baseline_co_complete_df, mode, source, 
                                                   "distance_1", paste0("emission_", source, "_1"))
    
    # Compute emissions for transport_type_2
    baseline_co_complete_df <- calculate_emissions(baseline_co_complete_df, mode, source, 
                                                   "distance_2", paste0("emission_", source, "_2"))
  }
}

# Calculate emissions for sea transport for each emission source
for (source in emission_sources) {
  baseline_co_complete_df <- baseline_co_complete_df %>%
    mutate(!!paste0("sea_emission_", source) := sea_distance_km * emission_factors[[source]]$sea * `Contribution (tons)`)
}

# Loop through each emission source to calculate the total emissions
for (source in emission_sources) {
  baseline_co_complete_df <- baseline_co_complete_df %>%
    mutate(!!paste0("total_emission_", source) := get(paste0("emission_", source, "_1")) +
             get(paste0("emission_", source, "_2")) +
             get(paste0("sea_emission_", source)))
}

# Print the first few rows to verify the results
print(head(baseline_co_complete_df))

# Save the results to an Excel file in the "data" folder
output_file <- file.path(base_path, "cobalt_emissions_output.xlsx")
write_xlsx(baseline_co_complete_df, output_file)
cat("Results saved to:", output_file, "\n")