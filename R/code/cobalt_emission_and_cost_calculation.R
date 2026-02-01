# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(here)
library(writexl)

# Define the base path to the "data" folder
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

# Load raw_data.xlsx
baseline_df <- load_excel_file(raw_data_path)
if (is.null(baseline_df)) stop("Cannot proceed without raw_data.xlsx")

# Load mineral_data.xlsx
baseline_co_df <- load_excel_file(mineral_data_path, sheet = "Co")
if (is.null(baseline_co_df)) stop("Cannot proceed without mineral_data.xlsx Co sheet")

# Split the "Country" column and handle single-country rows
baseline_df <- baseline_df %>%
  separate(Country, into = c("production_country", "refining_country", "manufacturing_country"), 
           sep = "_", fill = "right", extra = "drop") %>%
  mutate(across(c(production_country, refining_country, manufacturing_country), 
                ~ ifelse(. == "", NA, .))) %>%
  mutate(manufacturing_country = case_when(
    is.na(refining_country) & is.na(manufacturing_country) ~ production_country,
    TRUE ~ manufacturing_country
  ),
  refining_country = case_when(
    is.na(refining_country) & !is.na(manufacturing_country) ~ manufacturing_country, 
    TRUE ~ refining_country
  ),
  production_country = case_when(
    is.na(production_country) & !is.na(refining_country) ~ refining_country,
    TRUE ~ production_country
  ))

# Replace country abbreviations
baseline_df <- baseline_df %>%
  mutate(across(c(production_country, refining_country, manufacturing_country), 
                ~ recode(., 
                         "SK" = "South Korea", 
                         "UK" = "United Kingdom", 
                         "SouthAfrica" = "South Africa", 
                         "USA" = "United States")))

# Filter for Cobalt
baseline_co_raw_df <- baseline_df %>%
  filter(Mineral == "Co")

# Merge with transport data (left join)
baseline_co_complete_df <- left_join(baseline_co_raw_df, baseline_co_df, by = "Chain")

# Ensure numeric columns and fill missing values with 0
baseline_co_complete_df <- baseline_co_complete_df %>%
  mutate(Cathode = as.numeric(Cathode),
         Mining = as.numeric(Mining),
         Refining = as.numeric(Refining),
         distance_1 = as.numeric(distance_1),
         distance_2 = as.numeric(distance_2),
         sea_distance_km = as.numeric(sea_distance_km)) %>%
  mutate(Cathode = replace(Cathode, is.na(Cathode), 0),
         Mining = replace(Mining, is.na(Mining), 0),
         Refining = replace(Refining, is.na(Refining), 0),
         distance_1 = replace(distance_1, is.na(distance_1), 0),
         distance_2 = replace(distance_2, is.na(distance_2), 0),
         sea_distance_km = replace(sea_distance_km, is.na(sea_distance_km), 0))

# Calculate total cathode weight for cost
total_cathode_weight <- sum(baseline_co_complete_df$Cathode, na.rm = TRUE)
cat("Total Cathode Weight:", total_cathode_weight, "\n")

# Compute weighted percentage for cost (based on Cathode)
baseline_co_complete_df <- baseline_co_complete_df %>%
  mutate(`weighted %` = Cathode / total_cathode_weight)

# Define cost per kg - only for specific manufacturing countries
cost_per_kg_cathode <- tibble(
  country = c("China", "Japan", "South Korea"),
  cost_kg_cathode = c(3058950 / 95506, 3004630 / 53354, 84650 / 931)
)

# Calculate cost per kg of NMC811
cathode_per_kg_nmc811 <- 0.37
cost_per_kg_nmc811 <- cost_per_kg_cathode %>%
  mutate(cost_kg_nmc811 = cost_kg_cathode * cathode_per_kg_nmc811)

# Add cost per kg of NMC811 - only defined for the three manufacturing countries
baseline_co_complete_df <- baseline_co_complete_df %>%
  left_join(cost_per_kg_nmc811 %>% select(country, cost_kg_nmc811), 
            by = c("manufacturing_country" = "country")) %>%
  # Keep NA values as 0 to match original approach
  mutate(cost_kg_nmc811 = replace(cost_kg_nmc811, is.na(cost_kg_nmc811), 0))

# Define total battery weight
total_battery_weight_kg <- 400
total_cathode_weight_tons <- (total_battery_weight_kg * cathode_per_kg_nmc811) / 1000  # 0.148 tons

# Calculate Contribution (tons) for cost
baseline_co_complete_df <- baseline_co_complete_df %>%
  mutate(`Contribution (tons)` = total_cathode_weight_tons * `weighted %`)

# Calculate cost
baseline_co_complete_df <- baseline_co_complete_df %>%
  mutate(`Cost (USD)` = (`Contribution (tons)` * 1000 / cathode_per_kg_nmc811) * cost_kg_nmc811)

# Calculate total mining and refining weights for emissions
total_mining_weight <- sum(baseline_co_complete_df$Mining, na.rm = TRUE)
total_refining_weight <- sum(baseline_co_complete_df$Refining, na.rm = TRUE)
cat("Total Mining Weight:", total_mining_weight, "\n")
cat("Total Refining Weight:", total_refining_weight, "\n")

# Handle NA flow stages by setting defaults based on available data
baseline_co_complete_df <- baseline_co_complete_df %>%
  mutate(
    flow_stage_from = case_when(
      is.na(flow_stage_from) & Mining > 0 ~ "mining",
      is.na(flow_stage_from) & Refining > 0 ~ "refining",
      TRUE ~ flow_stage_from
    ),
    flow_stage_to = case_when(
      is.na(flow_stage_to) & Refining > 0 ~ "refining",
      is.na(flow_stage_to) & Cathode > 0 ~ "manufacturing",
      TRUE ~ flow_stage_to
    )
  )

# Compute weighted contribution for emissions based on flow stage
# FIX: Correct the denominator in the second case to use total_refining_weight
baseline_co_complete_df <- baseline_co_complete_df %>%
  mutate(
    `Emission Contribution (tons)` = case_when(
      flow_stage_from == "mining" & flow_stage_to == "refining" & Mining > 0 ~ 
        (Mining / total_mining_weight) * total_cathode_weight_tons,
      
      # FIXED: Changed to use total_refining_weight for more logical consistency
      flow_stage_from == "mining" & flow_stage_to == "refining" & Mining == 0 & Refining > 0 ~ 
        (Refining / total_refining_weight) * total_cathode_weight_tons,
      
      flow_stage_from == "refining" & flow_stage_to == "manufacturing" & Refining > 0 ~ 
        (Refining / total_refining_weight) * total_cathode_weight_tons,
      
      # FIXED: Added case for refining->manufacturing with Mining > 0 but Refining = 0
      # This fixes the Chain 73 issue
      flow_stage_from == "refining" & flow_stage_to == "manufacturing" & Mining > 0 & Refining == 0 ~
        (Mining / total_mining_weight) * total_cathode_weight_tons,
      
      # Add a default case for chains that have neither mining nor refining data
      Cathode > 0 & Mining == 0 & Refining == 0 ~ 
        (Cathode / total_cathode_weight) * total_cathode_weight_tons,
      
      TRUE ~ 0  # Default case
    )
  )

# Define emission factors
emission_factors <- list(
  ipcc = list(sea = 0.047, land = 0.1005, rail = 0.0726),
  ecoinvent = list(sea = 0.0105, land = 0.154, rail = 0.0615),
  greet = list(sea = 0.025, land = 0.105, rail = 0.065)
)

# Initialize all emission columns to 0
baseline_co_complete_df <- baseline_co_complete_df %>%
  mutate(
    emission_ecoinvent_1 = 0, emission_ecoinvent_2 = 0,
    emission_greet_1 = 0, emission_greet_2 = 0,
    emission_ipcc_1 = 0, emission_ipcc_2 = 0,
    sea_emission_ecoinvent = 0, sea_emission_greet = 0, sea_emission_ipcc = 0,
    total_emission_ecoinvent = 0, total_emission_greet = 0, total_emission_ipcc = 0
  )

# Clean up transport types to handle whitespace and case sensitivity
baseline_co_complete_df <- baseline_co_complete_df %>%
  mutate(
    transport_type_1 = tolower(trimws(transport_type_1)),
    transport_type_2 = tolower(trimws(transport_type_2))
  )

# Calculate emissions for each source
for (source in c("ecoinvent", "greet", "ipcc")) {
  baseline_co_complete_df <- baseline_co_complete_df %>%
    mutate(
      # Distance 1 emissions
      !!paste0("emission_", source, "_1") := case_when(
        transport_type_1 == "truck" ~ distance_1 * emission_factors[[source]]$land * `Emission Contribution (tons)`,
        transport_type_1 == "rail" ~ distance_1 * emission_factors[[source]]$rail * `Emission Contribution (tons)`,
        TRUE ~ 0
      ),
      # Distance 2 emissions
      !!paste0("emission_", source, "_2") := case_when(
        transport_type_2 == "truck" ~ distance_2 * emission_factors[[source]]$land * `Emission Contribution (tons)`,
        transport_type_2 == "rail" ~ distance_2 * emission_factors[[source]]$rail * `Emission Contribution (tons)`,
        TRUE ~ 0
      ),
      # Sea emissions
      !!paste0("sea_emission_", source) := sea_distance_km * emission_factors[[source]]$sea * `Emission Contribution (tons)`,
      # Total emissions
      !!paste0("total_emission_", source) := get(paste0("emission_", source, "_1")) +
        get(paste0("emission_", source, "_2")) +
        get(paste0("sea_emission_", source))
    )
}

# Additional data validation check - ensure total contribution (tons) sums close to expected value
total_cost_contribution <- sum(baseline_co_complete_df$`Contribution (tons)`, na.rm = TRUE)
total_emission_contribution <- sum(baseline_co_complete_df$`Emission Contribution (tons)`, na.rm = TRUE)

cat("Total Cost Contribution (tons):", total_cost_contribution, "\n")
cat("Total Emission Contribution (tons):", total_emission_contribution, "\n")
cat("Expected Total (tons):", total_cathode_weight_tons, "\n")

if (abs(total_cost_contribution - total_cathode_weight_tons) > 0.001) {
  warning("Cost contribution does not sum close to total cathode weight!")
}

if (abs(total_emission_contribution - total_cathode_weight_tons) > 0.001) {
  warning("Emission contribution does not sum close to total cathode weight!")
}

# Calculate total cost and total emissions
total_cost_usd <- sum(baseline_co_complete_df$`Cost (USD)`, na.rm = TRUE)
total_emission_ecoinvent <- sum(baseline_co_complete_df$total_emission_ecoinvent, na.rm = TRUE)
total_emission_greet <- sum(baseline_co_complete_df$total_emission_greet, na.rm = TRUE)
total_emission_ipcc <- sum(baseline_co_complete_df$total_emission_ipcc, na.rm = TRUE)

cat("Total Cost (USD) for 400 kg NMC811 battery:", total_cost_usd, "\n")
cat("Total Emission (kg CO2e) - ecoinvent:", total_emission_ecoinvent, "\n")
cat("Total Emission (kg CO2e) - greet:", total_emission_greet, "\n")
cat("Total Emission (kg CO2e) - ipcc:", total_emission_ipcc, "\n")

# Save results
output_file <- file.path(base_path, "cobalt_emissions_and_cost_output.xlsx")
write_xlsx(baseline_co_complete_df, output_file)
cat("Results saved to:", output_file, "\n")