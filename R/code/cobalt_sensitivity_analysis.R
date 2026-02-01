# Cobalt Emissions and Cost Sensitivity Analysis (Standalone)
# This script performs sensitivity analysis on the cobalt emissions and cost calculations
# Adapted to work with your specific output file structure

# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(here)
library(writexl)
library(ggplot2)

# Check if corrplot and reshape2 are installed, if not, use base R alternatives
if (!requireNamespace("corrplot", quietly = TRUE)) {
  message("Package 'corrplot' not available, using base R plotting instead")
  use_corrplot <- FALSE
} else {
  library(corrplot)
  use_corrplot <- TRUE
}

if (!requireNamespace("reshape2", quietly = TRUE)) {
  message("Package 'reshape2' not available, using tidyr instead")
  use_reshape <- FALSE
} else {
  library(reshape2)
  use_reshape <- TRUE
}

# Set seed for reproducibility
set.seed(42)

# Define the base path to the "data" folder
base_path <- here("data")

# Define file names
output_data <- "cobalt_emissions_and_cost_output.xlsx"  # Your existing output file

# Define file paths
output_data_path <- file.path(base_path, output_data)

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

# Load the output file
output_df <- load_excel_file(output_data_path)
if (is.null(output_df)) stop("Cannot proceed without the output file.")

# Extract the mining, refining, and cathode totals from the existing data
total_mining_weight <- sum(output_df$Mining, na.rm = TRUE)
total_refining_weight <- sum(output_df$Refining, na.rm = TRUE)
total_cathode_weight <- sum(output_df$Cathode, na.rm = TRUE)

cat("Total Mining Weight:", total_mining_weight, "\n")
cat("Total Refining Weight:", total_refining_weight, "\n")
cat("Total Cathode Weight:", total_cathode_weight, "\n")

# Define emission factors (same as in the main code)
emission_factors <- list(
  ipcc = list(sea = 0.047, land = 0.1005, rail = 0.0726),
  ecoinvent = list(sea = 0.0105, land = 0.154, rail = 0.0615),
  greet = list(sea = 0.025, land = 0.105, rail = 0.065)
)

# Define constants for the model
cathode_per_kg_nmc811 <- 0.37
total_battery_weight_kg <- 400
total_cathode_weight_tons <- (total_battery_weight_kg * cathode_per_kg_nmc811) / 1000  # 0.148 tons

# Define cost per kg cathode
cost_per_kg_cathode <- tibble(
  country = c("China", "Japan", "South Korea"),
  cost_kg_cathode = c(3058950 / 95506, 3004630 / 53354, 84650 / 931)
)

# ------------------------------------------
# Monte Carlo Sensitivity Analysis
# ------------------------------------------

# Define number of simulations
num_simulations <- 10000

# Create a dataframe to store results
sensitivity_results <- data.frame(
  simulation = 1:num_simulations,
  sea_distance_factor = runif(num_simulations, 0.5, 1.5),
  land_distance_factor = runif(num_simulations, 0.5, 1.5),
  rail_distance_factor = runif(num_simulations, 0.5, 1.5),
  sea_emission_factor = runif(num_simulations, 0.8, 1.2),
  land_emission_factor = runif(num_simulations, 0.8, 1.2),
  rail_emission_factor = runif(num_simulations, 0.8, 1.2),
  battery_weight_factor = runif(num_simulations, 0.8, 1.2),
  cathode_percentage_factor = runif(num_simulations, 0.8, 1.2),
  china_cost_factor = runif(num_simulations, 0.8, 1.2),
  japan_cost_factor = runif(num_simulations, 0.8, 1.2),
  sk_cost_factor = runif(num_simulations, 0.8, 1.2),
  emission_source = sample(c("ipcc", "ecoinvent", "greet"), num_simulations, replace = TRUE),
  total_emissions_ecoinvent = numeric(num_simulations),
  total_emissions_greet = numeric(num_simulations),
  total_emissions_ipcc = numeric(num_simulations),
  total_cost = numeric(num_simulations)
)

# Ensure the output dataframe has the columns we need
# Based on analysis, we know:
# - transport_type_1, transport_type_2, and flow_stage_from are missing

cat("Starting Monte Carlo simulation with", num_simulations, "iterations...\n")
for (i in 1:num_simulations) {
  if (i %% 1000 == 0) cat("Processing simulation", i, "of", num_simulations, "\n")
  
  # Start with the original data for each simulation
  sim_df <- output_df
  
  # Adjust distances
  sim_df <- sim_df %>%
    mutate(
      sea_distance_km = sea_distance_km * sensitivity_results$sea_distance_factor[i],
      distance_1 = distance_1 * sensitivity_results$land_distance_factor[i],
      distance_2 = distance_2 * sensitivity_results$rail_distance_factor[i]
    )
  
  # Adjust emission factors for this simulation
  sim_emission_factors <- list(
    ipcc = list(
      sea = emission_factors$ipcc$sea * sensitivity_results$sea_emission_factor[i],
      land = emission_factors$ipcc$land * sensitivity_results$land_emission_factor[i],
      rail = emission_factors$ipcc$rail * sensitivity_results$rail_emission_factor[i]
    ),
    ecoinvent = list(
      sea = emission_factors$ecoinvent$sea * sensitivity_results$sea_emission_factor[i],
      land = emission_factors$ecoinvent$land * sensitivity_results$land_emission_factor[i],
      rail = emission_factors$ecoinvent$rail * sensitivity_results$rail_emission_factor[i]
    ),
    greet = list(
      sea = emission_factors$greet$sea * sensitivity_results$sea_emission_factor[i],
      land = emission_factors$greet$land * sensitivity_results$land_emission_factor[i],
      rail = emission_factors$greet$rail * sensitivity_results$rail_emission_factor[i]
    )
  )
  
  # Adjust cost factors
  sim_cost_per_kg_cathode <- cost_per_kg_cathode %>%
    mutate(cost_kg_cathode = case_when(
      country == "China" ~ cost_kg_cathode[1] * sensitivity_results$china_cost_factor[i],
      country == "Japan" ~ cost_kg_cathode[2] * sensitivity_results$japan_cost_factor[i],
      country == "South Korea" ~ cost_kg_cathode[3] * sensitivity_results$sk_cost_factor[i]
    ))
  
  sim_cost_per_kg_nmc811 <- sim_cost_per_kg_cathode %>%
    mutate(cost_kg_nmc811 = cost_kg_cathode * (cathode_per_kg_nmc811 * sensitivity_results$cathode_percentage_factor[i]))
  
  # Adjust battery weight and cathode percentage
  sim_total_battery_weight_kg <- total_battery_weight_kg * sensitivity_results$battery_weight_factor[i]
  sim_cathode_per_kg_nmc811 <- cathode_per_kg_nmc811 * sensitivity_results$cathode_percentage_factor[i]
  sim_total_cathode_weight_tons <- (sim_total_battery_weight_kg * sim_cathode_per_kg_nmc811) / 1000
  
  # Recalculate cost
  # Create a temporary dataframe with the cost mapping
  cost_mapping <- sim_cost_per_kg_nmc811 %>% 
    select(country, cost_kg_nmc811) %>%
    rename(manufacturing_country = country, sim_cost_kg_nmc811 = cost_kg_nmc811)
  
  sim_df <- sim_df %>%
    left_join(cost_mapping, by = "manufacturing_country") %>%
    mutate(sim_cost_kg_nmc811 = ifelse(is.na(sim_cost_kg_nmc811), 0, sim_cost_kg_nmc811)) %>%
    mutate(new_contribution_tons = sim_total_cathode_weight_tons * `weighted %`) %>%
    mutate(new_cost_usd = (new_contribution_tons * 1000 / sim_cathode_per_kg_nmc811) * sim_cost_kg_nmc811)
  
  # Recalculate emission contribution based on adjusted weight
  # Infer flow_stage_from based on flow_stage_to and mining/refining values if needed
  sim_df <- sim_df %>%
    mutate(
      inferred_flow_stage_from = case_when(
        flow_stage_to == "refining" ~ "mining",
        flow_stage_to == "manufacturing" ~ "refining",
        TRUE ~ NA_character_
      ),
      new_emission_contribution = case_when(
        # Mining to refining with Mining > 0
        inferred_flow_stage_from == "mining" & flow_stage_to == "refining" & Mining > 0 ~ 
          (Mining / total_mining_weight) * sim_total_cathode_weight_tons,
        
        # Mining to refining with Mining = 0, Refining > 0
        inferred_flow_stage_from == "mining" & flow_stage_to == "refining" & Mining == 0 & Refining > 0 ~ 
          (Refining / total_mining_weight) * sim_total_cathode_weight_tons,
        
        # Refining to manufacturing with Refining > 0
        inferred_flow_stage_from == "refining" & flow_stage_to == "manufacturing" & Refining > 0 ~ 
          (Refining / total_refining_weight) * sim_total_cathode_weight_tons,
        
        # Refining to manufacturing with Mining > 0, Refining = 0
        inferred_flow_stage_from == "refining" & flow_stage_to == "manufacturing" & Mining > 0 & Refining == 0 ~
          (Mining / total_mining_weight) * sim_total_cathode_weight_tons,
        
        # Cathode-only chains
        Cathode > 0 & Mining == 0 & Refining == 0 ~ 
          (Cathode / total_cathode_weight) * sim_total_cathode_weight_tons,
        
        TRUE ~ 0
      )
    )
  
  # Infer transport types from emission columns if needed
  sim_df <- sim_df %>%
    mutate(
      inferred_transport_type_1 = ifelse(emission_ecoinvent_1 > 0, 
                                         ifelse(emission_ecoinvent_1 / (distance_1 * `Emission Contribution (tons)`) > 0.1, 
                                                "truck", "rail"), 
                                         NA_character_),
      inferred_transport_type_2 = ifelse(emission_ecoinvent_2 > 0, 
                                         ifelse(emission_ecoinvent_2 / (distance_2 * `Emission Contribution (tons)`) > 0.1, 
                                                "truck", "rail"), 
                                         NA_character_)
    )
  
  # Calculate new emissions based on adjusted factors
  sim_df <- sim_df %>%
    mutate(
      # Reset emission columns
      new_emission_ecoinvent_1 = 0,
      new_emission_ecoinvent_2 = 0,
      new_emission_greet_1 = 0,
      new_emission_greet_2 = 0,
      new_emission_ipcc_1 = 0,
      new_emission_ipcc_2 = 0,
      new_sea_emission_ecoinvent = 0,
      new_sea_emission_greet = 0,
      new_sea_emission_ipcc = 0,
      
      # Calculate new values if we have distances and inferred transport types
      new_emission_ecoinvent_1 = case_when(
        !is.na(inferred_transport_type_1) & inferred_transport_type_1 == "truck" & distance_1 > 0 ~ 
          distance_1 * sim_emission_factors$ecoinvent$land * new_emission_contribution,
        !is.na(inferred_transport_type_1) & inferred_transport_type_1 == "rail" & distance_1 > 0 ~ 
          distance_1 * sim_emission_factors$ecoinvent$rail * new_emission_contribution,
        TRUE ~ 0
      ),
      
      new_emission_ecoinvent_2 = case_when(
        !is.na(inferred_transport_type_2) & inferred_transport_type_2 == "truck" & distance_2 > 0 ~ 
          distance_2 * sim_emission_factors$ecoinvent$land * new_emission_contribution,
        !is.na(inferred_transport_type_2) & inferred_transport_type_2 == "rail" & distance_2 > 0 ~ 
          distance_2 * sim_emission_factors$ecoinvent$rail * new_emission_contribution,
        TRUE ~ 0
      ),
      
      new_emission_greet_1 = case_when(
        !is.na(inferred_transport_type_1) & inferred_transport_type_1 == "truck" & distance_1 > 0 ~ 
          distance_1 * sim_emission_factors$greet$land * new_emission_contribution,
        !is.na(inferred_transport_type_1) & inferred_transport_type_1 == "rail" & distance_1 > 0 ~ 
          distance_1 * sim_emission_factors$greet$rail * new_emission_contribution,
        TRUE ~ 0
      ),
      
      new_emission_greet_2 = case_when(
        !is.na(inferred_transport_type_2) & inferred_transport_type_2 == "truck" & distance_2 > 0 ~ 
          distance_2 * sim_emission_factors$greet$land * new_emission_contribution,
        !is.na(inferred_transport_type_2) & inferred_transport_type_2 == "rail" & distance_2 > 0 ~ 
          distance_2 * sim_emission_factors$greet$rail * new_emission_contribution,
        TRUE ~ 0
      ),
      
      new_emission_ipcc_1 = case_when(
        !is.na(inferred_transport_type_1) & inferred_transport_type_1 == "truck" & distance_1 > 0 ~ 
          distance_1 * sim_emission_factors$ipcc$land * new_emission_contribution,
        !is.na(inferred_transport_type_1) & inferred_transport_type_1 == "rail" & distance_1 > 0 ~ 
          distance_1 * sim_emission_factors$ipcc$rail * new_emission_contribution,
        TRUE ~ 0
      ),
      
      new_emission_ipcc_2 = case_when(
        !is.na(inferred_transport_type_2) & inferred_transport_type_2 == "truck" & distance_2 > 0 ~ 
          distance_2 * sim_emission_factors$ipcc$land * new_emission_contribution,
        !is.na(inferred_transport_type_2) & inferred_transport_type_2 == "rail" & distance_2 > 0 ~ 
          distance_2 * sim_emission_factors$ipcc$rail * new_emission_contribution,
        TRUE ~ 0
      ),
      
      # Sea emissions
      new_sea_emission_ecoinvent = ifelse(sea_distance_km > 0, 
                                          sea_distance_km * sim_emission_factors$ecoinvent$sea * new_emission_contribution,
                                          0),
      new_sea_emission_greet = ifelse(sea_distance_km > 0, 
                                      sea_distance_km * sim_emission_factors$greet$sea * new_emission_contribution,
                                      0),
      new_sea_emission_ipcc = ifelse(sea_distance_km > 0, 
                                     sea_distance_km * sim_emission_factors$ipcc$sea * new_emission_contribution,
                                     0),
      
      # Total emissions
      new_total_emission_ecoinvent = new_emission_ecoinvent_1 + new_emission_ecoinvent_2 + new_sea_emission_ecoinvent,
      new_total_emission_greet = new_emission_greet_1 + new_emission_greet_2 + new_sea_emission_greet,
      new_total_emission_ipcc = new_emission_ipcc_1 + new_emission_ipcc_2 + new_sea_emission_ipcc
    )
  
  # Store totals for this simulation
  sensitivity_results$total_emissions_ecoinvent[i] <- sum(sim_df$new_total_emission_ecoinvent, na.rm = TRUE)
  sensitivity_results$total_emissions_greet[i] <- sum(sim_df$new_total_emission_greet, na.rm = TRUE)
  sensitivity_results$total_emissions_ipcc[i] <- sum(sim_df$new_total_emission_ipcc, na.rm = TRUE)
  sensitivity_results$total_cost[i] <- sum(sim_df$new_cost_usd, na.rm = TRUE)
}

cat("Monte Carlo simulation completed.\n")

# ------------------------------------------
# Data Analysis and Visualization
# ------------------------------------------

# Calculate correlation between input factors and outputs
cat("Calculating correlations...\n")
sensitivity_corr <- sensitivity_results %>%
  select(-simulation, -emission_source)

# Calculate correlation matrix
correlation_matrix <- cor(sensitivity_corr, method = "pearson")

# Calculate correlation specifically with outcomes
emissions_correlations <- data.frame(
  variable = colnames(sensitivity_corr)[1:11],  # Just the input variables, not outcomes
  ecoinvent = sapply(1:11, function(i) cor(sensitivity_corr[,i], sensitivity_corr$total_emissions_ecoinvent)),
  greet = sapply(1:11, function(i) cor(sensitivity_corr[,i], sensitivity_corr$total_emissions_greet)),
  ipcc = sapply(1:11, function(i) cor(sensitivity_corr[,i], sensitivity_corr$total_emissions_ipcc)),
  cost = sapply(1:11, function(i) cor(sensitivity_corr[,i], sensitivity_corr$total_cost))
)

# ------------------------------------------
# Create Visualizations
# ------------------------------------------

cat("Creating visualizations...\n")

# Create correlation heatmap
if (use_corrplot) {
  png("cobalt_sensitivity_correlation_heatmap.png", width = 3000, height = 2400, res = 300)
  corrplot(correlation_matrix, method = "color", type = "upper", 
           tl.col = "black", tl.srt = 45, 
           addCoef.col = "black", number.cex = 0.7,
           title = "Cobalt Transport Emissions & Cost Sensitivity Analysis - Correlation Matrix")
  dev.off()
} else {
  # Alternative using base R
  png("cobalt_sensitivity_correlation_heatmap.png", width = 3000, height = 2400, res = 300)
  heatmap(correlation_matrix, main = "Cobalt Transport Emissions & Cost Sensitivity Analysis - Correlation Matrix",
          col = colorRampPalette(c("blue", "white", "red"))(100))
  dev.off()
}

# Reshape the emissions correlations for plotting
emissions_correlations_long <- emissions_correlations %>%
  pivot_longer(cols = c(ecoinvent, greet, ipcc, cost), 
               names_to = "emission_source", 
               values_to = "correlation") %>%
  mutate(abs_correlation = abs(correlation))

# Create bar plot of variable impact on emissions
png("cobalt_sensitivity_variable_impact.png", width = 3600, height = 2400, res = 300)
ggplot(emissions_correlations_long, aes(x = reorder(variable, abs_correlation), 
                                        y = correlation, 
                                        fill = emission_source)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(x = "Variables", 
       y = "Pearson Correlation with Output", 
       title = "Cobalt Transport Sensitivity Analysis - Variable Impact",
       fill = "Output Metric") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1", 
                    labels = c("Cost (USD)", "Ecoinvent Emissions", "GREET Emissions", "IPCC Emissions"))
dev.off()

# Create scatter plots for the most influential variables
# For emissions
top_vars_emissions <- emissions_correlations_long %>%
  filter(emission_source == "ecoinvent") %>%
  arrange(desc(abs_correlation)) %>%
  head(3) %>%
  pull(variable)

# For cost
top_vars_cost <- emissions_correlations_long %>%
  filter(emission_source == "cost") %>%
  arrange(desc(abs_correlation)) %>%
  head(3) %>%
  pull(variable)

# Scatter plots for emissions
emissions_scatter_data <- sensitivity_results %>%
  select(all_of(top_vars_emissions), total_emissions_ecoinvent) %>%
  pivot_longer(cols = all_of(top_vars_emissions), 
               names_to = "variable", 
               values_to = "value")

png("cobalt_top_emissions_factors.png", width = 3000, height = 1800, res = 300)
ggplot(emissions_scatter_data, aes(x = value, y = total_emissions_ecoinvent)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = "Factor Value", 
       y = "Total Emissions (kg CO2e)", 
       title = "Top Factors Affecting Cobalt Transport Emissions (Ecoinvent)") +
  theme_minimal()
dev.off()

# Scatter plots for cost
cost_scatter_data <- sensitivity_results %>%
  select(all_of(top_vars_cost), total_cost) %>%
  pivot_longer(cols = all_of(top_vars_cost), 
               names_to = "variable", 
               values_to = "value")

png("cobalt_top_cost_factors.png", width = 3000, height = 1800, res = 300)
ggplot(cost_scatter_data, aes(x = value, y = total_cost)) +
  geom_point(alpha = 0.3, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = "Factor Value", 
       y = "Total Cost (USD)", 
       title = "Top Factors Affecting Cobalt Transport Cost") +
  theme_minimal()
dev.off()

# Density plots
# Reshape emissions data for density plot
emissions_data <- rbind(
  data.frame(emissions = sensitivity_results$total_emissions_ecoinvent, source = "Ecoinvent"),
  data.frame(emissions = sensitivity_results$total_emissions_greet, source = "GREET"),
  data.frame(emissions = sensitivity_results$total_emissions_ipcc, source = "IPCC")
)

# Emissions distribution
png("cobalt_emissions_distribution.png", width = 3000, height = 1800, res = 300)
ggplot(emissions_data, aes(x = emissions, fill = source)) +
  geom_density(alpha = 0.5) +
  labs(x = "Total Emissions (kg CO2e)", 
       y = "Density", 
       title = "Distribution of Cobalt Transport Emissions",
       fill = "Emission Source") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")
dev.off()

# Cost distribution
png("cobalt_cost_distribution.png", width = 3000, height = 1800, res = 300)
ggplot(sensitivity_results, aes(x = total_cost)) +
  geom_density(fill = "darkgreen", alpha = 0.5) +
  labs(x = "Total Cost (USD)", 
       y = "Density", 
       title = "Distribution of Cobalt Transport Cost") +
  theme_minimal()
dev.off()

# Save the sensitivity analysis results
write_xlsx(sensitivity_results, file.path(base_path, "cobalt_sensitivity_results.xlsx"))
write_xlsx(emissions_correlations_long, file.path(base_path, "cobalt_sensitivity_correlations.xlsx"))

# Print summary of results
summary_results <- data.frame(
  Metric = c("Ecoinvent Emissions (kg CO2e)", "GREET Emissions (kg CO2e)", 
             "IPCC Emissions (kg CO2e)", "Total Cost (USD)"),
  Min = c(min(sensitivity_results$total_emissions_ecoinvent),
          min(sensitivity_results$total_emissions_greet),
          min(sensitivity_results$total_emissions_ipcc),
          min(sensitivity_results$total_cost)),
  Mean = c(mean(sensitivity_results$total_emissions_ecoinvent),
           mean(sensitivity_results$total_emissions_greet),
           mean(sensitivity_results$total_emissions_ipcc),
           mean(sensitivity_results$total_cost)),
  Max = c(max(sensitivity_results$total_emissions_ecoinvent),
          max(sensitivity_results$total_emissions_greet),
          max(sensitivity_results$total_emissions_ipcc),
          max(sensitivity_results$total_cost)),
  SD = c(sd(sensitivity_results$total_emissions_ecoinvent),
         sd(sensitivity_results$total_emissions_greet),
         sd(sensitivity_results$total_emissions_ipcc),
         sd(sensitivity_results$total_cost))
)

# Print summary table
cat("\nSummary of Sensitivity Analysis Results:\n")
print(summary_results)

# Print the top 3 most influential factors for each output metric
top_factors <- emissions_correlations_long %>%
  group_by(emission_source) %>%
  arrange(emission_source, desc(abs_correlation)) %>%
  slice_head(n = 3) %>%
  ungroup()

cat("\nTop 3 most influential factors for each metric:\n")
print(top_factors)

cat("\nSensitivity analysis completed. Results saved to the data folder.\n")