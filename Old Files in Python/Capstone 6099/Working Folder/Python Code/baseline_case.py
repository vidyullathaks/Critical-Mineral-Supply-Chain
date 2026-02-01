import os
import pandas as pd
import numpy as np


base_path = os.path.expanduser("/Users/saachi/Library/CloudStorage/Box-Box/EV project/data")
file_name = "raw_data.xlsx"
file_name2 = "mineral_data.xlsx"

file_path = os.path.join(base_path, file_name)
file_path2 = os.path.join(base_path, file_name2)


#Loading the excel file into the df
if os.path.exists(file_path):
    baseline_df = pd.read_excel(file_path)
    print("File loaded successfully")
else:
    print("File not found at:", file_path)


# Check if the file exists
if os.path.exists(file_path2):
    try:
        # Load each subsheet into its own DataFrame
        baseline_li_df = pd.read_excel(file_path2, sheet_name="Li")
        baseline_ni_df = pd.read_excel(file_path2, sheet_name="Ni")
        baseline_co_df = pd.read_excel(file_path2, sheet_name="Co")
        baseline_mn_df = pd.read_excel(file_path2, sheet_name="Mn")
        print("Sheets 'Li', 'Ni','Co' and 'Mn' loaded successfully.")

    except Exception as e:
        print(f"Error loading sheets: {e}")

else:
    print("File not found at:", file_path2)



# Splitting and assigning new columns to countries
baseline_df[["production_country", "refining_country", "manufacturing_country"]] = (
    baseline_df["Country"]
    .apply(lambda x: x.split("_") + [""] * 3)
    .apply(lambda x: x[:3])
    .apply(pd.Series)
)

# Replacing empty strings with NaN
baseline_df.replace("", np.nan, inplace=True)
baseline_df.replace("SK", "South Korea", inplace=True)
baseline_df.replace("UK", "United Kingdom", inplace=True)
baseline_df.replace("SouthAfrica", "South Africa", inplace = True)
baseline_df.replace("USA", "United States", inplace = True)

baseline_mn_raw_df = baseline_df[baseline_df["Mineral"] == "Mn"]
baseline_li_raw_df = baseline_df[baseline_df["Mineral"] == "Li"]
baseline_ni_raw_df = baseline_df[baseline_df["Mineral"] == "Ni"]
baseline_co_raw_df = baseline_df[baseline_df["Mineral"] == "Co"]

# Dropping "Country" column
baseline_df.drop(columns=["Country"], inplace=True)

# Reordering the columns
column_order = ["production_country", "refining_country", "manufacturing_country"] + [col for col in baseline_df.columns if col not in ["production_country", "refining_country", "manufacturing_country"]]
baseline_df = baseline_df[column_order]

# Define emission factors based on different sources
# Define emission factors for different sources (keep them constant, not adding to df)
emission_factors = {
    "ipcc": {"sea": 0.047, "land": 0.1005, "rail": 0.0726},
    "ecoinvent": {"sea": 0.0105, "land": 0.154, "rail": 0.0615},
    "greet": {"sea": 0.025, "land": 0.105, "rail": 0.065},
}


baseline_li_complete_df = baseline_li_df.merge(baseline_li_raw_df, on=["Chain"], how="inner")
baseline_mn_complete_df = baseline_mn_df.merge(baseline_mn_raw_df, on=["Chain"], how="inner")
baseline_co_complete_df = baseline_co_df.merge(baseline_co_raw_df, on=["Chain"], how="inner")
baseline_ni_complete_df = baseline_ni_df.merge(baseline_ni_raw_df, on=["Chain"], how="inner")

baseline_mineral_complete_df = pd.concat([baseline_li_complete_df, baseline_mn_complete_df, baseline_co_complete_df, baseline_ni_complete_df], ignore_index=True)
baseline_mineral_complete_df[["Cathode","distance_1", "distance_2", "sea_distance_km"]] = baseline_mineral_complete_df[["Cathode", "distance_1", "distance_2", "sea_distance_km"]].fillna(0)

baseline_mineral_complete_df["weight_mineral_loc"] = baseline_mineral_complete_df["Mining"] + baseline_mineral_complete_df["Refining"] + baseline_mineral_complete_df["Cathode"]

baseline_li_total_weight = baseline_li_complete_df["Mining"].sum()
baseline_ni_total_weight = baseline_ni_complete_df["Mining"].sum()
baseline_co_total_weight = baseline_co_complete_df["Mining"].sum()
baseline_mn_total_weight = baseline_mn_complete_df["Mining"].sum()

baseline_mineral_complete_df = baseline_mineral_complete_df.rename(columns={'Mineral_x': 'Mineral'})


# Compute the weighted percentage based on the Mineral column
baseline_mineral_complete_df.loc[baseline_mineral_complete_df["Mineral"] == 'Li', "weighted %"] = (
    baseline_mineral_complete_df['weight_mineral_loc'] / baseline_li_total_weight
)

baseline_mineral_complete_df.loc[baseline_mineral_complete_df["Mineral"] == 'Co', "weighted %"] = (
    baseline_mineral_complete_df['weight_mineral_loc'] / baseline_co_total_weight
)

baseline_mineral_complete_df.loc[baseline_mineral_complete_df["Mineral"] == 'Ni', "weighted %"] = (
    baseline_mineral_complete_df['weight_mineral_loc'] / baseline_ni_total_weight
)

baseline_mineral_complete_df.loc[baseline_mineral_complete_df["Mineral"] == 'Mn', "weighted %"] = (
    baseline_mineral_complete_df['weight_mineral_loc'] / baseline_mn_total_weight
)

baseline_mineral_complete_df["weighted %"] = baseline_mineral_complete_df["weighted %"]

li_contribution_per_ton = 0.00034
ni_contribution_per_ton = 0.00010833
mn_contribution_per_ton = 0.0000128
co_contribution_per_ton = 0.0000128

baseline_mineral_complete_df.loc[baseline_mineral_complete_df["Mineral"] == 'Li', "Contribution (tons)"] = (
    baseline_mineral_complete_df['weighted %'] * li_contribution_per_ton
)

baseline_mineral_complete_df.loc[baseline_mineral_complete_df["Mineral"] == 'Co', "Contribution (tons)"] = (
    baseline_mineral_complete_df['weighted %'] * co_contribution_per_ton
)

baseline_mineral_complete_df.loc[baseline_mineral_complete_df["Mineral"] == 'Ni', "Contribution (tons)"] = (
    baseline_mineral_complete_df['weighted %'] * ni_contribution_per_ton
)

baseline_mineral_complete_df.loc[baseline_mineral_complete_df["Mineral"] == 'Mn', "Contribution (tons)"] = (
    baseline_mineral_complete_df['weighted %']  * mn_contribution_per_ton
)


# Define transport types and emission sources
transport_modes = ["truck", "rail"]
emission_sources = ["ecoinvent", "greet", "ipcc"]

# Loop through each emission source
for source in emission_sources:
    # Loop through each transport type
    for mode in transport_modes:
        # Set emission factor dynamically based on transport type
        transport_factor = emission_factors[source]["land"] if mode == "truck" else emission_factors[source]["rail"]

        # Compute emissions for transport_type_1
        baseline_mineral_complete_df.loc[baseline_mineral_complete_df["transport_type_1"] == mode, f"emission_{source}_1"] = (
            baseline_mineral_complete_df['distance_1'] * transport_factor * baseline_mineral_complete_df['Contribution (tons)']
        )

        # Compute emissions for transport_type_2
        baseline_mineral_complete_df.loc[baseline_mineral_complete_df["transport_type_2"] == mode, f"emission_{source}_2"] = (
            baseline_mineral_complete_df['distance_2'] * transport_factor * baseline_mineral_complete_df['Contribution (tons)']
        )



for source in emission_sources:
    baseline_mineral_complete_df[f"sea_emission_{source}"] = baseline_mineral_complete_df["sea_distance_km"] * emission_factors[f'{source}']['sea'] * baseline_mineral_complete_df['Contribution (tons)']


# Loop through each emission source to calculate the total emissions
for source in emission_sources:
    baseline_mineral_complete_df[f"total_emission_{source}"] = (
        baseline_mineral_complete_df[f"emission_{source}_1"] +
        baseline_mineral_complete_df[f"emission_{source}_2"] +
        baseline_mineral_complete_df[f"sea_emission_{source}"]
    )



