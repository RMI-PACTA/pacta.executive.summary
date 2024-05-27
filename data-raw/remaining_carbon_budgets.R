# styler: off
carbon_emissions <- read_xlsx("/Users/antoinelalechere/Library/CloudStorage/OneDrive-RMI/PACTA_Documents/General Projects/Scenarios/IEA WEO/WEO 2023 Extended Data/WEO2023_Extended_Data.xlsx",
                              sheet = "World CO2 Emissions",
                              range = "J7:AP45") %>%
  select(-c("2022...5", "2030...6", "2035...7", "2040...8", "2045...9", "2050...10", "...11", "2030...12", "2050...13",
            "2022...15", "2030...16", "2035...17", "2040...18", "2045...19", "2050...20", "...21", "2030...22", "2050...23", "...24",
            "...31", "2030...32", "2050...33", "...14")) %>%
  rename(
    sector = "...1",
    emissions_2022 = "2022...25",
    emissions_2030 = "2030...26",
    emissions_2035 = "2035...27",
    emissions_2040 = "2040...28",
    emissions_2045 = "2045...29",
    emissions_2050 = "2050...30",
  ) %>%
  filter(sector %in% c("Coal", "Oil", "Natural gas", "Electricity and heat sectors", "Iron and steel**", "Cement**", "Passenger cars", "Aviation")) %>%
  mutate(
    scenario_source = "WEO2023",
    unit = "Mt CO2 (2022)"
  )

carbon_emissions <- carbon_emissions[-5,]
carbon_emissions <- carbon_emissions[-5,]
carbon_emissions <- carbon_emissions[-5,]
carbon_emissions <- carbon_emissions[-5,]
carbon_emissions <- carbon_emissions[-5,]
carbon_emissions <- carbon_emissions[-5,]


remaining_carbon_budgets <- carbon_emissions %>%
  mutate(
    remaining_carbon_budget = emissions_2022 * 4 + emissions_2030 * 4, # remaining carbon dbusget is the interpolized carbon budget until 2030 for a sector
    weighting_factor = remaining_carbon_budget/ sum(remaining_carbon_budget)
         ) %>%
  mutate(ald_sector = case_when(
    sector == "Coal" ~ "coal",
    sector == "Oil" ~ "oil",
    sector == "Natural gas" ~ "gas",
    sector == "Electricity and heat sectors" ~ "power",
    sector == "Iron and steel**" ~ "steel",
    sector == "Passenger cars" ~ "automotive",
    sector == "Aviation" ~ "aviation",
    sector == "Cement**" ~ "cement"
  )) %>%
  select("scenario_source", "unit", "remaining_carbon_budget", "weighting_factor", "ald_sector")
# styler: on

usethis::use_data(remaining_carbon_budgets, overwrite = TRUE)
