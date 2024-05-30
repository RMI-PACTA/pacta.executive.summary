# styler: off
# The Extended file data for WEO 2023 is stored here:
# https://portal.azure.com/#view/Microsoft_Azure_FileStorage/FileShareMenuBlade/~/browse/storageAccountId/%2Fsubscriptions%2Ffeef729b-4584-44af-a0f9-4827075512f9%2FresourceGroups%2FRMI-SP-PACTA-PROD%2Fproviders%2FMicrosoft.Storage%2FstorageAccounts%2Fpactarawdata/path/scenario-sources/protocol/SMB
# The specific file is 'weo_2023-20240222/WEO2023 extended data/WEO2023_Extended_Data.xlsx'
# Please download the file and replace the dummy path in variable 'file_scenario_emissions_data'
# to the path on your computer before running the code.
file_scenario_emissions_data <- "PATH/TO/WEO2023/SCENARIO/EXTENDED/DATA"
carbon_emissions <- readxl::read_xlsx(file_scenario_emissions_data,
                              sheet = "World CO2 Emissions",
                              range = "J7:AP45") %>%
  filter(!row_number() %in% c(12, 13, 14, 18, 19, 20)) %>%
  rename(
    sector = "...1",
    emissions_2022 = "2022...25",
    emissions_2030 = "2030...26",
    emissions_2035 = "2035...27",
    emissions_2040 = "2040...28",
    emissions_2045 = "2045...29",
    emissions_2050 = "2050...30",
  ) %>%
  select(c("sector", "2010", "2015", "2021", "emissions_2022", "emissions_2030", "emissions_2035", "emissions_2040", "emissions_2045", "emissions_2050")) %>%
  filter(sector %in% c("Coal", "Oil", "Natural gas", "Electricity and heat sectors", "Iron and steel**", "Cement**", "Passenger cars", "Aviation")) %>%
  mutate(
    scenario_source = "WEO2023",
    unit = "Mt CO2 (2022)"
  )

remaining_carbon_budgets <- carbon_emissions %>%
  mutate(
    # Remaining carbon budget for a sector is the sum of scenario emissions from 2022 until 2030.
    # We interpolate the emissions linearly between points for which we have scenario values (2022 and 2030).
    # The sum of 7 linearly interpolated points between x and y (including x and y) is equal to 4x + 5y:
    # x + 7*x + ((1 + 2 + 3 + 4 + 5 + 6 + 7)/7)*(y-x) + y = 8x + 5y - 4x = 4x + 5y
    remaining_carbon_budget = emissions_2022 * 4 + emissions_2030 * 5,
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
