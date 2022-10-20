# technology and sector mapping between P4I and P4B
# HDV and shipping not consistently defined across both versions at this time
# styler: off
p4i_p4b_sector_technology_mapper <- tibble::tribble(
  ~sector_p4b,   ~technology_p4b,             ~sector_p4i,    ~technology_p4i,
  "automotive",  "electric",                 "Automotive",   "Electric",
  "automotive",  "fuelcell",                 "Automotive",   "FuelCell",
  "automotive",  "hybrid",                   "Automotive",   "Hybrid",
  "automotive",  "ice",                      "Automotive",   "ICE",
  "automotive",  "electric_hdv",             "Automotive",   "Electric_HDV",
  "automotive",  "fuelcell_hdv",             "Automotive",   "FuelCell_HDV",
  "automotive",  "hybrid_hdv",               "Automotive",   "Hybrid_HDV",
  "automotive",  "ice_hdv",                  "Automotive",   "ICE_HDV",
  "coal",        "coal",                     "Coal",         "Coal",
  "oil_and_gas", "gas",                      "Oil&Gas",      "Gas",
  "oil_and_gas", "oil",                      "Oil&Gas",      "Oil",
  "oil and gas", "gas",                      "Oil&Gas",      "Gas",
  "oil and gas", "oil",                      "Oil&Gas",      "Oil",
  "power",       "coalcap",                  "Power",        "CoalCap",
  "power",       "gascap",                   "Power",        "GasCap",
  "power",       "hydrocap",                 "Power",        "HydroCap",
  "power",       "nuclearcap",               "Power",        "NuclearCap",
  "power",       "oilcap",                   "Power",        "OilCap",
  "power",       "renewablescap",            "Power",        "RenewablesCap",
  "aviation",    "freight",                  "Aviation",     "Freight",
  "aviation",    "passenger",                "Aviation",     "Passenger",
  "cement",      "grinding",                 "Cement",       "Grinding",
  "cement",      "integrated facility",      "Cement",       "Integrated facility",
  "steel",       "ac-electric arc furnace",  "Steel",        "Ac-Electric Arc Furnace",
  "steel",       "bof shop",                 "Steel",        "Bof Shop",
  "steel",       "dc-electric arc furnace",  "Steel",        "Dc-Electric Arc Furnace",
  "steel",       "open hearth meltshop",     "Steel",        "Open Hearth Meltshop",
  "steel",       "basic oxygen furnace",     "Steel",        "Basic Oxygen Furnace",
  "steel",       "electric arc furnace",     "Steel",        "Electric Arc Furnace",
  "steel",       "open hearth furnace",      "Steel",        "Open Hearth Furnace"
)

# styler: on


usethis::use_data(p4i_p4b_sector_technology_mapper, overwrite = TRUE)
