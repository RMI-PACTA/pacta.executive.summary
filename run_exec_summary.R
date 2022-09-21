devtools::load_all()

# temp to read peers data
project_code <- "PA2022CH"

# additional data


investor_name <- "Test"
portfolio_name <- "TestPortfolio_Input"
peer_group <- "pensionfund"
start_year <- 2022
scenario_source <- "GECO2021" # added because of new format
scenario_selected <- "1.5C-Unif"
scenario_geography <- "Global"
equity_market <- "GlobalMarket"
portfolio_allocation_method_equity <- "portfolio_weight"
portfolio_allocation_method_bonds <- "portfolio_weight"
pacta_sectors <- c("Power", "Automotive", "Oil&Gas", "Coal", "Steel", "Aviation", "Cement")
tech_roadmap_sectors <- c("Power", "Automotive", "Oil&Gas", "Coal")
green_techs <- c(
  "RenewablesCap", "HydroCap", "NuclearCap", "Hybrid", "Electric", "FuelCell",
  "Hybrid_HDV", "Electric_HDV", "FuelCell_HDV", "Dc-Electric Arc Furnace",
  "Dc-Electric Arc Furnace"
)
brown_techs <- c(
  "CoalCap", "GasCap", "OilCap", "ICE", "Oil", "Gas", "Coal", "ICE_HDV",
  "Bof Shop", "Open Hearth Meltshop", "Grinding", "Integrated facility",
  "Freight", "Passenger"
)

equity_results_portfolio <- readr::read_rds(file.path("..", "pacta.portfolio.analysis", "working_dir", "40_Results", portfolio_name, "Equity_results_portfolio.rds"))
bonds_results_portfolio <- readr::read_rds(file.path("..", "pacta.portfolio.analysis", "working_dir", "40_Results", portfolio_name, "Bonds_results_portfolio.rds"))
peers_equity_results_aggregated <- readr::read_rds(file.path("../pacta-data", "2021Q4", paste0(project_code, "_peers_equity_portfolio.rds")))
peers_bonds_results_aggregated <- readr::read_rds(file.path("../pacta-data", "2021Q4", paste0(project_code, "_peers_bonds_portfolio.rds")))
peers_equity_results_individual <- readr::read_rds(file.path("../pacta-data", "2021Q4", paste0(project_code, "_peers_equity_portfolio_ind.rds")))
peers_bonds_results_individual <- readr::read_rds(file.path("../pacta-data", "2021Q4", paste0(project_code, "_peers_bonds_portfolio_ind.rds")))
indices_equity_results_portfolio <- readr::read_rds(file.path("../pacta-data", "2021Q4", "Indices_equity_portfolio.rds"))
indices_bonds_results_portfolio <- readr::read_rds(file.path("../pacta-data", "2021Q4", "Indices_bonds_portfolio.rds"))
audit_file <- readr::read_rds(file.path("../pacta.portfolio.analysis", "working_dir", "30_Processed_Inputs", portfolio_name, "audit_file.rds"))
emissions <- NULL # TODO: needs fixing in pacta.portfolio.analysis
# emissions <- readr::read_rds(file.path("../pacta.portfolio.analysis", "working_dir", "30_Processed_Inputs", portfolio_name, "emissions.rds"))

prep_data_executive_summary <- prep_data_executive_summary(
  investor_name = investor_name,
  portfolio_name = portfolio_name,
  peer_group = peer_group,
  start_year = start_year,
  scenario_source = scenario_source,
  scenario_selected = scenario_selected,
  scenario_geography = scenario_geography,
  equity_market = equity_market,
  portfolio_allocation_method_equity = portfolio_allocation_method_equity,
  portfolio_allocation_method_bonds = portfolio_allocation_method_bonds,
  pacta_sectors = pacta_sectors,
  tech_roadmap_sectors = tech_roadmap_sectors,
  green_techs = green_techs,
  brown_techs = brown_techs,
  equity_results_portfolio = equity_results_portfolio,
  bonds_results_portfolio = bonds_results_portfolio,
  peers_equity_results_aggregated = peers_equity_results_aggregated,
  peers_bonds_results_aggregated = peers_bonds_results_aggregated,
  peers_equity_results_individual = peers_equity_results_individual,
  peers_bonds_results_individual = peers_bonds_results_individual,
  indices_equity_results_portfolio = indices_equity_results_portfolio,
  indices_bonds_results_portfolio = indices_bonds_results_portfolio,
  audit_file = audit_file,
  emissions = emissions
)

data_prep_green_brown_bars <- prep_green_brown_bars(
  results_portfolio = prep_data_executive_summary$results_portfolio,
  scenario_selected = scenario_selected
)

data_prep_fossil_bars <- prep_fossil_bars(
  results_portfolio = prep_data_executive_summary$results_portfolio,
  peers_results_aggregated = prep_data_executive_summary$peers_results_aggregated,
  indices_results_portfolio = prep_data_executive_summary$indices_results_portfolio,
  scenario_selected = scenario_selected
)

data_prep_scores_eq <- prep_scores(
  results_portfolio = prep_data_executive_summary$results_portfolio,
  peers_results_aggregated = prep_data_executive_summary$peers_results_aggregated,
  asset_class = "equity",
  scenario_source = scenario_source
)

data_prep_scores_cb <- prep_scores(
  results_portfolio = prep_data_executive_summary$results_portfolio,
  peers_results_aggregated = prep_data_executive_summary$peers_results_aggregated,
  asset_class = "bonds",
  scenario_source = scenario_source
)


scatter <- get("toy_data_scatter")

# View(scatter)

data_prep_scatter_eq <- prep_scatter(
  results_portfolio = prep_data_executive_summary$results_portfolio,
  peers_results_aggregated = prep_data_executive_summary$peers_results_aggregated,
  peers_results_individual = prep_data_executive_summary$peers_results_individual,
  indices_results_portfolio = prep_data_executive_summary$indices_results_portfolio,
  scenario_source = "GECO2021",
  scenario_selected = scenario_selected,
  asset_class = "equity"
)

data_prep_scatter_cb <- prep_scatter(
  results_portfolio = prep_data_executive_summary$results_portfolio,
  peers_results_aggregated = prep_data_executive_summary$peers_results_aggregated,
  peers_results_individual = prep_data_executive_summary$peers_results_individual,
  indices_results_portfolio = prep_data_executive_summary$indices_results_portfolio,
  scenario_source = "GECO2021",
  scenario_selected = scenario_selected,
  asset_class = "bonds"
)
