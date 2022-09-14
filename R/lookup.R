#' Lookup valid values
#'
#' @name lookup

time_horizon_lookup <- 5

# TODO: use r2dii.data instead?
green_techs_lookup <- c(
  "renewablescap", "hydrocap", "nuclearcap", "hybrid", "electric", "fuelcell",
  "hybrid_hdv", "electric_hdv", "fuelcell_hdv", "dc-electric arc furnace",
  "ac-electric arc furnace"
)
brown_techs_lookup <- c(
  "coalcap", "gascap", "oilcap", "ice", "oil", "gas", "coal", "ice_hdv"
  # TODO: add hard-to-abate techs
)
