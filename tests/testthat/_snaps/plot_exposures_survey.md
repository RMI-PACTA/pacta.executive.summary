# if `data` is not a data frame errors gracefully

    is.data.frame(data) is not TRUE

# if `data` has zero rows errors gracefully

    `zero_row` must have some rows.
    x `zero_row` has zero rows.

# with missing crucial columns errors gracefully

    `data` must have all the expected names.
    x Missing names: asset_class, entity, exposure_perc_aum, sector.

# with multiple values for `asset_class` errors gracefully

    `toy_data_exposures_survey` must have a single value of `asset_class`.
    i Do you need to pick one value? E.g. pick 'equity' with: `subset(toy_data_exposures_survey, asset_class == 'equity')`.
    x Provided: equity, bonds.

# with wrong values of `entity` errors gracefully

    Each value of `"entity"` must be one of these:
    portfolio, peers.
    x You passed: bad.

# with wrong values of `sector` errors gracefully

    Each value of `"sector"` must be one of these:
    blue, automotive, hybrid, blue1, hybrid_hdv, blue2, ice, blue3, ice_hdv, blue01, fuelcell, blue02, electric_hdv, blue03, electric, green, aviation, dark_yellow, cement, brown, coal, almost_black, oil_and_gas, fossil_fuels, oil, grey, gas, orange, power, oilcap, oil_cap, oil_capacity, orange1, coalcap, coal_cap, coal_capacity, orange01, gascap, gas_cap, gas_capacity, orange02, nuclearcap, nuclear_cap, nuclear_capacity, orange03, hydrocap, hydro_cap, hydro_capacity, orange04, renewablescap, renewables_cap, renewables_capacity, dark_purple, shipping, ruby_red, steel, steel_grey, na, , , , , , , , , .
    x You passed: bad.

# with non-numeric values of `exposure_perc_aum` errors gracefully

    is.numeric(data$exposure_perc_aum) is not TRUE

# with wrong values of `exposure_perc_aum` errors gracefully

    (data$exposure_perc_aum <= 1) & (data$exposure_perc_aum >= 0) are not all TRUE

