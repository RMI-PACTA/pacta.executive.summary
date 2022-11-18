# if `data` is not a data frame errors gracefully

    is.data.frame(data) is not TRUE

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
    coal, oil_and_gas.
    x You passed: bad.

# with non-numeric values of `exposure_perc_aum` errors gracefully

    is.numeric(data$exposure_perc_aum) is not TRUE

# with wrong values of `exposure_perc_aum` errors gracefully

    (data$exposure_perc_aum <= 1) & (data$exposure_perc_aum >= 0) are not all TRUE

