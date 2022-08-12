# if `data` is not a data frame errors gracefully

    is.data.frame(data) is not TRUE

# if `data` has zero rows errors gracefully

    `zero_row` must have some rows.
    x `zero_row` has zero rows.

# with missing crucial columns errors gracefully

    `data` must have all the expected names.
    x Missing names: aligned_scen_temp, asset_class, entity, perc_aum, sector, technology.

# with multiple values for `asset_class` errors gracefully

    could not find function "first"

# with missing crucial values in `entity` errors gracefully

    "entity" in data_missing must have at least one record equal to: portfolio.

# with wrong values of `sector` errors gracefully

    Each value of `"sector"` must be one of these:
    power, fossil_fuels, automotive.
    x You passed: bad.

# with wrong values of `aligned_scen_temp` errors gracefully

    Each value of `"aligned_scen_temp"` must be one of these:
    >3.2C, 2.7-3.2C, <2C.
    x You passed: bad.

# with non-numeric values of `perc_aum` errors gracefully

    is.numeric(data$perc_aum) is not TRUE

# with wrong values of `perc_aum` errors gracefully

    (data$perc_aum <= 1) & (data$perc_aum >= 0) are not all TRUE

