# if `data` is not a data frame errors gracefully

    is.data.frame(data) is not TRUE

# if `data` has zero rows errors gracefully

    `zero_row` must have some rows.
    x `zero_row` has zero rows.

# with missing crucial columns errors gracefully

    `data` must have all the expected names.
    x Missing names: asset_class, exposure_perc_aum, sector_or_tech.

# with wrong values of `sector_or_tech` errors gracefully

    Each value of `"sector_or_tech"` must be one of these:
    coal, other_fossil_fuels, fossil_power, renewables_power.
    x You passed: bad.

# with non-numeric values of `exposure_perc_aum` errors gracefully

    is.numeric(data$exposure_perc_aum) is not TRUE

# with wrong values of `exposure_perc_aum` errors gracefully

    (data$exposure_perc_aum <= 1) & (data$exposure_perc_aum >= 0) are not all TRUE

