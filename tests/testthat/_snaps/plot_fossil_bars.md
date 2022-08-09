# if `data` is not a data frame errors gracefully

    is.data.frame(data) is not TRUE

# if `data` has zero rows errors gracefully

    `zero_row` must have some rows.
    x `zero_row` has zero rows.

# with missing crucial columns errors gracefully

    `data` must have all the expected names.
    x Missing names: asset_class, entity_name, entity_type, perc_aum, tech.

# with wrong values of `tech` errors gracefully

    Each value of `"tech"` must be one of these:
    coal, oil, gas.
    x You passed: bad.

# with non-numeric values of `perc_aum` errors gracefully

    is.numeric(data$perc_aum) is not TRUE

# with wrong values of `perc_aum` errors gracefully

    (data$perc_aum <= 1) & (data$perc_aum >= 0) are not all TRUE

