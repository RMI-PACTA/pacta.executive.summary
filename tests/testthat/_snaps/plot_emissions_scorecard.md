# if `data` is not a data frame errors gracefully

    is.data.frame(data) is not TRUE

# if `data` has zero rows errors gracefully

    `zero_row` must have some rows.
    x `zero_row` has zero rows.

# with missing crucial columns errors gracefully

    `data` must have all the expected names.
    x Missing names: asset_class, emissions, entity.

# with wrong values of `asset_class` errors gracefully

    Each value of `"asset_class"` must be one of these:
    bonds, equity.
    x You passed: bad.

# with wrong values of `entity` errors gracefully

    Each value of `"entity"` must be one of these:
    portfolio, benchmark.
    x You passed: bad.

# with non-numeric values of `emissions` errors gracefully

    is.numeric(data$emissions) is not TRUE

