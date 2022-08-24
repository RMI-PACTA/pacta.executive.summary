# if `data` is not a data frame errors gracefully

    is.data.frame(data) is not TRUE

# if `data` has zero rows errors gracefully

    `zero_row` must have some rows.
    x `zero_row` has zero rows.

# with missing crucial columns errors gracefully

    `data` must have all the expected names.
    x Missing names: asset_class, score.

# with wrong values of `asset_class` errors gracefully

    Each value of `"asset_class"` must be one of these:
    equity, bonds.
    x You passed: bad.

# with wrong values of `score` errors gracefully

    Each value of `"score"` must be one of these:
    A+, A, B, C, D, E.
    x You passed: bad.

