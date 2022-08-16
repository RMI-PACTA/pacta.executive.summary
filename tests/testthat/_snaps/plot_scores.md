# if `data` is not a data frame errors gracefully

    is.data.frame(data) is not TRUE

# if `data` has zero rows errors gracefully

    `zero_row` must have some rows.
    x `zero_row` has zero rows.

# with missing crucial columns errors gracefully

    `data` must have all the expected names.
    x Missing names: asset_class, entity, scope, score, sector.

# with multiple values for `asset_class` errors gracefully

    `toy_data_scores` must have a single value of `asset_class`.
    i Do you need to pick one value? E.g. pick 'bonds' with: `subset(toy_data_scores, asset_class == 'bonds')`.
    x Provided: bonds, equity.

# with wrong values of `scope` errors gracefully

    Each value of `"scope"` must be one of these:
    portfolio, sector.
    x You passed: bad.

# with missing crucial values in `entity` errors gracefully

    "entity" in data_missing must have at least one record equal to: this_portfolio.

---

    "entity" in data_missing must have at least one record equal to: peers.

# with wrong values of `sector` errors gracefully

    Each value of `"sector"` must be one of these:
    NA, power, automotive, coal, oil, gas, cement, steel.
    x You passed: bad.

# with wrong values of `score` errors gracefully

    Each value of `"score"` must be one of these:
    A+, A, B, C, D, E.
    x You passed: bad.

