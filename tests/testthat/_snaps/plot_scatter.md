# if `data` is not a data frame errors gracefully

    is.data.frame(data) is not TRUE

# if `data` has zero rows errors gracefully

    `zero_row` must have some rows.
    x `zero_row` has zero rows.

# with missing crucial columns errors gracefully

    `data` must have all the expected names.
    x Missing names: asset_class, entity_name, entity_type, score, tech_mix_green.

# with multiple values for `asset_class` errors gracefully

    `toy_data_scatter` must have a single value of `asset_class`.
    i Do you need to pick one value? E.g. pick 'equity' with: `subset(toy_data_scatter, asset_class == 'equity')`.
    x Provided: equity, bonds.

# with wrong values of `entity_type` errors gracefully

    Each value of `"entity_type"` must be one of these:
    average, this_portfolio, peer, benchmark.
    x You passed: bad.

# with non-numeric values of `tech_mix_green` and `score` errors gracefully

    is.numeric(data$tech_mix_green) is not TRUE

---

    is.numeric(data$score) is not TRUE

# with wrong values of `tech_mix_green` and `score` errors gracefully

    (data$tech_mix_green <= 1) & (data$tech_mix_green >= 0) are not all TRUE

---

    (data$score <= 100) & (data$score >= 0) are not all TRUE

