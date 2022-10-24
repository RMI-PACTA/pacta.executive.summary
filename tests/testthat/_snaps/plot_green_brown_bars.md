# if `data` is not a data frame errors gracefully

    is.data.frame(data) is not TRUE

# if `data` has zero rows errors gracefully

    `zero_row` must have some rows.
    x `zero_row` has zero rows.

# with missing crucial columns errors gracefully

    `data` must have all the expected names.
    x Missing names: asset_class, perc_sec_exposure, perc_tech_exposure, sector, tech_type.

# with wrong values of `tech_type` errors gracefully

    Each value of `"tech_type"` must be one of these:
    green, nuclear, brown, other.
    x You passed: bad.

# with non-numeric values of `perc_tech_exposure` and `perc_sec_exposure` errors gracefully

    is.numeric(data$perc_tech_exposure) is not TRUE

---

    is.numeric(data$perc_sec_exposure) is not TRUE

# with wrong values of `perc_tech_exposure` and `perc_sec_exposure` errors gracefully

    (data$perc_tech_exposure <= 1) & (data$perc_tech_exposure >=  .... are not all TRUE

---

    (data$perc_sec_exposure <= 1) & (data$perc_sec_exposure >= 0) are not all TRUE

