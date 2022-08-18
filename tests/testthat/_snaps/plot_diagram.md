# if `data` is not a data frame errors gracefully

    is.data.frame(data) is not TRUE

# if `data` does not have 2 rows errors gracefully

    nrow(data) == 2 is not TRUE

# with missing crucial columns errors gracefully

    `data` must have all the expected names.
    x Missing names: asset_class, emissions_pacta, emissions_pacta_perc, exposure_asset_class, exposure_asset_class_perc, exposure_pacta, exposure_pacta_perc_asset_class_exposure, exposure_portfolio.

# with missing crucial values of `asset_class` errors gracefully

    "asset_class" in data_wrong must have at least one record equal to: equity.

# with non-numeric values of numeric columns errors gracefully

    is.numeric(data$exposure_portfolio) is not TRUE

---

    is.numeric(data$exposure_asset_class) is not TRUE

---

    is.numeric(data$exposure_asset_class_perc) is not TRUE

---

    is.numeric(data$exposure_pacta) is not TRUE

---

    is.numeric(data$exposure_pacta_perc_asset_class_exposure) is not TRUE

---

    is.numeric(data$emissions_pacta) is not TRUE

---

    is.numeric(data$emissions_pacta_perc) is not TRUE

# with wrong values of percentage columns errors gracefully

    (data$exposure_asset_class_perc <= 1) & (data$exposure_asset_class_perc >=  .... are not all TRUE

---

    (data$exposure_pacta_perc_asset_class_exposure <= 1) & (data$exposure_pacta_perc_asset_class_exposure >=  .... are not all TRUE

---

    (data$emissions_pacta_perc <= 1) & (data$emissions_pacta_perc >=  .... are not all TRUE

