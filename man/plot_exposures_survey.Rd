% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_exposures_survey.R
\name{plot_exposures_survey}
\alias{plot_exposures_survey}
\title{Create a bar plot showing exposure to a sector}
\usage{
plot_exposures_survey(data)
}
\arguments{
\item{data}{A data frame. In principle, an output of
\code{prep_exposures_survey()}. Requirements:
\itemize{
\item Must have columns: \code{asset_class}, \code{entity}, \code{sector}, \code{exposure_perc_aum}.
\item \code{asset_class} must have a single value.
\item \code{entity} must be one of: "portfolio", "peers".
\item \code{sector} must be one of PACTA sectors. Run
\code{names(r2dii.colours::colour_aliases_pacta)} for the list of values.
\item \code{exposure_perc_aum} must be a percentage in decimal format, with values
between 0 and 1.
}}
}
\value{
an object of class "ggplot".
}
\description{
Create a bar plot showing exposure to a sector
}
\examples{
data <- toy_data_exposures_survey \%>\%
  dplyr::filter(asset_class == "equity", technology == "coal")

plot_exposures_survey(data)
}
