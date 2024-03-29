% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_green_brown_bars.R
\name{plot_green_brown_bars}
\alias{plot_green_brown_bars}
\title{Create a bar plot with exposures to low and high carbon technologies}
\usage{
plot_green_brown_bars(data)
}
\arguments{
\item{data}{A data frame. In principle, an output of
\code{prep_green_brown_bars()}. Requirements:
\itemize{
\item Must have columns: \code{asset_class},\code{tech_type}, \code{sector},
\code{perc_sec_exposure}, \code{perc_tech_exposure}.
\item \code{tech_type} column must only have following values: "green",
"nuclear", "brown", "other".
\item \code{perc_tech_exposure} and \code{perc_sec_exposure} must be percentages in
decimal format, with values between 0 and 1.
}}
}
\value{
an object of class "ggplot".
}
\description{
Create a bar plot with exposures to low and high carbon technologies
}
\examples{
plot_green_brown_bars(toy_data_green_brown_bars)
}
