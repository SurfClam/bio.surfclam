% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Footprint.R
\name{Footprint}
\alias{Footprint}
\title{Plot the annual landings, footprint (m2), and associated limits and trigger levels annually.}
\usage{
Footprint(CatchData, SelBank)
}
\arguments{
\item{CatchData}{is average catch per year. Columns are ordered as "Year" "se" "sd" and "CPUE". CPUE is the mean CPUE in a givne year.
NOte the required input can be generated by the CatchTable function}

\item{SelBank}{a numeric delimiter of the assessment region (1 = Banquereau and 2 = Grand Bank).}
}
\description{
Function to subset loci and populations
}

