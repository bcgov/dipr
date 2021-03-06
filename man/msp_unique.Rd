% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unique-msp.R
\name{msp_unique}
\alias{msp_unique}
\title{Process msp data and return unique visits}
\usage{
msp_unique(msp_c, primary_icd = FALSE)
}
\arguments{
\item{msp_c}{A data.table object of msc.c status.}

\item{primary_icd}{Default FALSE. Set to TRUE is memory issues are occuring. This feature
returns only the primary ICD 9 code}
}
\description{
This method takes the raw msp input object (using \code{read_dat}) and
returns a condensed data.table that has one row per unique
visit to a doctor or hospital. This function requires data to be in
msp.c format with those column heading.
}
\author{
Jenny Sutherland, Sam Albers
}
