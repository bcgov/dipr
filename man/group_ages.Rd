% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group-ages.R
\name{group_ages}
\alias{group_ages}
\title{Create age categories from a age vector}
\usage{
group_ages(age_column, age_group = 5, max_age = 100, return_chr = TRUE)
}
\arguments{
\item{age_column}{a numeric vector of age to be categorized}

\item{age_group}{Either a manually specified set of age groups OR
an age interval to be divided up into equal parts. To manually specify the age range
pass a vector of the right hand size of the age range. See example.}

\item{max_age}{Maximum age in the age internal. For example if max_age is 100 then
the function will calculate intervals all the way to 100+. This is ignored if a manual
age range is specified.}

\item{return_chr}{Should the function return a character or vector?}
}
\value{
A vector of age group the same length as the \code{age_column}
}
\description{
Create age categories from a age vector
}
\examples{
  c_vec <- seq(1:100)
  g_vec <- group_ages(c_vec)

  g_vec <- group_ages(c_vec, age_group = list(0:24, 25:50, 51:54, 55))

}
