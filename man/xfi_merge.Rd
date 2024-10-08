% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xfi_merge.R
\name{xfi_merge}
\alias{xfi_merge}
\title{Merge Social Matrix data with Sprinklr}
\usage{
xfi_merge(sprinklr_data, matrix_data, lob)
}
\arguments{
\item{sprinklr_data}{Name of sprinklr dataset in memory}

\item{matrix_data}{Name of social matrix data in memory}

\item{lob}{The line of business of interest}
}
\value{
Dataframe of merged data (Sprinklr with Social Matrix data)
}
\description{
This takes the Sprinklr data, splits out the placement name and applies the nomenclature to it. It then
uses \code{\link[fuzzyjoin:fuzzy_full_join]{fuzzy_full_join function}}. Finally,
we stanardize the column names - for some reason, Sprinklr column names differ by platform.
}
\details{
Running the function also checks how well the merge worked and reports the results in the console.
}
\examples{
\dontrun{
merged <- xfi_merge("West")
}
}
