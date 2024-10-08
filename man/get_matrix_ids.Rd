% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_matrix_ids.R
\name{get_matrix_ids}
\alias{get_matrix_ids}
\title{Read in the Xfinity Social Matrices list}
\usage{
get_matrix_ids(sheet, full_range, email)
}
\arguments{
\item{sheet}{The name or number of the sheet within the workbook}

\item{full_range}{The lower range that the sheet covers (e.g., B43)}

\item{email}{A google email to authenticate with \code{\link[googlesheets4]{googlesheets4-package}}}
}
\value{
Dataframe with the LOB, the matrix/social platform name, and the link associated with each
}
\description{
The Xfinity social matrices list is the overall sheet that contains hyperlinks to each
social matrix. The social matrices cover all social channels for each LOB of Xfinity.
}
\details{
Note that the user is required to have the workbook ID of the sheet saved as an environmental variable \code{XFI_MATRIX_ID}
}
\examples{
\dontrun{
q3_matrices <- get_matrix_ids(sheet = 3, full_range = "B43", email = first_last.email.com)
}
}
