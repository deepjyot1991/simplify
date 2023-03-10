% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{compare_md5_change}
\alias{compare_md5_change}
\title{Compare given md5 values with the one present in _md5.csv file}
\usage{
compare_md5_change(md5_data, file_path)
}
\arguments{
\item{md5_data}{A character vector representing md5 values.}

\item{file_path}{A character vector representing md5 file path.}
}
\value{
FALSE if md5 values match, TRUE otherwise.
}
\description{
Return TRUE if provided md5 values matches with the one in the file.
}
