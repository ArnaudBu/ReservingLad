% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Payments2Triangle.R
\name{Payments2Triangle}
\alias{Payments2Triangle}
\title{Converts Payments to a triangle}
\usage{
Payments2Triangle(accidentDate, transactionDate, cashFlows, years = NA,
  mode = "year", evalDate = "01-01")
}
\arguments{
\item{accidentDate}{Date vector. Reference date for the triangle: accident date or underwriting date depending on the axis}

\item{transactionDate}{Date vector. Date of transaction}

\item{cashFlows}{Numeric vector. Cash flows corresponding to the reference date and made at transactionDate}

\item{years}{Numeric vector. Years to include in the analysis. By default, all years are taken.}

\item{mode}{string. The mode of aggregation for triangle. The different possible modes are:
\itemize{
\item{semester: aggregation over semesters for accident period and semesters for development period. Implies evalDate in ("07-01", "01-01")}
\item{quarter: aggregation over quarters for accident period and semesters for development period. Implies evalDate in ("10-01","07-01","04-01", "01-01")}
\item{year: aggregation on yearly step from evalDate for both accident and development period}
\item{yearRef: aggregation on fiscal years for accident periods and a first development period corresponding to the timelapse between beginning of the year and evalDate}}}

\item{evalDate}{String. date of evaluation, with format "m-d" (ex: "12-23"). All data after this date for the maximal year will not be considered.}
}
\value{
the triangle as a matrix
}
\description{
\code{Payments2Triangle} converts a list of cash flows to a triangle.
}
