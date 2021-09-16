#' Converts Payments to a triangle
#'
#' \code{Payments2Triangle} converts a list of cash flows to a triangle.
#'
#' @param accidentDate Date vector. Reference date for the triangle: accident date or underwriting date depending on the reference
#' @param transactionDate Date vector. Date of transaction.
#' @param cashFlows Numeric vector. Cash flows corresponding to the reference date and made at transaction date.
#' @param years Numeric vector. Years to include in the analysis. By default, all years are taken.
#' @param mode string. The mode of aggregation for triangle. The different possible modes are:
#'   \itemize{
#'   \item{semester: aggregation over semesters for accident period and semesters for development period. Implies evalDate in ("07-01", "01-01")}
#'   \item{quarter: aggregation over quarters for accident period and semesters for development period. Implies evalDate in ("10-01","07-01","04-01", "01-01")}
#'   \item{year: aggregation on yearly step from evalDate for both accident and development period}
#'   \item{yearRef: aggregation on fiscal years for accident periods and a first development period corresponding to the timelapse between beginning of the year and evalDate}}
#' @param evalDate String. Date of evaluation, with format "m-d" (ex: "12-23"). All data after this date for the maximal year will not be considered.
#'
#' @return Triangle as a matrix
#'
#' @export
Payments2Triangle <- function(accidentDate, transactionDate, cashFlows, years = NA, mode = "year", evalDate = "01-01"){
  
  # Check the validity of the vector parameters
  if(!lubridate::is.Date(accidentDate)){stop("Invalid format for accidentDate. Should be a date type.")}
  if(!lubridate::is.Date(transactionDate)){stop("Invalid format for transactionDate. Should be a date type.")}
  if(!is.numeric(cashFlows)){stop("Invalid format for cashFlows. Should be a numeric type.")}
  n = length(accidentDate)
  m = length(transactionDate)
  l = length(cashFlows)
  if(m != n | m != l | n != l){stop("The three vectors should have the same lenghts.")}

  # Check that no accident date is posterior to a transaction date
  if(!all(accidentDate <= transactionDate)){stop("Some accident date are posterior to the associated transaction date.")}

  # Check the validity of the years vector
  if(is.na(years[1])){years <- min(lubridate::year(accidentDate)):max(lubridate::year(transactionDate)+1)}
  
  # Sort year vector if not in order
  years <- sort(years)
  
  # Verify that year vector is continuous
  if(!all( min(years):max(years) %in% years)){warning("Discontinuous years used")}

  # Check modes and evalDate
  if(!(mode %in% c("year", "quarter", "semester", "yearRef"))){stop("Invalid mode. Should be year, quarter, semester or yearRef.")}
  if(mode == "semester" & !(evalDate %in% c("01-01", "07-01"))){stop("Mode is semester. Please enter a proper evalDate (01-01 or 07-01).")}
  if(mode == "quarter" & !(evalDate %in% c("01-01", "04-01", "07-01", "10-01"))){stop("Mode is quarter Please enter a proper evalDate (01-01, 04-01, 07-01 or 10-01.")}

  # Limit dates
  dateLim <- as.Date(paste0(years, "-", evalDate), format = "%Y-%m-%d")

  # Construction of the table
  data <- data.table::data.table(accident = as.Date(accidentDate),
                                 transaction = as.Date(transactionDate),
                                 cashflows = cashFlows)

  # Removal of data outside bonds
  if(mode == "year"){
    data <- data[accident >= min(dateLim) & accident < max(dateLim) & transaction >= min(dateLim) & transaction < max(dateLim)]
  } else{
    data <- data[accident >= as.Date(paste0(min(years), "-01-01"), format = "%Y-%m-%d") & accident < max(dateLim) & transaction >= as.Date(paste0(min(years), "-01-01"), format = "%Y-%m-%d") & transaction < max(dateLim)]
  }

  # Completion of the table with potential missing dates

  comp <- data.table::data.table(accident =  seq(min(data$accident), max(data$accident), by = 1),
                                 transaction = seq(min(data$accident), max(data$accident), by = 1),
                                 cashflows = 0)

  data <- data.table::rbindlist(list(data, comp))
  
  # Reorder data by accident date
  data.table::setorder(data, "accident")

  # Create accident and development year values for further aggregation
  if(mode == "quarter"){
    data[, c("accidentYear", "developmentYear") := list(paste(lubridate::year(accident), lubridate::quarter(accident, with_year = FALSE), sep = "|"),
                                                        (lubridate::year(transaction) - lubridate::year(accident)) * 4 + lubridate::quarter(transaction, with_year = FALSE) - lubridate::quarter(accident, with_year = FALSE)
                                                        )
                 ]
  } else if(mode == "semester"){
    data[, c("accidentYear", "developmentYear") := list(paste(lubridate::year(accident), lubridate::semester(accident, with_year = FALSE), sep = "|"),
                                                        (lubridate::year(transaction) - lubridate::year(accident)) * 4 + lubridate::semester(transaction, with_year = FALSE) - lubridate::semester(accident, with_year = FALSE)
                                                        )
    ]
  } else if(mode == "yearRef"){
    data[, accidentYear := lubridate::year(accident)]
    data[, developmentYear := cut(transaction, breaks = c(as.Date(paste0(accidentYear, "-01-01"), format = "%Y-%m-%d"), dateLim[dateLim > as.Date(paste0(accidentYear, "-01-01"), format = "%Y-%m-%d")]), labels = FALSE) -1, by = accidentYear]
  } else if(mode == "year"){
    data[, c("accidentYear", "developmentYear") := list(cut(accident, breaks = dateLim, labels = paste(dateLim[-length(dateLim)], dateLim[-1], sep = "|")),
                                                        cut(transaction, breaks = dateLim, labels = FALSE) - cut(accident, breaks = dateLim, labels = FALSE))
    ]
  }
  
  
  # Prevent values from not existing when aggregating
  dataComp <- expand.grid(accidentYear = unique(data$accidentYear),
                          developmentYear = unique(data$developmentYear),
                          cashflows = NA)
  dataComp <- dataComp[!(paste(dataComp$accidentYear, dataComp$developmentYear) %in% unique(paste(data$accidentYear, data$developmentYear))),]

  data <- data.table::rbindlist(list(data, dataComp), fill = TRUE)

  # Value aggregation
  agg_data <- data[, .(cashflows = sum(cashflows, na.rm = T)), by = list(accidentYear, developmentYear)]
  data.table::setorderv(agg_data, c("accidentYear", "developmentYear"))
  
  # Put NAs on values that should not exist and aggregate
  agg_data[(cashflows == 0 & data.table::shift(cashflows, -1) == 0) | (cashflows == 0 & developmentYear > data.table::shift(developmentYear, -1)), cashflows := NA]
  agg_data[, cashflows := cumsum(cashflows), by = accidentYear]


  # Convert to triangle
  triangle <- data.table::dcast(agg_data, accidentYear ~ developmentYear, value.var = "cashflows")

  # Naming before converting to triangle to keep names
  rowNames <- triangle$accidentYear
  triangle <- as.matrix(triangle[,-1])

  
  # Name rows and columns
  colnames(triangle) <- 1:ncol(triangle)
  rownames(triangle) <- rowNames

  # Return value for function
  return(triangle)

}
