#' Converts Payments to a triangle
#'
#' \code{Payments2Triangle} converts a list of cash flows to a triangle.
#'
#' @param accidentDate Date vector. Reference date for the triangle: accident date or underwriting date depending on the axis
#' @param transactionDate Date vector. Date of transaction
#' @param cashFlows Numeric vector. Cash flows corresponding to the reference date and made at transactionDate
#' @param years Numeric vector. Years to include in the analysis. By default, all years are taken.
#' @param mode string. The mode of aggregation for triangle. The different possible modes are:
#'   \itemize{
#'   \item{semester: aggregation over semesters for accident period and semesters for development period. Implies evalDate in ("07-01", "01-01")}
#'   \item{quarter: aggregation over quarters for accident period and semesters for development period. Implies evalDate in ("10-01","07-01","04-01", "01-01")}
#'   \item{year: aggregation on yearly step from evalDate for both accident and development period}
#'   \item{yearRef: aggregation on fiscal years for accident periods and a first development period corresponding to the timelapse between beginning of the year and evalDate}}
#' @param evalDate String. date of evaluation, with format "m-d" (ex: "12-23"). All data after this date for the maximal year will not be considered.
#'
#' @return the triangle as a matrix
#'
#' @import lubridate data.table dplyr
#'
#' @export
Payments2Triangle <- function(accidentDate, transactionDate, cashFlows, years = NA, mode = "year", evalDate = "01-01"){
  accident <- transaction <- developmentYear <- accidentYear <- cashflows <- NULL
  # Check the validity of the vector parameters
  if(!is.Date(accidentDate)){stop("Invalid format for accidentDate")}
  if(!is.Date(transactionDate)){stop("Invalid format for transactionDate")}
  if(!is.numeric(cashFlows)){stop("Invalid format for cashFlows")}
  n = length(accidentDate)
  m = length(transactionDate)
  l = length(cashFlows)
  if(m != n | m != l | n != l){stop("Invalid vectors length")}

  # Check that no accident date is posterior to a transaction date
  if(!all(accidentDate <= transactionDate)){stop("Some accident date are posterior to the associated transaction date.")}

  # Check the validity of the years vector
  if(is.na(years[1])){years <- min(year(accidentDate)):max(year(transactionDate))}
  years <- sort(years)
  if(!all( min(years):max(years) %in% years)){warning("Discontinuous years used")}

  # Check modes and evalDate
  if(!(mode %in% c("year", "quarter", "semester", "yearRef"))){stop("Invalid mode")}
  if(mode == "semester" & !(evalDate %in% c("01-01", "07-01"))){stop("Mode is semester. Please enter a proper evalDate (01-01 or 07-01).")}
  if(mode == "quarter" & !(evalDate %in% c("01-01", "04-01", "07-01", "10-01"))){stop("Mode is quarter Please enter a proper evalDate (01-01, 04-01, 07-01 or 10-01.")}

  # Limit dates
  dateLim <- as.Date(paste0(years, "-", evalDate), format = "%Y-%m-%d")

  # Construction of the table
  data <- data.table(accident = accidentDate,
                     transaction = transactionDate,
                     cashflows = cashFlows)

  # Removal of data outside bonds
  if(mode == "year"){
    data <- data[accident >= min(dateLim) & accident < max(dateLim) & transaction >= min(dateLim) & transaction < max(dateLim)]
  } else{
    data <- data[accident >= as.Date(paste0(min(years), "-01-01"), format = "%Y-%m-%d") & accident < max(dateLim) & transaction >= as.Date(paste0(min(years), "-01-01"), format = "%Y-%m-%d") & transaction < max(dateLim)]
  }

  # Completion of the table with potential missing dates

  comp <- data.table(accident =  seq(min(data$accident), max(data$accident), by = 1),
                     transaction = seq(min(data$accident), max(data$accident), by = 1),
                     cashflows = 0)

  data <- rbindlist(list(data, comp))

  # Conversion to triangle
  if(mode == "quarter"){
    data <- data %>%
      arrange(accident) %>%
      mutate(accidentYear = as.factor(paste(year(accident),quarter(accident, with_year = FALSE), sep = "|")),
             developmentYear = (year(transaction) - year(accident)) * 4 + quarter(transaction, with_year = FALSE) - quarter(accident, with_year = FALSE)) %>%
      data.table
  } else if(mode == "semester"){
    data <- data %>%
      arrange(accident) %>%
      mutate(accidentYear = as.factor(paste(year(accident),semester(accident, with_year = FALSE), sep = "|")),
             developmentYear = (year(transaction) - year(accident)) * 2 + semester(transaction, with_year = FALSE) - semester(accident, with_year = FALSE)) %>%
      data.table
  } else if(mode == "yearRef"){
    data <- data %>%
      arrange(accident) %>%
      mutate(accidentYear = as.factor(year(accident))) %>%
      data.table
      data <- data[, developmentYear := cut(transaction, breaks = c(as.Date(paste0(accidentYear, "-01-01")), dateLim[dateLim > as.Date(paste0(accidentYear, "-01-01"))]), labels = FALSE) -1, by = accidentYear]
  } else if(mode == "year"){
    data <- data %>%
      arrange(accident) %>%
      mutate(accidentYear = as.factor(cut(accident, breaks = dateLim, labels = paste(dateLim[-length(dateLim)], dateLim[-1], sep = "|"))),
             developmentYear = cut(transaction, breaks = dateLim, labels = FALSE) - cut(accident, breaks = dateLim, labels = FALSE)) %>%
      data.table
  }

  dataComp <- expand.grid(accidentYear = levels(data$accidentYear),
                          developmentYear = min(data$developmentYear):max(data$developmentYear))

  dataComp$cashflows = 0

  data <- rbindlist(list(data, dataComp), fill = TRUE)

  # Agrégation des valeurs

  data2 <- data %>%
    group_by(accidentYear, developmentYear) %>%
    summarize(cashflows = sum(cashflows)) %>%
    arrange(accidentYear, developmentYear) %>%
    mutate(cashflows = cumsum(cashflows)) %>%
    select(accidentYear, developmentYear, value = cashflows) %>%
    data.table

  # Conversion en triangle

  triangle <- dcast(data2, accidentYear ~ developmentYear)

  # Naming

  rowNames <- triangle$accidentYear

  triangle <- as.matrix(triangle[,-1])

  while(ncol(triangle) < nrow(triangle)){
    triangle <- cbind(triangle, triangle[, ncol(triangle)])
  }

  triangle[row(triangle) + col(triangle) > nrow(triangle) + 1] <- NA

  colnames(triangle) <- 1:ncol(triangle)
  rownames(triangle) <- rowNames

  # Retour du triangle

  return(triangle)

}
