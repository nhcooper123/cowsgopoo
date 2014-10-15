# This function is designed to take a dataset of IDs for animals
# For given date of birth extract how many days in each age class
# ------------------------------------------------------------------

# replace nas with zeros
# example for sally with write table and read in data and source.
# weird id as numbers not letters why?

# Convert dates from dd/mm/yyyy format to yyyy-mm-dd
convert.date <- function(date) {
  as.Date(date, "%d/%m/%Y")
}

# Extract monthly intervals from date of birth of individual
# to the end date of the reporting period
get.month.intervals <- function(data, ID, DOB.col, end) {
  seq(from = data[ID, DOB.col], to = end, by = "month")
}

# Extract number of age classes
no.classes <- function(age.classes) {
  length(age.classes)
}

# Make empty data frame for cut off dates
build.cut.off.data <- function(age.classes) {
  cut.off.dates <- data.frame(array(dim = c(length(age.classes), 2)))
  colnames(cut.off.dates) <- c("start", "end")
  return(cut.off.dates)
}

# Work out end and start cut off dates for a given class
get.cut.off.dates <- function(data, ID, DOB.col, start, end, age.classes) {
  
  for (x in seq_along(age.classes)) {
    
    cut.off.dates$start[x] <- as.character(get.month.intervals(data, ID, DOB.col, end)[age.classes[[x]][1] + 1])
    cut.off.dates$end[x] <- as.character(get.month.intervals(data, ID, DOB.col, end)[age.classes[[x]][2] + 1])
 
    # Some tidying needed to stop cut offs going beyond the reporting period
    if (is.na(cut.off.dates$start[x])) {
      cut.off.dates$start[x] <- as.character(start)
    }

    if (is.na(cut.off.dates$end[x])) {
      cut.off.dates$end[x] <- as.character(end)
    }
  }

  return(cut.off.dates)
}

# Calculate number of days spent in a category with a given start 
# and end date
get.days.in.class <- function(start.cut.off.date, end.cut.off.date) {
  as.numeric(difftime(end.cut.off.date, start.cut.off.date, units = "days"))
}

# Identify column numbers
column.ID <- function(data, column.name) { 
  which(names(data) == column.name)
}	

# Build empty dataframe for output
build.data <- function(data, ID.col) {
  cow.data <- data.frame(array(dim = c(length(data[, ID.col]), 4)))
  colnames(cow.data) <- c("ID", "zero.three.month_days", "three.twelve.month_days", 
    	"twelve.twentyfour.month_days")
  
  return(cow.data)
}

# --------------------------------------------------------
# Overall function
# Input is a data frame with variables for 
# ID of animal, date of birth, start and end dates of the 
# period.
# --------------------------------------------------------

cowsgopoo <- function(data, ID, DOB, start, end) {

  if (!is.data.frame(data)) 
    stop("'data' must be an object of class 'data.frame'")

  # Define variable columns
  ID.col <- column.ID(data, ID)

  DOB.col <- column.ID(data, DOB)
  
  # Check that all required variables were entered
  if (length(ID.col) == 0)
    stop("ID variable not found in data")  

  if (length(DOB.col) == 0)
    stop("Date of birth variable not found in data")  

  if (length(start) == 0)
    stop("Start date not entered")  

  if (length(end) == 0)
    stop("End date not entered") 

  # Convert dates to R useable format
  data[, DOB.col] <- convert.date(data[, DOB.col])

  start <- convert.date(start)
 
  end <- convert.date(end)
 
  # Build empty output file = number of days in each class, plus ID of cow
  cow.data <- build.data(data, ID.col)

  # Loop through each cow and add results to output
  for(ID in seq_along(data[, ID.col])) {
  
    # Get start and end cut off dates for three categories
    cut.off.dates.list <- get.cut.off.dates(data, ID, DOB.col, start.col, end.col)

    # Extract number of days in each class
    days.in.class <- get.days.in.class(cut.off.dates.list$start, cut.off.dates.list$end)

    # Outputs
    cow.data$ID[ID] <- data[, ID.col][ID]
    cow.data$zero.three.month_days[ID] <- days.in.class[1]
    cow.data$three.twelve.month_days[ID] <- days.in.class[2]
    cow.data$twelve.twentyfour.month_days[ID] <- days.in.class[3]

  }

  return(cow.data)

}

# ---------------------------
# Example
# ---------------------------

source("cowsgopoo.R")

myherd <- data.frame(cowID = c("a", "b", "c", "d"), 
	                 dob = c("13/01/2013", "20/06/2013", "27/11/2010", "01/11/2013"))

myclasses <- list(c(0, 3), c(3, 12), c(12, 24), c(24, NA))

cowsgopoo(data = myherd, ID = "cowID", DOB = "dob", start = "01/01/2013", end = "31/12/2013",
          age.classes = myclasses)
