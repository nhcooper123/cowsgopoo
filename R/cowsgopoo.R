# This function is designed to take a dataset of IDs for animals
# For given date of birth extract how many days in each age class
# ------------------------------------------------------------------

# example for sally with write table and read in data and source.

# Identify column numbers
column.ID <- function(data, column.name) { 
  which(names(data) == column.name)
} 

# Convert dates from dd/mm/yyyy format to yyyy-mm-dd
convert.date <- function(date) {
  as.Date(date, "%d/%m/%Y")
}

# Extract monthly intervals from date of birth of individual
# to the end date of the reporting period
get.month.intervals <- function(data, ID, DOB.col, end.date) {
  seq(from = data[ID, DOB.col], to = end.date, by = "month")
}

# Make empty data frame for cut off dates
build.cut.off.data <- function(age.classes) {
  cut.off.dates <- data.frame(array(dim = c(length(age.classes), 2)))
  colnames(cut.off.dates) <- c("start", "end")
  return(cut.off.dates)
}

# Work out end and start cut off dates for a given class
get.cut.off.dates <- function(data, ID, DOB.col, start.date, end.date, 
                              age.classes, cut.off.dates) {
  for (x in seq_along(age.classes)) {
    cut.off.dates$start[x] <- as.character(get.month.intervals(data, ID, DOB.col, end.date)
                                           [age.classes[[x]][1] + 1])
    cut.off.dates$end[x] <- as.character(get.month.intervals(data, ID, DOB.col, end.date)
                                         [age.classes[[x]][2] + 1]) 
  }
  return(cut.off.dates)
}

# Replace dates with NAs for animals with start dates for age classes 
# outside the reporting period 

fix.start.dates <- function(start.date, age.classes, cut.off.dates) {
  for (x in seq_along(age.classes)) {
    if (!is.na(cut.off.dates$start[x])) {
      if (cut.off.dates$start[x] < start.date & cut.off.dates$end[x] < start.date) {
        cut.off.dates$start[x] <- NA
      } 
      if (cut.off.dates$start[x] < start.date & cut.off.dates$end[x] > start.date) {
        cut.off.dates$start[x] <- as.character(start.date)   
      }
    }
  }  
  return(cut.off.dates)
}

# Where there is a start date but no end date,
# replace with end date of period
fix.missing.end.dates <- function(end.date, age.classes, cut.off.dates) {
  for (x in seq_along(age.classes)) {
    if (!is.na(cut.off.dates$start[x]) & is.na(cut.off.dates$end[x])) {
      cut.off.dates$end[x] <- as.character(end.date)
    }
  } 
  return(cut.off.dates) 
}

# Make cut off dates data frame and fill it
fill.cut.off.dates <- function(data, ID, DOB.col, start.date, end.date, age.classes) {
  cut.off.dates <- build.cut.off.data(age.classes)
  cut.off.dates <- get.cut.off.dates(data, ID, DOB.col, start.date, end.date, 
                              age.classes, cut.off.dates)
  return(cut.off.dates)
}

# Make cut off dates data frame, fill it and fix all the dates
add.cut.off.dates <- function(data, ID, DOB.col, start.date, end.date, age.classes, cut.off.dates) {
  cut.off.dates <- fill.cut.off.dates(data, ID, DOB.col, start.date, end.date, age.classes)
  cut.off.dates <- fix.missing.end.dates(end.date, age.classes, cut.off.dates)
  cut.off.dates <- fix.start.dates(start.date, age.classes, cut.off.dates)
  return(cut.off.dates)
}

# Calculate number of days spent in a category with a given start 
# and end date
get.days.in.class <- function(start, end) {
  as.numeric(difftime(start, end, units = "days"))
}

# Build empty data frame for days in age class output
build.days.data <- function(age.classes) {
  days.data <- data.frame(array(dim = c(length(age.classes), 1)))
  colnames(days.data) <- c("days.in.class")
  return(days.data)
}

# Calculate number of days spent in a category with a given start 
# and end date for all categories
get.days.in.all.classes <- function(cut.off.dates, age.classes, days.data) {
  for (i in seq_along(age.classes)) {
    days.data$days.in.class[i] <- get.days.in.class(as.Date(cut.off.dates$end[i]), 
                                                    as.Date(cut.off.dates$start[i]))
    # Replace NAs with zeros as they represent no days spent in an age class
    if(is.na(days.data$days.in.class[i])) {
      days.data$days.in.class[i] <- 0
    }
  }
  return(days.data)
}

# Make days data frame and fill it
fill.days.in.all.classes <- function(cut.off.dates, age.classes) {
  days.data <- build.days.data(age.classes)
  days.data <- get.days.in.all.classes(cut.off.dates, age.classes, days.data)
  return(days.data)
}

# Build empty dataframe for output
build.data <- function(data, ID.col, age.classes) {
  cow.data <- data.frame(array(dim = c(length(data[, ID.col]), (length(age.classes) + 1))))
  colnames(cow.data) <- c("ID", paste("age class", seq(from = 1, to = length(age.classes), by = 1)))
  return(cow.data)
}

# Build output for age classes
age.classes.table <- function(age.classes) {
  age.table <- data.frame(array(dim = c(length(age.classes), 3)))
  colnames(age.table) <- c("age class", "start month", "end month")
  for(x in seq_along(age.classes)) {
    age.table[x, "age class"] <- x
    age.table[x, "start month"] <- age.classes[[x]][1]
    age.table[x, "end month"] <- age.classes[[x]][2]
  }
  return(age.table)
}

# Getting the total days in each age class
cow.totals <- function(cow.data, ID.col) {
  colSums(cow.data[, -ID.col])
}

# Overall cowsgopoo function
# Input is a data frame with variables for 
# ID of animal, date of birth, start and end dates of the 
# period (dd/mm/yyyy), and age classes in months.
# Outputs days in each age class for each individual plus
# total days all individuals spent in each age class, age classes, 
# start and end dates, total days in reporting period,
# and total number of individuals.

cowsgopoo <- function(data, ID, DOB, start.date, end.date, age.classes) {
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
  if (length(start.date) == 0)
    stop("Start date not entered")  
  if (length(end.date) == 0)
    stop("End date not entered") 

  # Convert dates to R useable format
  data[, DOB.col] <- convert.date(data[, DOB.col])
  start.date <- convert.date(start.date)
  end.date <- convert.date(end.date)
 
  # Build empty output file = number of days in each class, plus ID of cow
  cow.data <- build.data(data, ID.col, age.classes)

  # Loop through each cow and add results to output
  for(ID in seq_along(data[, ID.col])) {
  
    # Get start and end cut off dates for three categories
    cut.off.dates.list <- add.cut.off.dates(data, ID, DOB.col, start.date, end.date, 
                                            age.classes, cut.off.dates)

    # Extract number of days in each class
    days.in.class <- fill.days.in.all.classes(cut.off.dates.list, age.classes)

    # Outputs
    cow.data$ID[ID] <- as.character(data[, ID.col][ID])
    cow.data[ID, 2:(length(age.classes) + 1)] <- days.in.class$days.in.class   
  }
  # Overall outputs
  cow.results <- list(results = cow.data, totals = cow.totals(cow.data, ID.col), 
                      age.classes = age.classes.table(age.classes), start.date = start.date, 
                      end.date = end.date, reporting.period = end.date - start.date,
                      number.individuals = length(unique(cow.data$ID))) 
  return(cow.results)
}