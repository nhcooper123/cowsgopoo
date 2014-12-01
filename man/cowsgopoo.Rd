\name{cowsgopoo}
\alias{cowsgopoo}
\title{Function for extracting the number of days animals spend in each age class for calculating Nitrogen 
deposition in grazed fields}
\usage{
cowsgopoo(data, ID, DOB, start.date, end.date, age.classes}
\arguments{
  \item{data}{data frame of class 'data.frame' containing identifier/ID of each animal, plus
  its date of birth in the format dd/mm/yyyy.}

  \item{ID}{column name of the variable containing the identifier/ID for each 
  individual animal.}

  \item{DOB}{column name of the variable containing the date of birth for each 
  individual animal.}

  \item{start.date}{the start date of the reporting period in the format dd/mm/yyyy.}

  \item{end.date}{the end date of the reporting period in the format in the format dd/mm/yyyy. Note that the 
  function will fail if any of the dates of birth of the animals are after the end date of the reporting 
  period.}

  \item{age.classes}{a list of class 'list' with the start and end months of the age class. See the example
  below. Note that the function only works with age classes defined by month}

}
\value{
Outputs an table containing the number of days each animal was in each age class (\$results), the total number of days all animals spent in each age class (\$totals), a table of the age classes (\$age.classes), the start and end
dates of the reporting period, the number of days in the reporting period, and the number of individual animals in the analysis.
}
\description{
cowsgopoo allows users to work out how many days each individual animal in a herd spent in a variety of user defined age classes within a user defined reporting period. This is designed to help with yearly Nitrogen budget estimates.
}
\examples{
# Example dataset with IDs (cowID) a-d and random dates of birth (dob)
myherd <- data.frame(cowID = c("a", "b", "c", "d"), 
	                 dob = c("13/01/2013", "20/06/2013", "27/11/2010", "01/11/2013"))

# Example age classes of 0-3 months, 3-12 months, 12-24 months and over 24 months. Note the use of Inf to
# indicate over 24 months.
myclasses <- list(c(0, 3), c(3, 12), c(12, 24), c(24, Inf))

cowsgopoo(data = myherd, ID = "cowID", DOB = "dob", start = "01/01/2013", end = "31/12/2013",
          age.classes = myclasses)

}

