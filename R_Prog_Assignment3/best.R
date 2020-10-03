## These functions use data from the Hospital Compare
## web site http://hospitalcompare.hhs.gov run by the
## U.S. Dept of Health & Human Services. The information
## is related to the quality of care at over 4,000
## Medicare-certified hospitals in the U.S.

## This function finds the hospital with the lowest
## 30-day mortality rate for a given outcome in a
## particular state.
## Usage: best(state, outcome)
## state is the 2-letter abbreviation for a state name.
## outcome is one of: heart attack, heart failure, pneumonia
##
best <- function(state, outcome) {
  library(base)
  library(stats)
  library(dplyr)
  library(data.table)
  library(dplyr)
  setwd("C:/Users/edherrin/OneDrive/datascience/datasciencecoursera/R_Prog_Assignment3")
  best_dt <- data.table::fread("outcome-of-care-measures.csv")

  # Read outcome data
  out_dt <- data.table::fread('outcome-of-care-measures.csv')

  outcome <- tolower(outcome)

  # Column name is same as variable so changing it 
  chosen_state <- state 

  # validate input
  if (!state %in% unique(best_dt[["State"]])) {
    stop("Invalid Input: State")
  }
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("Invalid Input: outcome (Must be one of: heart attack, 
         heart failure, pneumonia")
  }
  # rename columns
  setnames(out_dt
           , tolower(sapply(colnames(out_dt), gsub, pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", replacement = "" ))
  )
  # filter by state
  out_dt <- out_dt[state == chosen_state]
  col_indices <- grep(paste0("hospital name|state|^",outcome), colnames(out_dt))
  # filter data 
  out_dt <- out_dt[, .SD ,.SDcols = col_indices]
  out_dt[, outcome] <- out_dt[,  as.numeric(get(outcome))]
  # remove missing values
  out_dt <- out_dt[complete.cases(out_dt),]
  # order by outcome 
  out_dt <- out_dt[order(get(outcome), `hospital name`)]
  
  return(out_dt[, "hospital name"][1])
  
}