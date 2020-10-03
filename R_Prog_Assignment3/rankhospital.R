## These functions use data from the Hospital Compare
## web site http://hospitalcompare.hhs.gov run by the
## U.S. Dept of Health & Human Services. The information
## is related to the quality of care at over 4,000
## Medicare-certified hospitals in the U.S.

## This function finds the rank of a hospital within a state
## for a given outcome.
## Usage: rankhospital(state, outcome, num)
## state is the 2-letter abbreviation for a state name.
## outcome is one of: heart attack, heart failure, pneumonia
## num can be either a number, or "best", "worst"
##
rankhospital <- function(state, outcome, num = "best") {
  options(warn = -1)
  library(base)
  library(stats)
  library(dplyr)
  library(data.table)
  library(dplyr)
  
  setwd("C:/Users/edherrin/OneDrive/datascience/datasciencecoursera/R_Prog_Assignment3")
  best_dt <- data.table::fread("outcome-of-care-measures.csv")
  
  state <- toupper(state)
  outcome <- tolower(outcome)
  
  # change column name 
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
  setnames(best_dt
           , tolower(sapply(colnames(best_dt), gsub, pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", replacement = "" ))
  )
  # filter by state
  best_dt <- best_dt[state == chosen_state]
  # select columns
  col_indices <- grep(paste0("hospital name|state|^",outcome), colnames(best_dt))
  # filter unnecessary data 
  best_dt <- best_dt[, .SD ,.SDcols = col_indices]
  
  best_dt[, outcome] <- best_dt[,  as.numeric(get(outcome))]
  # remove missing Values
  best_dt <- best_dt[complete.cases(best_dt),]
  # order by outcome 
  best_dt <- best_dt[order(get(outcome), `hospital name`)]
  best_dt <- best_dt[,  .(`hospital name` = `hospital name`, state = state, rate = get(outcome), Rank = .I)]
  
  if (num == "best"){
    return(best_dt[1,`hospital name`])
  }
  
  if (num == "worst"){
    return(best_dt[.N,`hospital name`])
  }
  
  return(best_dt[num,`hospital name`])
  
}