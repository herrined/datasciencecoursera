## These functions use data from the Hospital Compare
## web site http://hospitalcompare.hhs.gov run by the
## U.S. Dept of Health & Human Services. The information
## is related to the quality of care at over 4,000
## Medicare-certified hospitals in the U.S.

## This function finds the highest ranking hospital in each
## state for a given outcome.
## Usage: rankall(outcome, num = "best")
## outcome is one of: heart attack, heart failure, pneumonia
## num can be either a number, or "best", "worst"
##
rankall <- function(outcome, num = "best") {
  library(stats)
  library(dplyr)
  library(data.table)
  library(dplyr)
  
  setwd("C:/Users/edherrin/OneDrive/datascience/datasciencecoursera/R_Prog_Assignment3")
  best_dt <- data.table::fread("outcome-of-care-measures.csv")
  
  outcome <- tolower(outcome)
  # validate input
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("Invalid Input: outcome (Must be one of: heart attack, 
         heart failure, pneumonia")
  }
  setnames(best_dt
           , tolower(sapply(colnames(best_dt), gsub, pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", replacement = "" ))
  )
  col_indices <- grep(paste0("hospital name|state|^",outcome), colnames(best_dt))
  # filter data 
  best_dt <- best_dt[, .SD ,.SDcols = col_indices]
  # Change outcome column class
  best_dt[, outcome] <- best_dt[,  as.numeric(get(outcome))]
  if (num == "best"){
    return(best_dt[order(state, get(outcome), `hospital name`)
                  , .(hospital = head(`hospital name`, 1))
                  , by = state])
  }
  if (num == "worst"){
    return(best_dt[order(get(outcome), `hospital name`)
                  , .(hospital = tail(`hospital name`, 1))
                  , by = state])
  }
  
  return(best_dt[order(state, get(outcome), `hospital name`)
                , head(.SD,num)
                , by = state, .SDcols = c("hospital name") ])
}