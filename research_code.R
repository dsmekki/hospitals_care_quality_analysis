## Let's read the data in and explore it to know the type of data and type of
## varibales we're dealing with. 

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
head(outcome[, 11])
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])

## Secondly, we create a function called "best" to find the best hospital in a state

best <- function(state, outcome) {
  if(getwd() != "C:/Users/m7mek/Documents/hospitals_care_quality_analysis") {
    setwd("C:/Users/m7mek/Documents/hospitals_care_quality_analysis")
  }
  outcome_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # check for the validity of state input
  if(!(state %in% unique(outcome_df[, 7]))) {
    stop("invalid state")
  }
  # check for the valdity of outcome input
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  } else if(outcome == "heart attack") {
    test_list <- split(outcome_df, outcome_df[, 7])
    test_df <- test_list[[state]]
    test_df[,11] <- as.numeric(test_df[,11])
    return(test_df[with(test_df, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name))[1], 2])
    
  } else if(outcome == "heart failure") {
    test_list <- split(outcome_df, outcome_df[, 7])
    test_df <- test_list[[state]]
    test_df[,17] <- as.numeric(test_df[,17])
    return(test_df[with(test_df, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name))[1], 2])
    
  } else if(outcome == "pneumonia") {
    test_list <- split(outcome_df, outcome_df[, 7])
    test_df <- test_list[[state]]
    test_df[,23] <- as.numeric(test_df[,23])
    return(test_df[with(test_df, order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name))[1], 2])
    
  }
  
}



## Thirdly, we create a function called "rankhospital" to find the specified ranked hospital in a state

rankhospital <- function(state, outcome, num) {
  if(getwd() != "C:/Users/m7mek/Documents/hospitals_care_quality_analysis") {
    setwd("C:/Users/m7mek/Documents/hospitals_care_quality_analysis")
  }
  outcome_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # check for the validity of state input
  if(!(state %in% unique(outcome_df[, 7]))) {
    stop("invalid state")
  }
  # check for the valdity of outcome input
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  } else if(outcome == "heart attack") {
    
    if(all((is.numeric(num) & (length(num) == 1)) | (num %in% c("best", "worst")))) {
      test_list <- split(outcome_df, outcome_df[, 7])
      test_df <- test_list[[state]]
      test_df[,11] <- as.numeric(test_df[,11])
      test_df <- test_df[complete.cases(test_df[,11]), ]
      if(num == "best"){
        num <- 1
      } else if(num == "worst") {
        num <- nrow(test_df)
      } else if(num > nrow(test_df)) {
        return(NA)
      }
      return(test_df[with(test_df, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name))[num], 2])
    } else {
      stop("invalid num")
    }
    
  } else if(outcome == "heart failure") {
    if(all((is.numeric(num) & (length(num) == 1)) | (num %in% c("best", "worst")))) {
      test_list <- split(outcome_df, outcome_df[, 7])
      test_df <- test_list[[state]]
      test_df[,17] <- as.numeric(test_df[,17])
      test_df <- test_df[complete.cases(test_df[,17]), ]
      if(num == "best"){
        num <- 1
      } else if(num == "worst") {
        num <- nrow(test_df)
      } else if(num > nrow(test_df)) {
        return(NA)
      }
      return(test_df[with(test_df, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name))[num], 2])
    } else {
      stop("invalid num")
    }
    
  } else if(outcome == "pneumonia") {
    if(all((is.numeric(num) & (length(num) == 1)) | (num %in% c("best", "worst")))) {
      test_list <- split(outcome_df, outcome_df[, 7])
      test_df <- test_list[[state]]
      test_df[,23] <- as.numeric(test_df[,23])
      test_df <- test_df[complete.cases(test_df[,23]), ]
      if(num == "best"){
        num <- 1
      } else if(num == "worst") {
        num <- nrow(test_df)
      } else if(num > nrow(test_df)) {
        return(NA)
      }
      return(test_df[with(test_df, order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name))[num], 2])
    } else {
      stop("invalid num")
    }
    
  }
  
}



## Fourthly, we create a function called "rankall" to rank all hospitals in states depending on an outcome

rankall <- function(outcome, num = 1) {
  if(getwd() != "C:/Users/m7mek/Documents/hospitals_care_quality_analysis") {
    setwd("C:/Users/m7mek/Documents/hospitals_care_quality_analysis")
  }
  
  outcome_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors = FALSE)
  container_df <- data.frame(hospital = character(), state = character(), stringsAsFactors = FALSE)
  passed_entry <- 1
  
  # check for the valdity of outcome input
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  } else if(outcome == "heart attack") {
    
    if(all((is.numeric(num) & (length(num) == 1)) | (num %in% c("best", "worst")))) {
      test_list <- split(outcome_df, outcome_df[, 7])
      test_list <- test_list[with(test_list, order(names(test_list)))]
      for(states in seq_along(names(test_list))){
        test_df <- test_list[[states]]
        test_df[,11] <- as.numeric(test_df[,11])
        test_df <- test_df[complete.cases(test_df[,11]), ]
        if(nrow(test_df) == 0) {
          passed_entry <- list(NA, names(test_list)[states])
        } else {
          if(num == "best"){
            sorted_test_df <- test_df[with(test_df, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name)), ]
            returned_entry <- head(sorted_test_df,1)
            hospital_vc <- returned_entry[,2]
            state_vc <- returned_entry[,7]
            passed_entry <- list(hospital_vc, state_vc) #the resulted data frame index will not be like the required in the assignment
            container_df[nrow(container_df) + 1, ] <- passed_entry
          } else if(num == "worst") {
            sorted_test_df <- test_df[with(test_df, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name)), ]
            returned_entry <- tail(sorted_test_df,1)
            hospital_vc <- returned_entry[,2]
            state_vc <- returned_entry[,7]
            passed_entry <- list(hospital_vc, state_vc) #the resulted data frame index will not be like the required in the assignment
            container_df[nrow(container_df) + 1, ] <- passed_entry
          } else if(is.numeric(num) & num > nrow(test_df)) {
            passed_entry <- list(NA, names(test_list)[states]) #the resulted data frame index will not be like the required in the assignment
            container_df[nrow(container_df) + 1, ] <- passed_entry
          } else {
            returned_entry <- test_df[with(test_df, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name))[num], ]
            hospital_vc <- returned_entry[,2]
            state_vc <- returned_entry[,7]
            passed_entry <- list(hospital_vc, state_vc) #the resulted data frame index will not be like the required in the assignment
            container_df[nrow(container_df) + 1, ] <- passed_entry
          }
        }
      }
      container_df <- container_df[complete.cases(container_df$state),]
      return(container_df)
    } else {
      stop("invalid num")
    }
    
  } else if(outcome == "heart failure") {
    
    if(all((is.numeric(num) & (length(num) == 1)) | (num %in% c("best", "worst")))) {
      test_list <- split(outcome_df, outcome_df[, 7])
      test_list <- test_list[with(test_list, order(names(test_list)))]
      for(states in seq_along(names(test_list))){
        test_df <- test_list[[states]]
        test_df[,17] <- as.numeric(test_df[,17])
        test_df <- test_df[complete.cases(test_df[,17]), ]
        if(nrow(test_df) == 0) {
          passed_entry <- list(NA, names(test_list)[states])
        } else {
          if(num == "best"){
            sorted_test_df <- test_df[with(test_df, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)), ]
            returned_entry <- head(sorted_test_df,1)
            hospital_vc <- returned_entry[,2]
            state_vc <- returned_entry[,7]
            passed_entry <- list(hospital_vc, state_vc) #the resulted data frame index will not be like the required in the assignment
            container_df[nrow(container_df) + 1, ] <- passed_entry
          } else if(num == "worst") {
            sorted_test_df <- test_df[with(test_df, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)), ]
            returned_entry <- tail(sorted_test_df,1)
            hospital_vc <- returned_entry[,2]
            state_vc <- returned_entry[,7]
            passed_entry <- list(hospital_vc, state_vc) #the resulted data frame index will not be like the required in the assignment
            container_df[nrow(container_df) + 1, ] <- passed_entry
          } else if(is.numeric(num) & num > nrow(test_df)) {
            passed_entry <- list(NA, names(test_list)[states]) #the resulted data frame index will not be like the required in the assignment
            container_df[nrow(container_df) + 1, ] <- passed_entry
          } else {
            returned_entry <- test_df[with(test_df, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name))[num], ]
            hospital_vc <- returned_entry[,2]
            state_vc <- returned_entry[,7]
            passed_entry <- list(hospital_vc, state_vc) #the resulted data frame index will not be like the required in the assignment
            container_df[nrow(container_df) + 1, ] <- passed_entry
          }
        }
      }
      container_df <- container_df[complete.cases(container_df$state),]
      return(container_df)
    } else {
      stop("invalid num")
    }
    
  } else if(outcome == "pneumonia") {
    if(all((is.numeric(num) & (length(num) == 1)) | (num %in% c("best", "worst")))) {
      test_list <- split(outcome_df, outcome_df[, 7])
      test_list <- test_list[with(test_list, order(names(test_list)))]
      for(states in seq_along(names(test_list))){
        test_df <- test_list[[states]]
        test_df[,23] <- as.numeric(test_df[,23])
        test_df <- test_df[complete.cases(test_df[,23]), ]
        if(nrow(test_df) == 0) {
          passed_entry <- list(NA, names(test_list)[states])
        } else {
          if(num == "best") {
            sorted_test_df <- test_df[with(test_df, order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name)), ]
            returned_entry <- head(sorted_test_df,1)
            hospital_vc <- returned_entry[,2]
            state_vc <- returned_entry[,7]
            passed_entry <- list(hospital_vc, state_vc) #the resulted data frame index will not be like the required in the assignment
            container_df[nrow(container_df) + 1, ] <- passed_entry
          } else if(num == "worst") {
            sorted_test_df <- test_df[with(test_df, order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name)), ]
            returned_entry <- tail(sorted_test_df,1)
            hospital_vc <- returned_entry[,2]
            state_vc <- returned_entry[,7]
            passed_entry <- list(hospital_vc, state_vc) #the resulted data frame index will not be like the required in the assignment
            container_df[nrow(container_df) + 1, ] <- passed_entry
          } else if(is.numeric(num) & num > nrow(test_df)) {
            passed_entry <- list(NA, names(test_list)[states]) #the resulted data frame index will not be like the required in the assignment
            container_df[nrow(container_df) + 1, ] <- passed_entry
          } else {
            returned_entry <- test_df[with(test_df, order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name))[num], ]
            hospital_vc <- returned_entry[,2]
            state_vc <- returned_entry[,7]
            passed_entry <- list(hospital_vc, state_vc) #the resulted data frame index will not be like the required in the assignment
            container_df[nrow(container_df) + 1, ] <- passed_entry
          }
        }
      }
      container_df <- container_df[complete.cases(container_df$state),]
      return(container_df)
    } else {
      stop("invalid num")
    }
    
  }
  
}