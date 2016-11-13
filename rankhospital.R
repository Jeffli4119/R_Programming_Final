# set working directory, take care of this one  
setwd("/Users/jingzhili/Downloads/Week4")

# read csv file 
all_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")


rankhospital <- function(state, outcome, num = "best") {
        
        # read heart attack data, convert character to numeric data 
        all_data[, 11] <- as.numeric(all_data[, 11]) 
        # read heart failure data, convert character to numeric data 
        all_data[, 17] <- as.numeric(all_data[, 17]) 
        # read pneumonia data, convert character to numeric data 
        all_data[, 23] <- as.numeric(all_data[, 23]) 
        
        
        # list of all valid disease 
        valid_disease <- c("heart attack", "heart failure", "pneumonia")
        # list of all valid states
        valid_state <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID","IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS","MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
        
        # get hospital names
        find_hospital_rank <- function(all_data, number, state, rank) {
                
                # catch specific state's data, like "AL" "MD"
                specific_state <- all_data[all_data[, 7] == state, ]
                
                # catch disease's data in specific state 
                # (11 = heart attack, 17 = heart failure, 23 = pneumonia)
                specific_disease_state <- specific_state[, number]
                
                len <- dim(specific_state[!is.na(specific_disease_state),])
                
                # find the minium data in specific state and disease
                min <- min(specific_disease_state, na.rm=T)
                
                # find which number is the minium one 
                index <- which(specific_disease_state == min)
                
                # output the specific hostital
                hospital <- specific_state[index, 2]
                
                return(hospital)
                
        }
        
        
        
        # output invalid state when typo on state
        if (!state %in% valid_state) {
                stop("invalid state")
        } 
        
        # output invalid outcome when typo on disease
        else 
                if(!outcome %in% valid_disease) {
                        stop("invalid outcome")
                }
        
        # get best hospital for heart attack
        else 
                
                if(outcome == "heart attack") {
                        hospital <- find_hospital(all_data, 11, state)
                }
        
        # get best hospital for heart failure               
        else 
                if(outcome == "heart failure") {
                        hospital <- find_hospital(all_data, 17, state)
                } 
        
        # get best hospital for pneumonia 
        else 
                if(outcome == "pneumonia"){
                        hospital <- find_hospital(all_data, 23, state)
                }
        
        
        # output results
        return(hospital)
        
}

