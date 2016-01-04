rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  file <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  ## Check that state and outcome are valid
  valStates<-unique(file[,7])
  valOutcomes<-c("heart attack","heart failure","pneumonia")
  valNum<-c("best","worst")
  if(is.na(match(state,valStates))) {
    errMessage <- "invalid state"
    stop(errMessage)
  }
  if(is.na(match(outcome,valOutcomes))){
    errMessage <- "invalid outcome"
    stop(errMessage)
  }
  if(is.na(match(num,valNum)) && !is.numeric(num)){
    errMessage <- "invalid num"
    stop(errMessage)
  }
  ## Subset the file to retain only the state we are interested in
  subFile<-file[file$State==state,]
  ## Converts appropriate rows into numeric
  if(outcome == "heart attack") col<-11
  if(outcome == "heart failure") col<-17
  if(outcome == "pneumonia") col<-23
  subFile[, col] <- as.numeric(subFile[, col])
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  res<-data.frame(subFile[[col]],subFile[[2]],stringsAsFactors = F)
  death<-res[,1]
  name<-res[,2]
  res<-res[order(death,name,na.last = NA),]
  #print(tail(res))
  if (num=="best") res[1,2]
  else if (num=="worst") res[nrow(res),2] 
  else res[num,2]
}