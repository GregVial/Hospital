best <- function(state, outcome) {
  ## Read outcome data
  file <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  ## Check that state and outcome are valid
  valOutcomes<-c("heart attack","heart failure","pneumonia")
  valStates<-unique(file[,7])
  if(is.na(match(state,valStates))) {
    errMessage <- "invalid state"
    stop(errMessage)
  }
  if(is.na(match(outcome,valOutcomes))){
    errMessage <- "invalid outcome"
    stop(errMessage)
  }
  ## Subset the file to retain only the state we are interested in
  subFile<-file[file$State==state,]
  
  #print(dim(subFile))
  ## Converts appropriate rows into numeric
  if(outcome == "heart attack") col<-11
  if(outcome == "heart failure") col<-17
  if(outcome == "pneumonia") col<-23
  subFile[, col] <- as.numeric(subFile[, col])
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  good<-complete.cases(subFile)
  min<-min(subFile[,col],na.rm=T)
  result<- subFile[good & subFile[,col]==min,]
  SubResult<-sort(result[2])
  SubResult[[1]]
}