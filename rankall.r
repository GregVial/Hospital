rankall <- function(outcome, num = "best") {
  ## Read outcome data
  file <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  ## Check that state and outcome are valid
  valStates<-sort(unique(file[,7]))
  #print(length(valStates))
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
  ## Converts appropriate rows into numeric
  if(outcome == "heart attack") col<-11
  if(outcome == "heart failure") col<-17
  if(outcome == "pneumonia") col<-23
  file[, col] <- as.numeric(file[, col])
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  resState<-vector()
  resName<-vector()
  for (state in valStates){
    subFile<-file[file$State==state,]
    res<-data.frame(subFile[[col]],subFile[[2]],subFile[[7]],stringsAsFactors = F)
    death<-res[,1]
    name<-res[,2]
    res<-res[order(death,name),]
    if (num=="best") row <- 1
    else if (num=="worst") row <- nrow(res)-sum(is.na(death))
    else row <- num
    if(row<=nrow(res)) {
      resState<-c(resState,state)
      resName<-c(resName,res[row,2])
    } else {
      resState<-c(resState,state)
      resName<-c(resName,NA)
    }
  }
  #rank <- cbind(resName,resState)
  rank <- data.frame(resName,resState)
  rank <- rank[order(resState),]
  colnames(rank)<-c("hospital","state")
  #rownames(rank)<-sort(resState)
  rank
}