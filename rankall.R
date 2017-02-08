source("shared.R")

rankall <- function(outcome, num = "best") {

        ranked <- rankedByState(outcome = outcome)
        ## For each state, find the hospital of the given rank

        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name

        byState <- split(ranked, ranked$state)
        
        if (num == "best") idx <- 1
        else if (num == "worst") idx <- nrow(byState[[1]])
        else idx <- num
        
        result <- data.frame(byState[[1]][idx,])
        for(i in 2:length(byState)) {
                if (num == "worst") idx <- nrow(byState[[i]])
                state <- byState[[i]][idx,]
                if (!is.na(state$state)) {
                        result <- rbind(result, state)
                }
        }
        
        allStates <- levels(byState[[1]]$state)
        resultStates <- result$state
        naStates <- setdiff(allStates, resultStates)
        
        for(nas in naStates) {
                r <- data.frame(name=NA, state=nas, score=NaN)
                result <- rbind(result, r)
        }
        
        result[with(result, order(state, name, na.last = TRUE)),][,c(1,2)]
        
        

}
