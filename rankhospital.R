source("shared.R")

rankhospital <- function(state, outcome, num="best") {
        
        ranked <- rankedByState(state, outcome)
        size <- nrow(ranked)
        
        if (num == "best") return(ranked[1,]$name)
        else if (num == "worst") return(ranked[size, ]$name)
        else if (num > size) return(NA)
        else return(ranked[num,]$name)
        
}
