source("shared.R")

best <- function(state, outcome) {
        
        ranked <- rankedByState(state, outcome)
        as.character(ranked$name[1])
        
}
