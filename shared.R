rankedByState <- function(state=NULL, outcome) {
        
        data <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")
        cols <- list(
                "heart attack" = 11L,
                "heart failure" = 17L,
                "pneumonia" = 23L
        )
        allowedStates <- sort(unique(data$State))
        allowedOutcomes <- names(cols)
        
        if (!is.null(state) && !is.element(state, allowedStates)) stop("invalid state")       
        if (!is.element(outcome, allowedOutcomes)) stop("invalid outcome")
        
        subsetForOutcome <- function() {
                d <- data.frame(
                        data[,2],
                        data[,7],
                        suppressWarnings(
                                as.numeric(
                                        data[,cols[[outcome]]]
                                )
                        )
                )
                names(d) <- c("name","state", "score")
                if (!is.null(state)) return(d[!is.na(d$score) & d$state==state, ])
                else return(d[!is.na(d$score), ])
                
        }
        
        s <- subsetForOutcome()
        s[with(s, order(score, name)),]

}