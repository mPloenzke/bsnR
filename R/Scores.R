#' Scores object.
#'
#' Stores the scored neighbors and parameters from calling 
#' \code{\link{scoreNeighbors}}.
#'
#' @param BlocksObj \code{\link{Blocks}} object to score.
#' @param Scores Numeric vector of match scores.
#' @return An object of class \code{\link{Scores}}. 
Scores <- function(BlocksObj, Scores) {
    ScoresObj <- set(BlocksObj, "scoreVec", Scores)
    class(ScoresObj) <- "Scores"
    return(ScoresObj)
}
