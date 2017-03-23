#' Duplicates object.
#'
#' Stores the results from calling \code{\link{deDuplicate}}.
#'
#' @param ScoresObj \code{\link{Scores}} object.

#' @param dupsList A list with entries for each unique ID. This is a map
#' from the duplicate IDs to the unique IDs.
#' @param details Details to be printed regarding the thresholding and ordering
#' @return An object of class \code{\link{Duplicates}}. 
Duplicates <- function(ScoresObj, dupsList, details) {
    DupsObj <- ScoresObj
    DupsObj[["dupsList"]] <- dupsList
    DupsObj[["details"]] <- details
    class(DupsObj) <- "Duplicates"
    return(DupsObj)
}
