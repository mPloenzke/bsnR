#' Neighbors object.
#'
#' Stores the input data and parameters for calling \code{blockedSN}.
#'
#' @param dat Input data matrix used in the BSN algorithm.
#' @param ID Character or numeric value. Identifing column in dat.
#' This is the ID variable for which \code{Neighbors} will reference.
#' @param keyVars Character or numeric vector. Which variables to 
#' include when generating sort keys?
#' @return An object of class \code{\link{Neighbors}}. 
#'
#' @export
Neighbors <- function(dat, ID, keyVars) {
    if (missing(keyVars)) {
        keyVars <- colnames(dat)
    }
    keyVars <- keyVars[keyVars != ID]
    keyMat <- matrix(data = NA, nrow = length(keyVars), ncol = 3)
    colnames(keyMat) <- c("keyVars", "keyType", "keyWt")
    keyMat[, "keyVars"] <- keyVars
    keyMat[, "keyWt"] <- rep(1, nrow(keyMat))
    for (i in 1:nrow(keyMat)) {
        if (length(unique(dat[, keyMat[i, "keyVars"]])) < 2) {
            keyMat[i, "keyType"] <- "remove"
        } else if (length(unique(dat[, keyMat[i, "keyVars"]])) == 2) {
            keyMat[i, "keyType"] <- "binary"
        } else if (is.na(suppressWarnings(as.numeric(dat[, keyMat[i, "keyVars"]])[1]))) {
            keyMat[i, "keyType"] <- "string"
        } else {
            keyMat[i, "keyType"] <- "numeric"
        }
    }
    keyMat <- keyMat[keyMat[, "keyType"] != "remove", ]
    NeighborsObj <- list(rawData = dat, ID = ID, keyVars = keyMat)
    class(NeighborsObj) <- "Neighbors"
    return(NeighborsObj)
}
