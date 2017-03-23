#' Reveal scored neighbors matrix.
#'
#' @param object \code{\link{Duplicates}} object to assess how the deduplication went.
#' @param labels character or numeric vector containing the true match labels with 
#' rows corresponding to the raw data matrix (in object[['rawData']])
#' @param duplabels character vector containing the duplicated labels 
#' (i.e. object[['ID']] from previous steps) 
#' @return An object of class \code{\link{Neighbors}} containing the scored neighbor pairs. 
#'
#' @export
revealTruth <- function(object, labels, duplabels) {
    UseMethod("revealTruth", object)
}

#' @rdname revealTruth
#' @export 
revealTruth.default <- function(object, labels, duplabels) {
    print("Truth revealable on Duplicates objects only.")
    return(NULL)
}

#' @rdname revealTruth
#' @export 
revealTruth.list <- function(object, labels, duplabels) {
    if (length(labels) == 1) {
        labels <- object$rawData[, labels]
    }
    if (length(labels) != nrow(object$rawData)) {
        error("Labels do not fit.")
        return(NULL)
    }
    data <- object$rawData
    # true labels
    DupIDs <- table(labels)
    DupIDs <- names(DupIDs)[DupIDs > 1]
    data <- cbind(data, isDup = rep(0, nrow(data)))
    data[, "isDup"] <- ifelse(labels %in% DupIDs, 1, 0)
    # predicted labels
    pDupIDs <- do.call(c, lapply(3:length(object), function(i) {
        object[[i]][, duplabels]
    }))
    data <- cbind(data, pDup = rep(0, nrow(data)))
    data[, "pDup"] <- ifelse(data[, duplabels] %in% pDupIDs, 1, 0)
    tab <- caret::confusionMatrix(data[, "pDup"], data[, "isDup"])$table
    tab <- as.matrix(caret::confusionMatrix(tab)$byClass)
    print(tab)
    return(data)
}
