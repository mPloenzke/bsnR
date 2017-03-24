#' deDuplicate scored neighbors matrix.
#'
#' @param object \code{\link{Scores}} object containing pre-computed Neighbors and match scores.
#' @param thresh The quantile at which to classify scores as duplicates.
#' @param priority A list of structure (Var= 'Varx', DESC=TRUE) with 'Varx'
#' being either a numeric or character value corresponding to a column in rawData.
#' This parameter determines how to sort the duplicates. 
#' DESC is a logical for this ordering (descending).
#' @return An object of class \code{\link{Duplicates}} containing the scored neighbor pairs. 
#'
#' @export
deDuplicate <- function(object, thresh, priority) {
    UseMethod("deDuplicate", object)
}

#' @rdname deDuplicate
#' @export 
deDuplicate.default <- function(object, thresh, priority) {
    print("deDuplication allowable on Scores objects only.")
    return(NULL)
}

#' @rdname deDuplicate
#' @export 
deDuplicate.Scores <- function(object, thresh, priority) {
    if (missing(thresh)) {
        thresh <- 0.99
    }
    if (missing(priority)) {
        priority <- list(VAR = object[["ID"]], DESC = TRUE)
    }
    scoresMat <- object$Neighbors
    threshold <- as.numeric(thresh)
    thresh <- stats::quantile(as.numeric(scoresMat[, "matchScore"]), probs = as.numeric(thresh), na.rm = TRUE)
    dups <- which(scoresMat[, "matchScore"] > thresh)
    dupsIDs <- scoresMat[dups, ]
    hasDup <- unique(c(dupsIDs[, 1], dupsIDs[, 2]))
    oldIDs <- unique(object[["rawData"]][, object[["ID"]]])
    newIDs <- list()
    newIDs[["dupEntity1"]] <- list(uniqueID = "dupEntity1", children = c(dupsIDs[1, 1], dupsIDs[1, 2]), pair1 = list(children = c(dupsIDs[1, 
        1], dupsIDs[1, 2]), matchScore = dupsIDs[1, 4], background = dupsIDs[1, 3], uniqueID = "dupEntity1"))
    maxCount <- 2
    for (i in 2:nrow(dupsIDs)) {
        new <- FALSE
        id1 <- dupsIDs[i, 1]
        id2 <- dupsIDs[i, 2]
        for (j in 1:length(newIDs)) {
            notnew1 <- id1 %in% newIDs[[j]]$children
            notnew2 <- id2 %in% newIDs[[j]]$children
            if (notnew1 | notnew2) {
                newIDs[[paste0("dupEntity", j)]][["children"]] <- unique(c(newIDs[[j]]$children, id1, id2))
                ln <- length(newIDs[[paste0("dupEntity", j)]]) - 1
                newIDs[[paste0("dupEntity", j)]][[paste0("pair", ln)]] <- list(children = c(id1, id2), matchScore = dupsIDs[i, 4], 
                  background = dupsIDs[i, 3], uniqueID = paste0("dupEntity", j))
                maxCount <- max(maxCount, length(newIDs[[j]]$children))
            } else {
                new <- TRUE
            }
        }
        if (new) {
            newIDs[[paste0("dupEntity", (length(newIDs)+1))]] <- list(uniqueID = paste0("dupEntity", (length(newIDs)+1)), children = c(id1, id2), pair1 = list(children = c(id1, 
                id2), matchScore = dupsIDs[i, 4], background = dupsIDs[i, 3], uniqueID = paste0("dupEntity", (length(newIDs)+1))))
        }
    }
    dataList <- lapply(newIDs, function(i) {
        tdat <- merge(object$rawData, i$children, by.x = object$ID, by.y = 1, all.y = TRUE)
        tdat <- tdat[order(tdat[, priority$VAR], decreasing = priority$DESC), ]
        cbind(tdat, uniqueID = rep(i$uniqueID, nrow(tdat)))
    })
    object <- Duplicates(object, dataList, c(threshold, thresh, priority))
    return(object)
}
