#' Calculate match score generic.
#'
#' \code{scoreFamilies} returns a \code{\link{Scores}} object with the \code{Neighbors}
#' slot populated with a match score for each potential duplicate found from the BSN algorithm.
#' A match score is calculated by averaging the number of string/binary variable matches
#' consistent between the two records plus a similarity measure for the continuous variables.
#'
#' @param object object containing pre-computed Neighbors from the BSN algorithm.
#' @param familyDat data matrix containing family data so score.
#' @param selfID character referencing the column in familyDat which points to a self's ID.
#' @param motherID character referencing the column in familyDat which points to a self's mother's ID
#' @param fatherID character referencing the column in familyDat which points to a self's father's ID
#' @return An object of class \code{\link{Scores}} containing the scored neighbor pairs. 
#'
#' @export
scoreFamilies <- function(object, familyDat, selfID, motherID, fatherID) {
    UseMethod("scoreFamilies", object)
}

#' @rdname scoreFamilies
#' @export 
scoreFamilies.default <- function(object, familyDat, selfID, motherID, fatherID) {
    print("Scoring allowable on Blocks and Scores objects only.")
    print("Please first block the Neighbors object.")
    return(NULL)
}

#' @rdname scoreFamilies
#' @export 
scoreFamilies.Blocks <- function(object, familyDat, selfID, motherID, fatherID) {
    # score selfs
    selfScore <- scoreNeighbors(object)$Neighbors[, "matchScore"]
    # score mothers
    mothersObj <- object
    newDat <- suppressWarnings(merge(object$rawData[, c(object[["ID"]], motherID)], familyDat, by.x = c(object[["ID"]], motherID), 
        by.y = c(object[["ID"]], selfID), all.x = TRUE, suffixes = c("old", "new"))[, -2])
    mothersObj[["rawData"]] <- newDat
    motherScore <- scoreNeighbors(mothersObj)$Neighbors[, "matchScore"]
    # score fathers
    fathersObj <- object
    newDat <- suppressWarnings(merge(object$rawData[, c(object[["ID"]], fatherID)], familyDat, by.x = c(object[["ID"]], fatherID), 
        by.y = c(object[["ID"]], selfID), all.x = TRUE, suffixes = c("old", "new"))[, -2])
    fathersObj[["rawData"]] <- newDat
    fatherScore <- scoreNeighbors(fathersObj)$Neighbors[, "matchScore"]
    # score fathers fathers
    ffathersObj <- fathersObj
    newDat <- suppressWarnings(merge(fathersObj$rawData[, c(object[["ID"]], fatherID)], familyDat, by.x = c(object[["ID"]], fatherID), 
        by.y = c(object[["ID"]], selfID), all.x = TRUE, suffixes = c("old", "new"))[, -2])
    ffathersObj[["rawData"]] <- newDat
    ffatherScore <- scoreNeighbors(ffathersObj)$Neighbors[, "matchScore"]
    # score fathers mothers
    fmothersObj <- fathersObj
    newDat <- suppressWarnings(merge(fathersObj$rawData[, c(object[["ID"]], motherID)], familyDat, by.x = c(object[["ID"]], motherID), 
        by.y = c(object[["ID"]], selfID), all.x = TRUE, suffixes = c("old", "new"))[, -2])
    fmothersObj[["rawData"]] <- newDat
    fmotherScore <- scoreNeighbors(fmothersObj)$Neighbors[, "matchScore"]
    # score mothers fathers
    mfathersObj <- mothersObj
    newDat <- suppressWarnings(merge(mothersObj$rawData[, c(object[["ID"]], fatherID)], familyDat, by.x = c(object[["ID"]], fatherID), 
        by.y = c(object[["ID"]], selfID), all.x = TRUE, suffixes = c("old", "new"))[, -2])
    mfathersObj[["rawData"]] <- newDat
    mfatherScore <- scoreNeighbors(mfathersObj)$Neighbors[, "matchScore"]
    # score mothers mothers
    mmothersObj <- mothersObj
    newDat <- suppressWarnings(merge(mothersObj$rawData[, c(object[["ID"]], motherID)], familyDat, by.x = c(object[["ID"]], motherID), 
        by.y = c(object[["ID"]], selfID), all.x = TRUE, suffixes = c("old", "new"))[, -2])
    mmothersObj[["rawData"]] <- newDat
    mmotherScore <- scoreNeighbors(mmothersObj)$Neighbors[, "matchScore"]
    # score everything else this is time consuming - any better ideas?
    allScore <- sapply(1:nrow(object$Neighbors), function(id) {
        tsc <- try({
            fam1 <- familyDat[familyDat$requestId == object$Neighbors[id, 1], ]
            fam2 <- familyDat[familyDat$requestId == object$Neighbors[id, 2], ]
            self1 <- fam1[fam1[, selfID] == 1, ]  # find self
            fam1 <- fam1[fam1[, selfID] != 1, ]  # remove self
            mother1 <- fam1[fam1[, selfID] == self1[, motherID], ]  # find mother
            fam1 <- fam1[fam1[, selfID] != self1[, motherID], ]  # remove mother
            fam1 <- fam1[fam1[, selfID] != mother1[, motherID], ]  # remove mothers mother
            fam1 <- fam1[fam1[, selfID] != mother1[, fatherID], ]  # remove mothers father
            father1 <- fam1[fam1[, selfID] == self1[, fatherID], ]  # find father
            fam1 <- fam1[fam1[, selfID] != self1[, fatherID], ]  # remove father
            fam1 <- fam1[fam1[, selfID] != father1[, motherID], ]  # remove fathers mother
            fam1 <- fam1[fam1[, selfID] != father1[, fatherID], ]  # remove fathers father
            self2 <- fam2[fam2[, selfID] == 1, ]  # repeat for comparison family
            fam2 <- fam2[fam2[, selfID] != 1, ]  # remove self
            mother2 <- fam2[fam2[, selfID] == self2[, motherID], ]  # find mother
            fam2 <- fam2[fam2[, selfID] != self2[, motherID], ]  # remove mother
            fam2 <- fam2[fam2[, selfID] != mother2[, motherID], ]  # remove mothers mother
            fam2 <- fam2[fam2[, selfID] != mother2[, fatherID], ]  # remove mothers father
            father2 <- fam2[fam2[, selfID] == self2[, fatherID], ]  # find father
            fam2 <- fam2[fam2[, selfID] != self2[, fatherID], ]  # remove father
            fam2 <- fam2[fam2[, selfID] != father2[, motherID], ]  # remove fathers mother
            fam2 <- fam2[fam2[, selfID] != father2[, fatherID], ]  # remove fathers father
            greedyConcordance(fam1, fam2, object$keyVars)  # try to concord
        }, silent = TRUE)
        ifelse(class(tsc) == "try-error", 0, tsc)
    })
    scoreVec <- selfScore + motherScore + fatherScore + ffatherScore + fmotherScore + mfatherScore + mmotherScore + allScore
    object <- set(object, "scoreVec", scoreVec)
    return(object)
}

#' @rdname scoreFamilies
greedyConcordance <- function(fam1, fam2, keyVars) {
    numericVars <- keyVars[keyVars[, "keyType"] == "numeric", "keyVars"]
    numericWts <- as.numeric(keyVars[keyVars[, "keyType"] == "numeric", "keyWt"])
    matchVars <- keyVars[keyVars[, "keyType"] != "numeric", "keyVars"]
    matchWts <- as.numeric(keyVars[keyVars[, "keyType"] != "numeric", "keyWt"])
    numericData1 <- fam1[, numericVars]
    numericData2 <- fam2[, numericVars]
    matchData1 <- fam1[, matchVars]
    matchData2 <- fam2[, matchVars]
    score <- 0
    for (i in 1:nrow(fam1)) {
        s1match <- data.frame(matrix(rep(matchData1[i, ], each = nrow(matchData2)), nrow = nrow(matchData2)))
        s2match <- data.frame(as.matrix(matchData2, nrow = nrow(matchData2)))
        s1numeric <- data.frame(matrix(rep(numericData1[i, ], each = nrow(numericData2)), nrow = nrow(numericData2)))
        s2numeric <- data.frame(as.matrix(numericData2, nrow = nrow(numericData2)))
        idx <- !is.na(s1match) & !is.na(s2match)  # score match variables
        if (any(rowSums(idx) > 0)) {
            diff <- data.matrix(s1match[idx]) - data.matrix(s2match[idx])
            matchScr <- ifelse(diff == 0, 1, 0)
            matchScr <- matchScr * matrix(rep(matchWts, each = nrow(matchScore)), nrow = nrow(matchScore))
        }
        idy <- !is.na(s1numeric) & !is.na(s2numeric)  # score numeric variables
        if (any(rowSums(idy) > 0)) {
            diff <- data.matrix(s1numeric[idy]) - data.matrix(s2numeric[idy])
            numericScr <- ifelse(diff == 0, 1, (1/(abs(diff)+1)))
            numericScr <- numericScore * matrix(rep(numericWts, each = nrow(numericScore)), nrow = nrow(numericScore))
        }
        if (nrow(s2match) == 1) {
            score <- (sum(matchScr, na.rm = TRUE) + sum(numericScr, na.rm = TRUE))/length(c(numericVars, matchVars))
            break
        } else {
            tempScr <- rowSums(matchScr, na.rm = TRUE) + rowSums(numericScr, na.rm = TRUE)
            id <- which.max(sc)
            score <- score + max(sc)/length(c(numericVars, matchVars))
            matchData2 <- matchData2[-id, ]
        }
    }
    return(score)
}
