#' Reveal scored neighbors matrix.
#'
#' @param object object of class \code{\link{Duplicates}} to assess how the deduplication went.
#' @param truelabels character or numeric vector containing the true match labels with
#' rows corresponding to the raw data matrix (in object[['rawData']])
#' @param duplabels character vector containing the duplicated labels
#' (i.e. object[['ID']] from previous steps)
#' @return An object of class \code{\link{Neighbors}} containing the scored neighbor pairs.
#'
#' @export
revealTruth <- function(object, truelabels, duplabels) {
    UseMethod("revealTruth", object)
}

#' @rdname revealTruth
#' @export
revealTruth.default <- function(object, truelabels, duplabels) {
    print("Truth revealable on Duplicates objects only.")
    return(NULL)
}

#' @rdname revealTruth
#' @export
revealTruth.Duplicates <- function(object, truelabels, duplabels) {
    if (length(truelabels) == 1) {
        truelabels <- object$rawData[, truelabels]
    }
    if (length(truelabels) != nrow(object$rawData)) {
        print("truelabels do not fit. Please provide a column name in raw data corresponding to the labels or a vector or length nrow(raw data).")
        return(NULL)
    }
    data <- object$rawData
    DupIDs <- table(truelabels)
    DupIDs <- names(DupIDs)[DupIDs > 1]
    DupIDs <- lapply(1:length(DupIDs),function(i) {data[truelabels==DupIDs[i],]})
    accuracy <- matrix(data=NA,nrow=0,ncol=5)
    colnames(accuracy) <- c("ID1", "ID2", "isDup", "discovered", "pDup")
    for (ii in 1:length(DupIDs)) {
      for (jj in 2:nrow(DupIDs[[ii]])) {
        for (kk in 1:(jj-1)) {
          temp <- c(DupIDs[[ii]][kk,duplabels],DupIDs[[ii]][jj,duplabels],1)
          cc <- 0
          dd <- 0
          if (temp[1]%in%object$Neighbors[,1]) {
            bb <- which(object$Neighbors[,1]==temp[1])
            if (sum(object$Neighbors[bb,2]==temp[2])>0) {
              bb2 <- which(object$Neighbors[,2]==temp[2] & object$Neighbors[,1]==temp[1])
              dd <- as.numeric(object$Neighbors[bb2,4])>=as.numeric(object$details[[2]])
              if (object$Neighbors[bb2,3]=="0") {
                cc <- 1
                accuracy <- rbind(accuracy, c(temp,cc,as.numeric(dd)))
                next()
              }
            }
          }
          if (temp[1]%in%object$Neighbors[,2]) {
            bb <- which(object$Neighbors[,2]==temp[1])
            if (sum(object$Neighbors[bb,1]==temp[2])>0) {
              bb2 <- which(object$Neighbors[,1]==temp[2] & object$Neighbors[,2]==temp[1])
              dd <- as.numeric(object$Neighbors[bb2,4])>=as.numeric(object$details[[2]])
              if (object$Neighbors[bb2,3]=="0") {
                cc <- 1
                accuracy <- rbind(accuracy, c(temp,cc,as.numeric(dd)))
                next()
              }
            }
          }
          accuracy <- rbind(accuracy, c(temp,cc,as.numeric(dd)))
        }
      }
    }
    rows <- object$Neighbors[object$Neighbors[,"background"]=="0",c(1,2,4)]
    rows <- cbind(rows,0,1,as.numeric(as.numeric(rows[,3])>=as.numeric(object$details[[2]])))
    colnames(rows) <- c("ID1", "ID2", "Score", "isDup", "discovered", "pDup")
    accuracy <- cbind(t(apply(accuracy[,1:2], 1, sort)),accuracy[,3:5])
    colnames(accuracy) <- c("ID1", "ID2", "isDup", "discovered", "pDup")
    temp <- merge(accuracy,rows,by=c("ID1","ID2"),all=TRUE)
    acc <- as.matrix(temp[,1:6])
    acc[!stats::complete.cases(acc[,1:5]),] <- as.matrix(temp[!stats::complete.cases(acc[,1:5]),c(1,2,7:9,6)])
    colnames(acc) <- c("ID1", "ID2", "isDup", "discovered", "pDup", "Score")
    #nn <- nrow(data)
    #num.poss <- nn*(nn-1)/2
    #tab <- caret::confusionMatrix(acc[, "discovered"], acc[, "isDup"])$table
    #tab[1,1] <- num.poss - tab[1,2] - tab[2,1] - tab[2,2]
    #print("Duplicate Discovery:")
    #print(tab)
    #tab <- caret::confusionMatrix(acc[, "pDup"], acc[, "isDup"])$table
    #tab[1,1] <- num.poss - tab[1,2] - tab[2,1] - tab[2,2]
    #print("Duplicate Classification:")
    #print(tab)
    #tab <- as.matrix(caret::confusionMatrix(tab)$byClass)
    #print(tab)
    #return(data)
    return(acc)
}
