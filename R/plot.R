#' Plot Neighbors object generic.
#'
#' \code{plot} plots the contents of the object.
#'
#' @param object \code{\link{Neighbors}} oject to be summarized.
#' @param type Char, How to display the plot (eg. 'box', 'density')
#' @param ... other arguments to be passed.
#' @rdname plot
#' @export 
plot.Scores <- function(object, type, ...) {
    scores <- object[["Neighbors"]][, "matchScore"]
    bg <- object[["Neighbors"]][, "background"]
    if (type == "density") {
        if (length(unique(bg)) > 1) {
            sm::sm.density.compare(as.numeric(scores), as.factor(bg), xlab = "Match Score", ...)
            abline(v = object[["details"]][[1]], col = "red")
            title(main = "Match Score Density by Discovery Method")
            legend("right", c("BSN", "Random"), fill = 2:3)
        } else {
            sm::sm.density(as.numeric(scores), ...)
            abline(v = object[["details"]][[1]], col = "red")
            title(main = "Match Score Density from BSN")
        }
    } else if (type == "box") {
        if (length(unique(bg)) > 1) {
            boxplot(as.numeric(scores) ~ as.factor(bg), xlab = "Discovery Method", ylab = "Match Score", main = "Match Score by Discovery Method", 
                ...)
            abline(h = object[["details"]][[1]], col = "red")
        } else {
            boxplot(as.numeric(scores), ylab = "Match Score", main = "Match Score from BSN", ...)
            abline(h = object[["details"]][[1]], col = "red")
        }
    } else {
        print("Please provide a valid type argmument.")
    }
    
}

#' @rdname plot
#' @export 
plot.Duplicates <- function(object, type, ...) {
    scores <- object[["Neighbors"]][, "matchScore"]
    thresh <- object[["details"]][[2]]
    bg <- as.numeric(scores > thresh)
    if (type == "density") {
        sm::sm.density.compare(as.numeric(scores), as.factor(bg), xlab = "Match Score", ...)
        abline(v = object[["details"]][[2]], col = "red")
        title(main = "Match Score Density by Duplicate Pair Status")
        legend("right", c("Not Dup", "Dup"), fill = 2:3)
    } else if (type == "box") {
        boxplot(as.numeric(scores) ~ as.factor(bg), xlab = "Duplicate Pair", ylab = "Match Score", main = "Match Score by Duplicate Pair Status", 
            ...)
        abline(h = object[["details"]][[2]], col = "red")
    } else {
        print("Please provide a valid type argmument.")
    }
}

