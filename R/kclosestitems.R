#' @title K Closest Items
#'
#' @description
#' Returns the k most similar items to a given item based on the item-item
#' similarity matrix of a collaborative filtering model.
#'
#' @param CF An object of class \code{CF} created by \code{\link{CFbuilder}}.
#' @param Id_i A character string representing the item ID.
#' @param k A positive integer indicating the number of similar items to return.
#' Default is 10.
#'
#' @return A character vector containing the IDs of the k most similar items.
#'
#' @details
#' The similarity between items is obtained from the item similarity matrix
#' stored in \code{CF$SI}. The item itself is excluded from the result.
#'
#' @examples
#' data(movies, package = "CFilt")
#'
#' CF1 <- CFbuilder(movies[1:200, ], Datatype = "rating")
#'
#' # Find the 5 items most similar to a given item
#' kclosestitems(CF1, Id_i = "Frozen", k = 5)
#'
#' @seealso \code{\link{CFbuilder}}, \code{\link{topkitems}}, \code{\link{topkusers}}
#'
#' @export
kclosestitems <- function (CF, Id_i, k = 10) {

  # Validations ----
  if (!is.character(Id_i) || length(Id_i) != 1) {
    stop("*** 'Id_i' must be a single character string. ***")
  }
  if (!is.numeric(k) || length(k) != 1 || k <= 0) {
    stop("*** 'k' must be a single positive number. ***")
  }
  if (!(Id_i %in% colnames(CF$MU))) {
    stop("*** This is not a valid item. ***")
  }

  # Calculation ----
  MU <- CF$MU
  SI <- CF$SI

  j <- match(Id_i, colnames(MU))

  s <- SI[, j]
  s[j] <- 0
  ind <- head(order(s, decreasing = T, na.last = NA), k)

  return(colnames(SI)[ind])
}
