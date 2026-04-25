#' @title Top-K User Recommendation for an Item
#'
#' @description
#' Returns the top-k users to whom a given item should be recommended,
#' based on a collaborative filtering model.
#'
#' @param CF An object of class \code{CF} created by \code{\link{CFbuilder}}.
#' @param Id_i A character string representing the item ID.
#' @param k A positive integer indicating the number of users to return.
#' Default is 10.
#' @param type A character string indicating the recommendation strategy:
#' \itemize{
#'   \item \code{"user"}: user-based collaborative filtering
#'   \item \code{"item"}: item-based collaborative filtering
#' }
#'
#' @return A character vector containing the IDs of the top-k users
#' for whom the item is recommended.
#'
#' @details
#' For \code{type = "user"}, recommendations are based on similarities
#' between users. For \code{type = "item"}, recommendations are based on
#' similarities between items.
#'
#' Only users who have not yet consumed/rated the item are considered.
#'
#' @examples
#' data(movies, package = "CFilt")
#'
#' CF1 <- CFbuilder(movies[1:200, ], Datatype = "rating")
#'
#' # Recommend users for an item using user-based CF
#' topkusers(CF1, Id_i = "Frozen", k = 5, type = "user")
#'
#' # Recommend users for an item using item-based CF
#' topkusers(CF1, Id_i = "Frozen", k = 5, type = "item")
#' 
#' CF2 <- CFbuilder(movies[1:200,-3])
#' 
#' # Recommend users for an item using user-based CF
#' topkusers(CF2, Id_i = "Frozen", k = 5, type = "user")
#'
#' # Recommend users for an item using item-based CF
#' topkusers(CF2, Id_i = "Frozen", k = 3, type = "item")
#'
#' @seealso \code{\link{CFbuilder}}, \code{\link{topkitems}}
#'
#' @export

topkusers <- function(CF, Id_i, k = 10, type = "user") {
  
  # Validations ----
  if (!is.character(Id_i) || length(Id_i) != 1) {
    stop("*** 'Id_i' must be a single character string. ***")
  }
  
  if (!is.numeric(k) || length(k) != 1 || k <= 0) {
    stop("*** 'k' must be a single positive number. ***")
  }
  
  if (!type %in% c("user", "item")) {
    stop("*** 'type' must be either 'user' or 'item'. ***")
  }
  
  MU <- CF$MU
  if (!Id_i %in% colnames(MU)) {
    stop("*** This is not a valid item. ***")
  }
  
  j <- match(Id_i, colnames(MU))
  
  # Calculation ----
  if (type == "user") {
    s <- CF$SU
    v <- MU[, j]
    notas <- as.numeric(Matrix::crossprod(s, v)) / Matrix::colSums(s)
    
  } else {
    s <- CF$SI[, j]
    notas <- as.numeric(MU %*% s) / sum(s)
  }
  
  avaliados <- MU[, j] != 0
  notas[avaliados] <- NA
  

  ind <- head(order(notas, decreasing = TRUE, na.last = NA), k)
  return(rownames(MU)[ind])
}