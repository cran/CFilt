#' @title Top-K Item Recommendation
#'
#' @description
#' Returns the top-k items to recommend for a given user based on a
#' collaborative filtering model.
#'
#' @param CF An object of class \code{CF}.
#' @param Id_u A character string representing the user ID.
#' @param k A positive integer indicating the number of items to recommend.
#' Default is 10.
#' @param type A character string indicating the recommendation strategy:
#' \itemize{
#'   \item \code{"user"}: user-based collaborative filtering
#'   \item \code{"item"}: item-based collaborative filtering
#' }
#'
#' @return A character vector containing the IDs of the top-k recommended items.
#'
#' @details
#' For \code{type = "user"}, recommendations are computed based on similarities
#' between users. For \code{type = "item"}, recommendations are computed based
#' on similarities between items.
#'
#' Only items not yet consumed/rated by the user are considered.
#'
#' @examples
#' data(movies, package = "CFilt")
#'
#' CF1 <- CFbuilder(movies[1:200, ], Datatype = "rating")
#'
#' # Recommend users for an item using user-based CF
#' topkitems(CF1, Id_u = "1", k = 5, type = "user")
#'
#' # Recommend users for an item using item-based CF
#' topkitems(CF1, Id_u = "1", k = 3, type = "item")
#' 
#' CF2 <- CFbuilder(movies[1:200,-3])
#' 
#' # Recommend users for an item using user-based CF
#' topkitems(CF2, Id_u = "1", k = 5, type = "user")
#'
#' # Recommend users for an item using item-based CF
#' topkitems(CF2, Id_u = "1", k = 3, type = "item")
#'
#' @seealso \code{\link{CFbuilder}}, \code{\link{topkitems}}
#'
#' @export
topkitems <- function(CF, Id_u, k = 10, type = "user") {
  
  # Validations ----
  if (!is.character(Id_u) || length(Id_u) != 1) {
    stop("*** 'Id_u' must be a single character string. ***")
  }
  
  if (!is.numeric(k) || length(k) != 1 || k <= 0) {
    stop("*** 'k' must be a single positive number. ***")
  }
  
  if (!type %in% c("user", "item")) {
    stop("*** 'type' must be either 'user' or 'item'. ***")
  }
  
  MU <- CF$MU
  
  if (!Id_u %in% rownames(MU)) {
    stop("*** This is not a valid user. ***")
  }
  
  i <- match(Id_u, rownames(MU))
  
  # Calculation ----
  if (type == "user") {
    
    s <- CF$SU[, i]
    notas <- Matrix::crossprod(s, MU) / sum(s)
    notas <- as.numeric(notas)
    
  } else {  # type == "item"
    s <- CF$SI
    v <- MU[i, , drop = FALSE]
    
    notas <- v %*% s
    denom <- Matrix::colSums(s, na.rm = TRUE)
    
    denom[denom == 0] <- NA
    notas <- as.numeric(notas) / denom
  }
  
  nao_avaliados <- MU[i, ] == 0
  
  notas <- notas[nao_avaliados]
  itens <- colnames(MU)[nao_avaliados]
  
  ind <- head(order(notas, decreasing = TRUE, na.last = TRUE), k)
  return(itens[ind])
}