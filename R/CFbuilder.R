#'@title Build a Collaborative Filtering Object
#'@description
#'Creates an object of class \code{CF} from a dataset of user-item interactions.
#'The dataset can represent either explicit ratings or implicit consumption.
#'@param Data A data.frame containing:
#'\itemize{
#' \item 2 columns: (user, item) for consumption data
#' \item 3 columns: (user, item, rating) for rating data
#'}
#'@param Datatype A character string indicating the type of data:
#'\code{"consumption"} or \code{"rating"}.
#'Default is inferred from number of columns in \code{Data}.
#'@param similarity A character string indicating the similarity measure:
#'\itemize{
#' \item \code{"jaccard"} for consumption data
#' \item \code{"pearson"} or \code{"cosine"} for rating data
#'}
#'Default is chosen based on \code{Datatype}.
#'@return An object of class \code{CF}.
#'@examples
#'data(movies, package = "CFilt")
#'# --- Rating data ---
#'CF1 <- CFbuilder(Data = movies[1:300,],Datatype = "rating",
#'similarity = "pearson")
#'CF1_ <- CFbuilder(Data = movies[1:300,])
#'CF2 <- CFbuilder(Data = movies[1:300,],Datatype = "rating",
#'similarity = "cosine")
#'CF2_ <- CFbuilder(Data = movies[1:300,],similarity = "cosine")
#'# --- Consumption data ---
#'CF3 <- CFbuilder(Data = movies[1:300,-3],Datatype = "consumption",
#'similarity = "jaccard")
#'CF3_ <- CFbuilder(Data = movies[1:300,-3])
#'@importFrom utils head txtProgressBar setTxtProgressBar
#'@export

CFbuilder <- function(Data,
                      Datatype = ifelse(ncol(Data) == 2, "consumption", "rating"),
                      similarity = ifelse(Datatype == "consumption", "jaccard", "pearson")) {

  # Validations ----
  if (!is.data.frame(Data)) {
    stop("*** Data must be a dataframe object ***")
  }

  if (!(ncol(Data) %in% c(2, 3))) {
    stop("*** Datatype must have 2 (consumption) or 3 (rating) columns ***")
  }

  if (!Datatype %in% c("consumption", "rating")) {
    stop("*** Datatype must be 'consumption' or 'rating' ***")
  }


  # Setup ----

  Data <- as.data.frame(Data)
  name_u <- unique(as.character(Data[[1]]))
  name_i <- unique(as.character(Data[[2]]))
  m <- length(name_u)
  n <- length(name_i)

  obj_CF <- CF$new()
  obj_CF$datatype <- Datatype
  obj_CF$similarity <- similarity

  row_idx <- as.integer(factor(Data[[1]], levels = name_u))
  col_idx <- as.integer(factor(Data[[2]], levels = name_i))

  obj_CF$data_0 <- Data

  # Consumption ----

  if (obj_CF$datatype == "consumption" && ncol(Data) == 2) {

    ## jaccard ----

    if (obj_CF$similarity == "jaccard") {

      ### MU ----
      # Build user-item consumption matrix

      message("Step 1 of 3: Building MU")
      pb <- utils::txtProgressBar(min = 0, max = 3, style = 3)
      utils::setTxtProgressBar(pb, 0)

      row_idx <- as.integer(factor(Data[[1]], levels = name_u))
      col_idx <- as.integer(factor(Data[[2]], levels = name_i))
      utils::setTxtProgressBar(pb, 1)

      obj_CF$MU <- Matrix::sparseMatrix(
        i = row_idx,
        j = col_idx,
        x = 1,
        dims = c(m, n),
        dimnames = list(name_u, name_i)
      )
      utils::setTxtProgressBar(pb, 2)

      obj_CF$n_aval_u <- Matrix::rowSums(obj_CF$MU)
      obj_CF$n_aval_i <- Matrix::colSums(obj_CF$MU)

      utils::setTxtProgressBar(pb, 3)
      close(pb)

      ### SU ----
      # Build user-user Jaccard similarity

      message("Step 2 of 3: Building SU")
      pb <- utils::txtProgressBar(min = 0, max = 4, style = 3)
      utils::setTxtProgressBar(pb, 0)

      intersection_u <- Matrix::tcrossprod(obj_CF$MU)
      utils::setTxtProgressBar(pb, 1)

      nu <- obj_CF$n_aval_u
      inter_map_u <- Matrix::summary(intersection_u)
      i_idx <- inter_map_u$i
      j_idx <- inter_map_u$j
      x_vals <- inter_map_u$x
      union_u <- nu[i_idx] + nu[j_idx] - x_vals

      utils::setTxtProgressBar(pb, 2)
      jaccard_vals_u <- x_vals / union_u
      utils::setTxtProgressBar(pb, 3)

      obj_CF$SU <- Matrix::sparseMatrix(
        i = i_idx,
        j = j_idx,
        x = jaccard_vals_u,
        dims = dim(intersection_u),
        symmetric = TRUE,
        dimnames = dimnames(intersection_u)
      )
      obj_CF$IntU <- intersection_u

      utils::setTxtProgressBar(pb, 4)
      close(pb)

      ### SI ----
      # Build item-item Jaccard similarity

      message("Step 3 of 3: Building SI")
      pb <- utils::txtProgressBar(min = 0, max = 4, style = 3)
      utils::setTxtProgressBar(pb, 0)

      intersection_i <- Matrix::crossprod(obj_CF$MU)
      utils::setTxtProgressBar(pb, 1)

      ni <- obj_CF$n_aval_i
      inter_map_i <- Matrix::summary(intersection_i)
      i_idx <- inter_map_i$i
      j_idx <- inter_map_i$j
      x_vals <- inter_map_i$x
      union_i <- ni[i_idx] + ni[j_idx] - x_vals

      utils::setTxtProgressBar(pb, 2)
      jaccard_vals_i <- x_vals / union_i
      utils::setTxtProgressBar(pb, 3)

      obj_CF$SI <- Matrix::sparseMatrix(
        i = i_idx,
        j = j_idx,
        x = jaccard_vals_i,
        dims = dim(intersection_i),
        symmetric = TRUE,
        dimnames = dimnames(intersection_i)
      )
      obj_CF$IntI <- intersection_i

      utils::setTxtProgressBar(pb, 4)
      close(pb)
    }
  }


  # Ratings ----
  else if (obj_CF$datatype == "rating" && ncol(Data) == 3) {

    ## MU ----
    message("Step 1 of 3: Building MU")
    pb <- utils::txtProgressBar(min = 0, max = 1, style = 3)
    utils::setTxtProgressBar(pb, 0)

    ratings <- as.numeric(Data[[3]])
    ratings[ratings == 0 & !is.na(ratings)] <- .Machine$double.xmin
    ratings[is.na(ratings)] <- 0L

    obj_CF$MU <- Matrix::sparseMatrix(
      i = row_idx,
      j = col_idx,
      x = ratings,
      dims = c(m, n),
      dimnames = list(name_u, name_i)
    )
    utils::setTxtProgressBar(pb, 1)
    close(pb)

    ## pearson ----

    if (obj_CF$similarity == "pearson") {

      binary_MU <- sign(obj_CF$MU)
      obj_CF$n_aval_u <- Matrix::rowSums(binary_MU)
      obj_CF$n_aval_i <- Matrix::colSums(binary_MU)
      obj_CF$averages_u <- Matrix::rowSums(obj_CF$MU)/obj_CF$n_aval_u
      obj_CF$averages_i <- Matrix::colSums(obj_CF$MU)/obj_CF$n_aval_i

      ### SU ----
      message("Step 2 of 3: Building SU")
      pb <- utils::txtProgressBar(min = 0, max = 3, style = 3)
      utils::setTxtProgressBar(pb, 0)

      trip <- Matrix::summary(obj_CF$MU)
      M_u <- Matrix::sparseMatrix(
        i = trip$i,
        j = trip$j,
        x = obj_CF$averages_u[trip$i],
        dims = dim(obj_CF$MU),
        dimnames = dimnames(obj_CF$MU)
      )
      utils::setTxtProgressBar(pb, 1)

      MU_centered_u <- obj_CF$MU - M_u

      numer_u <- Matrix::tcrossprod(MU_centered_u)
      utils::setTxtProgressBar(pb, 2)

      norms_u <- sqrt(Matrix::rowSums(MU_centered_u^2))
      norm_prod <- norms_u %o% norms_u

      s_u <- Matrix::summary(numer_u)
      numv <- s_u$x
      denv <- norm_prod[cbind(s_u$i, s_u$j)]
      pe_u <- ifelse(denv > 0, numv / denv, 0)

      obj_CF$SU <- Matrix::sparseMatrix(
        i = s_u$i, j = s_u$j, x = pe_u,
        dims = dim(numer_u), symmetric = TRUE,
        dimnames = dimnames(numer_u)
      )
      obj_CF$IntU <- Matrix::tcrossprod(binary_MU)

      utils::setTxtProgressBar(pb, 3)
      close(pb)

      ### SI ----
      message("Step 3 of 3: Building SI")
      pb <- utils::txtProgressBar(min = 0, max = 3, style = 3)
      utils::setTxtProgressBar(pb, 0)

      M_i <- Matrix::sparseMatrix(
        i = trip$i,
        j = trip$j,
        x = obj_CF$averages_i[trip$j],
        dims = dim(obj_CF$MU),
        dimnames = dimnames(obj_CF$MU)
      )
      utils::setTxtProgressBar(pb, 1)

      MU_centered_i <- obj_CF$MU - M_i
      numer_i <- Matrix::crossprod(MU_centered_i)
      utils::setTxtProgressBar(pb, 2)

      norms_i <- sqrt(Matrix::colSums(MU_centered_i^2))
      norm_prod <- norms_i %o% norms_i

      s_i <- Matrix::summary(numer_i)
      numv <- s_i$x
      denv <- norm_prod[cbind(s_i$i, s_i$j)]
      pe_i <- ifelse(denv > 0, numv / denv, 0)

      obj_CF$SI <- Matrix::sparseMatrix(
        i = s_i$i, j = s_i$j, x = pe_i,
        dims = dim(numer_i), symmetric = TRUE,
        dimnames = dimnames(numer_i)
      )
      obj_CF$IntI <- Matrix::crossprod(binary_MU)

      utils::setTxtProgressBar(pb, 3)
      close(pb)
    }

    ## cosine ----
    else if (obj_CF$similarity == "cosine") {

      binary_MU <- sign(obj_CF$MU)

      ### SU ----
      message("Step 2 of 3: Building SU")
      pb <- utils::txtProgressBar(min = 0, max = 2, style = 3)
      utils::setTxtProgressBar(pb, 0)

      numer_u <- Matrix::tcrossprod(obj_CF$MU)
      norms_u <- sqrt(Matrix::rowSums(obj_CF$MU * obj_CF$MU))
      norm_prod <- norms_u %o% norms_u
      utils::setTxtProgressBar(pb, 1)

      s_u <- Matrix::summary(numer_u)
      numv <- s_u$x
      denv <- norm_prod[cbind(s_u$i, s_u$j)]
      cosu <- ifelse(denv > 0, numv / denv, 0)

      obj_CF$SU <- Matrix::sparseMatrix(
        i = s_u$i, j = s_u$j, x = cosu,
        dims = dim(numer_u), symmetric = TRUE,
        dimnames = dimnames(numer_u)
      )
      obj_CF$IntU <- Matrix::tcrossprod(binary_MU)

      utils::setTxtProgressBar(pb, 2)
      close(pb)

      ### SI ----
      message("Step 3 of 3: Building SI")
      pb <- utils::txtProgressBar(min = 0, max = 2, style = 3)
      utils::setTxtProgressBar(pb, 0)

      numer_i <- Matrix::crossprod(obj_CF$MU)
      norms_i <- sqrt(Matrix::colSums(obj_CF$MU * obj_CF$MU))
      norm_prod <- norms_i %o% norms_i
      utils::setTxtProgressBar(pb, 1)

      s_i <- Matrix::summary(numer_i)
      numv <- s_i$x
      denv <- norm_prod[cbind(s_i$i, s_i$j)]
      cosi <- ifelse(denv > 0, numv / denv, 0)

      obj_CF$SI <- Matrix::sparseMatrix(
        i = s_i$i, j = s_i$j, x = cosi,
        dims = dim(numer_i), symmetric = TRUE,
        dimnames = dimnames(numer_i)
      )
      obj_CF$IntI <- Matrix::crossprod(binary_MU)

      utils::setTxtProgressBar(pb, 2)
      close(pb)
    }
  }

  # Return ----
  return(obj_CF)
}
