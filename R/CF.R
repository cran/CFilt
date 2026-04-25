#'@title Recommendation by Collaborative Filtering
#'@description CF is a class of objects that stores
#'information about a recommendation system. This information includes the
#'consumption or rating of each (user, item) pair in the utility matrix MU, the
#'similarities between each pair of users in the similarity matrix SU, the
#'similarities between each pair of items in the similarity matrix SI, the
#'number of items consumed and/or rated by each user in the vector n_aval_u,
#'the number of users who consumed and/or rated each item in the vector
#'n_aval_i, the average rating value of each user in the vector averages_u, the
#'average rating value received by each item in the vector averages_i, the
#'number of items consumed in common by each pair of users in the matrix Int_U,
#'and the number of users in common for each pair of items in the matrix Int_I.
#'The class contains methods such as addNewUser, addNewEmptyUser, deleteUser,
#'addNewItem, addNewEmptyItem, deleteItem, newRating and deleteRating, which
#'modify the object's structure by altering users, items, or consumption data.
#'The class also includes functions such as kClosestItems, topKUsers, and
#'topKItems, which return items to recommend to a user or users to whom an item
#'should be recommended. An object of the CF class is created using the
#'CFBuilder function.
#'@field MU The Utility Matrix, a matrix that contains all the users' ratings.
#'The rows comprise users and the columns, items.
#'@field SU The user similarity matrix.
#'@field SI The item similarity matrix
#'@field IntU A symmetric matrix that records the number of items in common
#'between pairs of users.
#'@field IntI A symmetric matrix that records the number of users in common
#'between pairs of items.umber of items in common that
#'@field averages_u A vector that contains the averages of users' ratings.
#'@field averages_i A vector that contains the averages of items' ratings.
#'@field n_aval_u A vector that stores the number of items rated by each user.
#'@field n_aval_i A vector that stores the number of users who consumed each
#'item.
#'@field datatype A character that indicates the type of data, which can be
#'either "consumption" or "rating".
#'@field similarity  A character string indicating the similarity measure used.
#' It can be "pearson" or "cosine" for rating data, and "jaccard" for consumption data.
#'@field data_0 A data.frame containing the original dataset used to build the object.
#' This corresponds to the input data provided to CFbuilder and is stored for reference.
#'
#'@references
#'\itemize{
#'\item LINDEN, G.; SMITH, B.; YORK, J. Amazon. com recommendations:
#'Item-to-item collaborative filtering. Internet Computing, IEEE, v. 7, n. 1,
#'p. 76-80,2003
#'\item Aggarwal, C. C. (2016). Recommender systems (Vol. 1). Cham: Springer
#'International Publishing.
#'\item Leskovec, J., Rajaraman, A., & Ullman, J. D. (2020). Mining of massive
#'data sets. Cambridge university press.
#'}
#'
#'@seealso \code{\link[CFilt]{CFbuilder}}
#'
#'
#'@author Jessica Kubrusly
#'@importFrom Matrix sparseMatrix rbind2 cbind2 Matrix
#'@importFrom R6 R6Class
#'@export
#'@details
#'This class implements a collaborative filtering system supporting both
#'explicit (ratings) and implicit (consumption) feedback. The internal state
#'is updated incrementally after each operation.
#'@examples
#'data(movies, package = "CFilt")
#'# --- Rating data ---
#'objectCF_r <- CFbuilder(Data = movies[1:500,],Datatype = "rating",
#'similarity = "pearson")
#'dim(objectCF_r$MU)
#'colnames(objectCF_r$MU) #movies Id
#'rownames(objectCF_r$MU) #users Id
#'dim(objectCF_r$SU)
#'dim(objectCF_r$SI)
#'objectCF_r$averages_u
#'objectCF_r$averages_i
#'objectCF_r$n_aval_u
#'objectCF_r$n_aval_i
#'objectCF_r$addnewemptyuser(Id_u = "newuser1")
#'objectCF_r$newrating(Id_u = "newuser1",Id_i = "Frozen",r = 5)
#'objectCF_r$MU["newuser1","Frozen"]
#'objectCF_r$newrating(Id_u = list("newuser1","newuser1","newuser1"),
#'Id_i = list("Thor: The Dark World","The Lego Movie","Despicable Me 2"),
#'r = list(2,3,4))
#'objectCF_r$n_aval_u["newuser1"]
#'objectCF_r$averages_u["newuser1"]
#'objectCF_r$addnewemptyuser(Id_u = "newuser2")
#'objectCF_r$newrating(Id_u = list("newuser2","newuser1"),
#'Id_i = list("Frozen","Her"),r = c(2,1))
#'objectCF_r$addnewemptyuser(Id_u = list("newuser3","newuser4"))
#'objectCF_r$newrating(Id_u = list("newuser3","newuser3","newuser4","newuser4"),
#'Id_i = list("The Lego Movie","Wreck-It Ralph","Fast & Furious 6",
#'"12 Years a Slave"),r = list(4,5,4,2))
#'objectCF_r$addnewemptyitem(Id_i = list("movie1","movie2","movie3"))
#'objectCF_r$newrating(
#'Id_u = list("newuser1","newuser1","newuser1",
#'            "newuser2","newuser2","newuser2",
#'            "newuser3","newuser3","newuser3",
#'            "newuser4","newuser4","newuser4"),
#'Id_i = list("movie1","movie2","movie3",
#'            "movie1","movie2","movie3",
#'            "movie1","movie2","movie3",
#'            "movie1","movie2","movie3"),
#'r = list(4,5,4,2,
#'         1,2,1,1,
#'         4,3,1,2))
#'objectCF_r$MU[,"movie1"]
#'objectCF_r$SI["movie1","movie2"]
#'objectCF_r$SU["newuser2","newuser4"]
#'# --- Consumption data ---
#'objectCF_c <- CFbuilder(Data = movies[1:300,-3],Datatype = "consumption",
#'similarity = "jaccard")
#'objectCF_c$addnewemptyuser(Id_u = list("newuser1","newuser2","newuser3"))
#'objectCF_c$newrating(Id_u = list("newuser1","newuser2","newuser3"),
#'Id_i = list("Frozen","Frozen","Frozen"))
#'objectCF_c$newrating(Id_u = list("newuser1","newuser1","newuser1"),
#'Id_i = list("Gravity","The Wolverine","Iron Man 3"))
#'objectCF_c$addnewemptyitem(Id_i = list("movie1","movie2","movie3"))
#'objectCF_c$newrating(Id_u = list("newuser1","newuser1","newuser2","newuser2",
#'"newuser3"),Id_i = list("movie1","movie2","movie1","movie3","movie3"))
#'objectCF_c$MU[,"movie1"]
#'objectCF_c$SI["movie1","movie2"]
#'objectCF_c$SI["movie1","movie3"]
#'objectCF_c$SI["movie2","movie3"]
#'objectCF_c$SU["newuser1","newuser2"]
#'objectCF_c$SU["newuser2","newuser3"]
#' @export
CF <- R6::R6Class(
  "CF",
  public = list(

    # Fields ----
    MU = NULL,
    SU = NULL,
    SI = NULL,
    IntI = NULL,
    IntU = NULL,
    n_aval_u = NULL,
    n_aval_i = NULL,
    averages_u = NULL,
    averages_i = NULL,
    datatype   = NULL,
    similarity = NULL,
    data_0 = NULL,

    # Methods ----

    #' @description Add a new empty user to the system.
    #' This method creates a new user with no interactions.
    #' @param Id_u A character string (or a list of strings) representing user ID(s).
    #' @return Invisibly returns the updated object.
    addnewemptyuser = function(Id_u) {
      ## Recursion ----
      if (is.list(Id_u)) {
        for (u in Id_u) self$addnewemptyuser(u)
        return(invisible(self))
      }

      ## Validations ----
      if (!is.character(Id_u) || length(Id_u) != 1) {
        stop("*** Id_u must be a single character string. ***")
      }
      if (Id_u %in% rownames(self$MU)) {
        stop(paste0("*** User ", Id_u, " is already registered in the system. ***"))
      }

      ## Ratings ----
      if (self$datatype == "rating") {

        ### Basic Info ----
        N <- ncol(self$MU)
        M <- nrow(self$MU)
        k <- M + 1

        ### Update MU ----
        novo_usuario <- Matrix::sparseMatrix(
          i = integer(0),
          j = integer(0),
          x = numeric(0),
          dims = c(1L, N),
          dimnames = list(Id_u, colnames(self$MU))
        )
        self$MU <- Matrix::rbind2(self$MU, novo_usuario)

        ### Update counters ----
        self$n_aval_u <- c(self$n_aval_u, setNames(0L, Id_u))
        self$averages_u <- c(self$averages_u, setNames(NA_real_, Id_u))

        ### Update IntU ----
        new_row <- Matrix::Matrix(0, nrow = 1, ncol = M, sparse = TRUE)
        self$IntU <- Matrix::rbind2(self$IntU, new_row)
        self$IntU <- Matrix::cbind2(self$IntU, Matrix::Matrix(0, nrow = k, ncol = 1, sparse = TRUE))
        rownames(self$IntU) <- colnames(self$IntU) <- rownames(self$MU)

        ### Update SU ----
        new_col <- Matrix::Matrix(0, nrow = M, ncol = 1, sparse = TRUE)
        self$SU <- Matrix::cbind2(self$SU, new_col)
        self$SU <- Matrix::rbind2(self$SU, Matrix::Matrix(0, nrow = 1, ncol = k, sparse = TRUE))
        self$SU[k, k] <- 1
        rownames(self$SU) <- colnames(self$SU) <- rownames(self$MU)

        return(invisible(self))
      }

      ## Consumption ----
      if (self$datatype == "consumption") {

        ### Basic Info ----
        N <- ncol(self$MU)
        M <- nrow(self$MU)
        k <- M + 1

        ### Update MU ----
        novo_usuario <- Matrix::sparseMatrix(
          i = integer(0),
          j = integer(0),
          x = numeric(0),
          dims = c(1L, N),
          dimnames = list(Id_u, colnames(self$MU))
        )
        self$MU <- Matrix::rbind2(self$MU, novo_usuario)

        ### Update counters ----
        self$n_aval_u <- c(self$n_aval_u, setNames(0L, Id_u))

        ### Update IntU ----
        new_row <- Matrix::Matrix(0, nrow = 1, ncol = M, sparse = TRUE)
        self$IntU <- Matrix::rbind2(self$IntU, new_row)
        self$IntU <- Matrix::cbind2(self$IntU, Matrix::Matrix(0, nrow = k, ncol = 1, sparse = TRUE))
        rownames(self$IntU) <- colnames(self$IntU) <- rownames(self$MU)

        ### Update SU ----
        new_col <- Matrix::Matrix(0, nrow = M, ncol = 1, sparse = TRUE)
        self$SU <- Matrix::cbind2(self$SU, new_col)
        self$SU <- Matrix::rbind2(self$SU, Matrix::Matrix(0, nrow = 1, ncol = k, sparse = TRUE))
        self$SU[k, k] <- 1
        rownames(self$SU) <- colnames(self$SU) <- rownames(self$MU)

        return(invisible(self))
      }

      stop("*** Unknown datatype: must be 'rating' or 'consumption'. ***")
    },

    #' @description Add a new empty item to the system.
    #' @param Id_i A character string (or list of strings) representing item ID(s).
    addnewemptyitem = function(Id_i) {
      ## Recursion ----
      if (is.list(Id_i)) {
        for (i in Id_i) self$addnewemptyitem(i)
        return(invisible(self))
      }

      ## Validations ----
      if (!is.character(Id_i) || length(Id_i) != 1) {
        stop("*** Id_i must be a single character string. ***")
      }
      if (Id_i %in% colnames(self$MU)) {
        stop(paste0("*** Item ", Id_i, " is already registered in the system. ***"))
      }

      ## Rating ----
      if (self$datatype == "rating") {

        ### Basic Info ----
        N <- ncol(self$MU)
        M <- nrow(self$MU)
        k <- N + 1

        ### Update MU ----
        novo_item <- Matrix::sparseMatrix(
          i = integer(0),
          j = integer(0),
          x = numeric(0),
          dims = c(M, 1L),
          dimnames = list(rownames(self$MU), Id_i)
        )
        self$MU <- Matrix::cbind2(self$MU, novo_item)

        ### Update counters ----
        self$n_aval_i <- c(self$n_aval_i, setNames(0L, Id_i))
        self$averages_i <- c(self$averages_i, setNames(NA_real_, Id_i))

        ### Update IntI ----
        new_col <- Matrix::Matrix(0, nrow = N, ncol = 1, sparse = TRUE)
        self$IntI <- Matrix::cbind2(self$IntI, new_col)
        self$IntI <- Matrix::rbind2(self$IntI, Matrix::Matrix(0, nrow = 1, ncol = k, sparse = TRUE))
        rownames(self$IntI) <- colnames(self$IntI) <- colnames(self$MU)

        ### Update SI ----
        new_col <- Matrix::Matrix(0, nrow = N, ncol = 1, sparse = TRUE)
        self$SI <- Matrix::cbind2(self$SI, new_col)
        self$SI <- Matrix::rbind2(self$SI, Matrix::Matrix(0, nrow = 1, ncol = k, sparse = TRUE))
        self$SI[k, k] <- 1
        rownames(self$SI) <- colnames(self$SI) <- colnames(self$MU)

        return(invisible(self))
      }

      ## Consumption ----
      if (self$datatype == "consumption") {

        ### Basic Info ----
        N <- ncol(self$MU)
        M <- nrow(self$MU)
        k <- N + 1

        ### Update MU ----
        novo_item <- Matrix::sparseMatrix(
          i = integer(0),
          j = integer(0),
          x = numeric(0),
          dims = c(M, 1L),
          dimnames = list(rownames(self$MU), Id_i)
        )
        self$MU <- Matrix::cbind2(self$MU, novo_item)

        ### Update counters ----
        self$n_aval_i <- c(self$n_aval_i, setNames(0L, Id_i))

        ### Update IntI ----
        new_col <- Matrix::Matrix(0, nrow = N, ncol = 1, sparse = TRUE)
        self$IntI <- Matrix::cbind2(self$IntI, new_col)
        self$IntI <- Matrix::rbind2(self$IntI, Matrix::Matrix(0, nrow = 1, ncol = k, sparse = TRUE))
        rownames(self$IntI) <- colnames(self$IntI) <- colnames(self$MU)

        ### Update SI ----
        new_col <- Matrix::Matrix(0, nrow = N, ncol = 1, sparse = TRUE)
        self$SI <- Matrix::cbind2(self$SI, new_col)
        self$SI <- Matrix::rbind2(self$SI, Matrix::Matrix(0, nrow = 1, ncol = k, sparse = TRUE))
        self$SI[k, k] <- 1
        rownames(self$SI) <- colnames(self$SI) <- colnames(self$MU)

        return(invisible(self))
      }

      stop("*** Unknown datatype: must be 'rating' or 'consumption'. ***")
    },

    #' @description Add a new rating or consumption.
    #' @param Id_u A character string (or a list of strings) representing user ID(s).
    #' @param Id_i A character string (or a list of strings) representing item ID(s).
    #' @param r A numeric (or a list of numeric) for rating value(s) (only for rating data)
    newrating = function(Id_u, Id_i, r = NULL) {

      if (is.list(Id_u) && is.list(Id_i)) {

        u = length(Id_u)

        if(self$datatype == "rating"){
          for (k in 1:u) {
            self$newrating(Id_u = Id_u[[k]],
                      Id_i = Id_i[[k]],
                      r = r[[k]])
          }
        } else {
          for (k in 1:u) {
            self$newrating(Id_u = Id_u[[k]],
                      Id_i = Id_i[[k]])
          }
        }
      } else {

        if (!is.character(Id_u)) {
          stop("*** Id_u self$MUst be a character. ***")
        }

        if (!is.character(Id_i)) {
          stop("*** Id_i self$MUst be a character. ***")
        }

        M = nrow(self$MU)
        N = ncol(self$MU)

        i <- match(Id_u, rownames(self$MU))
        if (is.na(i)) {
          stop("*** This is not a valid user. ***")
        }

        j <- match(Id_i, colnames(self$MU))
        if (is.na(j)) {
          stop("*** This is not a valid item ***")
        }


        # Dados de Avaliacoes ####
        if (self$datatype == "rating") {

          if (self$MU[i, j]!=0) {
            stop(paste0("*** User ",Id_u," has already consumed item ",Id_i," ***"))
          }

          if (!is.numeric(r)) {
            stop("*** r self$MUst be numeric. ***")
          }


          ## Alteracao self$MU ####
          self$MU[i, j] <<- r


          # Alteracao n_aval_i ####
          self$n_aval_i[j] <<- self$n_aval_i[j] + 1
          # Alteracao n_aval_u ####
          self$n_aval_u[i] <<- self$n_aval_u[i] + 1

          #Alteracao averages_i
          self$averages_i[j] <<- ifelse(
            is.na(self$averages_i[j]),
            r,
            (self$averages_i[j]*(self$n_aval_i[j] - 1) + r)/(self$n_aval_i[j]))


          #Alteracao averages_i
          self$averages_u[i] <<- ifelse(
            is.na(self$averages_u[i]),
            r,
            (self$averages_u[i]*(self$n_aval_u[i] - 1) + r)/(self$n_aval_u[i]))


          # Alteracao self$IntU ####
          self$IntU[i,] <<- self$IntU[i,] + (!is.na(self$MU[,j]))
          self$IntU[,i] <<- self$IntU[,i] + (!is.na(self$MU[,j]))
          self$IntU[i,i] <<- self$IntU[i,i] - 1

          self$IntI[j,] <<- self$IntI[j,] + (!is.na(self$MU[i,]))
          self$IntI[,j] <<- self$IntI[,j] + (!is.na(self$MU[i,]))
          self$IntI[j,j] <<- self$IntI[j,j] - 1


          if(self$similarity=="pearson"){

            s = sapply(X = 1:M,
                       FUN = "pearson",
                       type = "user",
                       i = i,CF = self
            )
          } else {
            if(self$similarity=="cosine"){
              s = sapply(X = 1:M,
                         FUN = "cosine",
                         type = "user",
                         i = i,CF = self
              )
            }
          }

          self$SU[i,] <<- s
          self$SU[,i] <<- s


          if(self$similarity=="pearson"){

            s = sapply(X = 1:N,
                       FUN = "pearson",
                       type = "item",
                       i = j,CF = self
            )
          } else {
            if(self$similarity=="cosine"){
              s = sapply(X = 1:N,
                         FUN = "cosine",
                         type = "item",
                         i = j,CF = self
              )
            }
          }

          self$SI[j,] <<- s
          self$SI[,j] <<- s


        }

        # Dados de Consumo ####
        if (self$datatype == "consumption") {

          if (self$MU[i, j] != 0) {
            stop(paste0("*** User ",Id_u," has already consumed item ",Id_i," ***"))
          }


          ## Alteracao self$MU ####
          self$MU[i, j] <<- 1


          # Alteracao n_aval_i ####
          self$n_aval_i[j] <<- self$n_aval_i[j] + 1


          # Alteracao n_aval_u ####
          self$n_aval_u[i] <<- self$n_aval_u[i] + 1


          # Alteracao self$IntU ####
          self$IntU[i,] <<- self$IntU[i,] + self$MU[,j]
          self$IntU[,i] <<- self$IntU[,i] + self$MU[,j]
          self$IntU[i,i] <<- self$IntU[i,i] - 1

          self$IntI[j,] <<- self$IntI[j,] + self$MU[i,]
          self$IntI[,j] <<- self$IntI[,j] + self$MU[i,]
          self$IntI[j,j] <<- self$IntI[j,j] - 1


          s = sapply(X = 1:M,
                     FUN = "jaccard",
                     type ="user",
                     i    = i,
                     CF = self
          )

          self$SU[i,] <<- s
          self$SU[,i] <<- s


          s = sapply(X = 1:N,
                     FUN = "jaccard",
                     type ="item",
                     i    = j,
                     CF = self
          )

          self$SI[j,] <<- s
          self$SI[,j] <<- s


        }
      }
    },

    #' @description Delete a user from the system.
    #' @param Id_u A character string (or a list of strings) representing user ID(s).
    deleteuser = function(Id_u) {
      ## Recursion ----
      if (is.list(Id_u)) {
        for (k in seq_along(Id_u)) {
          self$deleteuser(Id_u[[k]])
        }
        return(invisible(self))
      }

      ## Validations ----
      if (!is.character(Id_u) || length(Id_u) != 1)
        stop("*** Id_u must be a single character string. ***")

      if (!(Id_u %in% rownames(self$MU)))
        stop("*** No user with this Id ***")

      ## Rating ----
      if (self$datatype == "rating") {

        ### Basic Info ----
        k <- which(rownames(self$MU) == Id_u)
        N <- ncol(self$MU)
        M <- nrow(self$MU)

        user_ratings <- self$MU[k, , drop = FALSE]
        consumed <- as.vector(user_ratings != 0)
        j_idx <- which(consumed)

        ### Update counts ----
        self$n_aval_i[j_idx] <- self$n_aval_i[j_idx] - 1L
        self$n_aval_u <- self$n_aval_u[-k]

        ### Update averages ----
        ratings <- user_ratings@x
        self$averages_u <- self$averages_u[-k]
        if (length(j_idx) > 0) {
          old_avg_i <- self$averages_i[j_idx]
          new_count_i <- self$n_aval_i[j_idx]
          self$averages_i[j_idx] <- ifelse(new_count_i > 0,
                                           (old_avg_i * (new_count_i + 1) - ratings) / new_count_i,
                                           NA
          )
        }

        ### Update IntI ----
        if (length(j_idx) > 0) {
          self$IntI[j_idx, j_idx] <- self$IntI[j_idx, j_idx] - 1L
        }

        ### Update SI ----
        if (length(j_idx) > 0) {
          for (idx in j_idx) {
            if (self$similarity == "pearson") {
              s <- pearson(type = "item",
                           i = idx,
                           j = 1:ncol(self$MU),
                           CF = self)
            } else {
              s <- cosine(type = "item",
                          i = idx,
                          j = 1:ncol(self$MU),
                          CF = self)
            }
            self$SI[idx, ] <- s
            self$SI[, idx] <- s
          }
        }

        ### Update MU ----
        self$MU <- self$MU[-k, , drop = FALSE]

        ### Update IntU ----
        self$IntU <- self$IntU[-k, -k, drop = FALSE]
        rownames(self$IntU) <- colnames(self$IntU) <- rownames(self$MU)

        ### Update SU ----
        self$SU <- self$SU[-k, -k, drop = FALSE]
        rownames(self$SU) <- colnames(self$SU) <- rownames(self$MU)

        return(invisible(self))
      }

      ## Consumption ----
      if (self$datatype == "consumption") {

        ### Basic Info ----
        k <- which(rownames(self$MU) == Id_u)
        N <- ncol(self$MU)
        M <- nrow(self$MU)

        user_consumo <- self$MU[k, , drop = FALSE]
        consumed <- as.vector(user_consumo != 0)
        j_idx <- which(consumed)

        ### Update counts ----
        self$n_aval_i[j_idx] <- self$n_aval_i[j_idx] - 1L
        self$n_aval_u <- self$n_aval_u[-k]

        ### Update IntI ----
        if (length(j_idx) > 0) {
          self$IntI[j_idx, j_idx] <- self$IntI[j_idx, j_idx] - 1L
        }

        ### Update SI ----
        if (length(j_idx) > 0) {
          for (idx in j_idx) {
            s <- jaccard(type = "item",
                         i = idx,
                         j = 1:ncol(self$MU),
                         CF = self)
            self$SI[idx, ] <- s
            self$SI[, idx] <- s
          }
        }

        ### Update MU ----
        self$MU <- self$MU[-k, , drop = FALSE]

        ### Update IntU and SU ----
        self$IntU <- self$IntU[-k, -k, drop = FALSE]
        self$SU <- self$SU[-k, -k, drop = FALSE]
        rownames(self$IntU) <- colnames(self$IntU) <- rownames(self$MU)
        rownames(self$SU) <- colnames(self$SU) <- rownames(self$MU)

        return(invisible(self))
      }

      stop("*** Unknown datatype: must be 'rating' or 'consumption'. ***")
    },

    #' @description Delete an item from the system.
    #' @param Id_i A character string (or a list of strings) representing item ID(s).
    deleteitem = function(Id_i) {
      ## Recursion ----
      if (is.list(Id_i)) {
        for (k in seq_along(Id_i)) {
          self$deleteitem(Id_i[[k]])
        }
        return(invisible(self))
      }

      ## Validations ----
      if (!is.character(Id_i) || length(Id_i) != 1)
        stop("*** Id_i must be a single character string. ***")

      if (!(Id_i %in% colnames(self$MU)))
        stop(paste0("*** Item ", Id_i, " is not registered in the system. ***"))

      ## Rating ----
      if (self$datatype == "rating") {
        ### Basic Info ----
        k <- which(colnames(self$MU) == Id_i)
        N <- ncol(self$MU)
        M <- nrow(self$MU)

        item_ratings <- self$MU[, k, drop = FALSE]
        consumed <- as.vector(item_ratings != 0)
        i_idx <- which(consumed)

        ### Update counts ----
        self$n_aval_u[i_idx] <- self$n_aval_u[i_idx] - 1L
        self$n_aval_i <- self$n_aval_i[-k]

        ### Update averages ----
        ratings <- item_ratings@x
        self$averages_i <- self$averages_i[-k]
        if (length(i_idx) > 0) {
          old_avg_u <- self$averages_u[i_idx]
          new_count_u <- self$n_aval_u[i_idx]
          self$averages_u[i_idx] <- ifelse(new_count_u > 0,
                                           (old_avg_u * (new_count_u + 1) - ratings) / new_count_u,
                                           NA
          )
        }

        ### Update IntI ----
        self$IntI <- self$IntI[-k, -k, drop = FALSE]

        ### Update SI ----
        self$SI <- self$SI[-k, -k, drop = FALSE]

        ### Update MU ----
        self$MU <- self$MU[, -k, drop = FALSE]

        ### Update IntU ----
        if (length(i_idx) > 0) {
          self$IntU[i_idx, i_idx] <- self$IntU[i_idx, i_idx] - 1L
        }

        ### Update SU ----
        if (length(i_idx) > 0) {
          users <- rownames(self$MU)[i_idx]

          if (self$similarity == "pearson") {
            for (user_i in users) {
              s <- sapply(colnames(self$SU),
                          FUN = "pearson",
                          type = "user",
                          i = user_i,
                          CF = self)
              self$SU[user_i, ] <- s
              self$SU[, user_i] <- s
            }
          } else {
            for (user_i in users) {
              s <- sapply(colnames(self$SU),
                          FUN = "cosine",
                          type = "user",
                          i = user_i,
                          CF = self)
              self$SU[user_i, ] <- s
              self$SU[, user_i] <- s
            }
          }
        }


        return(invisible(self))
      }

      ## Consumption ----
      if (self$datatype == "consumption") {
        ### Basic Info ----
        k <- which(colnames(self$MU) == Id_i)
        N <- ncol(self$MU)
        M <- nrow(self$MU)

        item_consumo <- self$MU[, k, drop = FALSE]
        consumed <- as.vector(item_consumo != 0)
        i_idx <- which(consumed)

        ### Update counts ----
        self$n_aval_u[i_idx] <- self$n_aval_u[i_idx] - 1L
        self$n_aval_i <- self$n_aval_i[-k]

        ### Update MU ----
        self$MU <- self$MU[, -k, drop = FALSE]

        ### Update IntI and SI ----
        self$IntI <- self$IntI[-k, -k, drop = FALSE]
        self$SI <- self$SI[-k, -k, drop = FALSE]
        rownames(self$IntI) <- colnames(self$IntI) <- colnames(self$MU)
        rownames(self$SI) <- colnames(self$SI) <- colnames(self$MU)

        ### Update IntU ----
        if (length(i_idx) > 0) {
          self$IntU[i_idx, i_idx] <- self$IntU[i_idx, i_idx] - 1L
        }

        ### Update SU ----
        if (length(i_idx) > 0) {
          inter <- as.matrix(self$IntU)
          union <- outer(self$n_aval_u, self$n_aval_u, "+") - inter
          self$SU[i_idx, ] <- ifelse(union[i_idx, ] > 0, inter[i_idx, ] / union[i_idx, ], 0)
          self$SU[, i_idx] <- Matrix::t(self$SU[i_idx, , drop = FALSE])
        }

        return(invisible(self))
      }

      stop("*** Unknown datatype: must be 'rating' or 'consumption'. ***")
    },

    #' @description Delete a rating or consumption.
    #' @param Id_u A character string (or a list of strings) representing user ID(s).
    #' @param Id_i A character string (or a list of strings) representing item ID(s).
    deleterating = function(Id_u, Id_i) {

      ## Recursion ----
      if (is.list(Id_u) && is.list(Id_i)) {
        mapply(function(u, i) self$deleterating(u, i),
               Id_u, Id_i, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        return(invisible(self))
      }

      ## Validations ----
      if (!is.character(Id_u) || length(Id_u) != 1) stop("*** Id_u must be a single character. ***")
      if (!is.character(Id_i) || length(Id_i) != 1) stop("*** Id_i must be a single character. ***")

      if (!(Id_u %in% rownames(self$MU))) stop("*** This is not a valid user. ***")
      if (!(Id_i %in% colnames(self$MU))) stop("*** This is not a valid item. ***")

      ## Rating ----
      if (self$datatype == "rating") {

        i <- match(Id_u, rownames(self$MU))
        j <- match(Id_i, colnames(self$MU))

        if (self$MU[i, j] == 0) {
          stop("*** This rating does not exist. ***")
        }

        ### Update counts ----
        self$n_aval_i[j] <- self$n_aval_i[j] - 1L
        self$n_aval_u[i] <- self$n_aval_u[i] - 1L

        ### Update averages ----
        self$averages_u[i] <- (self$averages_u[i]*(self$n_aval_u[i] + 1L) - self$MU[i, j])/(self$n_aval_u[i])
        self$averages_i[j] <- (self$averages_i[j]*(self$n_aval_i[j] + 1L) - self$MU[i, j])/(self$n_aval_i[j])

        ### Update IntU ----
        # Update user intersection matrix by removing contributions of this rating
        col_j_bin <- (self$MU[, j] != 0)
        self$IntU[i, ] <- self$IntU[i, ] - col_j_bin
        self$IntU[, i] <- self$IntU[, i] - col_j_bin
        self$IntU[i, i] <- self$IntU[i, i] + 1L

        ### Update IntI ----
        # Update item intersection matrix similarly
        row_i_bin <- (self$MU[i, ] != 0)
        self$IntI[j, ] <- self$IntI[j, ] - row_i_bin
        self$IntI[, j] <- self$IntI[, j] - row_i_bin
        self$IntI[j, j] <- self$IntI[j, j] + 1L

        ### Update MU ----
        # Remove rating from user-item matrix
        self$MU[i, j] <- 0

        ### Update SU ----
        if (self$similarity == "pearson") {
          s <- sapply(X = list(1:nrow(self$MU)),
                      FUN = "pearson",
                      type = "user",
                      i = i,
                      CF = self
          )
        } else if (self$similarity == "cosine") {
          s <- sapply(X = list(1:nrow(self$MU)),
                      FUN = "cosine",
                      type = "user",
                      i = i,
                      CF = self
          )
        }

        self$SU[i, ] <- s
        self$SU[, i] <- s

        ### Update SI ----
        if (self$similarity == "pearson") {
          s <- sapply(X = list(1:ncol(self$MU)),
                      FUN = "pearson",
                      type = "item",
                      i = j,
                      CF = self
          )
        } else if (self$similarity == "cosine") {
          s <- sapply(X = list(1:ncol(self$MU)),
                      FUN = "cosine",
                      type = "item",
                      i = j,
                      CF = self
          )
        }

        self$SI[j, ] <- s
        self$SI[, j] <- s

        return(invisible(NULL))

      }

      ## Consumption ----
      if (self$datatype == "consumption") {

        i <- match(Id_u, rownames(self$MU))
        j <- match(Id_i, colnames(self$MU))

        if (self$MU[i, j] == 0) {
          stop("*** This rating does not exist. ***")
        }

        ### Update counts ----
        # Decrement total ratings counts for user and item
        self$n_aval_i[j] <- self$n_aval_i[j] - 1
        self$n_aval_u[i] <- self$n_aval_u[i] - 1

        ### Update IntU ----
        # Update user intersection matrix by removing contributions of this rating
        self$IntU[i, ] <- self$IntU[i, ] - self$MU[, j]
        self$IntU[, i] <- self$IntU[, i] - self$MU[, j]
        self$IntU[i, i] <- self$n_aval_u[i]

        ### Update IntI ----
        # Update item intersection matrix similarly
        self$IntI[j, ] <- self$IntI[j, ] - self$MU[i, ]
        self$IntI[, j] <- self$IntI[, j] - self$MU[i, ]
        self$IntI[j, j] <- self$n_aval_i[j]

        ### Update MU ----
        # Remove rating from user-item matrix
        self$MU[i, j] <- 0

        ### Update SU ----
        # Recalculate user similarity for affected user
        intersection <- self$IntU[i, ]
        union <- self$n_aval_u[i] + self$n_aval_u - intersection
        s_user <- intersection / union
        s_user[is.nan(s_user)] <- 0
        s_user[i] <- 1
        self$SU[i, ] <- s_user
        self$SU[, i] <- s_user

        ### Update SI ----
        # Recalculate item similarity for affected item
        intersection_i <- self$IntI[j, ]
        union_i <- self$n_aval_i[j] + self$n_aval_i - intersection_i
        s_item <- intersection_i / union_i
        s_item[is.nan(s_item)] <- 0
        s_item[j] <- 1
        self$SI[j, ] <- s_item
        self$SI[, j] <- s_item

        return(invisible(NULL))
      }
    }
  )
)
