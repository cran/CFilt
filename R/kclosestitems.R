#' Recommendation Functions
#'
#' Functions that provide items to be recommended to system users.
#' 
#' @param CF A CF objec
#' @param Id_i the item Id
#' @param k an integer
#' @export
#' @author Jessica Kubrusly
#' @examples
#'objectCF_r <- CFbuilder(Data = movies[1:500,], Datatype = "ratings", 
#'similarity = "pearson")
#'kclosestitems(CF = objectCF_r, Id_i = "The Lego Movie")
#'kclosestitems(CF = objectCF_r, Id_i = "Lincoln", k=5)


kclosestitems = function(
    CF,
    Id_i,
    k = 10) {
  
  
  MU         = CF$MU
  SU         = CF$SU
  SI         = CF$SI
  IntI       = CF$IntI
  IntU       = CF$IntU
  averages_u = CF$averages_u
  averages_i = CF$averages_i
  n_aval_u   = CF$n_aval_u
  n_aval_i   = CF$n_aval_i
  datatype   = CF$datatype
  similarity = CF$similarity
  
  
  "A function that returns the k most similar items to the item Id_i.
      Id_i: a character, the item ID;
      k: a numeric, the number of items to be returned."
  
  if (!is.character(Id_i)) {
    stop("*** Id_i must be a character.  ***")
  }
  if (!is.integer(k) &&
      k <= 0) {
    stop("*** k must be a positive number ***")
  }
  
  
  M = nrow(MU)
  N = ncol(MU)
  
  encontrou_j = Id_i %in%colnames(MU)
  if(encontrou_j){
    j = which(colnames(MU) == Id_i)   
  } else {
    stop("*** This is not a valid item. ***")
  }
  
  s = SI[,j]
  s[j] = 0
  ind = order(s,decreasing = T,na.last = T)[1:k]
  return(colnames(SI)[ind])
}