#' Recommendation Functions
#' 
#' Function that provide an estimate of the user's rating 
#' for the item.
#' 
#' @param CF A CF object
#' @param Id_u the user Id
#' @param Id_i the item Id
#' @param type "user" or "item"
#' @param neighbors number of neighbors in the calculation.
#' @export
#' @author Jessica Kubrusly
#' @examples
#'objectCF_r <- CFbuilder(Data = movies[1:500,], Datatype = "ratings", 
#'similarity = "cosine")
#'estimaterating(CF=objectCF_r,Id_u="35",Id_i="Despicable Me 2")
#'estimaterating(CF=objectCF_r,Id_u="35",Id_i="Her")

estimaterating = function(
    CF,
    Id_u,
    Id_i,
    type = "user",
    neighbors = 
      ifelse(type == "user",nrow(CF$MU)-1,ncol(CF$MU)-1)){
  
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
  
  "A function that returns the estimated rating or consumption given by a 
      user for an item they have not yet consumed.
      Id_u: a character, the user ID;
      Id_i: a character, the item ID;
      type: a character, 'user' or 'item';
      neighbors: a numeric, the number users or itens used for the estimates."
  
  
  
  
  if (!is.character(Id_u)) {
    stop("*** Id_u must be a character. ***")
  }
  if (!is.character(Id_i)) {
    stop("*** Id_i must be a character. ***")
  }
  if (!is.numeric(neighbors)) {
    stop("*** neighbors must be a number. ***")
  }
  if (neighbors %% 1 != 0) {
    stop("*** neighbors must be a integer. ***")
  }
  if (neighbors <= 0) {
    stop("*** neighbors must be a positive number. ***")
  }
  if (type == "user" &&
      neighbors >= nrow(MU)) {
    stop(paste("*** neighbors must be <", nrow(MU)," ***"))
  }
  if (type == "item" &&
      neighbors >= ncol(MU)) {
    stop(paste("neighbors must be <", ncol(MU)," ***"))
  }
  if (type != "user" &&
      type != "item") {
    stop("type should be one of 'user' or 'item'. ***")
  }
  
  
  M = nrow(MU)
  N = ncol(MU)
  
  i = which(rownames(MU) == Id_u) 
  j = which(colnames(MU) == Id_i) 
  
  if(!is.na(MU[i,j]) & datatype == "ratings"){
    stop("*** the user has alredy consumtion this item ***")  
  }
  
  if(MU[i,j] != 0 & datatype == "consumption"){
    stop("*** the user has alredy consumtion this item ***")  
  }
  
  
  if (type == "user") {
    s = SU[i,]
    v = MU[,j]
    if(neighbors == (nrow(MU)-1)){  
      r = sum(s*v,na.rm = T)/sum(s,na.rm = T)
      return(r)
    } else {
      indices = order(s,decreasing = T,na.last = T)[1:neighbors]
      r = sum(s[indices]*v[indices],na.rm = T)/sum(s[indices,na.rm = T])
      return(r)
    } 
    
  } else {
    if(type =="item"){
      s = SI[,j]
      v = MU[i,]
      if(neighbors == (ncol(MU)-1)){
        r = sum(s*v,na.rm = T)/sum(s,na.rm = T)
        return(r)
      } else {
        indices = order(s,decreasing = T,na.last = T)[1:neighbors]
        r = sum(s[indices]*v[indices],na.rm = T)/sum(s[indices],na.rm = T)
        return(r)
      }  
    } else {
      stop("*** type must be user or item ***")
    }
    
    
  }
}


