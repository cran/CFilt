#' Recommendation Function
#'
#' Functions that provide the top k users 
#' to recommend the item Id_i.
#' 
#' @param CF A CF objec
#' @param Id_i the item Id
#' @param k an integer
#' @param type "user" or "item"
#' @export
#' @author Jessica Kubrusly
#' @examples
#'objectCF_r <- CFbuilder(Data = movies[1:500,], Datatype = "ratings", 
#'similarity = "pearson")
#'colnames(objectCF_r$MU)
#'topkusers(CF = objectCF_r, Id_i = "The Lego Movie")
#'topkusers(CF = objectCF_r, Id_i = "Her")

topkusers = function(
    CF,
    Id_i,
    k = 10,
    type = "user") {
  
  
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
  
  
  "A function that returns the top k most relevant users for the item.
      Id_i: a character, the item ID;
      k: a numeric, the number of users to be returned;
      type: a character, 'user' or 'item'."
  
  M = nrow(MU)
  N = ncol(MU)
  
  j = which(colnames(MU) == Id_i) 
  
  if (type == "user") {
    
    s = SU
    v = matrix(MU[,j],ncol = 1,nrow = nrow(MU))
    v[is.na(v)] <- 0
    
    notas = (s%*%v)/(apply(s,MARGIN = 2,FUN = "sum",na.rm = T))
    
    if(datatype == "ratings"){
      notas = notas[is.na(MU[,j])]
      users = rownames(MU)[is.na(MU[,j])]
    } else {
      notas = notas[MU[,j]==0]
      users = rownames(MU)[MU[,j]==0]
    }
    
    ind = order(notas,decreasing = T,na.last = T)[1:k]
    return(users[ind])  
    
  } else {
    
    if(type == "item"){
      
      s = matrix(SI[,j],ncol = 1,nrow=nrow(SI))
      v = MU
      v[is.na(v)] <- 0
      
      notas = (v%*%s)/sum(s,na.rm = T)
      
      
      if(datatype == "ratings"){
        notas = notas[is.na(MU[,j])]
        users = rownames(MU)[is.na(MU[,j])]
      } else {
        notas = notas[MU[,j]==0]
        users = rownames(MU)[MU[,j]==0]
      }
      
      ind = order(notas,decreasing = T,na.last = T)[1:k]
      return(users[ind])
      
    } else {
      stop("*** type must be user or item ***")
    }
    
  }
}

