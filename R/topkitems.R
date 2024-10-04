#' Recommendation Function
#'
#' Functions that provide the top k items 
#' to be recommended to the user Id_u.
#' 
#' @param CF A CF objec
#' @param Id_u the user Id
#' @param k an integer
#' @param type "user" or "item"
#' @export
#' @author Jessica Kubrusly
#' @examples
#'objectCF_r <- CFbuilder(Data = movies[1:500,], Datatype = "ratings", 
#'similarity = "pearson")
#'u1 = rownames(objectCF_r$MU)[1]
#'topkitems(CF=objectCF_r,Id_u = u1)
#'u2 = rownames(objectCF_r$MU)[2]
#'topkitems(CF=objectCF_r,Id_u = u2)


topkitems = function(
    CF,
    Id_u,
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
  
  
  "A function that returns the top k most relevant items for the user.
      Id_u: a character, the user ID;
      k: a numeric, the number of items to be returned;
      type: a character, 'user' or 'item'."
  
  M = nrow(MU)
  N = ncol(MU)
  
  i = which(rownames(MU) == Id_u) 
  
  if (type == "user") {
    
    
    s = matrix(SU[,i],nrow = 1,ncol = ncol(SU))
    v = MU
    v[is.na(v)] <- 0
    
    notas = (s%*%v)/sum(s)
    
    
    if(datatype == "ratings"){
      notas = notas[is.na(MU[i,])]
      itens = colnames(MU)[is.na(MU[i,])]  
    } else {
      notas = notas[MU[i,]==0]
      itens = colnames(MU)[MU[i,]==0]
    }
    
    ind = order(notas,decreasing = T,na.last = T)[1:k]
    return(itens[ind])  
    
  } else {
    
    if(type == "item"){
      
      s = SI
      v = matrix(MU[i,],ncol=ncol(MU),nrow=1)
      v[is.na(v)] <- 0
      notas = (v%*%s)/(apply(s, MARGIN=2,FUN = "sum",na.rm = T))
      
      
      if(datatype == "ratings"){
        notas = notas[is.na(MU[i,])]
        itens = colnames(MU)[is.na(MU[i,])]  
      } else {
        notas = notas[MU[i,]==0]
        itens = colnames(MU)[MU[i,]==0]
      }
      
      ind = order(notas,decreasing = T,na.last = T)[1:k]
      return(itens[ind])
      
    } else {
      stop("*** type must be user or item ***")
    }
    
  }
}

