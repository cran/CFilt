#' Similarity calculation Function
#'
#' Functions that returns the Jaccard similarity between two items or users.
#' 
#' @param CF A CF objec
#' @param type "user" or "item"
#' @param i "user" or "item" Id or index
#' @param j "user" or "item" Id or index#' 
#' @export
#' @author Jessica Kubrusly
#' @examples
#'objectCF_r <- CFbuilder(Data = movies[1:500,c(1,2)], Datatype = "consumption", 
#'similarity = "jaccard")
#'jaccard(CF=objectCF_r,type = "user",i="1",j="2")
#'jaccard(CF=objectCF_r,type = "item",i="Her",j="Frozen")


jaccard = function(CF,type,i,j){
  
  if(CF$datatype != "consumption"){
    stop("Only for consumption data")
  }
  
  if(type=="item"){
    
    n1 = CF$n_aval_i[i]
    n2 = CF$n_aval_i[j]
    n12 = CF$IntI[i,j]
    uniao <- n1 + n2 - n12
    return(n12/uniao)
    
  } else {
    
    if(type=="user"){
      
      n1 = CF$n_aval_u[i]
      n2 = CF$n_aval_u[j]
      n12 = CF$IntU[i,j]
      uniao <- n1 + n2 - n12
      return(n12/uniao)
      
    }
  }
  stop("*** type must be item or user ***")
}

