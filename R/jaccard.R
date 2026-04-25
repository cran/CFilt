jaccard <- function(CF,type,i,j){
  
  if(CF$datatype != "consumption"){
    stop("Only for consumption data")
  }
  
  # Computes Jaccard similarity between two items
  if(type=="item"){
    
    n1 <- CF$n_aval_i[i]
    n2 <- CF$n_aval_i[j]
    n12 <- CF$IntI[i,j]
    union <- n1 + n2 - n12
    return(n12/union)
    
  } else {
    
    # Computes Jaccard similarity between two users
    if(type=="user"){
      
      n1 <- CF$n_aval_u[i]
      n2 <- CF$n_aval_u[j]
      n12 <- CF$IntU[i,j]
      union <- n1 + n2 - n12
      return(n12/union)
      
    }
  }
  stop("*** type must be item or user ***")
}
