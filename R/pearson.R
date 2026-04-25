pearson <- function(CF, type = c("user", "item"), i, j) {
  
  
  # Validation ----
  if (CF$datatype != "rating") {
    stop("*** only defined for rating data. ***")
  }
  
  if (!type %in% c("user", "item")) {
    stop("*** type must be 'user' or 'item' ***")
  }
  
  # Single comparison ----
  
  if(length(j)==1){
    if(type=="user"){
      v = CF$MU[i, , drop = FALSE]
      v = as.vector(v) - CF$averages_u[i]
      v[CF$MU[i, ] == 0] = NA
      
      w = CF$MU[j, , drop = FALSE]
      w = as.vector(w) - CF$averages_u[j]
      w[CF$MU[j, ] == 0] = NA
      
      s = ifelse(sum(v*v,na.rm=T)==0 | sum(w*w,na.rm=T)==0,0,
                 sum(v*w,na.rm=T)/(sqrt(sum(v*v,na.rm=T)*sum(w*w,na.rm=T))))
      return(s)
    } else {
      if(type=="item"){
        v = CF$MU[, i, drop = FALSE]
        v = as.vector(v) - CF$averages_i[i]
        v[CF$MU[,i] == 0] = NA
        
        w = CF$MU[,j, drop = FALSE]
        w = as.vector(w) - CF$averages_i[j]
        w[CF$MU[,j] == 0] = NA
        
        s = ifelse(sum(v*v,na.rm=T)==0 | sum(w*w,na.rm=T)==0,0,
                   sum(v*w,na.rm=T)/(sqrt(sum(v*v,na.rm=T)*sum(w*w,na.rm=T))))
        return(s)
        
      }
    }
    stop("*** type must be item or user ***")
  }
  
  # Multiple comparisons ----
  
  else { 
    if(type=="user"){
      v = CF$MU[i, , drop = FALSE]
      v = as.vector(v) - CF$averages_u[i]
      v[CF$MU[i, ] == 0] = NA
      
      W = CF$MU[j, , drop = FALSE]
      
      W_centered = W - matrix(CF$averages_u[j],
                              nrow = length(j),
                              ncol = ncol(CF$MU),
                              byrow = FALSE)
      
      W_centered[W == 0] = NA
      
      s = apply(W_centered, MARGIN = 1, function(x){
        ifelse(sum(v*v,na.rm=T)==0 | sum(x*x,na.rm=T)==0,0,
               sum(v*x,na.rm=T)/(sqrt(sum(v*v,na.rm=T)*sum(x*x,na.rm=T))))
      })
      return(s)
    } else {
      if(type=="item"){
        v = CF$MU[, i, drop = FALSE]
        v = as.vector(v) - CF$averages_i[i]
        v[CF$MU[, i] == 0] = NA
        
        W = CF$MU[, j, drop = FALSE]
        
        W_centered = W - matrix(CF$averages_i[j],
                                nrow = nrow(CF$MU),
                                ncol = length(j),
                                byrow = TRUE)
        
        W_centered[W == 0] = NA
        
        s = apply(W_centered, MARGIN = 2, function(x){
          ifelse(sum(v*v,na.rm=T)==0 | sum(x*x,na.rm=T)==0,0,
                 sum(v*x,na.rm=T)/(sqrt(sum(v*v,na.rm=T)*sum(x*x,na.rm=T))))
        })
        return(s)
      }
    }
    stop("*** type must be item or user ***")
  }
}