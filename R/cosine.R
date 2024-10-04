#' Similarity calculation Function
#'
#' Functions that returns the cosine similarity between two items or users.
#' 
#' @param CF A CF objec
#' @param type "user" or "item"
#' @param i "user" or "item" Id or index
#' @param j "user" or "item" Id or index#' 
#' @export
#' @author Jessica Kubrusly
#' @examples
#'objectCF_r <- CFbuilder(Data = movies[1:500,], Datatype = "ratings", 
#'similarity = "cosine")
#'cosine(CF=objectCF_r,type = "user",i="1",j="2")
#'cosine(CF=objectCF_r,type = "item",i="Her",j="Frozen")

cosine = function(CF,type,i,j){
  
  if(CF$datatype != "ratings"){
    stop("Only for ratings data")
  }
  
  if(length(j)==1){
    if(type=="user"){
      v = CF$MU[i,]
      w = CF$MU[j,]
      s = ifelse(sum(v*v,na.rm=T)==0 | sum(w*w,na.rm=T)==0,0,
                 sum(v*w,na.rm=T)/(sqrt(sum(v*v,na.rm=T)*sum(w*w,na.rm=T))))
      return(s)
    } else {
      if(type=="item"){
        v = CF$MU[,i]
        w = CF$MU[,j]
        s = ifelse(sum(v*v,na.rm=T)==0 | sum(w*w,na.rm=T)==0,0,
                   sum(v*w,na.rm=T)/(sqrt(sum(v*v,na.rm=T)*sum(w*w,na.rm=T))))
        return(s)
      }
    }
    stop("*** type must be item or user ***")
  } else {
    if(type=="user"){
      v = CF$MU[i,]
      w = CF$MU[j,]
      s = apply(w,MARGIN = 1,function(x){
        ifelse(sum(v*v,na.rm=T)==0 | sum(x*x,na.rm=T)==0,0,
               sum(v*x,na.rm=T)/(sqrt(sum(v*v,na.rm=T)*sum(x*x,na.rm=T))))
      })
      return(s)
    } else {
      if(type=="item"){
        v = CF$MU[,i]
        w = CF$MU[,j]
        s = apply(w,MARGIN = 2,function(x){
          ifelse(sum(v*v,na.rm=T)==0 | sum(x*x,na.rm=T)==0,0,
                 sum(v*x,na.rm=T)/(sqrt(sum(v*v,na.rm=T)*sum(x*x,na.rm=T))))
        })
        return(s)
      }
      stop("*** type must be item or user ***")
    }
  }
}
