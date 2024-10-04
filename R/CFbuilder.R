#'@name CFbuilder
#'@aliases CFbuilder
#'@title The constructor function of the CFilt class.
#'@description
#'The constructor function of the CFilt class.
#'@usage 
#'CFbuilder(Data,Datatype,similarity)
#'
#'CFbuilder(
#'Data,
#'Datatype = ifelse(ncol(Data)==2,"consumption","ratings"),
#'similarity = ifelse(Datatype == "consumption","jaccard","pearson")
#')
#'@param Data a dataframe with 2 or 3 columns. The first column indicates the 
#'user ID, the second the item ID and the third the rating (only if 
#'Datatype = 'rating').
#'@param Datatype a character that indicates the data type: 'rating' or 
#''consumption'.
#'@param similarity a character that indicates the similarity type. 
#'For 'datatype='ratings', 'cossine' or 'person'. For datatype='consumption', 
#''jaccard'.
#'@author Jessica Kubrusly 
#'@return a CF class object.
#'@references
#'LINDEN, G.; SMITH, B.; YORK, J. Amazon. com recommendations: Item-to-item 
#'collaborative filtering. Internet Computing, IEEE, v. 7, n. 1, p. 76-80,2003
#'@seealso \code{\link[CFilt]{CF-class}}
#'@keywords "Collaborative Filtering" "Cosine Similarity" "Pearson Similarity" 
#'"Jaccard Silimarity"
#'@export
#'@examples
#'CF1 <- CFbuilder(Data = movies[1:300,], Datatype = "ratings", 
#'similarity = "pearson") #or
#'CF1_ <- CFbuilder(Data = movies[1:300,])
#'CF2 <- CFbuilder(Data = movies[1:300,], Datatype = "ratings", 
#'similarity = "cosine") #or
#'CF2_ <- CFbuilder(Data = movies[1:300,], similarity = "cosine")
#'CF3 <- CFbuilder(Data = movies[1:300,-3], Datatype = "consumption", 
#'similarity = "jaccard") #or
#'CF3_ <- CFbuilder(Data = movies[1:300,-3])

CFbuilder <-
  function(Data,
           Datatype = 
             ifelse(
               ncol(Data)==2,
               "consumption",
               "ratings"),
           similarity = 
             ifelse(
               Datatype == "consumption",
               "jaccard",
               "pearson")) {
    
    
    if(ncol(Data)!= 2 && ncol(Data)!= 3){
      stop("*** Datatype can have only 2 (consumption) or 3(ratings) columns. ***")
    }
    
    if (Datatype != "consumption" &&
        Datatype != "ratings") {
      stop("*** Datatype can be only 'consumption' or 'ratings'. ***")
    }
    
    if(!is.data.frame(Data)){
      stop("*** Data must be a dataframe object ***")
    }
    
    Data = as.data.frame(Data)
    
    obj_FC <- CF()
    
    obj_FC$datatype = Datatype
    obj_FC$similarity = similarity
    
    n_linhas = nrow(Data)
    
    Data[, 1] <- as.character(Data[, 1])
    Data[, 2] <- as.character(Data[, 2])
    

    nome_u = unique(Data[,1])
    m = length(nome_u)
    
    nome_i = unique(Data[,2])
    n = length(nome_i)
    
    if (Datatype == "consumption" && ncol(Data) == 2) {
      if(similarity=="jaccard"){
        
        obj_FC$n_aval_u = numeric(m)
        obj_FC$n_aval_i = numeric(n)
        
        names(obj_FC$n_aval_u) = nome_u
        names(obj_FC$n_aval_i) = nome_i

          
        ### Matriz de Utilidade ####
      
        obj_FC$MU = matrix(0,
                           nrow = m,
                           ncol = n,
                           dimnames = list(nome_u,nome_i)
        )
        
        message("Step 1 of 3: Building MU") # Configuração da barra de progresso
  
        pb <- txtProgressBar(min = 0,
                             max = n_linhas,
                             style = 3)
      
  
  
        sapply(1:n_linhas,function(i){
          utils::setTxtProgressBar(pb, i) # Início da barra de progresso
          usua = as.character(Data[i, 1])
          item = as.character(Data[i, 2])
          if(obj_FC$MU[usua, item] == 0){
            obj_FC$MU[usua, item] = 1
            obj_FC$n_aval_i[item] = obj_FC$n_aval_i[item] + 1
            obj_FC$n_aval_u[usua] = obj_FC$n_aval_u[usua] + 1
          }
        })
  
        close(pb) 
  
  
        
        ### SU ####
        message("Step 2 of 3: Building SU")
        pb <- txtProgressBar(min = 0,
                             max = m,
                             style = 3)
        
        obj_FC$SU   = matrix(0, m, m, dimnames = list(nome_u, nome_u))
        obj_FC$IntU = matrix(NA, m, m, dimnames = list(nome_u, nome_u))
  
        sapply(1:m,function(i){
        
          utils::setTxtProgressBar(pb,i)
          
          n1 = obj_FC$n_aval_u[i]
          
          linha_i = obj_FC$MU[i,]
  
          intersecao = apply(X=obj_FC$MU,
                             MARGIN = 1,
                             FUN = function(linha){
                               sum(linha*linha_i)
                               })
          
          obj_FC$IntU[i,] = intersecao           
          obj_FC$IntU[,i] = intersecao
          
          indices_n_nulo = which(intersecao!=0)
          
          n12 = intersecao[indices_n_nulo]
          n2 = obj_FC$n_aval_u[indices_n_nulo]
          uniao <- n1 + n2 - n12
          s = n12/uniao
          
          obj_FC$SU[i,indices_n_nulo] =  s
          obj_FC$SU[indices_n_nulo,i] =  s
          
        })
  
        utils::setTxtProgressBar(pb, m)
        
        obj_FC$SU[m,m] = 1
        obj_FC$IntU[m,m] = obj_FC$n_aval_u[m]
        
        close(pb)
        
        
        ### SI ####
        message("Step 3 of 3: Building SI")
        pb <- txtProgressBar(min = 0,
                             max = n,
                             style = 3)
        
        obj_FC$SI = matrix(0, n, n, dimnames = list(nome_i, nome_i))
        obj_FC$IntI = matrix(NA, n, n, dimnames = list(nome_i, nome_i))
        
        sapply(1:n,function(i){
          
          utils::setTxtProgressBar(pb, i)
          
          n1 = obj_FC$n_aval_i[i]
          
          coluna_i = obj_FC$MU[,i]
          
          intersecao = apply(X=obj_FC$MU,
                             MARGIN = 2,
                             FUN = function(coluna){
                               sum(coluna*coluna_i)
                             })
          
          obj_FC$IntI[i,] = intersecao           
          obj_FC$IntI[,i] = intersecao
          
          indices_n_nulo = which(intersecao!=0)
          
          n12 = intersecao[indices_n_nulo]
          n2 = obj_FC$n_aval_i[indices_n_nulo]
          uniao <- n1 + n2 - n12
          s = n12/uniao
          
          obj_FC$SI[i,indices_n_nulo] =  s
          obj_FC$SI[indices_n_nulo,i] =  s
        
        })
        
        utils::setTxtProgressBar(pb, n)
        
      
        close(pb)
        
        return(obj_FC)
      } else {
        stop("For consumption data similarity must to be jaccard.")
      }
    } else {
      if (Datatype == "ratings" && ncol(Data) == 3) {
        
        if(similarity == "cosine"){
        
          Data[, 3] <- as.numeric(Data[, 3])
      
          #criar a matriz
          obj_FC$MU = matrix(NA,
                             nrow = m,
                             ncol = n,
                             dimnames = list(nome_u, nome_i)
          )
          
          #Construindo a MU, averages_u, averages_i, n_aval_i e n_aval_u
          obj_FC$averages_u = rep(0, m)
          obj_FC$averages_i = rep(0, n)
          obj_FC$n_aval_u = rep(0, m)
          obj_FC$n_aval_i = rep(0, n)
          names(obj_FC$averages_u) <- nome_u
          names(obj_FC$averages_i) <- nome_i
          names(obj_FC$n_aval_u) <- nome_u
          names(obj_FC$n_aval_i) <- nome_i
          
          
          n_linhas = dim(Data)[1]
          message("Step 1 of 3: Building MU")
          pb <- txtProgressBar(min = 0,
                               max = n_linhas,
                               style = 3)
          
          sapply(1:n_linhas, function(i){
            
            utils::setTxtProgressBar(pb, i)
            
            usua =  as.character(Data[i, 1])
            item =  as.character(Data[i, 2])
            nota =  as.numeric(Data[i, 3])
            
            obj_FC$MU[usua, item] = nota
            
            obj_FC$averages_u[usua] <-
              (obj_FC$averages_u[usua] * obj_FC$n_aval_u[usua])/
              (obj_FC$n_aval_u[usua] + 1) + nota/(obj_FC$n_aval_u[usua] + 1)
            
            obj_FC$averages_i[item] <-
              (obj_FC$averages_i[item] * obj_FC$n_aval_i[item])/
              (obj_FC$n_aval_i[item] + 1) + nota/(obj_FC$n_aval_i[item] + 1)
            
            obj_FC$n_aval_u[usua] <- obj_FC$n_aval_u[usua] + 1
            obj_FC$n_aval_i[item] <- obj_FC$n_aval_i[item] + 1
          })
          
          close(pb)
          
          
          ### SU ####
          message("Step 2 of 3: Building SU")
          pb <- txtProgressBar(min = 0,
                               max = m,
                               style = 3)
          obj_FC$SU = matrix(0, m, m, dimnames = list(nome_u, nome_u))
          obj_FC$IntU = matrix(NA, m, m, dimnames = list(nome_u, nome_u))
          
          sapply(1:m, function(i){
          
            utils::setTxtProgressBar(pb, i)
            
            linha_i = !is.na(obj_FC$MU[i, ])
            
            intersecao = apply(X=obj_FC$MU,
                               MARGIN = 1,
                               FUN = function(linha){
                                 linha_j = !is.na(linha)
                                 sum(linha_j*linha_i,na.rm = T)
                               })
            
            obj_FC$IntU[i,] = intersecao           
            obj_FC$IntU[,i] = intersecao
            
            indices_n_nulo = which(intersecao!=0)
            
            v = obj_FC$MU[i,]
            s = apply(
              obj_FC$MU[indices_n_nulo,], 
              MARGIN = 1, 
              function(w){
                sum(v*w,na.rm = T)/sqrt(sum(v*v,na.rm = T)*sum(w*w,na.rm = T))
              }
            )
            obj_FC$SU[i,indices_n_nulo] =  s
            obj_FC$SU[indices_n_nulo,i] =  s
            
            
            
          })
          
          utils::setTxtProgressBar(pb, m)
          
          close(pb)
          
          
          ### SI ####
          message("Step 3 of 3: Building SI")
          pb <- txtProgressBar(min = 0,
                               max = n,
                               style = 3)
          
          obj_FC$SI = matrix(0, n, n, dimnames = list(nome_i, nome_i))
          obj_FC$IntI = matrix(NA, n, n, dimnames = list(nome_i, nome_i))
          
          sapply(1:n, function(i){
            
            utils::setTxtProgressBar(pb, i)
            
            coluna_i = !is.na(obj_FC$MU[,i])
            
            intersecao = apply(X=obj_FC$MU,
                               MARGIN = 2,
                               FUN = function(coluna){
                                 coluna_j = !is.na(coluna)
                                 sum(coluna_j*coluna_i, na.rm = T)
                               })
            
            obj_FC$IntI[i,] = intersecao           
            obj_FC$IntI[,i] = intersecao
            
            indices_n_nulo = which(intersecao!=0)
            
            v = obj_FC$MU[,i]
            
            s = apply(
              obj_FC$MU[,indices_n_nulo], 
              MARGIN = 2, 
              function(w){
                sum(v*w,na.rm = T)/sqrt(sum(v*v,na.rm = T)*sum(w*w,na.rm = T))
              }
            )
            obj_FC$SI[i,indices_n_nulo] =  s
            obj_FC$SI[indices_n_nulo,i] =  s
          })
          
          utils::setTxtProgressBar(pb, n)
          
          close(pb)
          
          return(obj_FC)
        } else {
            if(similarity == "pearson"){
              
              Data[, 3] <- as.numeric(Data[, 3])
              
              #criar a matriz
              obj_FC$MU = matrix(NA,
                                 nrow = m,
                                 ncol = n,
                                 dimnames = list(nome_u, nome_i)
              )
              
              #Construindo a MU, averages_u, averages_i, n_aval_i e n_aval_u
              obj_FC$averages_u = rep(0, m)
              obj_FC$averages_i = rep(0, n)
              obj_FC$n_aval_u = rep(0, m)
              obj_FC$n_aval_i = rep(0, n)
              names(obj_FC$averages_u) <- nome_u
              names(obj_FC$averages_i) <- nome_i
              names(obj_FC$n_aval_u) <- nome_u
              names(obj_FC$n_aval_i) <- nome_i
              
              
              n_linhas = dim(Data)[1]
              message("Step 1 of 3: Building MU")
              pb <- txtProgressBar(min = 0,
                                   max = n_linhas,
                                   style = 3)
              
              sapply(1:n_linhas, function(i){
                
                utils::setTxtProgressBar(pb, i)
                
                usua =  as.character(Data[i, 1])
                item =  as.character(Data[i, 2])
                nota =  as.numeric(Data[i, 3])
                
                obj_FC$MU[usua, item] = nota
                
                obj_FC$averages_u[usua] <-
                  (obj_FC$averages_u[usua] * obj_FC$n_aval_u[usua])/
                  (obj_FC$n_aval_u[usua] + 1) + nota/(obj_FC$n_aval_u[usua] + 1)
                
                obj_FC$averages_i[item] <-
                  (obj_FC$averages_i[item] * obj_FC$n_aval_i[item])/
                  (obj_FC$n_aval_i[item] + 1) + nota/(obj_FC$n_aval_i[item] + 1)
                
                obj_FC$n_aval_u[usua] <- obj_FC$n_aval_u[usua] + 1
                obj_FC$n_aval_i[item] <- obj_FC$n_aval_i[item] + 1
              })
              
              close(pb)
              
              
              ### SU ####
              message("Step 2 of 3: Building SU")
              pb <- txtProgressBar(min = 0,
                                   max = m,
                                   style = 3)
              obj_FC$SU = matrix(0, m, m, dimnames = list(nome_u, nome_u))
              obj_FC$IntU = matrix(NA, m, m, dimnames = list(nome_u, nome_u))
              
              sapply(1:m, function(i){
                
                utils::setTxtProgressBar(pb, i)
                
                linha_i = !is.na(obj_FC$MU[i, ])
                
                intersecao = apply(X=obj_FC$MU,
                                   MARGIN = 1,
                                   FUN = function(linha){
                                     linha_j = !is.na(linha)
                                     sum(linha_j*linha_i, na.rm = T)
                                   })
                
                obj_FC$IntU[i,] = intersecao           
                obj_FC$IntU[,i] = intersecao
                
                indices_n_nulo = which(intersecao!=0)
    
                v = obj_FC$MU[i,]- obj_FC$averages_u[i]
                
                w = obj_FC$MU[indices_n_nulo,] - 
                  matrix(obj_FC$averages_u[indices_n_nulo],
                         nrow = length(indices_n_nulo),
                         ncol = length(v),
                         byrow = F)
                
                s = apply(w, MARGIN = 1, function(linha_w){
                  ifelse(sum(v*v,na.rm=T)==0 | 
                           sum(linha_w*linha_w,na.rm=T) == 0 | 
                           sum(v*linha_w,na.rm=T) == 0,0,
                         sum(v*linha_w,na.rm=T)/
                           (sqrt(sum(v*v,na.rm=T)*sum(linha_w*linha_w,na.rm=T)))
                  )
                }
                )
                
                obj_FC$SU[i,indices_n_nulo] =  s
                obj_FC$SU[indices_n_nulo,i] =  s
              
              })
              

              utils::setTxtProgressBar(pb, m)
              
              close(pb)
              
              
              ### SI ####
              message("Step 3 of 3: Building SI")
              pb <- txtProgressBar(min = 0,
                                   max = n,
                                   style = 3)
              obj_FC$SI = matrix(0, n, n, dimnames = list(nome_i, nome_i))
              obj_FC$IntI = matrix(NA, n, n, dimnames = list(nome_i, nome_i))
              
              sapply(1:n, function(i){
                
                utils::setTxtProgressBar(pb, i)
                
                
                coluna_i = !is.na(obj_FC$MU[,i])
                
                intersecao = apply(X=obj_FC$MU,
                                   MARGIN = 2,
                                   FUN = function(coluna){
                                     coluna_j = !is.na(coluna)
                                     sum(coluna_j*coluna_i,na.rm = T)
                                   })
                
                obj_FC$IntI[i,] = intersecao           
                obj_FC$IntI[,i] = intersecao
                
                indices_n_nulo = which(intersecao!=0)
                
                v = obj_FC$MU[,i]- obj_FC$averages_i[i]
                w = obj_FC$MU[,indices_n_nulo] - 
                  matrix(obj_FC$averages_i[indices_n_nulo],
                         nrow = length(v),
                         ncol = length(indices_n_nulo),
                         byrow = T)
                
                s = apply(w, MARGIN = 2, function(coluna_w){
                  ifelse(sum(v*v,na.rm=T)==0 | 
                           sum(coluna_w*coluna_w,na.rm=T) == 0 | 
                           sum(v*coluna_w,na.rm=T) == 0,0,
                         sum(v*coluna_w,na.rm=T)/
                           (sqrt(sum(v*v,na.rm=T)*sum(coluna_w*coluna_w,na.rm=T)))
                  )
                }
                )
                
                
                obj_FC$SI[i,indices_n_nulo] =  s
                obj_FC$SI[indices_n_nulo,i] =  s
              })
              
              
              utils::setTxtProgressBar(pb, n)
              
              close(pb)
              
              return(obj_FC)
            } else {
              stop("For rating data similarity must be cosine or pearson")
            }
          }
      } else {
        stop("*** Datatype must be consumption or ratings***")
      }
    }
  }


