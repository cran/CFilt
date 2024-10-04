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
#'@field IntU A symmetric matrix that records the number of users in common who 
#'consumed each pair of items.
#'@field IntI A symmetric matrix that records the number of items in common that 
#'have been consumed by each pair of users.
#'@field averages_u A vector that contains the averages of users' ratings.
#'@field averages_i A vector that contains the averages of items' ratings.
#'@field n_aval_u A vector that stores the number of items rated by each user.
#'@field n_aval_i A vector that stores the number of users who consumed each 
#'item.
#'@field datatype A character that indicates the type of data, which can be 
#'either "consumption" or "rating".
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
#'@importFrom utils txtProgressBar
#'@import methods 
#'@export
#'@examples
#'objectCF_r <- CFbuilder(Data = movies[1:500,], Datatype = "ratings", 
#'similarity = "pearson")
#'dim(objectCF_r$MU)
#'colnames(objectCF_r$MU) #movies Id
#'rownames(objectCF_r$MU) #users Id
#'dim(objectCF_r$SU)
#'dim(objectCF_r$SI)
#'objectCF_r$averages_u
#'hist(objectCF_r$averages_u)
#'objectCF_r$averages_i
#'hist(objectCF_r$averages_i)
#'objectCF_r$n_aval_u
#'summary(objectCF_r$n_aval_u)
#'barplot(table(objectCF_r$n_aval_u))
#'objectCF_r$n_aval_i
#'summary(objectCF_r$n_aval_i)
#'barplot(table(objectCF_r$n_aval_i))
#'objectCF_r$addnewuser(Id_u = "newuser1", 
#'Ids_i = "The Hunger Games: Catching Fire", r = 5)
#'rownames(objectCF_r$MU) #users Id
#'objectCF_r$n_aval_u["newuser1"]
#'objectCF_r$averages_u["newuser1"]
#'objectCF_r$addnewuser(Id_u = "newuser2",
#'Ids_i = c("Frozen","Her","Iron Man 3"),r = c(2,4,3))
#'rownames(objectCF_r$MU) #users Id
#'objectCF_r$n_aval_u["newuser2"]
#'objectCF_r$averages_u["newuser2"]
#'objectCF_r$addnewuser(Id_u = list("newuser3","newuser4"),
#'Ids_i = list(c("Lincoln","Monsters University","The Lego Movie","Frozen"),
#'c("The Wolverine","The Lego Movie")),r = list(c(1,4,5,4),c(4,5)))
#'rownames(objectCF_r$MU) #users Id
#'objectCF_r$n_aval_u[c("newuser3","newuser4")]
#'objectCF_r$averages_u[c("newuser3","newuser4")]
#'objectCF_r$newrating(Id_u = list("newuser1","newuser1","newuser2","newuser4"),
#'Id_i = list("The Lego Movie","Wreck-It Ralph","Fast & Furious 6",
#'"12 Years a Slave"),r = list(4,5,4,2))
#'objectCF_r$n_aval_u[c("newuser1","newuser2","newuser3","newuser4")]
#'objectCF_r$averages_u[c("newuser1","newuser2","newuser3","newuser4")]
#'objectCF_r$addnewitem(Id_i = "Oppenheimer",
#'Ids_u = c("newuser1","newuser2","newuser3","newuser4","1","2","4","6","10",
#'"11","20","32"),r = c(1,2,3,1,5,4,5,4,1,3,5,4))
#'colnames(objectCF_r$MU)
#'objectCF_r$n_aval_i["Oppenheimer"]
#'objectCF_r$averages_i["Oppenheimer"]
#'objectCF_c <- CFbuilder(Data = movies[1:500,-3], Datatype = "consumption", 
#'similarity = "jaccard")
#'dim(objectCF_c$MU)
#'colnames(objectCF_c$MU) #movies Id
#'rownames(objectCF_c$MU) #users Id
#'dim(objectCF_c$SU)
#'dim(objectCF_c$SI)
#'objectCF_c$averages_u
#'objectCF_c$averages_i
#'objectCF_c$n_aval_u
#'summary(objectCF_c$n_aval_u)
#'barplot(table(objectCF_c$n_aval_u))
#'objectCF_c$n_aval_i
#'summary(objectCF_c$n_aval_i)
#'barplot(table(objectCF_c$n_aval_i))
#'objectCF_c$addnewuser(Id_u = "newuser1", 
#'Ids_i = "The Hunger Games: Catching Fire")
#'rownames(objectCF_c$MU) #users Id
#'objectCF_c$n_aval_u["newuser1"]
#'objectCF_c$addnewuser(Id_u = "newuser2",
#'Ids_i = c("Frozen","Her","Iron Man 3"))
#'rownames(objectCF_c$MU) #users Id
#'objectCF_c$n_aval_u["newuser2"]
#'objectCF_c$addnewuser(Id_u = list("newuser3","newuser4"),Ids_i = list(
#'c("Lincoln","Monsters University","The Lego Movie","Frozen"),
#'c("The Wolverine","The Lego Movie")))
#'rownames(objectCF_c$MU)
#'objectCF_c$n_aval_u[c("newuser3","newuser4")]
#'objectCF_c$MU["newuser1","The Lego Movie"]
#'objectCF_c$newrating(Id_u = list("newuser1","newuser1","newuser2","newuser4"),
#'Id_i = list("The Lego Movie","Wreck-It Ralph","Fast & Furious 6",
#'"12 Years a Slave"))
#'objectCF_c$n_aval_u[c("newuser1","newuser2","newuser3","newuser4")]
#'objectCF_c$averages_u[c("newuser1","newuser2","newuser3","newuser4")]
#'objectCF_c$addnewitem(Id_i = "Oppenheimer",
#'Ids_u = c("newuser1","newuser2","newuser3","newuser4","1","2","4","6","10",
#'"11","20","32"),r = c(1,2,3,1,5,4,5,4,1,3,5,4))
#'colnames(objectCF_c$MU)
#'objectCF_c$n_aval_i["Oppenheimer"]
#'objectCF_c$averages_i["Oppenheimer"]

CF = setRefClass(
  
  "CF",
  
  fields = list(
    MU         = "matrix",
    SU         = "matrix",
    SI         = "matrix",
    IntI       = "matrix",
    IntU       = "matrix",
    averages_u = "numeric",
    averages_i = "numeric",
    n_aval_u   = "numeric",
    n_aval_i   = "numeric",
    datatype   = "character",
    similarity = "character"
  ),
  
  methods = list(
    
    addnewuser = function(Id_u, Ids_i, r = NULL) {
      "A method that adds a new user who consumed items already existing in the 
      recommendation system. 
      Id_u: a character, the new user ID;
      Ids_i: a character vector, the IDs of the items consumed by the user;
      r: a numeric vector, the ratings of the items consumed by the new user 
      (only for ratings datatype).
      To add more than one new user, lists can be used. 
      Id_u: a list of characters; 
      Ids_i: a list of characters vectors; 
      r: list of numeric vectors."

      
      if (is.list(Id_u) && is.list(Ids_i)) {
        u = length(Id_u)
        if(datatype == "ratings"){
          for (k in 1:u) {
            addnewuser(Id_u = Id_u[[k]], 
                       Ids_i = Ids_i[[k]],
                       r = r[[k]])
            }
        } else {
          for (k in 1:u) {
            addnewuser(Id_u = Id_u[[k]], 
                       Ids_i = Ids_i[[k]])
          }  
        }
      } else{
        
        if (!is.character(Id_u)) {
            stop("*** Id_u must be a character. ***")
        }
          
        if (sum(!is.character(Ids_i)) != 0) {
            stop("*** Ids_i must be a character vector. ***")
          }
          
          
        # Verificar se existe um usuario com o mesmo identificador:
          
        N = dim(MU)[2]
        M = dim(MU)[1]
            
        k = M + 1 # linha do novo usuario
          
        encontrou_u = Id_u %in% rownames(MU)
        if(encontrou_u){
          stop(paste0("*** user ",Id_u," is already registered in the system ***"))
        }
        
        encontrou_j = Ids_i %in% colnames(MU)
        if(prod(encontrou_j)==0){
            stop("*** There are consumed items that are not yet registered in the system. ***")
          }
          
          
        # Dados de Avaliacoes ####
        if (datatype == "ratings") {
        
          if(is.null(r)){
            stop("*** r must be not NULL ***")
          }
          
          if(length(r) != length(Ids_i)){
            stop("*** r and Ids_i must have same size ***")
          }
          
          
          ## MU ####
          novo_usuario <- matrix(NA, 
                                 nrow =  1, 
                                 ncol = N, 
                                 dimnames = 
                                   list(Id_u, colnames(MU)))
          novo_usuario[,Ids_i] = r
          itens_aval = !is.na(novo_usuario[1,])
          
          MU <<- rbind(MU, novo_usuario)
          
          
          n_aval_i <<- n_aval_i + itens_aval
          n_aval_u <<- c(n_aval_u,sum(itens_aval))
          names(n_aval_u)[k] <<- Id_u
          
          
          averages_u[k] <<- mean(r)
          names(averages_u)[k] <<- Id_u
          
          averages_i[itens_aval] <<-
            (averages_i[itens_aval]*(n_aval_i[itens_aval] - 1) + 
              novo_usuario[itens_aval])/(n_aval_i[itens_aval])
          
          #alteracao IntU
          if(sum(itens_aval)!=1){
            int_u_k = apply(X = MU[-k,itens_aval], 
                            MARGIN = 1, 
                            FUN = function(x){
                              sum(!is.na(x))
                            })  
          } else {
            int_u_k = !is.na(MU[-k,itens_aval])
          }
          
          IntU <<- cbind(IntU,int_u_k)
          IntU <<- rbind(IntU,c(int_u_k,n_aval_u[k]))
          colnames(IntU) <<- rownames(MU)
          rownames(IntU) <<- rownames(MU)
          
          #alteracao SU
          
          if(similarity=="pearson"){
            s = pearson(type="user",i = k,j = (1:k),CF = .self)
          } else {
            s = cosine(type="user",i = k,j = (1:k),CF = .self)
          }
          
          SU <<- cbind(SU,s[-k])
          SU <<- rbind(SU,s)
          colnames(SU)[k] <<- Id_u
          rownames(SU)[k] <<- Id_u
          
          
          #alteracao IntI 
          itens = colnames(IntI)[itens_aval]
          n_itens = length(itens)
          
          for(i in 1:n_itens){
            
            item_i = itens[i]
            
            for(j in i:n_itens){
              item_j = itens[j]
              IntI[item_i,item_j] <<- 
                IntI[item_i,item_j] + 1
              IntI[item_j,item_i] <<- IntI[item_i,item_j]
            }
            
            if(similarity=="pearson"){
              
              s = sapply(X = colnames(SI),
                         FUN = "pearson",
                         type = "item",
                         i = item_i,CF = .self
              )  
            } else {
              if(similarity=="cosine"){
                s = sapply(X = colnames(SI),
                           FUN = "cosine",
                           type = "item",
                           i = item_i,CF = .self
                ) 
              }
            }
            
            SI[item_i,] <<- s
            SI[,item_i] <<- s
            
          }
        
        
          
          item_i = itens[n_itens]
          
          
          
          if(similarity=="pearson"){
            
            s = sapply(X = colnames(SI),
                       FUN = "pearson",
                       type = "item",
                       i = item_i,CF = .self
            )  
          } else {
            if(similarity=="cosine"){
              s = sapply(X = colnames(SI),
                         FUN = "cosine",
                         type = "item",
                         i = item_i,CF = .self
              ) 
            }
          }
          
          SI[item_i,] <<- s
          SI[,item_i] <<- s
          
          
        }
     
        # Dados de Consumo ####
        if (datatype == "consumption") {
          
          ## MU ####
          novo_usuario <- matrix(0, 1, N, dimnames = list(Id_u, colnames(MU)))
          novo_usuario[,Ids_i] = 1 
          
          MU <<- rbind(MU, novo_usuario)
          
          
          n_aval_i <<- n_aval_i + novo_usuario[1,] 
          
          n_aval_u <<- c(n_aval_u,sum(novo_usuario))
          names(n_aval_u)[k] <<- Id_u
          
          
        #alteracao IntU
        novo_usu = (MU[k,]==1)
        if(sum(novo_usu)!=1){
          int_u_k = apply(
            X = MU[-k,novo_usu],
            MARGIN = 1,
            FUN = "sum")  
        } else {
          int_u_k = MU[-k,novo_usu]
        }
        
        
        IntU <<- cbind(IntU,int_u_k)
        IntU <<- rbind(IntU,c(int_u_k,n_aval_u[k]))
        colnames(IntU) <<- rownames(MU)
        rownames(IntU) <<- rownames(MU)
        
        
        
        #alteracao SU
        
        s = jaccard(type="user", i = k, j = (1:k),CF = .self)
        
        
        SU <<- cbind(SU,s[-k])
        SU <<- rbind(SU,s)
        colnames(SU)[k] <<- Id_u
        rownames(SU)[k] <<- Id_u
        
        #alteracao IntI 
        itens = colnames(IntI)[which(MU[k,]==1)]
        n_itens = length(itens)
        for(i in 1:n_itens){
          
          item_i = itens[i]
          
          for(j in i:n_itens){
            
            item_j = itens[j]
            IntI[item_i,item_j] <<- 
              IntI[item_i,item_j] + 1
            IntI[item_j,item_i] <<- IntI[item_i,item_j]
          }
        

          s = sapply(X = colnames(SI), 
                     FUN = "jaccard", 
                     type ="item", 
                     i    = item_i,CF = .self
                     )
          
          SI[item_i,] <<- s
          SI[,item_i] <<- s
        
          
        }
        
        
        item_i = itens[n_itens]
        
        
        s = sapply(X = colnames(SI), 
                   FUN = "jaccard", 
                   type = "item", 
                   i    = item_i,CF = .self
        )
        
        SI[item_i,] <<- s
        SI[,item_i] <<- s
       
      }
    }
  },
  
    addnewitem = function(Id_i, Ids_u, r = NULL) {
      "A method that adds a new item that has been consumed by already existing 
      users in the recommendation system.
      Id_i: a character, the new item ID;
      Ids_u: a character vector, the IDs of the users who consumed the new item;
      r: a numeric vector, the ratings given by the users for the new item 
      (only for ratings datatype).
      To add more than one new item, lists can be used. 
      Id_i: a list of characters; 
      Ids_u: a list of characters vectors; 
      r: list of numeric vectors."
      
      
      if (is.list(Ids_u) && is.list(Id_i)) {
        i = length(Id_i)
        if(datatype == "ratings"){
          for (k in 1:i) {
            addnewitem(Id_i = Id_i[[k]],
                       Ids_u = Ids_u[[k]],
                       r = r[[k]])
          }  
        } else {
          for (k in 1:i) {
            addnewitem(Id_i = Id_i[[k]],
                       Ids_u = Ids_u[[k]])
          }
        }
      } else {
        
        if (!is.character(Id_i)) {
          stop("*** Id_i must be a character. ***")
        }
        if (sum(!is.character(Ids_u)) != 0) {
          stop("*** Ids_u must be a character vector. ***")
        }
        
        
        
        # Verificar se existe um usuario com o mesmo identificador:
        N = dim(MU)[2]
        M = dim(MU)[1]
        
        k = N + 1 # coluna do novo item
        
        #verificar se o item ja esta cadastrado
        for (i in 1:N) {
          if (colnames(MU)[i] == Id_i) {
            stop(paste0("*** ",Id_i," is an already existing item. If you want to add a new rating, use $newrating(). ***"))
          }
        }
        
        encontrou_j = Id_i %in% colnames(MU)
        if(encontrou_j){
          stop(paste0("*** item ",Id_i," is already registered in the system ***"))
        }
        
        encontrou_i = Ids_u %in% rownames(MU)
        if(prod(encontrou_i)==0){
          stop("*** There are users that are not yet registered in the system. ***")
        }
        
        
      
      # Dados de Avaliacoes ####
      if (datatype == "ratings") {
        
        
          if(is.null(r)){
            stop("*** r must be not NULL ***")
          }
          
          if(length(r) != length(Ids_u)){
            stop("*** r and Ids_i must have same size ***")
          }
          
          
          ## Alteracao MU ####
          novo_item <- matrix(NA,
                              nrow =  M,
                              ncol = 1, 
                              dimnames = 
                                list(rownames(MU),Id_i))
          novo_item[Ids_u,] = r
          users_aval = !is.na(novo_item[,1])
          
        
          MU <<- cbind(MU, novo_item)
          
          
          n_aval_u <<- n_aval_u + users_aval
          n_aval_i <<- c(n_aval_i,sum(users_aval))
          names(n_aval_i)[k] <<- Id_i
          
          
          averages_i[k] <<- mean(r)
          names(averages_i)[k] <<- Id_i
          
          
          averages_u[users_aval] <<-
            (averages_u[users_aval]*(n_aval_u[users_aval] - 1) + 
               novo_item[users_aval])/(n_aval_u[users_aval])
            
          
          #alteracao IntI
          if(sum(users_aval)!=1){
            int_i_k = apply(X = MU[users_aval,-k], 
                            MARGIN = 2, 
                            FUN = function(x){
                              sum(!is.na(x))
                            })
            
          } else {
            int_i_k = !is.na(MU[users_aval,-k])
          }
          
          IntI <<- cbind(IntI,int_i_k)
          IntI <<- rbind(IntI,c(int_i_k,n_aval_i[k]))
          
          colnames(IntI) <<- colnames(MU)
          rownames(IntI) <<- colnames(MU)
          
          #alteracao SI
          
          if(similarity=="pearson"){
            s = pearson(type="item",i = k,j = (1:k),CF = .self)
          } else {
            s = cosine(type="item",i = k,j = (1:k),CF = .self)
          }
          
          SI <<- cbind(SI,s[-k])
          SI <<- rbind(SI,s)
          colnames(SI)[k] <<- Id_i
          rownames(SI)[k] <<- Id_i
          
          
          #alteracao IntU
          users = colnames(IntU)[users_aval]
          n_users = length(users)
          
          for(i in 1:n_users){
            
            user_i = users[i]
            
            for(j in i:n_users){
              user_j = users[j]
              IntU[user_i,user_j] <<- 
                IntU[user_i,user_j] + 1
              IntU[user_j,user_i] <<- IntU[user_i,user_j]
            }
            
            if(similarity=="pearson"){
              
              s = sapply(X = colnames(SU),
                         FUN = "pearson",
                         type = "user",
                         i = user_i,CF = .self
              )  
            } else {
              if(similarity=="cosine"){
                s = sapply(X = colnames(SU),
                           FUN = "cosine",
                           type = "user",
                           i = user_i,CF = .self
                ) 
              }
            }
            
            SU[user_i,] <<- s
            SU[,user_i] <<- s
            
           
          }
          
          user_i = users[n_users]
          
          
          if(similarity=="pearson"){
            
            s = sapply(X = colnames(SU),
                       FUN = "pearson",
                       type = "user",
                       i = user_i,CF = .self
            )  
          } else {
            if(similarity=="cosine"){
              s = sapply(X = colnames(SU),
                         FUN = "cosine",
                         type = "user",
                         i = user_i,CF = .self
              ) 
            }
          }
          
          SU[user_i,] <<- s
          SU[,user_i] <<- s
      
      }
      
      # Dados de Consumo ####
      if (datatype == "consumption") {
        
        
        
        
        ## Alteracao MU ####
        novo_item <- matrix(0, M, 1, 
                            dimnames = list(rownames(MU),Id_i))
        novo_item[Ids_u,] = 1 
        
        MU <<- cbind(MU, novo_item)
        
        
        n_aval_u <<- n_aval_u + novo_item[,1] 
        
        n_aval_i <<- c(n_aval_i,sum(novo_item))
        names(n_aval_i)[k] <<- Id_i
        
        
        
        
        #alteracao IntI
        novo_ite = (MU[,k]==1)
        if(sum(novo_ite)!=1){
          int_i_k = apply(X = MU[novo_ite,-k], 
                          MARGIN = 2, 
                          FUN = "sum")  
        } else {
          int_i_k = novo_ite
        }
        
        IntI <<- cbind(IntI,int_i_k)
        IntI <<- rbind(IntI,c(int_i_k,n_aval_i[k]))
        colnames(IntI) <<- colnames(MU)
        rownames(IntI) <<- colnames(MU)
  
        
        #alteracao SI
        
        s = jaccard(type="item",
                    i = k, 
                    j = (1:k),
                    CF = .self)
        
        
        SI <<- cbind(SI,s[-k])
        SI <<- rbind(SI,s)
        SI[k,k] <<- 1
        colnames(SI)[k] <<- Id_i
        rownames(SI)[k] <<- Id_i
        
        
        #alteracao IntU 
        usuas = colnames(IntU)[which(MU[,k]==1)]
        n_usuas = length(usuas)
        for(i in 1:n_usuas){
          
          user_i = usuas[i]
          
          for(j in i:n_usuas){
            user_j = usuas[j]
            IntU[user_i,user_j] <<- 
              IntU[user_i,user_j] + 1
            IntU[user_j,user_i] <<- IntU[user_i,user_j]
          }
          
      
          s = sapply(X = colnames(SU), 
                     FUN = "jaccard", 
                     type ="user", 
                     i    = user_i,CF = .self
          )
          
          SU[user_i,] <<- s
          SU[,user_i] <<- s
      
        
        }
        
        
        
        user_i = usuas[n_usuas]
        
        s = sapply(X = colnames(SU), 
                   FUN = "jaccard", 
                   type ="user", 
                   i    = user_i,
                   CF = .self
        )
        
        SU[user_i,] <<- s
        SU[,user_i] <<- s

      }
    }  
  },
  
    addnewemptyuser = function(Id_u) {
      "A method that adds a new user who has not yet consumed any existing items 
      in the recommendation system. 
      Id_u: a character, the new user ID;
      To add more than one new user, lists can be used. 
      Id_u: a list of characters;" 

      
      if (is.list(Id_u)) {
        u = length(Id_u)
        for (k in 1:u) {
          addnewemptyuser(Id_u = Id_u[[k]])
        }
      } else {
      
        
        if (!is.character(Id_u)) {
          stop("*** Id_u must be a character. ***")
        }
        
        
        
        # Verificar se existe um usuario com o mesmo identificador:
        
        N = dim(MU)[2]
        M = dim(MU)[1]
        
        k = M + 1 # linha do novo usuario
        
        for (i in 1:M) {
          if (rownames(MU)[i] == Id_u) {
            stop("*** This user ID already exist. If you want to add a new rating, use $newrating(). ***")
          }
        }
        
        encontrou_u = Id_u %in% rownames(MU)
        if(encontrou_u){
          stop("*** This user is already registered in the system. ***")
        }
        
        
        # Dados de Avaliacoes ####
        if (datatype == "ratings") {
          
          
          ## Alteracao MU ####
          novo_usuario <- matrix(NA, nrow = 1, ncol = N, 
                                 dimnames = list(Id_u, colnames(MU)))
        
          MU <<- rbind(MU, novo_usuario)
          
          averages_u <<- c(averages_u,NA)
          names(averages_u)[k] <<- Id_u
          
          n_aval_u <<- c(n_aval_u,0)
          names(n_aval_u)[k] <<- Id_u
          
          
          #alteracao IntU
          IntU <<- cbind(IntU,rep(0,nrow(IntU)))
          IntU <<- rbind(IntU,rep(0,ncol(IntU)))
          colnames(IntU) <<- rownames(MU)
          rownames(IntU) <<- rownames(MU)
          
          #alteracao SU
          SU <<- cbind(SU,rep(0,nrow(SU)))
          SU <<- rbind(SU,rep(0,ncol(SU)))
          SU[k,k] <<- 1
          colnames(SU)[k] <<- Id_u
          rownames(SU)[k] <<- Id_u
          
        }
          
        # Dados de Consumo ####
        if (datatype == "consumption") {
          
          ## Alteracao MU ####
          novo_usuario <- matrix(0, 1, N, dimnames = list(Id_u, colnames(MU)))
          
          
          MU <<- rbind(MU, novo_usuario)
          
          
          n_aval_u <<- c(n_aval_u,0)
          names(n_aval_u)[k] <<- Id_u
          
          
          
          #alteracao IntU
          IntU <<- cbind(IntU,rep(0,nrow(IntU)))
          IntU <<- rbind(IntU,rep(0,ncol(IntU)))
          colnames(IntU) <<- rownames(MU)
          rownames(IntU) <<- rownames(MU)
          
          #alteracao SU
          SU <<- cbind(SU,rep(0,nrow(SU)))
          SU <<- rbind(SU,rep(0,ncol(SU)))
          SU[k,k] <<- 1
          colnames(SU)[k] <<- Id_u
          rownames(SU)[k] <<- Id_u
          
        }
  
      }
    
    },
  
    addnewemptyitem = function(Id_i) {
      "A method that adds a new item that has not yet been consumed by any 
      existing user in the recommendation system.
      Id_i: a character, the new item ID;
      To add more than one new user, lists can be used. 
      Id_i: a list of characters;" 
      

      
      if (is.list(Id_i)) {
        i = length(Id_i)
        for (k in 1:i) {
          addnewuser(Id_i = Id_i[[k]])
        }
      } else {
        
        if (!is.character(Id_i)) {
          stop("*** Id_i must be a character. ***")
        }
        
        # Verificar se existe um usuario com o mesmo identificador:
        N = dim(MU)[2]
        M = dim(MU)[1]
        
        k = N + 1 # coluna do novo item
        
        #verificar se o item ja esta cadastrado
        for (i in 1:N) {
          if (colnames(MU)[i] == Id_i) {
            stop("*** This item ID already exist. If you want to add a new rating, use $newrating(). ***")
          }
        }
        
        encontrou_j = Id_i %in% colnames(MU)
        if(encontrou_j){
          stop("*** This item is already registered in the system. ***")
        }
        
        
        
        # Dados de Avaliacoes ####
        if (datatype == "ratings") {
          
          
          ## Alteracao MU ####
          novo_item <- matrix(NA, nrow = M, ncol = 1, 
                              dimnames = list(rownames(MU),Id_i))
          
          MU <<- cbind(MU, novo_item)
          
          
          averages_i <<- c(averages_i,NA)
          names(averages_i)[k] <<- Id_i
          
          n_aval_i <<- c(n_aval_i,0)
          names(n_aval_i)[k] <<- Id_i
          
          
          #alteracao IntI
          IntI <<- cbind(IntI,rep(0,nrow(IntI)))
          IntI <<- rbind(IntI,rep(0,ncol(IntI)))
          colnames(IntI) <<- colnames(MU)
          rownames(IntI) <<- colnames(MU)
          
          
          #alteracao SI
          SI <<- cbind(SI,rep(0,nrow(SI)))
          SI <<- rbind(SI,rep(0,ncol(SI)))
          SI[k,k] <<- 1
          colnames(SI)[k] <<- Id_i
          rownames(SI)[k] <<- Id_i
          
          
          
        }
        
        # Dados de Consumo ####
        if (datatype == "consumption") {
  
          ## Alteracao MU ####
          novo_item <- matrix(0, M, 1, 
                              dimnames = list(rownames(MU),Id_i))
          
          MU <<- cbind(MU, novo_item)
          
          
          n_aval_i <<- c(n_aval_i,0)
          names(n_aval_i)[k] <<- Id_i
        
          
          #alteracao IntI
          IntI <<- cbind(IntI,rep(0,nrow(IntI)))
          IntI <<- rbind(IntI,rep(0,ncol(IntI)))
          colnames(IntI) <<- colnames(MU)
          rownames(IntI) <<- colnames(MU)
          
          
          #alteracao SI
          SI <<- cbind(SI,rep(0,nrow(SI)))
          SI <<- rbind(SI,rep(0,ncol(SI)))
          SI[k,k] <<- 1
          colnames(SI)[k] <<- Id_i
          rownames(SI)[k] <<- Id_i
          
          
        }
    }
    },

    newrating = function(Id_u, Id_i, r = NULL) {
      "A method that adds a new rating or consumption of an existing user for
      an existing item that had not yet been rated by them.
      Id_u: a character, the user ID;
      Id_i: a character, the item ID;
      r: a numeric, the rating given by Id_u for Id_i (only for ratings 
      datatype).
      To add more than one new ratings, lists can be used. 
      Id_u: a list of characters; 
      Id_i: a list of characters; 
      r: list of numeric vectors."
      
      if (is.list(Id_u) && is.list(Id_i)) {
      
        u = length(Id_u)
        
        if(datatype == "ratings"){
          for (k in 1:u) {
            newrating(Id_u = Id_u[[k]], 
                      Id_i = Id_i[[k]],
                      r = r[[k]])
          }
        } else {
          for (k in 1:u) {
            newrating(Id_u = Id_u[[k]], 
                      Id_i = Id_i[[k]])
          }
        }
      } else {
        
        if (!is.character(Id_u)) {
          stop("*** Id_u must be a character. ***")
        }
        
        if (!is.character(Id_i)) {
          stop("*** Id_i must be a character. ***")
        }
        
        M = nrow(MU)
        N = ncol(MU)
        
        
        encontrou_i = Id_u %in% rownames(MU)
        if (!encontrou_i) {
          stop("*** This is not a valid user. ***")
        }
        i = which(Id_u == rownames(MU))
        
        
        encontrou_j = Id_i %in% colnames(MU)
        if (!encontrou_j) {
          stop("*** This is not a valid item ***")
        }
        j = which(Id_i == colnames(MU))
        
        
        
        # Dados de Avaliacoes ####
        if (datatype == "ratings") {
          
          if (!is.na(MU[i, j])) {
            stop(paste0("*** User ",Id_u," has already consumed item ",Id_i," ***"))
          }
          
          if (!is.numeric(r)) {
            stop("*** r must be numeric. ***")
          }
        
          
          ## Alteracao MU ####
          MU[i, j] <<- r
          
          
          # Alteracao n_aval_i ####
          n_aval_i[j] <<- n_aval_i[j] + 1
          # Alteracao n_aval_u ####
          n_aval_u[i] <<- n_aval_u[i] + 1
          
          #Alteracao averages_i
          averages_i[j] <<- ifelse(is.na(averages_i[j]),r,
            (averages_i[j]*(n_aval_i[j] - 1) + r)/(n_aval_i[j]))
          
          
          #Alteracao averages_i
          averages_u[i] <<- ifelse(is.na(averages_u[i]),r,
            (averages_u[i]*(n_aval_u[i] - 1) + r)/(n_aval_u[i]))
          
        
          # Alteracao IntU ####
          IntU[i,] <<- IntU[i,] + (!is.na(MU[,j]))
          IntU[,i] <<- IntU[,i] + (!is.na(MU[,j]))
          IntU[i,i] <<- IntU[i,i] - 1
          
          IntI[j,] <<- IntI[j,] + (!is.na(MU[i,]))
          IntI[,j] <<- IntI[,j] + (!is.na(MU[i,]))
          IntI[j,j] <<- IntI[j,j] - 1
          
          
          if(similarity=="pearson"){
            
            s = sapply(X = 1:M,
                       FUN = "pearson",
                       type = "user",
                       i = i,CF = .self
            )  
          } else {
            if(similarity=="cosine"){
              s = sapply(X = 1:M,
                         FUN = "cosine",
                         type = "user",
                         i = i,CF = .self
              ) 
            }
          }
          
          SU[i,] <<- s
          SU[,i] <<- s
          
          # for(k in 1:M){
          #   
          #   if(similarity=="pearson"){
          #     s = pearson(type="user",i = i,j = k)
          #   } else {
          #     s = cosine(type="user",i = i,j = k)
          #   }
          #   
          #   SU[i,k] <<- s
          #   SU[k,i] <<- s
          # }
          
          
          
          if(similarity=="pearson"){
            
            s = sapply(X = 1:N,
                       FUN = "pearson",
                       type = "item",
                       i = j,CF = .self
            )  
          } else {
            if(similarity=="cosine"){
              s = sapply(X = 1:N,
                         FUN = "cosine",
                         type = "item",
                         i = j,CF = .self
              ) 
            }
          }
          
          SI[j,] <<- s
          SI[,j] <<- s
          
     
      }
      
       # Dados de Consumo ####
        if (datatype == "consumption") {
          
          if (MU[i, j] != 0) {
            stop(paste0("*** User ",Id_u," has already consumed item ",Id_i," ***"))
          }
          
          
          ## Alteracao MU ####
          MU[i, j] <<- 1
          
          
          # Alteracao n_aval_i ####
          n_aval_i[j] <<- n_aval_i[j] + 1
          
          
          # Alteracao n_aval_u ####
          n_aval_u[i] <<- n_aval_u[i] + 1
          
          
          # Alteracao IntU ####
          IntU[i,] <<- IntU[i,] + MU[,j]
          IntU[,i] <<- IntU[,i] + MU[,j]
          IntU[i,i] <<- IntU[i,i] - 1
          
          IntI[j,] <<- IntI[j,] + MU[i,]
          IntI[,j] <<- IntI[,j] + MU[i,]
          IntI[j,j] <<- IntI[j,j] - 1
          
          
          s = sapply(X = 1:M, 
                     FUN = "jaccard", 
                     type ="user", 
                     i    = i,
                     CF = .self
          )
          
          SU[i,] <<- s
          SU[,i] <<- s
          
       
          s = sapply(X = 1:N, 
                     FUN = "jaccard", 
                     type ="item", 
                     i    = j,
                     CF = .self
          )
          
          SI[j,] <<- s
          SI[,j] <<- s
                    
       
        }
      }
  },
  
    deleteuser = function(Id_u) {
      "A method that deletes an user from the recommendation system. 
      Id_u: a character, the user ID;
      To delete more than one user, lists can be used. 
      Id_u: a list of characters;" 
      
      
      
      if (is.list(Id_u)) {
        u = length(Id_u)
        for (k in 1:u) {
          deleteuser(Id_u = Id_u[[k]])
        }
      } else {
        
        if (!is.character(Id_u)) {
          stop("*** Id_u must be a character. ***")
        }
        
        N = dim(MU)[2]
        M = dim(MU)[1]
        
        if(Id_u %in% rownames(MU)){
          k = which(Id_u == rownames(MU))
        } else {
          stop("*** No user with this Id ***")  
        }
        
        # Dados de Avaliacoes ####
        if (datatype == "ratings") {
        
          consumo = (!is.na(MU[k,]))
          ratings = MU[k,]
          
          n_aval_i <<- n_aval_i - consumo 
          n_aval_u <<- n_aval_u[-k]
          
          
          averages_i[consumo] <<-
            ifelse(n_aval_i[consumo]==0,NA,
            (averages_i[consumo]*(n_aval_i[consumo] + 1) - 
               ratings[consumo])/(n_aval_i[consumo]))
          
          averages_u <<- averages_u[-k]
          
          
          #alteracao IntU
          IntU <<- IntU[-k,-k]
          
          #alteracao SU
          SU <<- SU[-k,-k]
          
          ## Alteracao MU ####
          MU <<- MU[-k,]
          
          
          itens = colnames(IntI)[consumo]
          n_itens = length(itens)
          
          for(i in 1:n_itens){
            
            item_i = itens[i]
            
            for(j in i:n_itens){
              item_j = itens[j]
              IntI[item_i,item_j] <<- IntI[item_i,item_j] - 1
              IntI[item_j,item_i] <<- IntI[item_i,item_j]
            }
            
            
            
            if(similarity=="pearson"){
              
              s = sapply(X = colnames(SI),
                         FUN = "pearson",
                         type = "item",
                         i = item_i,CF = .self
              )  
            } else {
              if(similarity=="cosine"){
                s = sapply(X = colnames(SI),
                           FUN = "cosine",
                           type = "item",
                           i = item_i,CF = .self
                ) 
              }
            }
            
            SI[item_i,] <<- s
            SI[,item_i] <<- s
            
      
          }
          
          
          item_i = itens[n_itens]
          
          
          
          if(similarity=="pearson"){
            
            s = sapply(X = colnames(SI),
                       FUN = "pearson",
                       type = "item",
                       i = item_i,CF = .self
            )  
          } else {
            if(similarity=="cosine"){
              s = sapply(X = colnames(SI),
                         FUN = "cosine",
                         type = "item",
                         i = item_i,CF = .self
              ) 
            }
          }
          
          SI[item_i,] <<- s
          SI[,item_i] <<- s
      
          
        }
        
        
        # Dados de Consumo ####
        if (datatype == "consumption") {
          
          consumo = MU[k,]
          
          n_aval_i <<- n_aval_i - consumo 
          n_aval_u <<- n_aval_u[-k]
          
          itens = colnames(IntI)[which(consumo==1)]
          n_itens = length(itens)
          
          
          
          #alteracao IntU
          IntU <<- IntU[-k,-k]
          
          #alteracao SU
          SU <<- SU[-k,-k]
          
          ## Alteracao MU ####
          MU <<- MU[-k,]
          
          
          for(i in 1:n_itens){
            
            item_i = itens[i]
            
            for(j in i:n_itens){
              item_j = itens[j]
              IntI[item_i,item_j] <<- IntI[item_i,item_j] - 1
              IntI[item_j,item_i] <<- IntI[item_i,item_j]
            }
            
            
            s = sapply(X = colnames(SI), 
                       FUN = "jaccard", 
                       type ="item", 
                       i    = item_i,CF = .self
            )
            
            SI[item_i,] <<- s
            SI[,item_i] <<- s
          
          }
          
          
          item_i = itens[n_itens]
          
          
          
          s = sapply(X = colnames(SI), 
                     FUN = "jaccard", 
                     type ="item", 
                     i    = item_i,CF = .self
          )
          
          SI[item_i,] <<- s
          SI[,item_i] <<- s
          
          
        }
      }
    },

    deleteitem = function(Id_i) {
      "A method that deletes an item from the recommendation system. 
      Id_i: a character, the item ID;
      To delete more than one item, lists can be used. 
      Id_i: a list of characters;" 
      
      
      if (is.list(Id_i)) {
        i = length(Id_i)
        for (k in 1:i) {
          deleteuser(Id_i = Id_i[[k]])
        }
      } else {
         
        
        if (!is.character(Id_i)) {
          stop("*** Id_i must be a character. ***")
        }
        
        N = dim(MU)[2]
        M = dim(MU)[1]
        
        if(Id_i %in% colnames(MU)){
          k = which(Id_i == colnames(MU))
        } else {
          stop("*** No user with this Id ***")  
        }
        
        # Dados de Avaliacoes ####
        if (datatype == "ratings") {
          
          consumo = (!is.na(MU[,k]))
          ratings = MU[,k]
          
          n_aval_u <<- n_aval_u - consumo 
          n_aval_i <<- n_aval_i[-k]
          
          
          averages_u[consumo] <<-
            ifelse(n_aval_u[consumo]==0,NA,
            (averages_u[consumo]*(n_aval_u[consumo] + 1) - 
               ratings[consumo])/(n_aval_u[consumo]))
          
          averages_i <<- averages_i[-k]
          
          
          #alteracao IntI
          IntI <<- IntI[-k,-k]
          
          #alteracao SI
          SI <<- SI[-k,-k]
          
          ## Alteracao MU ####
          MU <<- MU[,-k]
          
          
          users = colnames(IntU)[consumo]
          n_users = length(users)
          
          for(i in 1:n_users){
            
            user_i = users[i]
            
            for(j in i:n_users){
              user_j = users[j]
              IntU[user_i,user_j] <<- IntU[user_i,user_j] - 1
              IntU[user_j,user_i] <<- IntU[user_i,user_j]
            }
            
            
            
            if(similarity=="pearson"){
              
              s = sapply(X = colnames(SU),
                         FUN = "pearson",
                         type = "user",
                         i = user_i,CF = .self
              )  
            } else {
              if(similarity=="cosine"){
                s = sapply(X = colnames(SU),
                           FUN = "cosine",
                           type = "user",
                           i = user_i,CF = .self
                ) 
              }
            }
            
            SU[user_i,] <<- s
            SU[,user_i] <<- s
            
          }
          
          
          user_i = users[n_users]
          
          
          if(similarity=="pearson"){
            
            s = sapply(X = colnames(SU),
                       FUN = "pearson",
                       type = "user",
                       i = user_i,CF = .self
            )  
          } else {
            if(similarity=="cosine"){
              s = sapply(X = colnames(SU),
                         FUN = "cosine",
                         type = "user",
                         i = user_i,CF = .self
              ) 
            }
          }
          
          SU[user_i,] <<- s
          SU[,user_i] <<- s
         
          
        }
        
        
      
      # Dados de Consumo ####
      if (datatype == "consumption") {
        
        
        consumo = MU[,k]
        
        n_aval_i <<- n_aval_i[-k] 
        n_aval_u <<- n_aval_u - consumo
        
        
        users = colnames(IntU)[which(consumo==1)]
        n_users = length(users)
        
        
        
        #alteracao IntU
        IntI <<- IntI[-k,-k]
        
        #alteracao SU
        SI <<- SI[-k,-k]
        
        ## Alteracao MU ####
        MU <<- MU[,-k]
        
        
        for(i in 1:n_users){
          
          user_i = users[i]
          
          for(j in i:n_users){
            user_j = users[j]
            IntU[user_i,user_j] <<- IntU[user_i,user_j] - 1
            IntU[user_j,user_i] <<- IntU[user_i,user_j]
          }
          
          for(j in 1:M){
            
            user_j = colnames(SU)[j]
            
            s = jaccard(type="user", 
                              i = user_i, 
                              j = user_j,CF = .self)
            
            SU[user_i,user_j] <<- s
            SU[user_j,user_i] <<- s
            
            
          }
        }
        
        
        user_i = users[n_users]
        for(j in 1:M){
          
          user_j = colnames(SU)[j]
          
          s = jaccard(type="user", 
                            i = user_i, 
                            j = user_j,CF = .self)
          
          
          SU[user_i,user_j] <<- s
          SU[user_j,user_i] <<- s
        }
        
      }
    
      }
    },
  
    deleterating = function(Id_u, Id_i) {
      "A method that deletes a existing rating or consumption of a user for an 
      item.
      Id_u: a character, the user ID;
      Id_i: a character, the item ID;
      To deletes more than one ratings, lists can be used. 
      Id_u: a list of characters; 
      Id_i: a list of characters."
    
      if (is.list(Id_u) && is.list(Id_i)) {
        u = length(Id_u)
        
        for (k in 1:u) {
          deleterating(Id_u = Id_u[[k]], Id_i = Id_i[[k]])
        }
      } else {
          
        if (!is.character(Id_u)) {
          stop("*** Id_u must be a character. ***")
        }
        if (!is.character(Id_i)) {
          stop("*** Id_i must be a character. ***")
        }
        
        
        M = nrow(MU)
        N = ncol(MU)
        
        encontrou_i = Id_u %in% rownames(MU)
        if (encontrou_i){
          i = which(rownames(MU) == Id_u)
        } else {
          stop("*** This is not a valid user. ***")
        }
        
        encontrou_j = Id_i %in% colnames(MU)
        if (encontrou_j){
          j = which(colnames(MU) == Id_i)
        } else {
          stop("*** This is not a valid item. ***")
        }
        
        
        if (MU[i, j]==0) {
          stop("*** This rating doesnt exist. ***")
        }
        
        # Dados de Avaliacoes ###
        if (datatype == "ratings") {
          
          
          # Alteracao n_aval_i ####
          n_aval_i[j] <<- n_aval_i[j] - 1
          
          
          # Alteracao n_aval_u ####
          n_aval_u[i] <<- n_aval_u[i] - 1
          
          
          
          averages_u[i] <<-
            (averages_u[i]*(n_aval_u[i] + 1) - MU[i, j])/(n_aval_u[i])
          
          averages_i[j] <<-
            (averages_i[j]*(n_aval_i[j] + 1) - MU[i, j])/(n_aval_i[j])
          
          
          
          
          # Alteracao IntU ####
          IntU[i,] <<- IntU[i,] - (!is.na(MU[,j]))
          IntU[,i] <<- IntU[,i] - (!is.na(MU[,j]))
          IntU[i,i] <<- IntU[i,i] + 1
          
          IntI[j,] <<- IntI[j,] - (!is.na(MU[i,]))
          IntI[,j] <<- IntI[,j] - (!is.na(MU[i,]))
          IntI[j,j] <<- IntI[j,j] + 1
          
          
          ## Alteracao MU ####
          MU[i, j] <<- NA
          
          
          if(similarity=="pearson"){
            
            s = sapply(X = list(1:M),
                       FUN = "pearson",
                       type = "user",
                       i = i,CF = .self
            )  
          } else {
            if(similarity=="cosine"){
              s = sapply(X = list(1:M),
                         FUN = "cosine",
                         type = "user",
                         i = i,CF = .self
              ) 
            }
          }
          
          SU[i,] <<- s
          SU[,i] <<- s
          
          
          
          if(similarity=="pearson"){
            
            s = sapply(X = list(1:N),
                       FUN = "pearson",
                       type = "item",
                       i = j,CF = .self
            )  
          } else {
            if(similarity=="cosine"){
              s = sapply(X = list(1:N),
                         FUN = "cosine",
                         type = "item",
                         i = j,CF = .self
              ) 
            }
          }
          
          SI[,j] <<- s
          SI[j,] <<- s
          
        }
        
        
        
        
        # Dados de Consumo ####
        if (datatype == "consumption") {
          
          
          # Alteracao n_aval_i ####
          n_aval_i[j] <<- n_aval_i[j] - 1
          
          
          # Alteracao n_aval_u ####
          n_aval_u[i] <<- n_aval_u[i] - 1
          
          
          # Alteracao IntU ####
          IntU[i,] <<- IntU[i,] - MU[,j]
          IntU[,i] <<- IntU[,i] - MU[,j]
          IntU[i,i] <<- IntU[i,i] + 1
          
          IntI[j,] <<- IntI[j,] - MU[i,]
          IntI[,j] <<- IntI[,j] - MU[i,]
          IntI[j,j] <<- IntI[j,j] + 1
        
            
          ## Alteracao MU ####
          MU[i, j] <<- 0
          
          s = sapply(X = list(1:M),
                     FUN = "jaccard",
                       type = "user",
                       i = i,
                     CF = .self
                     )
          SU[i,]<<- s
          SU[,i]<<- s
          
          
          s = sapply(X = list(1:N),
                     FUN = "jaccard",
                     type = "item",
                     i = j,
                     CF= .self
          )
          SI[j,]<<- s
          SI[,j]<<- s
          
        }
      }
      
    },
    
    changerating = function(Id_u, Id_i, r = NULL) {
      "A method that changes a rating or consumption of a user for an item 
      that has already been rated by them.
      Id_u: a character, the user ID;
      Id_i: a character, the item ID;
      r: a numeric, the rating given by Id_u for Id_i (only for ratings 
      datatype).
      To change more than one ratings, lists can be used. 
      Id_u: a list of characters; 
      Id_i: a list of characters; 
      r: list of numeric vectors."
      
      if (datatype == "consumption") {
        if(MU[Id_u,Id_i]==0){
          newrating(Id_u,Id_i)
        } else {
          deleterating(Id_u,Id_i)
        }
      }
      
      if (datatype == "ratings") {
        if(is.na(MU[Id_u,Id_i])){
          stop("*** Use newrating for the first rating ***")
        } else {
          deleterating(Id_u,Id_i)
          newrating(Id_u,Id_i,r)
        }
      }
    }
  )

)