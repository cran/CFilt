#'A Reference Class to represent a object CF
#'@description A class of objects created structured with the following objects: the MU - Utility Matrix, the SU - A Matrix of Similarity
#'between Users, the SI - A Matrix of Similarity between Items, and the vectors averages_u, averages_i, n_aval_u and n_aval_i. The class
#'contains methods, general functions with the objectives of manipulating the data and making recommendations, from the structures
#'present in the class. The data manipulation methods comprise addnewuser, addnewemptyuser, addnewitem, addnewemptyitem and newrating,
#'while the recommendations methods recommend, kclosestitems, topkusers, topkitems are created through choices available in the
#'Collaborative Filtering methodology. All objects and methods are accessed through the "$" character. A CF class object is created through
#'the CFbuilder function.
#'
#'@field MU A utility matrix, matrix that contains all the users ratings. The rows comprise users and the columns, itens.
#'@field SU1 A superior triangular user similarity matrix that contains the similarities between users, calculated using Cosine similarity
#'@field SU2 A superior triangular user similarity matrix that contains the similarities between users, calculated  using Pearson Correlation.
#'@field SI1 A superior triangular item similarity matrix that contains the similarities between items, calculated  using Cosine similarity.
#'@field SI2 A superior triangular item similarity matrix that contains the similarities between items, calculated using Adjusted Cosine similarity.
#'@field averages_u A vector that contains the averages of users ratings.
#'@field averages_i A vector that contains the averages of item ratings.
#'@field n_aval_u A vector that contains the numbers of ratings performed by each user.
#'@field n_aval_i A vector that contains the numbers of ratings received for each item.
#'
#'@references
#'\itemize{
#'\item LINDEN, G.; SMITH, B.; YORK, J. Amazon. com recommendations: Item-toitem collaborative filtering. Internet Computing, IEEE, v. 7, n. 1, p. 76-80,2003
#'\item Aggarwal, C. C. (2016). Recommender systems (Vol. 1). Cham: Springer International Publishing.
#'\item Leskovec, J., Rajaraman, A., & Ullman, J. D. (2020). Mining of massive data sets. Cambridge university press.
#'}
#'
#'@seealso \code{\link[CFilt]{CFbuilder}}
#'
#'
#'@author Thiago Lima, Jessica Kubrusly.
#'@import methods
#'@export
#'@examples
#'ratings<-movies[1:1000,]
#'objectCF<-CFbuilder(Data = ratings)
#'objectCF$MU
#'objectCF$SU1
#'objectCF$SU2
#'objectCF$SI1
#'objectCF$SI2
#'objectCF$averages_u
#'objectCF$averages_i
#'objectCF$n_aval_u
#'objectCF$n_aval_i
#'objectCF$addnewuser(Id_u = "Thiago",Ids_i = "The Hunger Games: Catching Fire",r = 5)
#'objectCF$addnewemptyuser(Id_u = "Jessica")
#'objectCF$addnewitem(Id_i = "Avengers: Endgame",Ids_u = c("1","2"),r = c(5,3))
#'objectCF$addnewemptyitem(Id_i = "Star Wars")
#'objectCF$newrating(Id_u = "1", Id_i = "Till Luck Do Us Part 2",r = 2)
#'objectCF$recommend(Id_u = "2", Id_i = "Iron Man 3", type = "user")
#'objectCF$recommend(Id_u = "2", Id_i = "Thor: The Dark World", type = "item")
#'objectCF$kclosestitems(Id_i = "Iron Man 3", k = 3)
#'objectCF$topkitems(Id_u = "3",k = 3, type = "user")
#'objectCF$topkitems(Id_u = "3",k = 3, type = "item")
#'objectCF$topkusers(Id_i = "Thor: The Dark World", k = 3,type = "user")
#'objectCF$topkusers(Id_i = "Thor: The Dark World", k = 3,type = "item")
#'objectCF$estimaterating(Id_u = "2",Id_i = "Iron Man 3", type = "user")
#'objectCF$deleterating("1","Brazilian Western")
#'objectCF$changerating("1","Wreck-It Ralph",2)
CF = setRefClass("CF",fields = list(
  MU="matrix",
  SU1="matrix",
  SU2="matrix",
  SI1="matrix",
  SI2="matrix",
  averages_u = "numeric",
  averages_i = "numeric",
  n_aval_u = "numeric",
  n_aval_i = "numeric"
),
methods =list(
  addnewuser=function(Id_u,Ids_i,r){
    "Adds a new user who rated one or more items. The object CF matrices and vectors will be updated. Id_u : a character, a user ID; Ids_i : a character vector, item IDs; r : a vector with its respective ratings."

    if(!is.character(Id_u)){stop("Id_u must be a character.")}
    if(sum(!is.character(Ids_i))!=0){stop("Ids_i must be a character vector.")}

    #Verificar se existe um usuÃ¡rio com o mesmo identificador:
    N = dim(MU)[2]
    M=dim(MU)[1]
    k=M + 1 #linha do novo usuario

    for(p in 1:length(r)){
      if(is.na(r[p])){stop("The vector 'r' can't have NA values.")}
    }



    for(i in 1:M){
      if(rownames(MU)[i]==Id_u){
        stop("This user ID already exist. If you want to add a new rating, use $newrating().")
      }
    }


    N = dim(MU)[2]

    M=dim(MU)[1]
    k=M + 1 #linha do novo usuario

    #Achar os indices dos itens
    indices_v=NULL
    encontrou_j = F
    for(j in 1:length(Ids_i)){
      for(i in 1:N){
        if(colnames(MU)[i]==Ids_i[j]){
          encontrou_j = T
          indices_v=c(indices_v,i)
          break
        }

      }
      if(!encontrou_j)
        stop("This is not a valid item.")
    }
    message("Progress:")
    pb <- txtProgressBar(min = 1, max = 5, style = 3)

    contador = 1
    setTxtProgressBar(pb, contador)

    #Alteracao MU
    novo_usuario<-matrix(NA,1,N,dimnames=list(Id_u,NULL))
    MU<<-rbind(MU,novo_usuario)
    t=1
    for(i in indices_v){
      MU[k,i]<<-r[t]
      t=t+1
    }

    #Alteracao averages_u
    averages_u<<-c(averages_u,mean(r,na.rm = T))
    names(averages_u)[k]<<-Id_u
    #Alteracao averages_i, n_aval_i e n_aval_u
    t=1
    for(j in indices_v){
      averages_i[j] <<- averages_i[j]*n_aval_i[j]/(n_aval_i[j]+1) + r[t]/(n_aval_i[j]+1)
      n_aval_i[j] <<- n_aval_i[j] + 1
      t=t+1
    }

    n_aval_u<<- c(n_aval_u,length(r))
    names(n_aval_u)[k]<<-Id_u
    #Verificar quais usuarios avaliaram os itens Ids_i e criar um vetor com a Uniao dos indices(sem repeticao)

    uniao_indices_i<-NULL
    for (i in indices_v){ #Cada item
      for(u in 1:M){ #percorre a coluna do item i
        if(!is.na(MU[u,i])){ #Verifica se o usuario avaliou ou nao
          if(is.null(uniao_indices_i)){uniao_indices_i=c(uniao_indices_i,u)} #Acrescenta o primeiro indice
          else{
            for(p in 1:length(uniao_indices_i)){ #Verifica se o indice ja existe no vetor uniao_indices_i
              if(uniao_indices_i[p]==u){
                ja_incluido=T
                break}
              ja_incluido=F
            }
            if(ja_incluido==F){uniao_indices_i=c(uniao_indices_i,u)} #Se o indice nao existe no vetor, acrescente o indice

          }
        }


      }
    }




    #Alteracao SU1
    SU1<<-rbind(SU1,0)
    rownames(SU1)[k]<<-Id_u
    SU1<<-cbind(SU1,0)
    colnames(SU1)[k]<<-Id_u


    u=1
    for(i in uniao_indices_i){
      contador = 1 + (u/length(uniao_indices_i))*1
      setTxtProgressBar(pb,contador)
      u=u+1
      if(i<k){
        SU1[i,k]<<-sum(MU[i,]*MU[k,],na.rm = T)/(sqrt(sum(MU[i,]^2,na.rm = T))*sqrt(sum(MU[k,]^2,na.rm = T)))
      }
    }



    #Alteracao SU2
    SU2<<-rbind(SU2,0)
    rownames(SU2)[k]<<-Id_u
    SU2<<-cbind(SU2,0)
    colnames(SU2)[k]<<-Id_u


    u=2
    for(i in uniao_indices_i){
      contador = 2 + (u/length(uniao_indices_i))*1
      setTxtProgressBar(pb,contador)
      u=u+1
      if(i<k){
        SU2[i,k]<<-sum((MU[i,]-averages_u[i])*(MU[k,]-averages_u[k]),na.rm = T)/
          (sqrt(sum((MU[i,]-averages_u[i])^2,na.rm = T))*sqrt(sum((MU[k,]-averages_u[k])^2,na.rm = T)))
      }
    }





    #Alteracao SI2

    u=3
    for(i in indices_v){

      for(j in indices_v){
        if(j>i){
          SI2[i,j]<<-sum((MU[,i]-averages_u[i])*(MU[,j]-averages_u[j]),na.rm = T)/(sqrt(sum((MU[,i]-averages_u[i])^2,na.rm = T))*sqrt(sum((MU[,j]-averages_u[j])^2,na.rm = T)))
        }
      }
      contador = 3 + (u/length(indices_v))*1
      setTxtProgressBar(pb,contador)
      u=u+1

    }




    #Alteracao SI1

    u=4
    for(i in indices_v){

      for(j in indices_v){
        if(j>i){
          SI1[i,j]<<-sum(MU[,i]*MU[,j],na.rm = T)/(sqrt(sum(MU[,i]^2,na.rm = T))*sqrt(sum(MU[,j]^2,na.rm = T)))
        }
      }
      contador = 4 + (u/length(indices_v))*1
      setTxtProgressBar(pb,contador)
      u=u+1

    }
    setTxtProgressBar(pb,5)
    close(pb)






  },
  newrating=function(Id_u,Id_i,r){
    "Adds a new rating from user Id_u to item Id_i.The object CF matrices and vectors will be updated. Id_u : a character, a user ID; Id_i : a character, an item ID; r : the rating. "
    if(!is.character(Id_u)){stop("Id_u must be a character.")}
    if(!is.character(Id_i)){stop("Id_i must be a character.")}

    M = nrow(MU)
    N = ncol(MU)
    encontrou_i = F
    for(i in 1:M){
      if(rownames(MU)[i]==Id_u){
        encontrou_i = T
        break
      }
    }
    if(!encontrou_i)
      stop("This is not a valid user.")
    #i guarda o num da linha de MU correspondente ao usuario Id_u


    encontrou_j = F
    for(j in 1:N){
      if(colnames(MU)[j]==Id_i){
        encontrou_j = T
        break
      }
    }
    if(!encontrou_j)
      stop("This is not a valid item.")


    if(!is.na(MU[i,j])){stop("This rating already exist.If you want change a rating, use $changerating()")}
    #Alteracao MU
    MU[i,j]<<-r
    #Alteracao averages_i e n_aval_i
    averages_i[j] <<- averages_i[j]*n_aval_i[j]/(n_aval_i[j]+1) + r/(n_aval_i[j]+1)
    n_aval_i[j] <<- n_aval_i[j] + 1
    #Alteracao averages_u e n_aval_u
    averages_u[i] <<- averages_u[i]*n_aval_u[i]/(n_aval_u[i]+1) + r/(n_aval_u[i]+1)
    n_aval_u[i] <<- n_aval_u[i] + 1








    ind_aval_i<-NULL #indices dos usuarios que avaliaram o item j
    for(k in 1:M){
      if(!is.na(MU[k,j]))
        ind_aval_i=c(ind_aval_i,k)
    }




    #Alteracao SU1

    for(k in ind_aval_i){
      #calcular a similaridade de i com k
      nova_sim_i_k = sum(MU[k,]*MU[i,],na.rm = T)/(sqrt(sum(MU[k,]^2,na.rm = T))*sqrt(sum(MU[i,]^2,na.rm = T)))
      if(i!=k){
        if(i < k){
          SU1[i,k] <<- nova_sim_i_k
        }else{
          SU1[k,i] <<- nova_sim_i_k
        }
      }
    }


    #Alteracao SU2

    for(k in ind_aval_i){
      #calcular a similaridade de i com k
      nova_sim_i_k = sum((MU[k,]-averages_u[k])*(MU[i,]-averages_u[i]),na.rm = T)/(sqrt(sum((MU[k,]-averages_u[k])^2,na.rm = T))*sqrt(sum((MU[i,]-averages_u[i])^2,na.rm = T)))
      if(i!=k){
        if(i < k){
          SU2[i,k] <<- nova_sim_i_k
        }else{
          SU2[k,i] <<- nova_sim_i_k
        }
      }
    }






    ind_aval_u<-NULL #indice dos itens que foram avaliados por i
    for(k in 1:N){
      if(!is.na(MU[i,k]))
        ind_aval_u=c(ind_aval_u,k)

    }

    #Alteracao SI1


    for(k in ind_aval_u){
      nova_sim_j_k = sum(MU[,k]*MU[,j],na.rm = T)/(sqrt(sum(MU[,k]^2,na.rm = T))*sqrt(sum(MU[,j]^2,na.rm = T)))
      if(j!=k){
        if(j < k){
          SI1[j,k] <<- nova_sim_j_k
        }else{
          SI1[k,j] <<- nova_sim_j_k
        }
      }
    }


    #Alteração SI2

    for(k in ind_aval_u){
      nova_sim_j_k = sum((MU[,k]-averages_u[k])*(MU[,j]-averages_u[j]),na.rm = T)/(sqrt(sum((MU[,k]-averages_u[k])^2,na.rm = T))*sqrt(sum((MU[,j]-averages_u[j])^2,na.rm = T)))
      if(j!=k){
        if(j < k){
          SI2[j,k] <<- nova_sim_j_k
        }else{
          SI2[k,j] <<- nova_sim_j_k
        }
      }
    }



  },
  addnewemptyuser=function(Id_u){
    "Adds a new user without ratings. The object CF matrices and vectors will be updated. Id_u : a character, a user ID."
    if(!is.character(Id_u)){stop("Id_u must be a character.")}


    N=ncol(MU)
    M=nrow(MU)
    k=M+1

    for(i in 1:M){
      if(rownames(MU)[i]==Id_u){
        stop("This user ID already exist.")
      }
    }


    #Alteracao MU
    novo_usuario<-matrix(NA,1,N,dimnames=list(Id_u,NULL))
    MU<<-rbind(MU,novo_usuario)
    #Alteracao averages_u e n_aval_u
    averages_u<<-c(averages_u,0)
    names(averages_u)[k]<<-Id_u
    n_aval_u<<-c(n_aval_u,0)
    names(n_aval_u)[k]<<-Id_u
    #Alteracao SU1
    SU1<<-rbind(SU1,0)
    rownames(SU1)[k]<<-Id_u
    SU1<<-cbind(SU1,0)
    colnames(SU1)[k]<<-Id_u

    #Alteracao SU2
    SU2<<-rbind(SU2,0)
    rownames(SU2)[k]<<-Id_u
    SU2<<-cbind(SU2,0)
    colnames(SU2)[k]<<-Id_u
    #Alteracao SI(Nao se altera)

  },
  addnewemptyitem=function(Id_i){
    "Adds a new item without ratings. The object CF matrices and vectors will be updated. Id_i : a character, an item ID."
    if(!is.character(Id_i)){stop("Id_i must be a character.")}

    M=nrow(MU)
    N=ncol(MU)
    k=N+1
    for(i in 1:N){
      if(colnames(MU)[i]==Id_i){
        stop("This Item ID already exist.")
      }
    }
    #Alteracao MU



    novo_item<-matrix(NA,M,1,dimnames=list(NULL,Id_i))
    MU<<-cbind(MU,novo_item)
    #Alteracao averages_i e n_aval_i
    averages_i<<-c(averages_i,0)
    names(averages_i)[k]<<-Id_i
    n_aval_i<<-c(n_aval_i,0)
    names(n_aval_i)[k]<<-Id_i
    #Alteracao SI1
    SI1<<-rbind(SI1,0)
    rownames(SI1)[k]<<-Id_i
    SI1<<-cbind(SI1,0)
    colnames(SI1)[k]<<-Id_i
    #Alteracao SI2
    SI2<<-rbind(SI2,0)
    rownames(SI2)[k]<<-Id_i
    SI2<<-cbind(SI2,0)
    colnames(SI2)[k]<<-Id_i
    #Alteracao SU(NaO SE ALTERA)

  },
  addnewitem=function(Id_i,Ids_u,r){

    "Adds a new item that has been rated by one or more users. The object CF matrices and vectors will be updated. Id_i : a character, an item ID; Ids_u : a character vector, a user IDs; r : a vector with its respective ratings."
    if(sum(!is.character(Ids_u))!=0){stop("Id_u must be a character.")}
    if(!is.character(Id_i)){stop("Id_i must be a character.")}

    for(p in 1:length(r)){
      if(is.na(r[p])){stop("The vector 'r' can't have NA values.")}
    }


    M = dim(MU)[1]


    N=dim(MU)[2]
    k=N + 1 #linha do novo usuario

    for(i in 1:N){
      if(colnames(MU)[i]==Id_i){
        stop("This Item ID already exist. If you want to add a new rating, use $newrating().")
      }
    }



    #Achar os indices dos usuarios que avaliaram o item
    indices_v=NULL
    encontrou_i = F
    for(i in 1:length(Ids_u)){
      for(j in 1:M){
        if(rownames(MU)[j]==Ids_u[i]){
          encontrou_i = T
          indices_v=c(indices_v,j)
          break
        }

      }
      if(!encontrou_i)
        stop("This is not a valid user.")
    }
    ##Barra de Progresso
    message("Progress:")
    pb <- txtProgressBar(min = 1, max = 5, style = 3)

    contador = 1
    setTxtProgressBar(pb, contador)
    ####

    #Alteracao MU
    novo_item<-matrix(NA,M,1,dimnames=list(NULL,Id_i))
    MU<<-cbind(MU,novo_item)
    t=1
    for(i in indices_v){
      MU[i,k]<<-r[t]
      t=t+1
    }
    #Alteracao averages_i
    averages_i<<-c(averages_i,mean(r,na.rm = T))
    names(averages_i)[k]<<-Id_i

    #Alteracao averages_u, n_aval_i e n_aval_u
    t=1
    for(i in indices_v){
      averages_u[i] <<- averages_u[i]*n_aval_u[i]/(n_aval_u[i]+1) + r[t]/(n_aval_u[i]+1)
      n_aval_u[i] <<- n_aval_u[i] + 1
      t=t+1
    }

    n_aval_i<<- c(n_aval_i,length(r))
    names(n_aval_i)[k]<<-Id_i

    #Verificar quais itens avaliados pelos usuarios Ids_u e criar um vetor com a Uniao dos indices(sem repeticao)
    uniao_indices_j<-NULL
    for (i in indices_v){ #Cada usuario
      for(u in 1:N){ #percorre a linha do usuario i
        if(!is.na(MU[i,u])){ #Verifica se o item e avaliado ou nao
          if(is.null(uniao_indices_j)){uniao_indices_j=c(uniao_indices_j,u)} #Acrescenta o primeiro indice
          else{
            for(p in 1:length(uniao_indices_j)){ #Verifica se o indice ja existe no vetor uniao_indices_j
              if(uniao_indices_j[p]==u){
                ja_incluido=T
                break}
              ja_incluido=F
            }
            if(ja_incluido==F){uniao_indices_j=c(uniao_indices_j,u)} #Se o indice nao existe no vetor, acrescente o indice

          }
        }


      }
    }



    #Alteracao SI1
    SI1<<-rbind(SI1,0)
    rownames(SI1)[k]<<-Id_i
    SI1<<-cbind(SI1,0)
    colnames(SI1)[k]<<-Id_i
    u=1
    for(i in uniao_indices_j){
      contador = 1 + (u/length(uniao_indices_j))*1
      setTxtProgressBar(pb,contador)
      u=u+1
      if(i<k){
        SI1[i,k]<<-sum(MU[,i]*MU[,k],na.rm = T)/(sqrt(sum(MU[,i]^2,na.rm = T))*sqrt(sum(MU[,k]^2,na.rm = T)))
      }
    }


    #Alteracao SI2
    SI2<<-rbind(SI2,0)
    rownames(SI2)[k]<<-Id_i
    SI2<<-cbind(SI2,0)
    colnames(SI2)[k]<<-Id_i
    u=2
    for(i in uniao_indices_j){
      contador = 2 + (u/length(uniao_indices_j))*1
      setTxtProgressBar(pb,contador)
      u=u+1
      if(i<k){
        SI2[i,k]<<-sum((MU[,i]-averages_u[i])*(MU[,k]-averages_u[k]),na.rm = T)/
          (sqrt(sum((MU[,i]-averages_u[i])^2,na.rm = T))*sqrt(sum((MU[,k]-averages_u[k])^2,na.rm = T)))
      }
    }

    #Alteracao SU1
    u=3
    for(i in indices_v){
      contador = 3 + (u/length(indices_v))*1
      setTxtProgressBar(pb,contador)
      u=u+1
      for(j in indices_v){
        if(j>i){
          SU1[i,j]<<-sum(MU[i,]*MU[j,],na.rm = T)/(sqrt(sum(MU[i,]^2,na.rm = T))*sqrt(sum(MU[j,]^2,na.rm = T)))
        }
      }

    }






    #Alteracao SU2
    u=4
    for(i in indices_v){
      contador = 4 + (u/length(indices_v))*1
      setTxtProgressBar(pb,contador)
      u=u+1
      for(j in indices_v){
        if(j>i){
          SU2[i,j]<<-sum((MU[i,]-averages_u[i])*(MU[j,]-averages_u[j]),na.rm = T)/(sqrt(sum((MU[i,]-averages_u[i])^2,na.rm = T))*sqrt(sum((MU[j,]-averages_u[j])^2,na.rm = T)))
        }
      }

    }
    setTxtProgressBar(pb,5)


    close(pb)

  },
  recommend=function(Id_u,Id_i,type,neighbors=5,cuts=3.5,similarity=ifelse(is_binary==TRUE,"cos","adjcos"), is_binary = FALSE){
    "A function that returns True if user Id_u will like item Id_i or returns FALSE, otherwise. The recommendation can be made through similarity between users, when type = 'user', as well as through the similarity between items, when type = 'item'.
      Id_u : a  character, a User ID; Id_i : a character, an Item ID; type: a character string, 'user' or 'item'; neighbors: number of similarities used for to estimates (default = 5); cuts: cut score designated to determine if it is recommended (default=3.5);
      similarity: the methodology used to estimate the rating. Must be one of 'cos', for cosine similarity, or 'adjcos' (default), for adjusted cosine similarity. When type='user', 'adjcos' comprises the Pearson similarity. This choice can alter the way the
      estimate is calculated; is_binary: it's TRUE if your dataset contains binary ratings or FALSE (default), otherwhise. If it's TRUE, similarity will be 'cos', because 'adjcos' isn't recommended for binary ratings."
    if(!is.character(similarity)){stop("similarity must be a character.")}
    if(!is.character(Id_u)){stop("Id_u must be a character.")}
    if(!is.character(Id_i)){stop("Id_i must be a character.")}
    if(type!="user" && type!="item"){stop("type should be one of 'user' or 'item'.")}
    if(similarity !="cos" && similarity !="adjcos"){stop("similarity should be one of 'cos' or 'adjcos'.")}
    M = nrow(MU)
    encontrou_i = F
    for(i in 1:M){
      if(rownames(MU)[i]==Id_u){
        encontrou_i = T
        break
      }
    }
    if(!encontrou_i)
      stop("This is not a valid user.")
    #i guarda o num da linha de MU correspondente ao usuario Id_u

    N = ncol(MU)
    encontrou_j = F
    for(j in 1:N){
      if(colnames(MU)[j]==Id_i){
        encontrou_j = T
        break
      }
    }
    if(!encontrou_j)
      stop("This is not a valid item.")



    if(type=="user"){
      #busca os n indices dos usuarios mais proximos de i que avaliaram j
      #no final v_ind e v_valores sao arrays de tamanho n ou menos,
      #a primeira posicao guarda um array com indices dos usuarios acima e a
      #segunda posicao um array com os valores das similaridades entre entre esses
      #usuarios e o usuario k
      if(similarity=="cos"){

        M = ncol(SU1)
        if(i==1){copia=c(NA,SU1[i,(i+1):M])}else{if(i==M){copia=c(SU1[1:(i-1),i],NA)}else{copia = c(SU1[1:(i-1),i],NA,SU1[i,(i+1):M])}}


        #varrer o copia colocando NA nos usuarios que nao avaliaram j
        for(l in 1:M){
          if(is.na(MU[l,j])){
            copia[l] = NA
          }
        }
        if(sum(!is.na(copia))==0){return(F)}else{
          v_ind   = NULL
          v_valor = NULL

          for(l in 1:neighbors){
            if(sum(!is.na(copia))==0){break}
            ind         = which.max(copia)
            valor       = max(copia,na.rm = T)

            if(valor <= 0)
              break

            v_ind   = c(v_ind,ind)
            v_valor = c(v_valor,valor)
            copia[ind]  = NA
          }


          #agora v_ind e v_valor guardam os indices e as similaridades dos usuarios mais semelhantes a i que avaliaram j
          #cuidado, os arrays podem ser vazios.

          n_ = length(v_ind)
          if(n_ == 0){
            stop("It isn't possible to estimate the rating for this user and item.")
          }
          nota = sum(MU[v_ind,j]*v_valor)/sum(v_valor)
          if(nota>=cuts)
            return(T)
          return(F)
        }
      }
      if(similarity=="adjcos"){
        M = ncol(SU2)
        if(i==1){copia=c(NA,SU2[i,(i+1):M])}else{if(i==M){copia=c(SU2[1:(i-1),i],NA)}else{copia = c(SU2[1:(i-1),i],NA,SU2[i,(i+1):M])}}


        #varrer o copia colocando NA nos usuarios que nao avaliaram j
        for(l in 1:M){
          if(is.na(MU[l,j])){
            copia[l] = NA
          }
        }
        if(sum(!is.na(copia))==0){return(F)}else{
          v_ind   = NULL
          v_valor = NULL

          for(l in 1:neighbors){
            if(sum(!is.na(copia))==0){break}
            ind         = which.max(copia)
            valor       = max(copia,na.rm = T)

            if(valor <= 0)
              break

            v_ind   = c(v_ind,ind)
            v_valor = c(v_valor,valor)
            copia[ind]  = NA
          }


          #agora v_ind e v_valor guardam os indices e as similaridades dos usuarios mais semelhantes a i que avaliaram j
          #cuidado, os arrays podem ser vazios.

          n_ = length(v_ind)
          if(n_ == 0){
            stop("It isn't possible to estimate the rating for this user and item.")
          }
          nota = averages_u[i] + ((sum((MU[v_ind,j]-averages_u[v_ind])*v_valor))/sum(v_valor))
          nota = as.numeric(nota)
          if(nota>=cuts)
            return(T)
          return(F)
        }


      }


    }
    if(type=="item"){
      #busca os n indices dos itens mais proximos de j que foram avaliados por i
      #no final v_ind e v_valores sao arrays de tamanho n ou menos,
      #a primeira posicao guarda um array com indices dos itens acima e a
      #segunda posicao um array com os valores das similaridades entre entre esses
      #itens e o item k
      if(similarity=="cos"){

        N = ncol(SI1)
        if(j==1){copia=c(NA,SI1[j,(j+1):N])}else{if(j==N){copia=c(SI1[1:(j-1),j],NA)}else{copia = c(SI1[1:(j-1),j],NA,SI1[j,(j+1):N])}}


        #varrer o copia colocando NA nos usuarios que nao avaliaram j
        for(l in 1:N){
          if(is.na(MU[i,l])){
            copia[l] = NA
          }
        }
        if(sum(!is.na(copia))==0){return(F)}else{
          v_ind   = NULL
          v_valor = NULL

          for(l in 1:neighbors){
            if(sum(!is.na(copia))==0){break}
            ind         = which.max(copia)
            valor       = max(copia,na.rm = T)

            if(valor <= 0)
              break

            v_ind   = c(v_ind,ind)
            v_valor = c(v_valor,valor)
            copia[ind]  = NA
          }


          #agora v_ind e v_valor guardam os indices e as similaridades dos itens mais semelhantes a j que foram avaliados por i
          #cuidado, os arrays podem ser vazios.

          n_ = length(v_ind)
          if(n_ == 0){
            stop("It isn't possible to estimate the rating for this user and item.")
          }
          nota = sum(MU[i,v_ind]*v_valor)/sum(v_valor)
          if(nota>=cuts)
            return(T)
          return(F)

        }
      }
      if(similarity=="adjcos"){




        N = ncol(SI2)
        if(j==1){copia=c(NA,SI2[j,(j+1):N])}else{if(j==N){copia=c(SI2[1:(j-1),j],NA)}else{copia = c(SI2[1:(j-1),j],NA,SI2[j,(j+1):N])}}

        #varrer o copia colocando NA nos usuarios que nao avaliaram j
        for(l in 1:N){
          if(is.na(MU[i,l])){
            copia[l] = NA
          }
        }
        if(sum(!is.na(copia))==0){return(F)}else{
          v_ind   = NULL
          v_valor = NULL

          for(l in 1:neighbors){
            if(sum(!is.na(copia))==0){break}
            ind         = which.max(copia)
            valor       = max(copia,na.rm = T)

            if(valor <= 0)
              break

            v_ind   = c(v_ind,ind)
            v_valor = c(v_valor,valor)
            copia[ind]  = NA
          }


          #agora v_ind e v_valor guardam os indices e as similaridades dos itens mais semelhantes a j que foram avaliados por i
          #cuidado, os arrays podem ser vazios.

          n_ = length(v_ind)
          if(n_ == 0){
            stop("It isn't possible to estimate the rating for this user and item.")
          }
          nota = sum(MU[i,v_ind]*v_valor)/sum(v_valor)
          if(nota>=cuts)
            return(T)
          return(F)

        }
      }

    }
  },
  kclosestitems=function(Id_i,k=5,similarity=ifelse(is_binary==TRUE,"cos","adjcos"), is_binary = FALSE){
    "A function that returns the k items most similar to an item. Id_i : A Character, a Item ID; k : Number of items most similar to item Id_i (deafult = 5);
     similarity: The methodology used to estimate the rating. Must be one of 'cos', for cosine similarity, or 'adjcos' (default), for adjusted cosine similarity.
     When type='user', 'adjcos' comprises the Pearson similarity. This choice can alter the way the estimate is calculated; is_binary: it's TRUE if your dataset
     contains binary ratings or FALSE (default), otherwhise. If it's TRUE, similarity will be 'cos', because 'adjcos' isn't recommended for binary ratings."
    if(!is.character(Id_i)){stop("Id_i must be a character.")}
    if(!is.character(similarity)){stop("similarity must be a character.")}
    N = ncol(MU)
    encontrou_j = F
    for(j in 1:N){
      if(colnames(MU)[j]==Id_i){
        encontrou_j = T
        break
      }
    }
    if(!encontrou_j)
      stop("This is not a valid item.")
    #j guarda o num da coluna de MU correspondente ao item Id_i
    if(similarity !="cos" && similarity !="adjcos"){stop("similarity should be one of 'cos' or 'adjcos'.")}
    if(similarity=="cos"){

      #Criaremos o vetor copia com as respectivas similaridades dos itens com o item Id_i
      if(j==1){copia = c(NA,SI1[j,(j+1):N])}else{if(j==ncol(SI1)){copia=c(SI1[1:(j-1),j],NA)}else{
        copia = c(SI1[1:(j-1),j],NA,SI1[j,(j+1):N])}}


      v_nomes = NULL #vetor com os nomes dos itens

      for(l in 1:k){     # encontrar os n itens mais similares ao item Id_i, guardando os indices, os valores e os respectivos nomes.

        ind         = which.max(copia)
        valor       = max(copia,na.rm = T)
        if(valor <= 0)
          break

        v_nomes = c(v_nomes,colnames(MU)[ind])
        copia[ind]  = NA


      }
      return(v_nomes)
    }
    if(similarity=="adjcos"){
      #Criaremos o vetor copia com as respectivas similaridades dos itens com o item Id_i
      if(j==1){copia = c(NA,SI2[j,(j+1):N])}else{if(j==ncol(SI2)){copia=c(SI2[1:(j-1),j],NA)}else{
        copia = c(SI2[1:(j-1),j],NA,SI2[j,(j+1):N])}}


      v_nomes = NULL #vetor com os nomes dos itens

      for(l in 1:k){     # encontrar os n itens mais similares ao item Id_i, guardando os indices, os valores e os respectivos nomes.

        ind         = which.max(copia)
        valor       = max(copia,na.rm = T)
        if(valor <= 0)
          break

        v_nomes = c(v_nomes,colnames(MU)[ind])
        copia[ind]  = NA


      }
      return(v_nomes)
    }

  },
  topkitems=function(Id_u,k=5,type,neighbors=5,cuts=3.5,similarity =ifelse(is_binary==TRUE,"cos","adjcos"), is_binary = FALSE){
    "A function that recommends k items for an Id_u user. The recommendation can be made through similarity between users, when type = 'user', as well as through similarity between items,
      when type = 'item'. Id_u : A character, a User ID; k : Number of recommendations (default=5); type: A character string, 'user' or 'item'; neighbors: Number of similarities used for the estimates(default=5);
      cuts: Cut score designated to determine if it is recommended (default = 3.5); similarity: The methodology used to estimate the rating. Must be one of 'cos', for cosine similarity,
      or 'adjcos' (default), for adjusted cosine similarity. When type='user', 'adjcos' comprises the Pearson similarity. This choice can alter the way the estimate is calculated;
      is_binary: it's TRUE if your dataset contains binary ratings or FALSE (default), otherwhise. If it's TRUE, similarity will be 'cos', because 'adjcos' isn't recommended for binary ratings."
    if(!is.character(similarity)){stop("similarity must be a character.")}
    if(!is.character(Id_u)){stop("Id_u must be a character.")}
    if(type!="user" && type!="item"){stop("type should be one of 'user' or 'item'.")}
    if(similarity !="cos" && similarity !="adjcos"){stop("similarity should be one of 'cos' or 'adjcos'.")}
    M = nrow(MU)
    N = ncol(MU)
    encontrou_i = F
    for(i in 1:M){
      if(rownames(MU)[i]==Id_u){
        encontrou_i = T
        break
      }
    }
    if(!encontrou_i)
      stop("This is not a valid user.")
    #i guarda o num da linha de MU correspondente ao usuario Id_u
    message("Progress:")
    pb <- txtProgressBar(min = 1, max = 3, style = 3)

    if(type=="item"){

      if(similarity=="cos"){


        v_notas=NULL
        v_ind = NULL
        for(j in 1:N){     # Percorrer o vetor das avaliacoes do usuario Id_u, e estimar as avaliacoes dos itens nao avaliados por ele.
          contador = 1 + (j/N)*1
          setTxtProgressBar(pb,contador)


          if(is.na(MU[i,j])){
            v_ind_b   = NULL
            v_valor_b = NULL

            if(j==1){copia = c(NA,SI1[j,(j+1):N])}else{if(j==ncol(SI1)){copia=c(SI1[1:(j-1),j],NA)}else{
              copia = c(SI1[1:(j-1),j],NA,SI1[j,(j+1):N])}}

            for(l in 1:N){
              if(is.na(MU[i,l])){
                copia[l] = NA
              }
            }
            if(sum(!is.na(copia))==0){nota=NA}else{
              for(s in 1:neighbors){
                if(sum(!is.na(copia))==0){break}
                ind         = which.max(copia)
                valor       = max(copia,na.rm = T)

                if(valor <= 0)
                  break


                v_ind_b   = c(v_ind_b,ind)
                v_valor_b = c(v_valor_b,valor)
                copia[ind]  = NA
              }
              n_=length(v_ind_b)
              if(n_==0){nota=NA}else{#Caso nao seja possivel estimar, nota=NA
                nota= sum(MU[i,v_ind_b]*v_valor_b,na.rm=T)/sum(v_valor_b)}
            }
            v_notas=c(v_notas,nota)
            v_ind=c(v_ind,j)
          }

        }
        nomes = NULL

        for(u in 1:k){
          contador = 2 + (u/k)
          setTxtProgressBar(pb,contador)

          if(sum(!is.na(v_notas))==0){break}
          nota<-max(v_notas,na.rm=T)
          indice<-v_ind[which.max(v_notas)] #Pega o indice da nota estimada referente a MU


          if(nota<cuts){break}
          nomes <- c(nomes,colnames(MU)[indice])

          v_notas[which.max(v_notas)]  = NA

        }

        if(length(nomes)==0){stop("Sorry, we don't have any items to recommend to this user.")}
        setTxtProgressBar(pb,3)
        close(pb)
        return(nomes)

      }
      if(similarity=="adjcos"){



        v_notas=NULL
        v_ind = NULL
        for(j in 1:N){     # Percorrer o vetor das avaliacoes do usuario Id_u, e estimar as avaliacoes dos itens nao avaliados por ele.
          contador = 1 + (j/N)*1
          setTxtProgressBar(pb,contador)


          if(is.na(MU[i,j])){
            v_ind_b   = NULL
            v_valor_b = NULL

            if(j==1){copia = c(NA,SI2[j,(j+1):N])}else{if(j==ncol(SI2)){copia=c(SI2[1:(j-1),j],NA)}else{
              copia = c(SI2[1:(j-1),j],NA,SI2[j,(j+1):N])}}

            for(l in 1:N){
              if(is.na(MU[i,l])){
                copia[l] = NA
              }
            }
            if(sum(!is.na(copia))==0){nota=NA}else{
              for(s in 1:neighbors){
                if(sum(!is.na(copia))==0){break}
                ind         = which.max(copia)
                valor       = max(copia,na.rm = T)

                if(valor <= 0)
                  break


                v_ind_b   = c(v_ind_b,ind)
                v_valor_b = c(v_valor_b,valor)
                copia[ind]  = NA
              }
              n_=length(v_ind_b)
              if(n_==0){nota=NA}else{#Caso nao seja possivel estimar, nota=NA
                nota= sum(MU[i,v_ind_b]*v_valor_b,na.rm=T)/sum(v_valor_b)}
            }
            v_notas=c(v_notas,nota)
            v_ind=c(v_ind,j)
          }

        }
        nomes = NULL

        for(u in 1:k){
          contador = 2 + (u/k)*1
          setTxtProgressBar(pb,contador)

          if(sum(!is.na(v_notas))==0){break}
          nota<-max(v_notas,na.rm=T)
          indice<-v_ind[which.max(v_notas)] #Pega o indice da nota estimada referente a MU


          if(nota<cuts){break}
          nomes <- c(nomes,colnames(MU)[indice])

          v_notas[which.max(v_notas)]  = NA

        }

        if(length(nomes)==0){stop("Sorry, we don't have any items to recommend to this user.")}
        setTxtProgressBar(pb,3)
        close(pb)
        return(nomes)

      }

    }
    if(type=="user"){

      if(similarity=="cos"){


        v_notas=  NULL
        v_ind = NULL

        for(j in 1:N){     # Percorrer o vetor das avaliacoes do usuario Id_u, e estimar as avaliacoes dos itens nao avaliados por ele.

          contador = 1 + (j/N)*1
          setTxtProgressBar(pb,contador)


          if(is.na(MU[i,j])){
            v_ind_b   = NULL
            v_valor_b = NULL

            if(i==1){copia=c(NA,SU1[i,(i+1):M])}else{if(i==M){copia=c(SU1[1:(i-1),i],NA)}else{copia = c(SU1[1:(i-1),i],NA,SU1[i,(i+1):M])}}


            #varrer o copia colocando NA nos usuarios que nao avaliaram j
            for(l in 1:M){
              if(is.na(MU[l,j])){
                copia[l] = NA
              }
            }
            if(sum(!is.na(copia))==0){nota=NA}else{
              for(s in 1:neighbors){
                if(sum(!is.na(copia))==0){break}
                ind         = which.max(copia)
                valor       = max(copia,na.rm = T)

                if(valor <= 0)
                  break


                v_ind_b   = c(v_ind_b,ind)
                v_valor_b = c(v_valor_b,valor)
                copia[ind]  = NA
              }
              n_=length(v_ind_b)
              if(n_==0){nota=NA}else{#Caso nao seja possivel estimar, nota=NA
                nota= sum(MU[v_ind_b,j]*v_valor_b,na.rm=T)/sum(v_valor_b)}
            }
            v_notas=c(v_notas,nota)
            v_ind=c(v_ind,j)
          }

        }
        nomes = NULL

        for(u in 1:k){
          contador = 2 + (u/k)*1
          setTxtProgressBar(pb,contador)
          if(sum(!is.na(v_notas))==0){break}
          nota<-max(v_notas,na.rm=T)
          indice<-v_ind[which.max(v_notas)] #Pega o indice da nota estimada referente a MU


          if(nota<cuts){break}
          nomes <- c(nomes,colnames(MU)[indice])

          v_notas[which.max(v_notas)]  = NA

        }

        if(length(nomes)==0){stop("Sorry, we don't have any items to recommend to this user.")}
        setTxtProgressBar(pb,3)
        close(pb)
        return(nomes)


      }
      #Utilizando a SU 2 - Coeficiente de Pearson (Coseno Ajustado)
      if(similarity=="adjcos"){

        v_notas=  NULL
        v_ind = NULL

        for(j in 1:N){     # Percorrer o vetor das avaliacoes do usuario Id_u, e estimar as avaliacoes dos itens nao avaliados por ele.

          contador = 1 + (j/N)*1
          setTxtProgressBar(pb,contador)


          if(is.na(MU[i,j])){
            v_ind_b   = NULL
            v_valor_b = NULL

            if(i==1){copia=c(NA,SU2[i,(i+1):M])}else{if(i==M){copia=c(SU2[1:(i-1),i],NA)}else{copia = c(SU2[1:(i-1),i],NA,SU2[i,(i+1):M])}}


            #varrer o copia colocando NA nos usuarios que nao avaliaram j
            for(l in 1:M){
              if(is.na(MU[l,j])){
                copia[l] = NA
              }
            }
            if(sum(!is.na(copia))==0){nota=NA}else{
              for(s in 1:neighbors){
                if(sum(!is.na(copia))==0){break}
                ind         = which.max(copia)
                valor       = max(copia,na.rm = T)

                if(valor <= 0)
                  break


                v_ind_b   = c(v_ind_b,ind)
                v_valor_b = c(v_valor_b,valor)
                copia[ind]  = NA
              }
              n_=length(v_ind_b)
              if(n_==0){nota=NA}else{#Caso nao seja possivel estimar, nota=NA
                nota= averages_u[i] + (sum((MU[v_ind_b,j]-averages_u[v_ind_b])*v_valor_b,na.rm=T)/sum(v_valor_b))}
                nota = as.numeric(nota)
              }
            v_notas=c(v_notas,nota)
            v_ind=c(v_ind,j)
          }

        }
        nomes = NULL

        for(u in 1:k){
          contador = 2 + (u/k)*1
          setTxtProgressBar(pb,contador)
          if(sum(!is.na(v_notas))==0){break}
          nota<-max(v_notas,na.rm=T)
          indice<-v_ind[which.max(v_notas)] #Pega o indice da nota estimada referente a MU


          if(nota<cuts){break}
          nomes <- c(nomes,colnames(MU)[indice])

          v_notas[which.max(v_notas)]  = NA

        }

        if(length(nomes)==0){stop("Sorry, we don't have any items to recommend to this user.")}
        setTxtProgressBar(pb,3)
        close(pb)
        return(nomes)






      }

    }
  },
  topkusers=function(Id_i,k=5,type,neighbors=5,cuts=3.5,similarity =ifelse(is_binary==TRUE,"cos","adjcos"), is_binary = FALSE){
    "A function that indicates the k users who will like the item Id_i.The recommendation can be made through similarity between users, when type = 'user', as well as through similarity between
     items, when type = 'item'. Id_i : A Character, a Item ID; k : Number of recommendations (default=5); type: A character string, 'user' or 'item'; neighbors: Number of similarities used for
     the estimates (default=5); cuts: Cut score designated to determine if it is recommended (Default=3.5); similarity: The methodology used to estimate the rating. Must be one of 'cos', for
     cosine similarity, or 'adjcos' (default), for adjusted cosine similarity. When type='user', 'adjcos' comprises the Pearson similarity. This choice can alter the way the estimate is calculated;
     is_binary: it's TRUE if your dataset contains binary ratings or FALSE (default), otherwhise. If it's TRUE, similarity will be 'cos', because 'adjcos' isn't recommended for binary ratings."
    if(!is.character(similarity)){stop("similarity must be a character.")}
    if(!is.character(Id_i)){stop("Id_i must be a character.")}
    if(type!="user" && type!="item"){stop("type should be one of 'user' or 'item'.")}
    if(similarity !="cos" && similarity !="adjcos"){stop("similarity should be one of 'cos' or 'adjcos'.")}
    M = nrow(MU)
    N = ncol(MU)
    encontrou_j = F
    for(j in 1:N){
      if(colnames(MU)[j]==Id_i){
        encontrou_j = T
        break
      }
    }
    if(!encontrou_j)
      stop("This is not a valid item.")


    message("Progress:")
    pb <- txtProgressBar(min = 1, max = 3, style = 3)

    if(type=="user"){
      ####type = user - VIA SU

      if(similarity=="cos"){
        ### VIA SU1 - Distancia Cosseno


        v_notas=  NULL
        v_ind = NULL


        for(i in 1:M){     # Percorrer o vetor das avaliacoes do usuario Id_u, e estimar as avaliacoes dos itens nao avaliados por ele.
          contador = 1 + (i/M)*1
          setTxtProgressBar(pb,contador)

          if(is.na(MU[i,j])){
            v_ind_b   = NULL
            v_valor_b = NULL
            M = ncol(SU1)
            if(i==1){copia=c(NA,SU1[i,(i+1):M])}else{if(i==M){copia=c(SU1[1:(i-1),i],NA)}else{copia = c(SU1[1:(i-1),i],NA,SU1[i,(i+1):M])}}


            #varrer o copia colocando NA nos usuarios que nao avaliaram j
            for(l in 1:M){
              if(is.na(MU[l,j])){
                copia[l] = NA
              }
            }
            if(sum(!is.na(copia))==0){nota=NA}else{
              for(s in 1:neighbors){
                if(sum(!is.na(copia))==0){break}
                ind         = which.max(copia)
                valor       = max(copia,na.rm = T)

                if(valor <= 0)
                  break


                v_ind_b   = c(v_ind_b,ind)
                v_valor_b = c(v_valor_b,valor)
                copia[ind]  = NA
              }
              n_=length(v_ind_b)
              if(n_==0){nota=NA}else{#Caso nao seja possivel estimar, nota=NA
                nota= sum(MU[v_ind_b,j]*v_valor_b,na.rm=T)/sum(v_valor_b)}
            }
            v_notas=c(v_notas,nota)
            v_ind=c(v_ind,i)
          }

        }
        nomes = NULL

        for(u in 1:k){
          contador = 2 + (u/k)
          setTxtProgressBar(pb,contador)
          nota<-max(v_notas,na.rm=T)
          indice<-v_ind[which.max(v_notas)] #Pega o indice da nota estimada referente a MU


          if(nota<cuts){break}
          nomes <- c(nomes,rownames(MU)[indice])

          v_notas[which.max(v_notas)]  = NA

        }
        if(length(nomes)==0){stop("Sorry, we don't have any users to recommend to this item.")}
        setTxtProgressBar(pb,3)
        close(pb)
        return(nomes)
      }
      if(similarity=="adjcos"){


        v_notas=  NULL
        v_ind = NULL


        for(i in 1:M){     # Percorrer o vetor das avaliacoes do usuario Id_u, e estimar as avaliacoes dos itens nao avaliados por ele.
          contador = 1 + (i/M)*1
          setTxtProgressBar(pb,contador)

          if(is.na(MU[i,j])){
            v_ind_b   = NULL
            v_valor_b = NULL
            M = ncol(SU2)
            if(i==1){copia=c(NA,SU2[i,(i+1):M])}else{if(i==M){copia=c(SU2[1:(i-1),i],NA)}else{copia = c(SU2[1:(i-1),i],NA,SU2[i,(i+1):M])}}


            #varrer o copia colocando NA nos usuarios que nao avaliaram j
            for(l in 1:M){
              if(is.na(MU[l,j])){
                copia[l] = NA
              }
            }
            if(sum(!is.na(copia))==0){nota=NA}else{
              for(s in 1:neighbors){
                if(sum(!is.na(copia))==0){break}
                ind         = which.max(copia)
                valor       = max(copia,na.rm = T)

                if(valor <= 0)
                  break


                v_ind_b   = c(v_ind_b,ind)
                v_valor_b = c(v_valor_b,valor)
                copia[ind]  = NA
              }
              n_=length(v_ind_b)
              if(n_==0){nota=NA}else{#Caso nao seja possivel estimar, nota=NA
                nota = averages_u[i] + (sum((MU[v_ind_b,j]-averages_u[v_ind_b])*v_valor_b,na.rm=T)/sum(v_valor_b))}
                nota = as.numeric(nota)
              }
            v_notas=c(v_notas,nota)
            v_ind=c(v_ind,i)
          }

        }
        nomes = NULL

        for(u in 1:k){
          contador = 2 + (u/k)*1
          setTxtProgressBar(pb,contador)
          nota<-max(v_notas,na.rm=T)
          indice<-v_ind[which.max(v_notas)] #Pega o indice da nota estimada referente a MU


          if(nota<cuts){break}
          nomes <- c(nomes,rownames(MU)[indice])

          v_notas[which.max(v_notas)]  = NA

        }
        if(length(nomes)==0){stop("Sorry, we don't have any users to recommend to this item.")}
        setTxtProgressBar(pb,3)
        close(pb)
        return(nomes)

      }




    }
    if(type=="item"){
      ## type = "item"
      if(similarity=="cos"){



        v_notas=NULL
        v_ind = NULL
        for(i in 1:M){     # Percorrer o vetor das avaliacoes do usuario Id_u, e estimar as avaliacoes dos itens nao avaliados por ele.

          contador = 1 + (i/M)*1
          setTxtProgressBar(pb,contador)

          if(is.na(MU[i,j])){
            v_ind_b   = NULL
            v_valor_b = NULL

            if(j==1){copia = c(NA,SI1[j,(j+1):N])}else{if(j==ncol(SI1)){copia=c(SI1[1:(j-1),j],NA)}else{
              copia = c(SI1[1:(j-1),j],NA,SI1[j,(j+1):N])}}

            for(l in 1:N){
              if(is.na(MU[i,l])){
                copia[l] = NA
              }
            }
            if(sum(!is.na(copia))==0){nota=NA}else{
              for(s in 1:neighbors){
                if(sum(!is.na(copia))==0){break}
                ind         = which.max(copia)
                valor       = max(copia,na.rm = T)

                if(valor <= 0)
                  break


                v_ind_b   = c(v_ind_b,ind)
                v_valor_b = c(v_valor_b,valor)
                copia[ind]  = NA
              }
              n_=length(v_ind_b)
              if(n_==0){nota=NA}else{#Caso nao seja possivel estimar, nota=NA
                nota= sum(MU[i,v_ind_b]*v_valor_b,na.rm=T)/sum(v_valor_b)}
            }
            v_notas=c(v_notas,nota)
            v_ind=c(v_ind,i)
          }

        }
        nomes = NULL

        for(u in 1:k){
          contador = 2 + (u/k)*1
          setTxtProgressBar(pb,contador)
          nota<-max(v_notas,na.rm=T)
          indice<-v_ind[which.max(v_notas)] #Pega o indice da nota estimada referente a MU


          if(nota<cuts){break}
          nomes <- c(nomes,rownames(MU)[indice])

          v_notas[which.max(v_notas)]  = NA

        }
        if(length(nomes)==0){stop("Sorry, we don't have any users to recommend to this item.")}
        setTxtProgressBar(pb,3)
        close(pb)
        return(nomes)


      }
      if(similarity=="adjcos"){



        v_notas=NULL
        v_ind = NULL
        for(i in 1:M){     # Percorrer o vetor das avaliacoes do usuario Id_u, e estimar as avaliacoes dos itens nao avaliados por ele.

          contador = 1 + (i/M)*1
          setTxtProgressBar(pb,contador)

          if(is.na(MU[i,j])){
            v_ind_b   = NULL
            v_valor_b = NULL

            if(j==1){copia = c(NA,SI2[j,(j+1):N])}else{if(j==ncol(SI2)){copia=c(SI2[1:(j-1),j],NA)}else{
              copia = c(SI2[1:(j-1),j],NA,SI2[j,(j+1):N])}}

            for(l in 1:N){
              if(is.na(MU[i,l])){
                copia[l] = NA
              }
            }
            if(sum(!is.na(copia))==0){nota=NA}else{
              for(s in 1:neighbors){
                if(sum(!is.na(copia))==0){break}
                ind         = which.max(copia)
                valor       = max(copia,na.rm = T)

                if(valor <= 0)
                  break


                v_ind_b   = c(v_ind_b,ind)
                v_valor_b = c(v_valor_b,valor)
                copia[ind]  = NA
              }
              n_=length(v_ind_b)
              if(n_==0){nota=NA}else{#Caso nao seja possivel estimar, nota=NA
                nota= sum(MU[i,v_ind_b]*v_valor_b,na.rm=T)/sum(v_valor_b)}
            }
            v_notas=c(v_notas,nota)
            v_ind=c(v_ind,i)
          }

        }
        nomes = NULL

        for(u in 1:k){
          contador = 2 + (u/k)*1
          setTxtProgressBar(pb,contador)
          nota<-max(v_notas,na.rm=T)
          indice<-v_ind[which.max(v_notas)] #Pega o indice da nota estimada referente a MU


          if(nota<cuts){break}
          nomes <- c(nomes,rownames(MU)[indice])

          v_notas[which.max(v_notas)]  = NA

        }
        if(length(nomes)==0){stop("Sorry, we don't have any users to recommend to this item.")}
        setTxtProgressBar(pb,3)
        close(pb)
        return(nomes)





      }


    }
  },
  estimaterating=function(Id_u,Id_i,type,neighbors=5,similarity =ifelse(is_binary==TRUE,"cos","adjcos"), is_binary = FALSE){
    "A function that returns the estimated rating for the evaluation of item Id_i by user Id_u. The recommendation can be made through similarity between users, when type = 'user', and also through the similarity between items, when type = 'item'.
     Id_u: A character, a user ID; Id_i: A character, an item ID; type: A character string, 'user' or 'item'; neighbors: Number of similarities used for the estimates.(Default=5); similarity: The methodology used to estimate the rating. Must be one of 'cos', for
     cosine similarity, or 'adjcos' (default), for adjusted cosine similarity. When type='user', 'adjcos' comprises the Pearson similarity. This choice can alter the way the estimate is calculated; is_binary: it's TRUE if your dataset contains binary ratings or
     FALSE (default), otherwhise. If it's TRUE, similarity will be 'cos', because 'adjcos' isn't recommended for binary ratings. "
    if(!is.character(similarity)){stop("similarity must be a character.")}
    if(!is.character(Id_u)){stop("Id_u must be a character.")}
    if(!is.character(Id_i)){stop("Id_i must be a character.")}
    if(type!="user" && type!="item"){stop("type should be one of 'user' or 'item'.")}
    if(similarity !="cos" && similarity !="adjcos"){stop("similarity should be one of 'cos' or 'adjcos'.")}
    M = nrow(MU)
    encontrou_i = F
    for(i in 1:M){
      if(rownames(MU)[i]==Id_u){
        encontrou_i = T
        break
      }
    }
    if(!encontrou_i)
      stop("This is not a valid user.")
    #i guarda o num da linha de MU correspondente ao usuario Id_u

    N = ncol(MU)
    encontrou_j = F
    for(j in 1:N){
      if(colnames(MU)[j]==Id_i){
        encontrou_j = T
        break
      }
    }
    if(!encontrou_j)
      stop("This is not a valid item.")



    if(type=="user"){
      #busca os n indices dos usuarios mais proximos de i que avaliaram j
      #no final v_ind e v_valores sao arrays de tamanho n ou menos,
      #a primeira posicao guarda um array com indices dos usuarios acima e a
      #segunda posicao um array com os valores das similaridades entre entre esses
      #usuarios e o usuario k
      if(similarity=="cos"){


        M = ncol(SU1)
        if(i==1){copia=c(NA,SU1[i,(i+1):M])}else{if(i==M){copia=c(SU1[1:(i-1),i],NA)}else{copia = c(SU1[1:(i-1),i],NA,SU1[i,(i+1):M])}}


        #varrer o copia colocando NA nos usuarios que nao avaliaram j
        for(l in 1:M){
          if(is.na(MU[l,j])){
            copia[l] = NA
          }
        }
        if(sum(!is.na(copia))==0){stop("It isn't possible to estimate the rating for this user and item.")}else{
          v_ind   = NULL
          v_valor = NULL

          for(k in 1:neighbors){
            if(sum(!is.na(copia))==0){break}
            ind         = which.max(copia)
            valor       = max(copia,na.rm = T)

            if(valor <= 0)
              break

            v_ind   = c(v_ind,ind)
            v_valor = c(v_valor,valor)
            copia[ind]  = NA
          }


          #agora v_ind e v_valor guardam os indices e as similaridades dos usuarios mais semelhantes a i que avaliaram j
          #cuidado, os arrays podem ser vazios.

          n_ = length(v_ind)
          if(n_ == 0){
            stop("It isn't possible to estimate the rating for this user and item.")
          }
          nota = sum(MU[v_ind,j]*v_valor)/sum(v_valor)
          return(nota)
        }
      }
      if(similarity=="adjcos"){
        M = ncol(SU2)
        if(i==1){copia=c(NA,SU2[i,(i+1):M])}else{if(i==M){copia=c(SU2[1:(i-1),i],NA)}else{copia = c(SU2[1:(i-1),i],NA,SU2[i,(i+1):M])}}


        #varrer o copia colocando NA nos usuarios que nao avaliaram j
        for(l in 1:M){
          if(is.na(MU[l,j])){
            copia[l] = NA
          }
        }
        if(sum(!is.na(copia))==0){stop("It isn't possible to estimate the rating for this user and item.")}else{
          v_ind   = NULL
          v_valor = NULL

          for(k in 1:neighbors){
            if(sum(!is.na(copia))==0){break}
            ind         = which.max(copia)
            valor       = max(copia,na.rm = T)

            if(valor <= 0)
              break

            v_ind   = c(v_ind,ind)
            v_valor = c(v_valor,valor)
            copia[ind]  = NA
          }


          #agora v_ind e v_valor guardam os indices e as similaridades dos usuarios mais semelhantes a i que avaliaram j
          #cuidado, os arrays podem ser vazios.

          n_ = length(v_ind)
          if(n_ == 0){
            stop("It isn't possible to estimate the rating for this user and item.")
          }
          nota = averages_u[i] + (sum((MU[v_ind,j]-averages_u[v_ind])*v_valor)/sum(v_valor))
          nota = as.numeric(nota)
          return(nota)
        }



      }
    }
    if(type=="item"){
      #busca os n indices dos itens mais proximos de j que foram avaliados por i
      #no final v_ind e v_valores sao arrays de tamanho n ou menos,
      #a primeira posicao guarda um array com indices dos itens acima e a
      #segunda posicao um array com os valores das similaridades entre entre esses
      #itens e o item k
      if(similarity=="cos"){

        N = ncol(SI1)
        if(j==1){copia=c(NA,SI1[j,(j+1):N])}else{if(j==N){copia=c(SI1[1:(j-1),j],NA)}else{copia = c(SI1[1:(j-1),j],NA,SI1[j,(j+1):N])}}

        #varrer o copia colocando NA nos usuarios que nao avaliaram j
        for(l in 1:N){
          if(is.na(MU[i,l])){
            copia[l] = NA
          }
        }
        if(sum(!is.na(copia))==0){stop("It isn't possible to estimate the rating for this user and item.")}else{
          v_ind   = NULL
          v_valor = NULL

          for(l in 1:neighbors){
            if(sum(!is.na(copia))==0){break}
            ind         = which.max(copia)
            valor       = max(copia,na.rm = T)

            if(valor <= 0)
              break

            v_ind   = c(v_ind,ind)
            v_valor = c(v_valor,valor)
            copia[ind]  = NA
          }


          #agora v_ind e v_valor guardam os indices e as similaridades dos itens mais semelhantes a j que foram avaliados por i
          #cuidado, os arrays podem ser vazios.

          n_ = length(v_ind)
          if(n_ == 0){
            stop("It isn't possible to estimate the rating for this user and item.")
          }
          nota = sum(MU[i,v_ind]*v_valor)/sum(v_valor)
          return(nota)

        }
      }
      if(similarity=="adjcos"){
        N = ncol(SI2)
        if(j==1){copia=c(NA,SI2[j,(j+1):N])}else{if(j==N){copia=c(SI2[1:(j-1),j],NA)}else{copia = c(SI2[1:(j-1),j],NA,SI2[j,(j+1):N])}}

        #varrer o copia colocando NA nos usuarios que nao avaliaram j
        for(l in 1:N){
          if(is.na(MU[i,l])){
            copia[l] = NA
          }
        }
        if(sum(!is.na(copia))==0){stop("It isn't possible to estimate the rating for this user and item.")}else{

          v_ind   = NULL
          v_valor = NULL

          for(k in 1:neighbors){
            if(sum(!is.na(copia))==0){break}
            ind         = which.max(copia)
            valor       = max(copia,na.rm = T)

            if(valor <= 0)
              break

            v_ind   = c(v_ind,ind)
            v_valor = c(v_valor,valor)
            copia[ind]  = NA
          }


          #agora v_ind e v_valor guardam os indices e as similaridades dos itens mais semelhantes a j que foram avaliados por i
          #cuidado, os arrays podem ser vazios.

          n_ = length(v_ind)
          if(n_ == 0){
            stop("It isn't possible to estimate the rating for this user and item.")
          }
          nota = sum(MU[i,v_ind]*v_valor)/sum(v_valor)
          return(nota)

        }


      }

    }

  },
  deleterating=function(Id_u,Id_i){
    "Deletes the rating from user Id_u to item Id_i. The object CF matrices and vectors will be updated. Id_u : A character, a user ID; Id_i : A character, an item ID. "
    if(!is.character(Id_u)){stop("Id_u must be a character.")}
    if(!is.character(Id_i)){stop("Id_i must be a character.")}

    M = nrow(MU)
    N = ncol(MU)
    encontrou_i = F
    for(i in 1:M){
      if(rownames(MU)[i]==Id_u){
        encontrou_i = T
        break
      }
    }
    if(!encontrou_i)
      stop("This is not a valid user.")
    #i guarda o num da linha de MU correspondente ao usuario Id_u


    encontrou_j = F
    for(j in 1:N){
      if(colnames(MU)[j]==Id_i){
        encontrou_j = T
        break
      }
    }
    if(!encontrou_j)
      stop("This is not a valid item.")

    if(is.na(MU[i,j])){stop("This rating doesnt exist.")}
    valor = MU[i,j]
    #Alteracao MU
    MU[i,j]<<-NA
    #Alteracao averages_i e n_aval_i
    averages_i[j] <<- averages_i[j]*n_aval_i[j]/(n_aval_i[j]-1) - valor/(n_aval_i[j]-1)
    n_aval_i[j] <<- n_aval_i[j] - 1
    #Alteracao averages_u e n_aval_u
    averages_u[i] <<- averages_u[i]*n_aval_u[i]/(n_aval_u[i]-1) - valor/(n_aval_u[i]-1)
    n_aval_u[i] <<- n_aval_u[i] - 1

    #Alteracao SUs
    ind_aval_i<-NULL #indices dos usuarios que avaliaram o item j
    for(k in 1:M){
      if(!is.na(MU[k,j]))
        ind_aval_i=c(ind_aval_i,k)
    }
    #Alteracao SU1

    for(k in ind_aval_i){
      #calcular a similaridade de i com k
      nova_sim_i_k = sum(MU[k,]*MU[i,],na.rm = T)/(sqrt(sum(MU[k,]^2,na.rm = T))*sqrt(sum(MU[i,]^2,na.rm = T)))
      if(i!=k){
        if(i < k){
          SU1[i,k] <<- nova_sim_i_k
        }else{
          SU1[k,i] <<- nova_sim_i_k
        }
      }
    }


    #Alteracao SU2

    for(k in ind_aval_i){
      #calcular a similaridade de i com k
      nova_sim_i_k = sum((MU[k,]-averages_u[k])*(MU[i,]-averages_u[i]),na.rm = T)/(sqrt(sum((MU[k,]-averages_u[k])^2,na.rm = T))*sqrt(sum((MU[i,]-averages_u[i])^2,na.rm = T)))
      if(i!=k){
        if(i < k){
          SU2[i,k] <<- nova_sim_i_k
        }else{
          SU2[k,i] <<- nova_sim_i_k
        }
      }
    }



    #Alteracao SIs
    ind_aval_u<-NULL #indice dos itens que foram avaliados por i
    for(k in 1:N){
      if(!is.na(MU[i,k]))
        ind_aval_u=c(ind_aval_u,k)
    }


    #Alteracao SI1
    for(k in ind_aval_u){
      nova_sim_j_k = sum(MU[,k]*MU[,j],na.rm = T)/(sqrt(sum(MU[,k]^2,na.rm = T))*sqrt(sum(MU[,j]^2,na.rm = T)))
      if(j!=k){
        if(j < k){
          SI1[j,k] <<- nova_sim_j_k
        }else{
          SI1[k,j] <<- nova_sim_j_k
        }
      }
    }


    #Alteração SI2

    for(k in ind_aval_u){
      nova_sim_j_k = sum((MU[,k]-averages_u[k])*(MU[,j]-averages_u[j]),na.rm = T)/(sqrt(sum((MU[,k]-averages_u[k])^2,na.rm = T))*sqrt(sum((MU[,j]-averages_u[j])^2,na.rm = T)))
      if(j!=k){
        if(j < k){
          SI2[j,k] <<- nova_sim_j_k
        }else{
          SI2[k,j] <<- nova_sim_j_k
        }
      }
    }

  },
  changerating=function(Id_u,Id_i,r){
    "Changes the rating from user Id_u to item Id_i. The object CF matrices and vectors will be updated. Id_u : A character, a user ID; Id_i : A character, an item ID; r : The new rating."
    if(!is.character(Id_u)){stop("Id_u must be a character.")}
    if(!is.character(Id_i)){stop("Id_i must be a character.")}
    M = nrow(MU)
    N = ncol(MU)
    encontrou_i = F
    for(i in 1:M){
      if(rownames(MU)[i]==Id_u){
        encontrou_i = T
        break
      }
    }
    if(!encontrou_i)
      stop("This is not a valid user.")
    #i guarda o num da linha de MU correspondente ao usuario Id_u


    encontrou_j = F
    for(j in 1:N){
      if(colnames(MU)[j]==Id_i){
        encontrou_j = T
        break
      }
    }
    if(!encontrou_j)
      stop("This is not a valid item.")
    if(is.na(MU[i,j])){stop("If you want to add a new rating, use $newrating().")}

    deleterating(Id_u,Id_i)
    newrating(Id_u,Id_i,r)
  }
)
)
