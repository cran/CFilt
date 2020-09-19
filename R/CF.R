#'A Reference Class to represent a object CF
#'@description A class of objects created structured with the following objects: the MU - Utility Matrix, the SU1 and SU2 - Matrices of Similarity
#'between Users, the SI1 e SI2 - Matrices of Similarity between Items, and the vectors averages_u, averages_i, n_aval_u and n_aval_i. The class
#'contains methods, general functions with the objectives of manipulating the data and making recommendations, from the structures
#'present in the class. The data manipulation methods comprise addsimilarity, addnewuser, addnewemptyuser, deleteuser, addnewitem, addnewemptyitem, deleteitem, newrating and deleterating,
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
#'objectCF<-CFbuilder(Data = ratings, sim_user="pearson", sim_item="adjcos")
#'objectCF$addsimilarity(sim_user="cos",sim_item="cos")
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
#'objectCF$deleteuser(Id_u = "Jessica")
#'objectCF$addnewitem(Id_i = "Avengers: Endgame",Ids_u = c("1","2"),r = c(5,3))
#'objectCF$addnewemptyitem(Id_i = "Star Wars")
#'objectCF$deleteitem(Id_i="Star Wars")
#'objectCF$newrating(Id_u = "1", Id_i = "Till Luck Do Us Part 2",r = 2)
#'objectCF$recommend(Id_u = "2", Id_i = "Iron Man 3", type = "user")
#'objectCF$kclosestitems(Id_i = "Iron Man 3", k = 3)
#'objectCF$topkitems(Id_u = "3",k = 3, type = "user")
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
  addsimilarity=function(sim_user='none',sim_item='none'){
    "Adds new methodologies even after the construction and modification of the CF object used. The matrices of similarities representing each requested methodology will be added.
    sim_user: a methodology used to estimate the rating by users similarity. Can be 'cos','pearson','both' or 'none'. If it equals 'cos' (Cosine Similarity), the SU1 will be built.
    If it equals 'pearson' (Pearson Similarity), the SU2 will be built. If it equals 'both', the SU1 and SU2 will be built. If it equals 'none', nothing will be built. sim_item: A
    methodology used to estimate the rating by itens similarity. Can be 'cos','adjcos','both' or 'none'. If it equals 'cos' (Cosine Similarity), the SI1 will be built. If it equals
    'adjcos' (Adjusted Cosine Similarity), the SI2 will be built. If it equals 'both', the SI1 and SI2 will be built. If it equals 'none', nothing will be built."


    if(sim_user!="cos" && sim_user!="pearson" && sim_user!="both" && sim_user!="none" ){stop("sim_user can be only 'cos', 'pearson','both' or 'none'.")}
    if(sim_item!="cos" && sim_item!="adjcos" && sim_item!="both" && sim_item!="none" ){stop("sim_user can be only 'cos', 'adjcos','both' or 'none'.")}
    if((sim_user=="cos"||sim_user=="both") && sum(dim(SU1))!=0){stop("You already have the Cosine similarity in SU1.")}
    if((sim_user=="pearson"||sim_user=="both") && sum(dim(SU2))!=0){stop("You already have the Pearson similarity in SU2.")}
    if((sim_item=="cos"||sim_item=="both") && sum(dim(SI1))!=0){stop("You already have the Cosine similarity in SI1.")}
    if((sim_item=="adjcos"||sim_item=="both") && sum(dim(SI2))!=0){stop("You already have the Adjusted Cosine similarity in SI2.")}
    if(sim_user=="none" && sim_item =="none"){stop("Nothing will change.")}




    m=nrow(MU)
    n=ncol(MU)
    nome_i=colnames(MU)
    nome_u = rownames(MU)




    if(sim_user=="cos" || sim_user=="both"){

      message("Building SU1...")
      pb <- txtProgressBar(min = 0, max = m, style = 3)


      SU1<<-matrix(NA,m,m,dimnames = list(nome_u,nome_u))
      for(i in 1:m){
        setTxtProgressBar(pb, i)
        for(j in i:m){
          if(j!=i){
            SU1[i,j]<<-sum(MU[i,]*MU[j,],na.rm = T)/(sqrt(sum((MU[i,])^2,na.rm = T))*sqrt(sum((MU[j,])^2,na.rm = T)))
          }
          if(j==i){SU1[i,j]<<-1}
        }
      }


      setTxtProgressBar(pb,m)
      close(pb)


    }
    if(sim_item=="cos" || sim_item=="both"){

      message("Building SI1...")
      pb <- txtProgressBar(min = 0, max = n, style = 3)


      SI1 <<- matrix(NA,n,n,dimnames=list(nome_i,nome_i))
      for(i in 1:n){
        setTxtProgressBar(pb, i)
        for(j in i:n){
          if(j!=i){
            SI1[i,j] <<- sum(MU[,i]*MU[,j],na.rm=T)/(sqrt(sum((MU[,i])^2,na.rm=T))*sqrt(sum((MU[,j])^2,na.rm=T)))
          }
          if(j==i){SI1[i,j]<<-1}
        }
      }

      setTxtProgressBar(pb,n)
      close(pb)

    }

    if(sim_item=="adjcos" || sim_item=="both"){


      message("Building SI2...")
      pb <- txtProgressBar(min = 0, max = n, style = 3)



      SI2<<-matrix(NA,n,n,dimnames = list(nome_i,nome_i))
      for(i in 1:n){
        setTxtProgressBar(pb, i)
        for(j in i:n){
          if(j!= i){
            SI2[i,j]<<-sum((MU[,i]-averages_u)*(MU[,j]-averages_u),na.rm = T)/(sqrt(sum((MU[,i]-averages_u)^2,na.rm = T))*sqrt(sum((MU[,j]-averages_u)^2,na.rm = T)))
          }
          if(j==i){SI2[i,j]<<-1}
        }
      }
      setTxtProgressBar(pb,n)
      close(pb)

    }

  if(sim_user=="pearson" || sim_user == "both" ){

    message("Building SU2...")
    pb <- txtProgressBar(min = 0, max = m, style = 3)

    SU2<<-matrix(NA,m,m,dimnames = list(nome_u,nome_u))

    for(i in 1:m){
      setTxtProgressBar(pb,i)

      for(j in i:m){



        if(j!= i){
          SU2[i,j]<<-sum((MU[i,]-averages_u[i])*(MU[j,]-averages_u[j]),na.rm = T)/(sqrt(sum((MU[i,]-averages_u[i])^2,na.rm = T))*sqrt(sum((MU[j,]-averages_u[j])^2,na.rm = T)))
        }
        if(j==i){SU2[i,j]<<-1}
      }
    }

    setTxtProgressBar(pb,m)
    close(pb)


  }











  },
  addnewuser=function(Id_u,Ids_i,r){
    "Adds a new user who rated one or more items. The object CF matrices and vectors will be updated. Id_u : a character, a user ID; Ids_i : a character vector, item IDs; r : a vector with its respective ratings.
    If you want to add more users, you can use lists, where Id_u: list of characters; Ids_i: list of vectors of characters; r: list of vectors of ratings. "

    if(is.list(Id_u) && is.list(Ids_i) && is.list(r)){
      u=length(Id_u)
      for(k in 1:u){
        if(k==u){
          Id_u = as.character(Id_u[[k]])
          Ids_i= as.character(Ids_i[[k]])
          r= as.numeric(r[[k]])
        }else{
        addnewuser(Id_u = Id_u[[k]], Ids_i = Ids_i[[k]], r=r[[k]])
        }
      }
    }




    if(!is.character(Id_u)){stop("Id_u must be a character.")}
    if(sum(!is.character(Ids_i))!=0){stop("Ids_i must be a character vector.")}

    have_SU1=F
    have_SU2=F
    have_SI1=F
    have_SI2=F
    if(sum(dim(SU1))!=0)
      have_SU1=TRUE
    if(sum(dim(SU2))!=0)
      have_SU2=TRUE
    if(sum(dim(SI1))!=0)
      have_SI1=TRUE
    if(sum(dim(SI2))!=0)
      have_SI2=TRUE



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





if(have_SU1==TRUE){
    #Alteracao SU1
    SU1<<-rbind(SU1,NA)
    rownames(SU1)[k]<<-Id_u
    SU1<<-cbind(SU1,0)
    colnames(SU1)[k]<<-Id_u
    SU1[k,k]<<-1

    u=1
    for(i in uniao_indices_i){
      contador = 1 + (u/length(uniao_indices_i))*1
      setTxtProgressBar(pb,contador)
      u=u+1
      if(i<k){
        SU1[i,k]<<-sum(MU[i,]*MU[k,],na.rm = T)/(sqrt(sum(MU[i,]^2,na.rm = T))*sqrt(sum(MU[k,]^2,na.rm = T)))
      }
    }

}
    if(have_SU2==TRUE){

    #Alteracao SU2
    SU2<<-rbind(SU2,NA)
    rownames(SU2)[k]<<-Id_u
    SU2<<-cbind(SU2,0)
    colnames(SU2)[k]<<-Id_u
    SU2[k,k]<<-1

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

}



    #Alteracao SI2
    if(have_SI2==TRUE){
    u=3
    for(i in indices_v){

      for(j in indices_v){
        if(j>i){
          SI2[i,j]<<-sum((MU[,i]-averages_u)*(MU[,j]-averages_u),na.rm = T)/(sqrt(sum((MU[,i]-averages_u)^2,na.rm = T))*sqrt(sum((MU[,j]-averages_u)^2,na.rm = T)))
        }
      }
      contador = 3 + (u/length(indices_v))*1
      setTxtProgressBar(pb,contador)
      u=u+1

    }

}


    #Alteracao SI1
    if(have_SI1==TRUE){
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

}
    setTxtProgressBar(pb,5)
    close(pb)






  },
  deleteuser=function(Id_u){
    "Deletes an already registered user. The object CF matrices and vectors will be updated. Id_u : A character, a user ID that will be deleted.
    If you want to delete more users, you can use lists where Id_u is a list of characters."

    if(is.list(Id_u)){
      u=length(Id_u)
      for(k in 1:u){
        if(k==u){
          Id_u = as.character(Id_u[[k]])
        }else{
        deleteuser(Id_u = Id_u[[k]])
        }
      }
    }







    if(!is.character(Id_u)){stop("Id_u must be a character.")}

    have_SU1=F
    have_SU2=F
    have_SI1=F
    have_SI2=F
    if(sum(dim(SU1))!=0)
      have_SU1=TRUE
    if(sum(dim(SU2))!=0)
      have_SU2=TRUE
    if(sum(dim(SI1))!=0)
      have_SI1=TRUE
    if(sum(dim(SI2))!=0)
      have_SI2=TRUE







    #Verificar se existe um usuÃ¡rio com o mesmo identificador:
    N = dim(MU)[2]
    M = dim(MU)[1]


    #Achar indice i do usuário, e caso não achar, informar que o usuário não existe.
    for(k in 1:M){
      if(rownames(MU)[k]==Id_u){
        i=k
        break
      }
      if(k==M){stop("This user ID doesn't exist. If you want to add a new user, use $addnewuser().")}
    }


    message("Progress:")
    pb <- txtProgressBar(min = 1, max = 5, style = 3)

    contador = 1
    setTxtProgressBar(pb, contador)


    #Vetores "avaliacoes" e "ind_aval" que contém respectivamente, as avaliações dos itens e os indices dos itens avaliados por Id_u.
    avaliacoes = NULL
    ind_aval = NULL
    t=1
    for(k in MU[i,]){
      if(!is.na(MU[i,t])){
        avaliacoes<-c(avaliacoes,k)
        ind_aval<-c(ind_aval,t)}
      t=t+1
    }




    #Alteracao MU

    MU<<-MU[-i,]


    #Alteracao averages_u e n_aval_u


    averages_u<<-averages_u[-i]
    n_aval_u<<-n_aval_u[-i]

    #Alteracao averages_i, n_aval_i
    t=1
    for(j in ind_aval){
      averages_i[j] <<- averages_i[j]*n_aval_i[j]/(n_aval_i[j]-1) - avaliacoes[t]/(n_aval_i[j]-1)
      n_aval_i[j] <<- n_aval_i[j] - 1
      t=t+1
    }




    #Alteracao SU1
    if(have_SU1==T){
    SU1<<-SU1[-i,-i]

}
    #Alteracao SU2
    if(have_SU2==T){
    SU2<<-SU2[-i,-i]
}



    #Alteracao SI2
    if(have_SI2==T){
    u=3
    for(k in ind_aval){

      for(j in ind_aval){
        if(j>k){
          SI2[k,j]<<-sum((MU[,k]-averages_u)*(MU[,j]-averages_u),na.rm = T)/(sqrt(sum((MU[,k]-averages_u)^2,na.rm = T))*sqrt(sum((MU[,j]-averages_u)^2,na.rm = T)))
        }
      }
      contador = 3 + (u/length(ind_aval))*1
      setTxtProgressBar(pb,contador)
      u=u+1

    }
}



    #Alteracao SI1
    if(have_SI1==T){
    u=4
    for( k in ind_aval){

      for(j in ind_aval){
        if(j>k){
          SI1[k,j]<<-sum(MU[,k]*MU[,j],na.rm = T)/(sqrt(sum(MU[,k]^2,na.rm = T))*sqrt(sum(MU[,j]^2,na.rm = T)))
        }
      }
      contador = 4 + (u/length(ind_aval))*1
      setTxtProgressBar(pb,contador)
      u=u+1

    }
    }
    setTxtProgressBar(pb,5)
    close(pb)






  },
  newrating=function(Id_u,Id_i,r){
    "Adds a new rating from user Id_u to item Id_i.The object CF matrices and vectors will be updated. Id_u : a character, a user ID; Id_i : a character, an item ID; r : the rating.
    If you want to add more ratings, you can use lists, where Id_u and Id_i are lists of characters and r is a list of ratings."

    if(is.list(Id_u) && is.list(Id_i) && is.list(r)){
      u=length(Id_u)
      for(k in 1:u){
        if(k==u){
          Id_u = as.character(Id_u[[k]])
          Id_i = as.character(Id_i[[k]])
             r = as.numeric(r[[k]])
        }else{
        newrating(Id_u = Id_u[[k]], Id_i = Id_i[[k]], r=r[[k]])
      }
    }
}








    if(!is.character(Id_u)){stop("Id_u must be a character.")}
    if(!is.character(Id_i)){stop("Id_i must be a character.")}

    have_SU1=F
    have_SU2=F
    have_SI1=F
    have_SI2=F
    if(sum(dim(SU1))!=0)
      have_SU1=TRUE
    if(sum(dim(SU2))!=0)
      have_SU2=TRUE
    if(sum(dim(SI1))!=0)
      have_SI1=TRUE
    if(sum(dim(SI2))!=0)
      have_SI2=TRUE




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
if(have_SU1==T){

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
}

    #Alteracao SU2
if(have_SU2==T){

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

}




    ind_aval_u<-NULL #indice dos itens que foram avaliados por i
    for(k in 1:N){
      if(!is.na(MU[i,k]))
        ind_aval_u=c(ind_aval_u,k)

    }

    #Alteracao SI1

if(have_SI1==T){
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

}
    #Alteração SI2
    if(have_SI2==T){
    for(k in ind_aval_u){
      nova_sim_j_k = sum((MU[,k]-averages_u)*(MU[,j]-averages_u),na.rm = T)/(sqrt(sum((MU[,k]-averages_u)^2,na.rm = T))*sqrt(sum((MU[,j]-averages_u)^2,na.rm = T)))
      if(j!=k){
        if(j < k){
          SI2[j,k] <<- nova_sim_j_k
        }else{
          SI2[k,j] <<- nova_sim_j_k
        }
      }
    }

}

  },
  addnewemptyuser=function(Id_u){
    "Adds a new user without ratings. The object CF matrices and vectors will be updated. Id_u : a character, a user ID.
    If you want to add more users, you can use lists, where Id_u is a list of characters."

    if(is.list(Id_u)){
      u=length(Id_u)
      for(k in 1:u){
        if(k==u){
          Id_u = as.character(Id_u[[k]])
        }else{
        addnewemptyuser(Id_u = Id_u[[k]])
      }
}
    }


    if(!is.character(Id_u)){stop("Id_u must be a character.")}

    have_SU1=F
    have_SU2=F

    if(sum(dim(SU1))!=0)
      have_SU1=TRUE
    if(sum(dim(SU2))!=0)
      have_SU2=TRUE




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
    if(have_SU1==T){
    SU1<<-rbind(SU1,NA)
    rownames(SU1)[k]<<-Id_u
    SU1<<-cbind(SU1,0)
    colnames(SU1)[k]<<-Id_u
    SU1[k,k]<<-1
    }
    #Alteracao SU2
    if(have_SU2==T){
    SU2<<-rbind(SU2,NA)
    rownames(SU2)[k]<<-Id_u
    SU2<<-cbind(SU2,0)
    colnames(SU2)[k]<<-Id_u
    SU2[k,k]<<-1
    }
    #Alteracao SI(Nao se altera)

  },
  addnewemptyitem=function(Id_i){
    "Adds a new item without ratings. The object CF matrices and vectors will be updated. Id_i : a character, an item ID.
    If you want to add more items, you can use lists where Id_i is a list of characters."


    if(is.list(Id_i)){
      u=length(Id_i)
      for(k in 1:u){
        if(k==u){
          Id_i = as.character(Id_i[[k]])
        }else{
          addnewemptyitem(Id_i = Id_i[[k]])
        }
      }
    }



     if(!is.character(Id_i)){stop("Id_i must be a character.")}



    have_SI1=F
    have_SI2=F

    if(sum(dim(SI1))!=0)
      have_SI1=TRUE
    if(sum(dim(SI2))!=0)
      have_SI2=TRUE


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
    if(have_SI1==T){
    SI1<<-rbind(SI1,NA)
    rownames(SI1)[k]<<-Id_i
    SI1<<-cbind(SI1,0)
    colnames(SI1)[k]<<-Id_i
    SI1[k,k]<<-1
    }
    #Alteracao SI2
    if(have_SI2==T){
    SI2<<-rbind(SI2,NA)
    rownames(SI2)[k]<<-Id_i
    SI2<<-cbind(SI2,0)
    colnames(SI2)[k]<<-Id_i
    SI2[k,k]<<-1
    }
    #Alteracao SU(NaO SE ALTERA)

  },
  addnewitem=function(Id_i,Ids_u,r){

    "Adds a new item that has been rated by one or more users. The object CF matrices and vectors will be updated. Id_i : a character, an item ID; Ids_u : a character vector, a user IDs; r : a vector with its respective ratings.
     If you want to add more items, you can use lists, where Id_i is a list of characters; Ids_u is a list of vectors of characters; r is a list of vectors of ratings. "


    if(is.list(Id_i) && is.list(Ids_u) && is.list(r)){
      u=length(Id_i)
      for(k in 1:u){
        if(k==u){
          Id_i = as.character(Id_i[[k]])
          Ids_u= as.character(Ids_u[[k]])
          r= as.numeric(r[[k]])
        }else{
          addnewitem(Id_i = Id_i[[k]], Ids_u = Ids_u[[k]], r=r[[k]])
        }
      }
    }



     if(sum(!is.character(Ids_u))!=0){stop("Ids_i must be a character.")}
    if(!is.character(Id_i)){stop("Id_i must be a character.")}

    for(p in 1:length(r)){
      if(is.na(r[p])){stop("The vector 'r' can't have NA values.")}
    }
    have_SU1=F
    have_SU2=F
    have_SI1=F
    have_SI2=F
    if(sum(dim(SU1))!=0)
      have_SU1=TRUE
    if(sum(dim(SU2))!=0)
      have_SU2=TRUE
    if(sum(dim(SI1))!=0)
      have_SI1=TRUE
    if(sum(dim(SI2))!=0)
      have_SI2=TRUE


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




    if(have_SI1==T){
    #Alteracao SI1

    SI1<<-rbind(SI1,NA)
    rownames(SI1)[k]<<-Id_i
    SI1<<-cbind(SI1,0)
    colnames(SI1)[k]<<-Id_i
    SI1[k,k]<<-1
    u=1
    for(i in uniao_indices_j){
      contador = 1 + (u/length(uniao_indices_j))*1
      setTxtProgressBar(pb,contador)
      u=u+1
      if(i<k){
        SI1[i,k]<<-sum(MU[,i]*MU[,k],na.rm = T)/(sqrt(sum(MU[,i]^2,na.rm = T))*sqrt(sum(MU[,k]^2,na.rm = T)))
      }
    }

    }

    if(have_SI2==T){
    #Alteracao SI2
    SI2<<-rbind(SI2,0)
    rownames(SI2)[k]<<-Id_i
    SI2<<-cbind(SI2,0)
    colnames(SI2)[k]<<-Id_i
    SI2[k,k]<<-1
    u=2
    for(i in uniao_indices_j){
      contador = 2 + (u/length(uniao_indices_j))*1
      setTxtProgressBar(pb,contador)
      u=u+1
      if(i<k){
        SI2[i,k]<<-sum((MU[,i]-averages_u)*(MU[,k]-averages_u),na.rm = T)/
          (sqrt(sum((MU[,i]-averages_u)^2,na.rm = T))*sqrt(sum((MU[,k]-averages_u)^2,na.rm = T)))
      }
    }
    }
    if(have_SU1==T){
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
    }





    if(have_SU2==T){
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
    }
    setTxtProgressBar(pb,5)
    close(pb)

  },
  deleteitem=function(Id_i){"Deletes an already registered item. The object CF matrices and vectors will be updated. Id_i : a character, a item ID that will be deleted.
    If you want to delete more items, you can use lists, where Id_i is a list of characters."

    if(is.list(Id_i)){
      u=length(Id_i)
      for(k in 1:u){
        if(k==u){
          Id_i = as.character(Id_i[[k]])
        }else{
          deleteitem(Id_i = Id_i[[k]])
        }
      }
    }




    if(!is.character(Id_i)){stop("Id_i must be a character.")}
    M = dim(MU)[1]


    N=dim(MU)[2]
    k=N + 1 #linha do novo usuario

    for(k in 1:N){
      if(colnames(MU)[k]==Id_i){
        j=k
        break
      }
      if(k==N){stop("This Item ID doesn't exist. If you want to add a new item, use $addnewitem().")}
      }

    have_SU1=F
    have_SU2=F
    have_SI1=F
    have_SI2=F
    if(sum(dim(SU1))!=0)
      have_SU1=TRUE
    if(sum(dim(SU2))!=0)
      have_SU2=TRUE
    if(sum(dim(SI1))!=0)
      have_SI1=TRUE
    if(sum(dim(SI2))!=0)
      have_SI2=TRUE



    #Vetores "avaliacoes" e "ind_aval" que representam respectivamente, as avaliações e os indices dos usuarios que avaliaram o item Id_i

    avaliacoes = NULL
    ind_aval = NULL
    t=1
    for(k in MU[,j]){
      if(!is.na(MU[t,j])){
        avaliacoes<-c(avaliacoes,k)
        ind_aval<-c(ind_aval,t)}
      t=t+1
    }


    ##Barra de Progresso
    message("Progress:")
    pb <- txtProgressBar(min = 1, max = 5, style = 3)

    contador = 1
    setTxtProgressBar(pb, contador)
    ####

    #Alteracao MU

    MU<<-MU[,-j]


    #Alteracao averages_i e n_aval_i

    averages_i<<-averages_i[-j]
    n_aval_i<<-n_aval_i[-j]
    #Alteracao averages_u e n_aval_u

    t=1
    for(k in ind_aval){
      averages_u[k] <<- averages_u[k]*n_aval_u[k]/(n_aval_u[k]-1) - avaliacoes[t]/(n_aval_u[k]-1)
      n_aval_u[k] <<- n_aval_u[k] - 1
      t=t+1
    }




    #Alteracao SI1
    if(have_SI1==T){
    SI1<<-SI1[-j,-j]
}

    #Alteracao SI2
    if(have_SI2==T){
    SI2<<-SI2[-j,-j]
}


    #Alteracao SU1
    if(have_SU1==T){

    u=3
    for(i in ind_aval){
      contador = 3 + (u/length(ind_aval))*1
      setTxtProgressBar(pb,contador)
      u=u+1
      for(k in ind_aval){
        if(k>i){
          SU1[i,k]<<-sum(MU[i,]*MU[k,],na.rm = T)/(sqrt(sum(MU[i,]^2,na.rm = T))*sqrt(sum(MU[k,]^2,na.rm = T)))
        }
      }

    }
}




    if(have_SU2==T){
    #Alteracao SU2
    u=4
    for(i in ind_aval){
      contador = 4 + (u/length(ind_aval))*1
      setTxtProgressBar(pb,contador)
      u=u+1
      for(k in ind_aval){
        if(k>i){
          SU2[i,k]<<-sum((MU[i,]-averages_u[i])*(MU[k,]-averages_u[k]),na.rm = T)/(sqrt(sum((MU[i,]-averages_u[i])^2,na.rm = T))*sqrt(sum((MU[k,]-averages_u[k])^2,na.rm = T)))
        }
      }

    }
    }
    setTxtProgressBar(pb,5)


    close(pb)},
  recommend=function(Id_u,Id_i,type,neighbors=5,cuts=3.5,similarity=ifelse(type=='user','pearson','adjcos')){
    "A function that returns True if user Id_u will like item Id_i or returns FALSE, otherwise. The recommendation can be made through similarity between users, when type = 'user', as well as through the similarity between items, when type = 'item'.
      Id_u : a  character, a User ID; Id_i : a character, an Item ID; type: a character string, 'user' or 'item'; neighbors: number of similarities used for to estimates (default = 5); cuts: cut score designated to determine if it is recommended (default=3.5);
      similarity: the methodology used to estimate the rating. If type = 'user', must be one of 'cos', for cosine similarity, or 'pearson' (default), for pearson similarity. If type='item', must be one of 'cos', for cosine similarity, or 'adjcos' (default), for adjusted cosine similarity. This choice can alter the way the
      estimate is calculated."

    if(!is.character(similarity)){stop("similarity must be a character.")}
    if(!is.character(Id_u)){stop("Id_u must be a character.")}
    if(!is.character(Id_i)){stop("Id_i must be a character.")}
    if(type!="user" && type!="item"){stop("type should be one of 'user' or 'item'.")}
    if(type=="user" && similarity == "adjcos"){stop("If type = 'user', similarity can be only 'cos' or 'pearson'.")}
    if(type=="item" && similarity == "pearson"){stop("If type = 'item', similarity can be only 'cos' or 'adjcos'.")}
    if(similarity !="cos" && similarity !="adjcos" && similarity !="pearson"){stop("similarity should be one of 'cos', 'adjcos' or 'pearson'.")}
    if(sum(dim(SU1))==0 && type=='user' && similarity == 'cos'){stop("if you want to make this recommendation, add the methodology of cosine similarity using the function add_cosine()")}
    if(sum(dim(SU2))==0 && type=='user' && similarity == 'pearson'){stop("if you want to make this recommendation, add the methodology of pearson similarity using the function add_pearson()")}
    if(sum(dim(SI1))==0 && type=='item' && similarity == 'cos'){stop("if you want to make this recommendation, add the methodology of cosine similarity using the function add_cosine()")}
    if(sum(dim(SI2))==0 && type=='item' && similarity == 'adjcos'){stop("if you want to make this recommendation, add the methodology of adjusted cosine similarity using the function add_adjcos()")}
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
      if(similarity=="pearson"){
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
          if(nota>=cuts){
            return(T)}
          return(F)

        }
      }

    }
  },
  kclosestitems=function(Id_i,k=5,similarity='adjcos'){
    "A function that returns the k items most similar to an item. Id_i : A Character, a Item ID; k : Number of items most similar to item Id_i (deafult = 5);
     similarity:  the methodology used to estimate the rating. Must be one of 'cos', for cosine similarity, or 'adjcos' (default), for adjusted cosine similarity. This choice can alter the way the
      estimate is calculated."
    if(!is.character(Id_i)){stop("Id_i must be a character.")}
    if(!is.character(similarity)){stop("similarity must be a character.")}


    if(similarity !="cos" && similarity !="adjcos"){stop("similarity should be one of 'cos' or 'adjcos'.")}
    if(sum(dim(SI1))==0  && similarity == 'cos'){stop("if you want to make this recommendation, add the methodology of cosine similarity using the function add_cosine()")}
    if(sum(dim(SI2))==0  && similarity == 'adjcos'){stop("if you want to make this recommendation, add the methodology of adjusted cosine similarity using the function add_adjcos()")}

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
  topkitems=function(Id_u,k=5,type,neighbors=5,cuts=3.5,similarity =ifelse(type=='user','pearson','adjcos')){
    "A function that recommends k items for an Id_u user. The recommendation can be made through similarity between users, when type = 'user', as well as through similarity between items,
      when type = 'item'. Id_u : a character, a User ID; k : number of recommendations (default=5); type: a character string, 'user' or 'item'; neighbors: number of similarities used for the estimates(default=5);
      cuts: cut score designated to determine if it is recommended (default = 3.5); similarity: the methodology used to estimate the rating. If type = 'user', must be one of 'cos', for cosine similarity,
      or 'pearson' (default), for pearson similarity. If type='item', must be one of 'cos', for cosine similarity, or 'adjcos' (default), for adjusted cosine similarity. This choice can alter the way the
      estimate is calculated."
    if(!is.character(similarity)){stop("similarity must be a character.")}
    if(!is.character(Id_u)){stop("Id_u must be a character.")}
    if(type!="user" && type!="item"){stop("type should be one of 'user' or 'item'.")}
    if(type=="user" && similarity == "adjcos"){stop("If type = 'user', similarity can be only 'cos' or 'pearson'.")}
    if(type=="item" && similarity == "pearson"){stop("If type = 'item', similarity can be only 'cos' or 'adjcos'.")}
    if(similarity !="cos" && similarity !="adjcos" && similarity !="pearson"){stop("similarity should be one of 'cos', 'adjcos' or 'pearson'.")}
    if(sum(dim(SU1))==0 && type=='user' && similarity == 'cos'){stop("if you want to make this recommendation, add the methodology of cosine similarity using the function add_cosine()")}
    if(sum(dim(SU2))==0 && type=='user' && similarity == 'pearson'){stop("if you want to make this recommendation, add the methodology of pearson similarity using the function add_pearson()")}
    if(sum(dim(SI1))==0 && type=='item' && similarity == 'cos'){stop("if you want to make this recommendation, add the methodology of cosine similarity using the function add_cosine()")}
    if(sum(dim(SI2))==0 && type=='item' && similarity == 'adjcos'){stop("if you want to make this recommendation, add the methodology of adjusted cosine similarity using the function add_adjcos()")}

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
      if(similarity=="pearson"){

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
  topkusers=function(Id_i,k=5,type,neighbors=5,cuts=3.5,similarity =ifelse(type=='user','pearson','adjcos')){
    "A function that indicates the k users who will like the item Id_i.The recommendation can be made through similarity between users, when type = 'user', as well as through similarity between
     items, when type = 'item'. Id_i : A Character, a Item ID; k : Number of recommendations (default=5); type: A character string, 'user' or 'item'; neighbors: Number of similarities used for
     the estimates (default=5); cuts: Cut score designated to determine if it is recommended (default=3.5);   similarity: the methodology used to estimate the rating. If type = 'user', must be one of 'cos', for cosine similarity, or 'pearson' (default), for pearson similarity. If type='item', must be one of 'cos', for cosine similarity, or 'adjcos' (default), for adjusted cosine similarity. This choice can alter the way the
      estimate is calculated."
    if(!is.character(similarity)){stop("similarity must be a character.")}
    if(!is.character(Id_i)){stop("Id_i must be a character.")}
    if(type!="user" && type!="item"){stop("type should be one of 'user' or 'item'.")}
    if(type=="user" && similarity == "adjcos"){stop("If type = 'user', similarity can be only 'cos' or 'pearson'.")}
    if(type=="item" && similarity == "pearson"){stop("If type = 'item', similarity can be only 'cos' or 'adjcos'.")}
    if(similarity !="cos" && similarity !="adjcos" && similarity !="pearson"){stop("similarity should be one of 'cos', 'adjcos' or 'pearson'.")}
    if(sum(dim(SU1))==0 && type=='user' && similarity == 'cos'){stop("if you want to make this recommendation, add the methodology of cosine similarity using the function add_cosine()")}
    if(sum(dim(SU2))==0 && type=='user' && similarity == 'pearson'){stop("if you want to make this recommendation, add the methodology of pearson similarity using the function add_pearson()")}
    if(sum(dim(SI1))==0 && type=='item' && similarity == 'cos'){stop("if you want to make this recommendation, add the methodology of cosine similarity using the function add_cosine()")}
    if(sum(dim(SI2))==0 && type=='item' && similarity == 'adjcos'){stop("if you want to make this recommendation, add the methodology of adjusted cosine similarity using the function add_adjcos()")}

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
      if(similarity=="pearson"){


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
  estimaterating=function(Id_u,Id_i,type,neighbors=5,similarity =ifelse(type=='user','pearson','adjcos')){
    "A function that returns the estimated rating for the evaluation of item Id_i by user Id_u. The recommendation can be made through similarity between users, when type = 'user', and also through the similarity between items, when type = 'item'.
     Id_u: A character, a user ID; Id_i: A character, an item ID; type: A character string, 'user' or 'item'; neighbors: Number of similarities used for the estimates.(default=5); similarity: the methodology used to estimate the rating. If type = 'user', must be one of 'cos', for cosine similarity, or 'pearson' (default), for pearson similarity. If type='item', must be one of 'cos', for cosine similarity, or 'adjcos' (default), for adjusted cosine similarity. This choice can alter the way the
      estimate is calculated."
    if(!is.character(similarity)){stop("similarity must be a character.")}
    if(!is.character(Id_u)){stop("Id_u must be a character.")}
    if(!is.character(Id_i)){stop("Id_i must be a character.")}
    if(type!="user" && type!="item"){stop("type should be one of 'user' or 'item'.")}
    if(type=="user" && similarity == "adjcos"){stop("If type = 'user', similarity can be only 'cos' or 'pearson'.")}
    if(type=="item" && similarity == "pearson"){stop("If type = 'item', similarity can be only 'cos' or 'adjcos'.")}
    if(similarity !="cos" && similarity !="adjcos" && similarity !="pearson"){stop("similarity should be one of 'cos', 'adjcos' or 'pearson'.")}
    if(sum(dim(SU1))==0 && type=='user' && similarity == 'cos'){stop("if you want to make this recommendation, add the methodology of cosine similarity using the function add_cosine()")}
    if(sum(dim(SU2))==0 && type=='user' && similarity == 'pearson'){stop("if you want to make this recommendation, add the methodology of pearson similarity using the function add_pearson()")}
    if(sum(dim(SI1))==0 && type=='item' && similarity == 'cos'){stop("if you want to make this recommendation, add the methodology of cosine similarity using the function add_cosine()")}
    if(sum(dim(SI2))==0 && type=='item' && similarity == 'adjcos'){stop("if you want to make this recommendation, add the methodology of adjusted cosine similarity using the function add_adjcos()")}

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
      if(similarity=="pearson"){
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
    "Deletes the rating from user Id_u to item Id_i. The object CF matrices and vectors will be updated. Id_u : A character, a user ID; Id_i : A character, an item ID.
    If you want to delete more ratings, you can use lists, where Id_u and Id_i are lists of characters."



    if(is.list(Id_u) && is.list(Id_i)){
      u=length(Id_u)
      for(k in 1:u){
        if(k==u){
          Id_u = as.character(Id_u[[k]])
          Id_i = as.character(Id_i[[k]])
        }else{
          deleterating(Id_u = Id_u[[k]], Id_i=Id_i[[k]])
        }
      }
    }

    if(!is.character(Id_u)){stop("Id_u must be a character.")}
    if(!is.character(Id_i)){stop("Id_i must be a character.")}

    have_SU1=F
    have_SU2=F
    have_SI1=F
    have_SI2=F
    if(sum(dim(SU1))!=0)
      have_SU1=TRUE
    if(sum(dim(SU2))!=0)
      have_SU2=TRUE
    if(sum(dim(SI1))!=0)
      have_SI1=TRUE
    if(sum(dim(SI2))!=0)
      have_SI2=TRUE

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

    if(have_SU1==T){

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
}

    #Alteracao SU2
   if(have_SU2==T){

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

}
    #Alteracao SIs
    ind_aval_u<-NULL #indice dos itens que foram avaliados por i
    for(k in 1:N){
      if(!is.na(MU[i,k]))
        ind_aval_u=c(ind_aval_u,k)
    }


    #Alteracao SI1
    if(have_SI1==T){
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
}

    #Alteração SI2
    if(have_SI2==T){

    for(k in ind_aval_u){
      nova_sim_j_k = sum((MU[,k]-averages_u)*(MU[,j]-averages_u),na.rm = T)/(sqrt(sum((MU[,k]-averages_u)^2,na.rm = T))*sqrt(sum((MU[,j]-averages_u)^2,na.rm = T)))
      if(j!=k){
        if(j < k){
          SI2[j,k] <<- nova_sim_j_k
        }else{
          SI2[k,j] <<- nova_sim_j_k
        }
      }
    }
}



  },
  changerating=function(Id_u,Id_i,r){
    "Changes the rating from user Id_u to item Id_i. The object CF matrices and vectors will be updated. Id_u : A character, a user ID; Id_i : A character, an item ID; r : The new rating.
    If you want to change more ratings, you can use lists where Id_u and Id_i are lists of characters and r is a list of ratings."

    if(is.list(Id_u) && is.list(Id_i) && is.list(r)){
      u=length(Id_u)
      for(k in 1:u){
        if(k==u){
          Id_u=as.character(Id_u[[k]])
          Id_i=as.character(Id_i[[k]])
             r=as.numeric(r[[k]])
        }else{
        changerating(Id_u = Id_u[[k]], Id_i = Id_i[[k]], r=r[[k]])
        }
      }
    }


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
