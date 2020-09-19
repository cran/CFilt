#'@name CFbuilder
#'@aliases CFbuilder
#'@title A function to create and build a CF class object
#'@description
#'A CF class object constructor. This function can perform two procedures: If Data is a data frame, style: User Id - Item Id - Ratings, it creates
#'and builds an FC class object from the data frame, containing a Utility Matrix, a User Similarity Matrix, an Item Similarity Matrix, a vector with
#'the number of user ratings, a vector with the number of ratings received for the items, a vector with the average ratings of each user and another
#'vector with the average ratings received from each item. If Data is the Utility Matrix, it also constructs all matrices and vectors. When building
#'the object, the progress percentage is shown. Step 1: Building the MU and vectors. Step 2: Building the SU. Step 3: Building the SI.
#'@usage CFbuilder(Data, sim_user, sim_item)
#'@param Data A data frame by style: User ID - Item ID - Ratings, or a Utility Matrix.
#'@param sim_user A methodology used to estimate the rating by users similarity. Can be 'cos','pearson','both' or 'none'. If it equals 'cos', the SU1 will be built. If it equals 'pearson', the SU2 will be built. If it equals 'both', the SU1 and SU2 will be built. If it equals 'none', nothing will be built.
#'@param sim_item A methodology used to estimate the rating by itens similarity. Can be 'cos','adjcos','both' or 'none'. If it equals 'cos', the SI1 will be built. If it equals 'adjcos', the SI2 will be built. If it equals 'both', the SI1 and SI2 will be built. If it equals 'none', nothing will be built.
#'@author Thiago Lima, Jessica Kubrusly.
#'@return a CF class object.
#'@references
#'LINDEN, G.; SMITH, B.; YORK, J. Amazon. com recommendations: Item-to-item collaborative filtering. Internet Computing, IEEE, v. 7, n. 1, p. 76-80,2003
#'@seealso \code{\link[CFilt]{CF-class}}
#'@keywords Refference Class Collaborative Filtering
#'
#'@export
#'@examples
#'ratings<-movies[1:1000,]
#'objectCF<-CFbuilder(Data = ratings, sim_user = "pearson", sim_item = "adjcos")
CFbuilder <- function(Data, sim_user = "pearson", sim_item = "adjcos"){


  cal_SU1=F
  cal_SU2=F
  cal_SI1=F
  cal_SI2=F
  if(sim_user == "both"){
    cal_SU1=TRUE
    cal_SU2=TRUE
  }
  if(sim_user=="cos"){cal_SU1=TRUE}
  if(sim_user=="pearson"){cal_SU2=TRUE}
  if(sim_user=="none"){
    cal_SU1=F
    cal_SU2=F
  }
  if(sim_user!="cos" && sim_user!="pearson" && sim_user!="both" && sim_user!="none" ){
    stop("sim_user can be only 'cos', 'pearson','both' or 'none'.")
  }



  if(sim_item == "both"){
    cal_SI1=TRUE
    cal_SI2=TRUE
  }
  if(sim_item=="cos"){cal_SI1=TRUE}
  if(sim_item=="adjcos"){cal_SI2=TRUE}
  if(sim_item=="none"){
    cal_SI1=F
    cal_SI1=F
  }
  if(sim_item!="cos" && sim_item!="adjcos" && sim_item!="both" && sim_item!="none" ){
    stop("sim_user can be only 'cos', 'adjcos','both' or 'none'.")
  }





  if(is.data.frame(Data)){
    obj_FC<-CF()

    Data<-as.data.frame(Data)
    #padrao da Data
    #objeto do type matrix ou dataframe
    #1a coluna tem o nome (ou identificador) dos usuarios
    #2a coluna tem o nome (ou identificador) dos itens
    #3a coluna tem um numero (1-5 ou 1-10) com as avalicoes ou 1 ou 0, indicanco se o usuario viu ou nao.

    #para saber os nomes e o num de usuarios distintos
    tab_u = table(as.numeric(as.character(Data[,1])))
    nome_u = names(tab_u)
    m = dim(tab_u)

    #para saber os nomes e o num de itens distintos
    tab_i = table(as.character(Data[,2]))
    nome_i = names(tab_i)
    n = dim(tab_i)

    #criar a matriz
    MU = matrix(NA,nrow=m,ncol=n,dimnames = list(nome_u,nome_i))

    #Construindo a MU, averages_u, averages_i, n_aval_i e n_aval_u
    averages_u=rep(0,m)
    averages_i=rep(0,n)
    n_aval_u=rep(0,m)
    n_aval_i=rep(0,n)
    names(averages_u)<-nome_u
    names(averages_i)<-nome_i
    names(n_aval_u)<-nome_u
    names(n_aval_i)<-nome_i


    n_linhas = dim(Data)[1]
    message("Step 1 of 3: Building MU")
    pb <- txtProgressBar(min = 0, max = n_linhas, style = 3)
    for(i in 1:n_linhas){

      setTxtProgressBar(pb, i)



      usua =  as.character(Data[i,1])
      item =  as.character(Data[i,2])
      nota =  as.numeric(Data[i,3])

      MU[usua,item]=nota

      averages_u[usua] <- averages_u[usua]*n_aval_u[usua]/(n_aval_u[usua]+1) + nota/(n_aval_u[usua]+1)
      averages_i[item] <- averages_i[item]*n_aval_i[item]/(n_aval_i[item]+1) + nota/(n_aval_i[item]+1)
      n_aval_u[usua]<-n_aval_u[usua]+1
      n_aval_i[item]<-n_aval_i[item]+1
    }
    close(pb)

    obj_FC$MU=MU
    obj_FC$averages_u=averages_u
    obj_FC$averages_i=averages_i
    obj_FC$n_aval_u=n_aval_u
    obj_FC$n_aval_i=n_aval_i

    message("Step 2 of 3: Building SU")
    pb <- txtProgressBar(min = 0, max = 2*m, style = 3)

    if(cal_SU1 == TRUE){
    #Criar SU1 - Coeficiente Cosseno:
    SU1=matrix(NA,m,m,dimnames = list(nome_u,nome_u))
    for(i in 1:m){
      setTxtProgressBar(pb, i)
      for(j in i:m){
        if(j!=i){
          SU1[i,j]<-sum(MU[i,]*MU[j,],na.rm = T)/(sqrt(sum((MU[i,])^2,na.rm = T))*sqrt(sum((MU[j,])^2,na.rm = T)))

        }
        if(j==i){SU1[i,j]<-1}
      }
    }

    obj_FC$SU1=SU1
  }

    #Criar SU2 - Coeficiente de Person:
if(cal_SU2==TRUE){
    SU2=matrix(NA,m,m,dimnames = list(nome_u,nome_u))

    for(i in 1:m){
      setTxtProgressBar(pb,(m+i))

      for(j in i:m){



        if(j!= i){
          SU2[i,j]<-sum((MU[i,]-averages_u[i])*(MU[j,]-averages_u[j]),na.rm = T)/(sqrt(sum((MU[i,]-averages_u[i])^2,na.rm = T))*sqrt(sum((MU[j,]-averages_u[j])^2,na.rm = T)))
        }
        if(j==i){SU2[i,j]<-1}
      }
    }


    obj_FC$SU2=SU2
}
    setTxtProgressBar(pb,2*m)
    close(pb)


    message("Step 3 of 3: Building SI")

    pb <- txtProgressBar(min = 0, max = 2*n, style = 3)


    if(cal_SI2==TRUE){
    #Criar SI2 - Cosseno Ajustado
    SI2=matrix(NA,n,n,dimnames = list(nome_i,nome_i))
    for(i in 1:n){
      setTxtProgressBar(pb, i)

      for(j in i:n){
        if(j!= i){
          SI2[i,j]=sum((MU[,i]-averages_u)*(MU[,j]-averages_u),na.rm = T)/(sqrt(sum((MU[,i]-averages_u)^2,na.rm = T))*sqrt(sum((MU[,j]-averages_u)^2,na.rm = T)))
        }
        if(j==i){SI2[i,j]<-1}
      }
    }

    obj_FC$SI2=SI2
    }


    if(cal_SI1==TRUE){

    #Criar SI1 - Distancia Cosseno normal
    SI1 = matrix(NA,n,n,dimnames=list(nome_i,nome_i))
    for(i in 1:n){
      setTxtProgressBar(pb, (n+i))
      for(j in i:n){
        if(j!=i){
          SI1[i,j] = sum(MU[,i]*MU[,j],na.rm=T)/(sqrt(sum((MU[,i])^2,na.rm=T))*sqrt(sum((MU[,j])^2,na.rm=T)))
        }
        if(j==i){SI1[i,j]<-1}
      }
    }

    obj_FC$SI1=SI1
    }
    setTxtProgressBar(pb,2*n)
    close(pb)




    return(obj_FC)







  }
  if(is.matrix(Data)){
    MU=Data
    obj_FC = CF(MU=MU)
    M=dim(MU)[1]
    N=dim(MU)[2]
    #Cria vetores de medias, para linhas e colunas:
    averages_u=apply(MU,1,mean,na.rm=T)
    averages_i=apply(MU,2,mean,na.rm=T)
    obj_FC$averages_u=averages_u
    obj_FC$averages_i=averages_i
    #Criar vetores de Numeros de avaliacoes de usuarios e quantas vzs um item nao avaliado
    aplica=!apply(MU,1:2,is.na)
    n_aval_u=apply(aplica,1,sum)
    n_aval_i=apply(aplica,2,sum)
    obj_FC$n_aval_u=n_aval_u
    obj_FC$n_aval_i=n_aval_i




    message("Step 1 of 2: Building SU")
    pb <- txtProgressBar(min = 0, max = 2*M, style = 3)

if(cal_SU1==TRUE){
    #Criar SU1 - Similaridade Cosseno :
    SU1=matrix(NA,M,M,dimnames = list(rownames(Data),rownames(Data)))
    for(i in 1:M){
      setTxtProgressBar(pb, i)
      for(j in i:M){
        if(j!= i){
          SU1[i,j]<-sum(MU[i,]*MU[j,],na.rm = T)/(sqrt(sum((MU[i,])^2,na.rm = T))*sqrt(sum((MU[j,])^2,na.rm = T)))
        }
        if(j==i){SU1[i,j]<-1}


      }
    }
    obj_FC$SU1=SU1
}

    if(cal_SU2==TRUE){
    #Criar SU2 - Adjusted Cosseno :
    SU2=matrix(NA,M,M,dimnames = list(rownames(Data),rownames(Data)))
    for(i in 1:M){
      setTxtProgressBar(pb, (M+i))
      for(j in i:M){
        if(j!= i){
          SU2[i,j]<-sum((MU[i,]-averages_u[i])*(MU[j,]-averages_u[j]),na.rm = T)/(sqrt(sum((MU[i,]-averages_u[i])^2,na.rm = T))*sqrt(sum((MU[j,]-averages_u[j])^2,na.rm = T)))
        }
        if(j==i){SU2[i,j]<-1}
      }
    }
    obj_FC$SU2=SU2
    }

    setTxtProgressBar(pb, 2*M)
    close(pb)



    message("Step 2 of 2: Building SI")
    n=dim(MU)[2]
    pb <- txtProgressBar(min = 0, max = 2*n, style = 3)

    if(cal_SI2==TRUE){

    #Criar SI2 - Cosseno Ajustado

    SI2=matrix(NA,n,n,dimnames = list(colnames(Data),colnames(Data)))
    for(i in 1:n){
      setTxtProgressBar(pb, i)
      for(j in i:n){
        if(j!= i){
          SI2[i,j]=sum((MU[,i]-averages_u)*(MU[,j]-averages_u),na.rm = T)/(sqrt(sum((MU[,i]-averages_u)^2,na.rm = T))*sqrt(sum((MU[,j]-averages_u)^2,na.rm = T)))
        }
        if(j==i){SI2[i,j]<-1}
      }
    }

    obj_FC$SI2=SI2
    }



    #Criar SI1 - Cosseno


    if(cal_SI1==TRUE){

    SI1=matrix(NA,n,n,dimnames = list(colnames(Data),colnames(Data)))
    for(i in 1:n){
      setTxtProgressBar(pb, (n+i))
      for(j in i:n){
        if(j!=i){
          SI1[i,j] = sum(MU[,i]*MU[,j],na.rm=T)/(sqrt(sum((MU[,i])^2,na.rm=T))*sqrt(sum((MU[,j])^2,na.rm=T)))
        }
        if(j==i){SI1[i,j]<-1}
      }
    }

    obj_FC$SI1=SI1
}
    setTxtProgressBar(pb, 2*n)
    close(pb)


    return(obj_FC)



  }
  if(!is.data.frame(Data) && !is.matrix(Data)){stop("The data would be a data frame or a matrix.")}
}


