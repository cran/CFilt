#'@name CFbuilder
#'@aliases CFbuilder
#'@title A function to create and build a CF class object
#'@description
#'A CF class object constructor. This function can perform two procedures: If Data is a data frame, style: User Id - Item Id - Ratings, it creates
#'and builds an FC class object from the data frame, containing a Utility Matrix, a User Similarity Matrix, an Item Similarity Matrix, a vector with
#'the number of user ratings, a vector with the number of ratings received for the items, a vector with the average ratings of each user and another
#'vector with the average ratings received from each item. If Data is the Utility Matrix, it also constructs all matrices and vectors. When building
#'the object, the progress percentage is shown. Step 1: Building the MU and vectors. Step 2: Building the SU. Step 3: Building the SI.
#'@usage CFbuilder(Data)
#'@param Data A data frame by style: User ID - Item ID - Ratings, or a Utility Matrix.
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
#'objectCF<-CFbuilder(Data = ratings)
CFbuilder <- function(Data){
  if(is.data.frame(Data)){


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


    #Criar SU1 - Coeficiente Cosseno:
    message("Step 2 of 3: Building SU")
    pb <- txtProgressBar(min = 0, max = 2*m, style = 3)

    SU1=matrix(0,m,m,dimnames = list(nome_u,nome_u))
    for(i in 1:m){
      setTxtProgressBar(pb, i)
      for(j in i:m){
        if(j!=i){
          SU1[i,j]<-sum(MU[i,]*MU[j,],na.rm = T)/(sqrt(sum((MU[i,])^2,na.rm = T))*sqrt(sum((MU[j,])^2,na.rm = T)))

        }
      }
    }






    #Criar SU2 - Coeficiente de Person:

    SU2=matrix(0,m,m,dimnames = list(nome_u,nome_u))

    for(i in 1:m){
      setTxtProgressBar(pb,(m+i))

      for(j in i:m){



        if(j!= i){
          SU2[i,j]<-sum((MU[i,]-averages_u[i])*(MU[j,]-averages_u[j]),na.rm = T)/(sqrt(sum((MU[i,]-averages_u[i])^2,na.rm = T))*sqrt(sum((MU[j,]-averages_u[j])^2,na.rm = T)))
        }
      }
    }
    close(pb)



    #Criar SI2 - Cosseno Ajustado
    message("Step 3 of 3: Building SI")
    SI2=matrix(0,n,n,dimnames = list(nome_i,nome_i))
    pb <- txtProgressBar(min = 0, max = 2*n, style = 3)
    for(i in 1:n){
      setTxtProgressBar(pb, i)

      for(j in i:n){
        if(j!= i)
          SI2[i,j]=sum((MU[,i]-averages_u[i])*(MU[,j]-averages_u[j]),na.rm = T)/(sqrt(sum((MU[,i]-averages_u[i])^2,na.rm = T))*sqrt(sum((MU[,j]-averages_u[j])^2,na.rm = T)))
      }
    }


    #Criar SI1 - Distancia Cosseno normal
    SI1 = matrix(0,n,n,dimnames=list(nome_i,nome_i))
    for(i in 1:n){
      setTxtProgressBar(pb, (n+i))
      for(j in i:n){
        if(j!=i){
          SI1[i,j] = sum(MU[,i]*MU[,j],na.rm=T)/(sqrt(sum((MU[,i])^2,na.rm=T))*sqrt(sum((MU[,j])^2,na.rm=T)))
        }
      }
    }
    close(pb)




    obj_FC=CF(MU=MU,averages_u=averages_u,averages_i=averages_i,n_aval_i=n_aval_i,n_aval_u=n_aval_u,SI1=SI1,SI2=SI2,SU1=SU1,SU2=SU2)
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



    #Criar SU1 - Similaridade Cosseno :
    message("Step 1 of 2: Building SU")
    SU1=matrix(0,M,M,dimnames = list(rownames(Data),rownames(Data)))
    pb <- txtProgressBar(min = 0, max = 2*M, style = 3)
    for(i in 1:M){
      setTxtProgressBar(pb, i)
      for(j in i:M){
        if(j!= i){
          SU1[i,j]<-sum(MU[i,]*MU[j,],na.rm = T)/(sqrt(sum((MU[i,])^2,na.rm = T))*sqrt(sum((MU[j,])^2,na.rm = T)))
        }


      }
    }
    #Criar SU2 - Adjusted Cosseno :
    SU2=matrix(0,M,M,dimnames = list(rownames(Data),rownames(Data)))
    for(i in 1:M){
      setTxtProgressBar(pb, (M+i))
      for(j in i:M){
        if(j!= i){
          SU2[i,j]<-sum((MU[i,]-averages_u[i])*(MU[j,]-averages_u[j]),na.rm = T)/(sqrt(sum((MU[i,]-averages_u[i])^2,na.rm = T))*sqrt(sum((MU[j,]-averages_u[j])^2,na.rm = T)))
        }
      }
    }

    close(pb)
    obj_FC$SU1=SU1
    obj_FC$SU2=SU2


    #Criar SI2 - Cosseno Ajustado
    message("Step 2 of 2: Building SI")
    n=dim(MU)[2]
    pb <- txtProgressBar(min = 0, max = 2*n, style = 3)
    SI2=matrix(0,n,n,dimnames = list(colnames(Data),colnames(Data)))
    for(i in 1:n){
      setTxtProgressBar(pb, i)
      for(j in i:n){
        if(j!= i){
          SI2[i,j]=sum((MU[,i]-averages_u[i])*(MU[,j]-averages_u[j]),na.rm = T)/(sqrt(sum((MU[,i]-averages_u[i])^2,na.rm = T))*sqrt(sum((MU[,j]-averages_u[j])^2,na.rm = T)))
        }
      }
    }

    #Criar SI1 - Cosseno

    SI1=matrix(0,n,n,dimnames = list(colnames(Data),colnames(Data)))
    for(i in 1:n){
      setTxtProgressBar(pb, (n+i))
      for(j in i:n){
        if(j!=i){
          SI1[i,j] = sum(MU[,i]*MU[,j],na.rm=T)/(sqrt(sum((MU[,i])^2,na.rm=T))*sqrt(sum((MU[,j])^2,na.rm=T)))
        }
      }
    }




    close(pb)

    obj_FC$SI1=SI1
    obj_FC$SI2=SI2

    return(obj_FC)



  }
  if(!is.data.frame(Data) && !is.matrix(Data)){stop("The data would be a data frame or a matrix.")}
}


