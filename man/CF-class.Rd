% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CF.R
\docType{class}
\name{CF-class}
\alias{CF-class}
\alias{CF}
\title{A Reference Class to represent a object CF}
\description{
A class of objects created structured with the following objects: the MU - Utility Matrix, the SU1 and SU2 - Matrices of Similarity
between Users, the SI1 e SI2 - Matrices of Similarity between Items, and the vectors averages_u, averages_i, n_aval_u and n_aval_i. The class
contains methods, general functions with the objectives of manipulating the data and making recommendations, from the structures
present in the class. The data manipulation methods comprise addsimilarity, addnewuser, addnewemptyuser, deleteuser, addnewitem, addnewemptyitem, deleteitem, newrating and deleterating,
while the recommendations methods recommend, kclosestitems, topkusers, topkitems are created through choices available in the
Collaborative Filtering methodology. All objects and methods are accessed through the "$" character. A CF class object is created through
the CFbuilder function.
}
\section{Fields}{

\describe{
\item{\code{MU}}{A utility matrix, matrix that contains all the users ratings. The rows comprise users and the columns, itens.}

\item{\code{SU1}}{A superior triangular user similarity matrix that contains the similarities between users, calculated using Cosine similarity}

\item{\code{SU2}}{A superior triangular user similarity matrix that contains the similarities between users, calculated  using Pearson Correlation.}

\item{\code{SI1}}{A superior triangular item similarity matrix that contains the similarities between items, calculated  using Cosine similarity.}

\item{\code{SI2}}{A superior triangular item similarity matrix that contains the similarities between items, calculated using Adjusted Cosine similarity.}

\item{\code{averages_u}}{A vector that contains the averages of users ratings.}

\item{\code{averages_i}}{A vector that contains the averages of item ratings.}

\item{\code{n_aval_u}}{A vector that contains the numbers of ratings performed by each user.}

\item{\code{n_aval_i}}{A vector that contains the numbers of ratings received for each item.}
}}

\section{Methods}{

\describe{
\item{\code{addnewemptyitem(Id_i)}}{Adds a new item without ratings. The object CF matrices and vectors will be updated. Id_i : a character, an item ID.
If you want to add more items, you can use lists where Id_i is a list of characters.}

\item{\code{addnewemptyuser(Id_u)}}{Adds a new user without ratings. The object CF matrices and vectors will be updated. Id_u : a character, a user ID.
If you want to add more users, you can use lists, where Id_u is a list of characters.}

\item{\code{addnewitem(Id_i, Ids_u, r)}}{Adds a new item that has been rated by one or more users. The object CF matrices and vectors will be updated. Id_i : a character, an item ID; Ids_u : a character vector, a user IDs; r : a vector with its respective ratings.
If you want to add more items, you can use lists, where Id_i is a list of characters; Ids_u is a list of vectors of characters; r is a list of vectors of ratings. }

\item{\code{addnewuser(Id_u, Ids_i, r)}}{Adds a new user who rated one or more items. The object CF matrices and vectors will be updated. Id_u : a character, a user ID; Ids_i : a character vector, item IDs; r : a vector with its respective ratings.
If you want to add more users, you can use lists, where Id_u: list of characters; Ids_i: list of vectors of characters; r: list of vectors of ratings. }

\item{\code{addsimilarity(sim_user = "none", sim_item = "none")}}{Adds new methodologies even after the construction and modification of the CF object used. The matrices of similarities representing each requested methodology will be added.
sim_user: a methodology used to estimate the rating by users similarity. Can be 'cos','pearson','both' or 'none'. If it equals 'cos' (Cosine Similarity), the SU1 will be built.
If it equals 'pearson' (Pearson Similarity), the SU2 will be built. If it equals 'both', the SU1 and SU2 will be built. If it equals 'none', nothing will be built. sim_item: A
methodology used to estimate the rating by itens similarity. Can be 'cos','adjcos','both' or 'none'. If it equals 'cos' (Cosine Similarity), the SI1 will be built. If it equals
'adjcos' (Adjusted Cosine Similarity), the SI2 will be built. If it equals 'both', the SI1 and SI2 will be built. If it equals 'none', nothing will be built.}

\item{\code{changerating(Id_u, Id_i, r)}}{Changes the rating from user Id_u to item Id_i. The object CF matrices and vectors will be updated. Id_u : A character, a user ID; Id_i : A character, an item ID; r : The new rating.
If you want to change more ratings, you can use lists where Id_u and Id_i are lists of characters and r is a list of ratings.}

\item{\code{deleteitem(Id_i)}}{Deletes an already registered item. The object CF matrices and vectors will be updated. Id_i : a character, a item ID that will be deleted.
If you want to delete more items, you can use lists, where Id_i is a list of characters.}

\item{\code{deleterating(Id_u, Id_i)}}{Deletes the rating from user Id_u to item Id_i. The object CF matrices and vectors will be updated. Id_u : A character, a user ID; Id_i : A character, an item ID.
If you want to delete more ratings, you can use lists, where Id_u and Id_i are lists of characters.}

\item{\code{deleteuser(Id_u)}}{Deletes an already registered user. The object CF matrices and vectors will be updated. Id_u : A character, a user ID that will be deleted.
If you want to delete more users, you can use lists where Id_u is a list of characters.}

\item{\code{estimaterating(
  Id_u,
  Id_i,
  type,
  neighbors = 5,
  similarity = ifelse(type == "user", "pearson", "adjcos")
)}}{A function that returns the estimated rating for the evaluation of item Id_i by user Id_u. The recommendation can be made through similarity between users, when type = 'user', and also through the similarity between items, when type = 'item'.
Id_u: A character, a user ID; Id_i: A character, an item ID; type: A character string, 'user' or 'item'; neighbors: Number of similarities used for the estimates.(default=5); similarity: the methodology used to estimate the rating. If type = 'user', must be one of 'cos', for cosine similarity, or 'pearson' (default), for pearson similarity. If type='item', must be one of 'cos', for cosine similarity, or 'adjcos' (default), for adjusted cosine similarity. This choice can alter the way the
 estimate is calculated.}

\item{\code{kclosestitems(Id_i, k = 5, similarity = "adjcos")}}{A function that returns the k items most similar to an item. Id_i : A Character, a Item ID; k : Number of items most similar to item Id_i (deafult = 5);
similarity:  the methodology used to estimate the rating. Must be one of 'cos', for cosine similarity, or 'adjcos' (default), for adjusted cosine similarity. This choice can alter the way the
 estimate is calculated.}

\item{\code{newrating(Id_u, Id_i, r)}}{Adds a new rating from user Id_u to item Id_i.The object CF matrices and vectors will be updated. Id_u : a character, a user ID; Id_i : a character, an item ID; r : the rating.
If you want to add more ratings, you can use lists, where Id_u and Id_i are lists of characters and r is a list of ratings.}

\item{\code{recommend(
  Id_u,
  Id_i,
  type,
  neighbors = 5,
  cuts = 3.5,
  similarity = ifelse(type == "user", "pearson", "adjcos")
)}}{A function that returns True if user Id_u will like item Id_i or returns FALSE, otherwise. The recommendation can be made through similarity between users, when type = 'user', as well as through the similarity between items, when type = 'item'.
Id_u : a  character, a User ID; Id_i : a character, an Item ID; type: a character string, 'user' or 'item'; neighbors: number of similarities used for to estimates (default = 5); cuts: cut score designated to determine if it is recommended (default=3.5);
similarity: the methodology used to estimate the rating. If type = 'user', must be one of 'cos', for cosine similarity, or 'pearson' (default), for pearson similarity. If type='item', must be one of 'cos', for cosine similarity, or 'adjcos' (default), for adjusted cosine similarity. This choice can alter the way the
estimate is calculated.}

\item{\code{topkitems(
  Id_u,
  k = 5,
  type,
  neighbors = 5,
  cuts = 3.5,
  similarity = ifelse(type == "user", "pearson", "adjcos")
)}}{A function that recommends k items for an Id_u user. The recommendation can be made through similarity between users, when type = 'user', as well as through similarity between items,
when type = 'item'. Id_u : a character, a User ID; k : number of recommendations (default=5); type: a character string, 'user' or 'item'; neighbors: number of similarities used for the estimates(default=5);
cuts: cut score designated to determine if it is recommended (default = 3.5); similarity: the methodology used to estimate the rating. If type = 'user', must be one of 'cos', for cosine similarity,
or 'pearson' (default), for pearson similarity. If type='item', must be one of 'cos', for cosine similarity, or 'adjcos' (default), for adjusted cosine similarity. This choice can alter the way the
estimate is calculated.}

\item{\code{topkusers(
  Id_i,
  k = 5,
  type,
  neighbors = 5,
  cuts = 3.5,
  similarity = ifelse(type == "user", "pearson", "adjcos")
)}}{A function that indicates the k users who will like the item Id_i.The recommendation can be made through similarity between users, when type = 'user', as well as through similarity between
items, when type = 'item'. Id_i : A Character, a Item ID; k : Number of recommendations (default=5); type: A character string, 'user' or 'item'; neighbors: Number of similarities used for
the estimates (default=5); cuts: Cut score designated to determine if it is recommended (default=3.5);   similarity: the methodology used to estimate the rating. If type = 'user', must be one of 'cos', for cosine similarity, or 'pearson' (default), for pearson similarity. If type='item', must be one of 'cos', for cosine similarity, or 'adjcos' (default), for adjusted cosine similarity. This choice can alter the way the
 estimate is calculated.}
}}

\examples{
ratings<-movies[1:1000,]
objectCF<-CFbuilder(Data = ratings, sim_user="pearson", sim_item="adjcos")
objectCF$addsimilarity(sim_user="cos",sim_item="cos")
objectCF$MU
objectCF$SU1
objectCF$SU2
objectCF$SI1
objectCF$SI2
objectCF$averages_u
objectCF$averages_i
objectCF$n_aval_u
objectCF$n_aval_i
objectCF$addnewuser(Id_u = "Thiago",Ids_i = "The Hunger Games: Catching Fire",r = 5)
objectCF$addnewemptyuser(Id_u = "Jessica")
objectCF$deleteuser(Id_u = "Jessica")
objectCF$addnewitem(Id_i = "Avengers: Endgame",Ids_u = c("1","2"),r = c(5,3))
objectCF$addnewemptyitem(Id_i = "Star Wars")
objectCF$deleteitem(Id_i="Star Wars")
objectCF$newrating(Id_u = "1", Id_i = "Till Luck Do Us Part 2",r = 2)
objectCF$recommend(Id_u = "2", Id_i = "Iron Man 3", type = "user")
objectCF$kclosestitems(Id_i = "Iron Man 3", k = 3)
objectCF$topkitems(Id_u = "3",k = 3, type = "user")
objectCF$topkusers(Id_i = "Thor: The Dark World", k = 3,type = "item")
objectCF$estimaterating(Id_u = "2",Id_i = "Iron Man 3", type = "user")
objectCF$deleterating("1","Brazilian Western")
objectCF$changerating("1","Wreck-It Ralph",2)
}
\references{
\itemize{
\item LINDEN, G.; SMITH, B.; YORK, J. Amazon. com recommendations: Item-toitem collaborative filtering. Internet Computing, IEEE, v. 7, n. 1, p. 76-80,2003
\item Aggarwal, C. C. (2016). Recommender systems (Vol. 1). Cham: Springer International Publishing.
\item Leskovec, J., Rajaraman, A., & Ullman, J. D. (2020). Mining of massive data sets. Cambridge university press.
}
}
\seealso{
\code{\link[CFilt]{CFbuilder}}
}
\author{
Thiago Lima, Jessica Kubrusly.
}
