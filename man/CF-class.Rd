% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CF.R
\docType{class}
\name{CF-class}
\alias{CF-class}
\alias{CF}
\title{Recommendation by Collaborative Filtering}
\description{
CF is a class of objects that stores
information about a recommendation system. This information includes the
consumption or rating of each (user, item) pair in the utility matrix MU, the
similarities between each pair of users in the similarity matrix SU, the
similarities between each pair of items in the similarity matrix SI, the
number of items consumed and/or rated by each user in the vector n_aval_u,
the number of users who consumed and/or rated each item in the vector
n_aval_i, the average rating value of each user in the vector averages_u, the
average rating value received by each item in the vector averages_i, the
number of items consumed in common by each pair of users in the matrix Int_U,
and the number of users in common for each pair of items in the matrix Int_I.
The class contains methods such as addNewUser, addNewEmptyUser, deleteUser,
addNewItem, addNewEmptyItem, deleteItem, newRating and deleteRating, which
modify the object's structure by altering users, items, or consumption data.
The class also includes functions such as kClosestItems, topKUsers, and
topKItems, which return items to recommend to a user or users to whom an item
should be recommended. An object of the CF class is created using the
CFBuilder function.
}
\section{Fields}{

\describe{
\item{\code{MU}}{The Utility Matrix, a matrix that contains all the users' ratings.
The rows comprise users and the columns, items.}

\item{\code{SU}}{The user similarity matrix.}

\item{\code{SI}}{The item similarity matrix}

\item{\code{IntU}}{A symmetric matrix that records the number of users in common who
consumed each pair of items.}

\item{\code{IntI}}{A symmetric matrix that records the number of items in common that
have been consumed by each pair of users.}

\item{\code{averages_u}}{A vector that contains the averages of users' ratings.}

\item{\code{averages_i}}{A vector that contains the averages of items' ratings.}

\item{\code{n_aval_u}}{A vector that stores the number of items rated by each user.}

\item{\code{n_aval_i}}{A vector that stores the number of users who consumed each
item.}

\item{\code{datatype}}{A character that indicates the type of data, which can be
either "consumption" or "rating".}
}}

\section{Methods}{

\describe{
\item{\code{addnewemptyitem(Id_i)}}{A method that adds a new item that has not yet been consumed by any 
existing user in the recommendation system.
Id_i: a character, the new item ID;
To add more than one new user, lists can be used. 
Id_i: a list of characters;}

\item{\code{addnewemptyuser(Id_u)}}{A method that adds a new user who has not yet consumed any existing items 
in the recommendation system. 
Id_u: a character, the new user ID;
To add more than one new user, lists can be used. 
Id_u: a list of characters;}

\item{\code{addnewitem(Id_i, Ids_u, r = NULL)}}{A method that adds a new item that has been consumed by already existing 
users in the recommendation system.
Id_i: a character, the new item ID;
Ids_u: a character vector, the IDs of the users who consumed the new item;
r: a numeric vector, the ratings given by the users for the new item 
(only for ratings datatype).
To add more than one new item, lists can be used. 
Id_i: a list of characters; 
Ids_u: a list of characters vectors; 
r: list of numeric vectors.}

\item{\code{addnewuser(Id_u, Ids_i, r = NULL)}}{A method that adds a new user who consumed items already existing in the 
recommendation system. 
Id_u: a character, the new user ID;
Ids_i: a character vector, the IDs of the items consumed by the user;
r: a numeric vector, the ratings of the items consumed by the new user 
(only for ratings datatype).
To add more than one new user, lists can be used. 
Id_u: a list of characters; 
Ids_i: a list of characters vectors; 
r: list of numeric vectors.}

\item{\code{changerating(Id_u, Id_i, r = NULL)}}{A method that changes a rating or consumption of a user for an item 
that has already been rated by them.
Id_u: a character, the user ID;
Id_i: a character, the item ID;
r: a numeric, the rating given by Id_u for Id_i (only for ratings 
datatype).
To change more than one ratings, lists can be used. 
Id_u: a list of characters; 
Id_i: a list of characters; 
r: list of numeric vectors.}

\item{\code{deleteitem(Id_i)}}{A method that deletes an item from the recommendation system. 
Id_i: a character, the item ID;
To delete more than one item, lists can be used. 
Id_i: a list of characters;}

\item{\code{deleterating(Id_u, Id_i)}}{A method that deletes a existing rating or consumption of a user for an 
item.
Id_u: a character, the user ID;
Id_i: a character, the item ID;
To deletes more than one ratings, lists can be used. 
Id_u: a list of characters; 
Id_i: a list of characters.}

\item{\code{deleteuser(Id_u)}}{A method that deletes an user from the recommendation system. 
Id_u: a character, the user ID;
To delete more than one user, lists can be used. 
Id_u: a list of characters;}

\item{\code{newrating(Id_u, Id_i, r = NULL)}}{A method that adds a new rating or consumption of an existing user for
an existing item that had not yet been rated by them.
Id_u: a character, the user ID;
Id_i: a character, the item ID;
r: a numeric, the rating given by Id_u for Id_i (only for ratings 
datatype).
To add more than one new ratings, lists can be used. 
Id_u: a list of characters; 
Id_i: a list of characters; 
r: list of numeric vectors.}
}}

\examples{
objectCF_r <- CFbuilder(Data = movies[1:500,], Datatype = "ratings", 
similarity = "pearson")
dim(objectCF_r$MU)
colnames(objectCF_r$MU) #movies Id
rownames(objectCF_r$MU) #users Id
dim(objectCF_r$SU)
dim(objectCF_r$SI)
objectCF_r$averages_u
hist(objectCF_r$averages_u)
objectCF_r$averages_i
hist(objectCF_r$averages_i)
objectCF_r$n_aval_u
summary(objectCF_r$n_aval_u)
barplot(table(objectCF_r$n_aval_u))
objectCF_r$n_aval_i
summary(objectCF_r$n_aval_i)
barplot(table(objectCF_r$n_aval_i))
objectCF_r$addnewuser(Id_u = "newuser1", 
Ids_i = "The Hunger Games: Catching Fire", r = 5)
rownames(objectCF_r$MU) #users Id
objectCF_r$n_aval_u["newuser1"]
objectCF_r$averages_u["newuser1"]
objectCF_r$addnewuser(Id_u = "newuser2",
Ids_i = c("Frozen","Her","Iron Man 3"),r = c(2,4,3))
rownames(objectCF_r$MU) #users Id
objectCF_r$n_aval_u["newuser2"]
objectCF_r$averages_u["newuser2"]
objectCF_r$addnewuser(Id_u = list("newuser3","newuser4"),
Ids_i = list(c("Lincoln","Monsters University","The Lego Movie","Frozen"),
c("The Wolverine","The Lego Movie")),r = list(c(1,4,5,4),c(4,5)))
rownames(objectCF_r$MU) #users Id
objectCF_r$n_aval_u[c("newuser3","newuser4")]
objectCF_r$averages_u[c("newuser3","newuser4")]
objectCF_r$newrating(Id_u = list("newuser1","newuser1","newuser2","newuser4"),
Id_i = list("The Lego Movie","Wreck-It Ralph","Fast & Furious 6",
"12 Years a Slave"),r = list(4,5,4,2))
objectCF_r$n_aval_u[c("newuser1","newuser2","newuser3","newuser4")]
objectCF_r$averages_u[c("newuser1","newuser2","newuser3","newuser4")]
objectCF_r$addnewitem(Id_i = "Oppenheimer",
Ids_u = c("newuser1","newuser2","newuser3","newuser4","1","2","4","6","10",
"11","20","32"),r = c(1,2,3,1,5,4,5,4,1,3,5,4))
colnames(objectCF_r$MU)
objectCF_r$n_aval_i["Oppenheimer"]
objectCF_r$averages_i["Oppenheimer"]
objectCF_c <- CFbuilder(Data = movies[1:500,-3], Datatype = "consumption", 
similarity = "jaccard")
dim(objectCF_c$MU)
colnames(objectCF_c$MU) #movies Id
rownames(objectCF_c$MU) #users Id
dim(objectCF_c$SU)
dim(objectCF_c$SI)
objectCF_c$averages_u
objectCF_c$averages_i
objectCF_c$n_aval_u
summary(objectCF_c$n_aval_u)
barplot(table(objectCF_c$n_aval_u))
objectCF_c$n_aval_i
summary(objectCF_c$n_aval_i)
barplot(table(objectCF_c$n_aval_i))
objectCF_c$addnewuser(Id_u = "newuser1", 
Ids_i = "The Hunger Games: Catching Fire")
rownames(objectCF_c$MU) #users Id
objectCF_c$n_aval_u["newuser1"]
objectCF_c$addnewuser(Id_u = "newuser2",
Ids_i = c("Frozen","Her","Iron Man 3"))
rownames(objectCF_c$MU) #users Id
objectCF_c$n_aval_u["newuser2"]
objectCF_c$addnewuser(Id_u = list("newuser3","newuser4"),Ids_i = list(
c("Lincoln","Monsters University","The Lego Movie","Frozen"),
c("The Wolverine","The Lego Movie")))
rownames(objectCF_c$MU)
objectCF_c$n_aval_u[c("newuser3","newuser4")]
objectCF_c$MU["newuser1","The Lego Movie"]
objectCF_c$newrating(Id_u = list("newuser1","newuser1","newuser2","newuser4"),
Id_i = list("The Lego Movie","Wreck-It Ralph","Fast & Furious 6",
"12 Years a Slave"))
objectCF_c$n_aval_u[c("newuser1","newuser2","newuser3","newuser4")]
objectCF_c$averages_u[c("newuser1","newuser2","newuser3","newuser4")]
objectCF_c$addnewitem(Id_i = "Oppenheimer",
Ids_u = c("newuser1","newuser2","newuser3","newuser4","1","2","4","6","10",
"11","20","32"),r = c(1,2,3,1,5,4,5,4,1,3,5,4))
colnames(objectCF_c$MU)
objectCF_c$n_aval_i["Oppenheimer"]
objectCF_c$averages_i["Oppenheimer"]
}
\references{
\itemize{
\item LINDEN, G.; SMITH, B.; YORK, J. Amazon. com recommendations:
Item-to-item collaborative filtering. Internet Computing, IEEE, v. 7, n. 1,
p. 76-80,2003
\item Aggarwal, C. C. (2016). Recommender systems (Vol. 1). Cham: Springer
International Publishing.
\item Leskovec, J., Rajaraman, A., & Ullman, J. D. (2020). Mining of massive
data sets. Cambridge university press.
}
}
\seealso{
\code{\link[CFilt]{CFbuilder}}
}
\author{
Jessica Kubrusly
}
