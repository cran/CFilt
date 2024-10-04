#' CFilt: A package about Recommendation by Collaborative Filtering in R.
#'
#'@description
#'The CFilt package provides one builder function CFbuilder and one class CF. 
#'CF is a class of objects that stores information about a 
#'recommendation system. This class contains methods that modify the 
#'object's structure and functions which return items to recommend to a user or 
#'users to whom an item should be recommended. 
#'An object of the CF class is created using the CFbuilder function.
#'
#'@details
#'
#' Two main goals:
#' \itemize{
#' \item Structure the database so that changes can be made in a practical way 
#' through object-oriented programming.
#' \item Make recommendations through choices by the Collaborative Filtering 
#' methodology in a practical, fast and efficient manner.
#' }
#' @docType package
#' @name CFilt
#'
#' @author Jessica Kubrusly 
#' 
#' @importFrom methods setRefClass
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar