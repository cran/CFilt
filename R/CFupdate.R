CFupdate <- function(obj_CF, Data_new){
  
  # Validations ----
  if (!inherits(obj_CF, "CF")) {
    stop("*** 'cf_object' must be a CF object created by CFbuilder() ***")
  }
  
  if (!is.data.frame(Data_new)) {
    stop("*** 'Data_new' must be a dataframe object ***")
  }
  
  if (obj_CF$datatype == "rating" && ncol(Data_new) != 3) {
    stop("*** CF object has datatype='ratings' but Data_new must have 3 columns ***")
  }
  
  if (obj_CF$datatype == "consumption" && ncol(Data_new) != 2) {
    stop("*** CF object has datatype='consumption' but Data_new must have 2 columns ***")
  }
  
  # Setup ----
  data_new <- as.data.frame(Data_new)
  data_old <- as.data.frame(obj_CF$data_0)
  
  key_old <- interaction(data_old[[1]], data_old[[2]], drop = TRUE)
  key_new <- interaction(data_new[[1]], data_new[[2]], drop = TRUE)
  
  pos <- match(key_new, key_old)
  
  # Add Rown ----
  idx_add <- which(is.na(pos))
  if (length(idx_add) > 0) {
    data_old <- rbind(data_old, data_new[idx_add, ])
    key_old  <- c(key_old, key_new[idx_add])
  }
  
  # Update row ----
  if (obj_CF$datatype == "rating") {
    idx_upd <- which(!is.na(pos))
    if (length(idx_upd) > 0) {
      data_old[pos[idx_upd], 3] <- data_new[idx_upd, 3]
    }
  }
  
  # Re-build ----
  Data <- data_old
  
  return(
    CFbuilder(
      Data = Data,
      Datatype = obj_CF$datatype,
      similarity = obj_CF$similarity
    )
  )
}
