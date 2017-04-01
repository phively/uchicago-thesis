# Function to choose rows from a sparse document-term matrix
split_set <- function(dtm, ind) {
# dtm: a document-term sparse matrix
# ind: rows to preserve from the old matrix
  # Identify which rows i (documents) to keep
  sparse_ind <- dtm$i %in% ind
  # Vector of counts of original documents, 1 to D
  map_old <- 1:length(dtm$dimnames$Docs)
  # Remapped document number, 1 to D* with 0 in the place of dropped documents
  map_new <- cumsum(map_old %in% ind) * (map_old %in% ind)

  # Create sparse matrix keeping the elements that meet the above criteria
  dtm$i <- dtm$i[sparse_ind] # Keep all i corresponding to a document number in ind
  dtm$i <- map_new[dtm$i] # Relabel the i
  dtm$j <- dtm$j[sparse_ind] # Keep all j corresponding to ind
  dtm$v <- dtm$v[sparse_ind] # Keep all v corresponding to ind
  dtm$nrow <- max(map_new) # Reset row count
  dtm$dimnames$Docs <- dtm$dimnames$Docs[map_old %in% ind] # Update document names vector
  
  #Return results
  return(dtm)
}