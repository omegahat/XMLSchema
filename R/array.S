SOAP.logical <-
function(x)
{
  val <- as.logical(x)

  if(is.na(val)) {
   val <- as.logical(as.integer(x))
  }

  val
}  


getNodeById <-
  #
  # Find the top-level node identified by id=value of id
  # Strip the `#' prefix off `id' first.
function(id, root)
{
  id <- gsub("^#", "", id)

  for(i in xmlChildren(root)) {
    k <- xmlAttrs(i)
    if(!is.null(k) && !is.na(match("id", names(k))))
      if(id == k[["id"]])
        return(i)
  }

  return(NULL)
}


