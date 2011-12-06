
coerceListToS4 =
function(.x = list(...), .obj, ..., .partialMatch = TRUE, .coerce = TRUE)
{

   if(is.character(.obj))
      .obj = new(.obj)

   ids = slotNames(.obj)   
   m = (if(.partialMatch) pmatch else match)(names(.x), ids)   

   if(any(is.na(m)))
      stop("don't recognize slot(s) named ", paste(names(.x)[is.na(m)], collapse = ", "))

   names(.x) = ids[m]
  
   k = getClass(class(.obj))
   types = k@slots
   for(i in names(.x)) {
      slot(.obj, i) = if(.coerce) as(.x[[i]], types[[i]]) else .x[[i]]
   }
   .obj
}


checkHomogeneousList =
function(obj, types)
{

   w = sapply(types, function(k) all(sapply(obj, is, k)))
   if(!any(w)) {
     "Homogeneous list does not have homogeneous elements"
   } else
     TRUE
}
