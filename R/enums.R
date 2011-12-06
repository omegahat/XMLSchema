
defEnum =
function(name, values, coerce = TRUE, where = globalenv())
{
   def = setClass(name, contains = "StringEnum", where = where)
   if(coerce) {
      f = function(from) {
             values = ""
             name = ""
             if(!is.na(from) && !(from %in% values))
                stop("not a valid value for ", name, ". Must be one of ", paste(values, collapse = ", "))
             new(name, as.character(from))
           }
      body(f)[[2]][[3]] = values
      body(f)[[3]][[3]] = name
      environment(f) = DefaultFunctionNamespace
      setAs("character", name, f, where = where)
   }
   def
}
