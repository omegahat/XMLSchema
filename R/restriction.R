
         
createRestrictionType =
function(name, rnode, namespaces = list(), targetNamespace = NA, base = xmlGetAttr(rnode, "base"))
{
     #XXX need to figure out the namespace and check it is an XSD double or integer
  
 if(length(base) > 1) base = base[2]

   if(base == "double" || base == "integer") {
      r = getRestrictedRange(rnode)
      className = switch(base, double = "RestrictedDouble", integer = "RestrictedInteger")
      def = new(className, name = name, range = r$range, inclusive = r$inclusive)
   } else if (base == "hexBinary") {
      if("length" %in% names(rnode))
        len = xmlGetAttr(rnode[["length"]], "value", NA)
      else
        len = NA
      rx = if(!is.na(len)) sprintf("([a-fA-F0-9]{2}){%d}", as.integer(len)) else "([a-fA-F]){2}"
      
      def = new("RestrictedHexBinary", length = as.integer(len), pattern = rx)

   } else {
                                        #XXX Assume enumeration values for present.
     if(!(base %in% c("string", "token", "NMTOKEN", "Name", "ID", "IDREF"))) {
        warning("treating ", name, " as an enumeration type")
     }
     
     def <- new("EnumValuesDef", name = name, values = xmlSApply(rnode,  xmlGetAttr, "value"))
     if(length(name) == 0)  #XXX
              def@name = paste(def@values, collapse = ",")
   }

   def@nsuri = targetNamespace
   def@name = name
   
   def
}

getRestrictedRange =
function(node)
{
  ids = names(node)

  range = structure(as.numeric(c(NA, NA)), names = c("min", "max"))
  inc =  structure(logical(2), names = c("min", "max"))
  for(w in c("min", "max")) {
     i = grep(sprintf("^%s", w), ids)
     if(length(i)) {
        range[w] = xmlGetAttr(node[[i]], "value", , as.numeric)
        inc[w] = ids[i] == sprintf("%sInclusive", w)
     }
  }

  
  list(range = range, inclusive = inc)
}


makeRestrictedFunc =
function(className, base, range, inc)
{

   f = function(from) {
         from = as(from, base)
         if(from <= range["min"])
             stop("")

         if(from >= range["max"])
             stop("")
         
         new(className, from)
    }

    body(f)[[2]][[3]][[3]] = base

    body(f)[[3]][[2]][[3]] = range["min"]
    body(f)[[3]][[3]][[2]] = paste("value is too small. Must be larger than", range["min"])


    body(f)[[4]][[2]][[3]] = range["max"]
    body(f)[[4]][[3]][[2]] = paste("value is too large. Must be less than", range["max"])

   if(inc["min"])
          body(f)[[3]][[2]][[1]] = as.name("<")
   if(inc["max"])
          body(f)[[4]][[2]][[1]] = as.name(">")
   
   body(f)[[5]][[2]] = className

   
   if(all(is.na(range)))
       body(f) = body(f)[[4]]
   else if(is.na(range["min"]))
     body(f) = body(f)[-2]
   else if(is.na(range["max"]))  # could have removed the two.
     body(f) = body(f)[-3]   


   environment(f) = globalenv()
   
   f
}
