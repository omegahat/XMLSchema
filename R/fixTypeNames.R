fixTypeNames =
  #
  # Find entries in the schema collection that have the same name
  # for an R type.
  #
  # XXX We could make this smarter by having it compute the prefixes based on the number
  # of duplicates for a given name so that we can leave one of them  with the same name
  # as in the schema.
  #
function(types, computeNames = computeUniqueTypeNames, prefixes = computePrefixes(names(types)), verbose = FALSE, ...)
{

  tt = structure(unlist(types, recursive = FALSE), names = as.character(unlist(lapply(types, names))))
     # the identity of the schema they came from
  whichSchema = structure(rep(1:length(types), sapply(types, length)), names = names(tt))
  
  dups = unique(names(tt)[duplicated(names(tt))])

  for(id in dups) {
     w = names(tt) == id
     els = tt[ w ]
     if(verbose)
        cat("Multiple (", sum(w), ") types with the same name ", id, "\n", sep = "")


     newNames = computeNames(els, names(types)[ whichSchema[w] ], id)
     if(length(newNames) == 0)
       next
     
     updated = mapply(setRTypeName, els, newNames)
#     prefix = prefixes[ whichSchema[w] ]   # [ sapply(els, slot, "nsuri") ]
#     updated = mapply(setRTypeName, els, sprintf("%s%s", prefix, rep(id, length(prefix))))

     tt[w] = updated
  }

    # now put the elements of tt back into types in the correct places.
  bySchema = split(tt, whichSchema)
  for(i in seq(along = bySchema)) {
    types[[i]][] = bySchema[[i]]
  }

  types
}

computeUniqueTypeNames =
function(types, nsuris, name)  
{
  isEl = sapply(types, is, "Element")
  if(all(isEl) || sum(!isEl) == 1)
    return(NULL)
  
  ans = rep(name, length(types))
  i = which(!isEl)
  
  prefix = if(any(duplicated(nsuris[i]))) {
        # What do we do here. Give them unique names
        # across the entire collection of schemas.
        # e.g. id, id2, id3, id4, id5 ...
    sprintf("%s%d", "id", seq(2, length = sum(i) - 1L))
  } else
    paste(".", basename(nsuris[i][-1]), sep = "")

  ans[i] = sprintf("%s%s", c("", prefix), name)

  ans
}

isDupType =
function(id, types)
{
  els = types[ names(types) == id ]
  isEl = sapply(els, is, "Element")
  ns = sapply(els, slot, "nsuri")
  ans = sapply(els, XMLSchema:::mapSOAPTypeToS, types)

  structure(ans, names = ns)
}

computePrefixes =
function(x, ...)
{
  structure(c("", sprintf("%s.", basename(x[-1]))), names = x)
}
  

setGeneric("setRTypeName",
            function(obj, name, ...) {
              standardGeneric("setRTypeName")
            })

setMethod("setRTypeName", "Element", 
            function(obj, name, ...) {
                obj@Rname = name
                obj@type@Rname = name
                obj
            })

setMethod("setRTypeName", "GenericSchemaType", 
            function(obj, name, ...) {
                obj@Rname = name
                obj
            })
