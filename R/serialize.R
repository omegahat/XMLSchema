
exportClassDefs =
function(x, file = NULL, where = globalenv(), ...)
{
  if(!is.null(file)) {
   if(is.character(file)) {
     file = file(file, "w")
     on.exit(close(file))     
   }

   if(!isOpen(file)) {
     file = open(file, "w")
     on.exit(close(file))
   }
 }
  classNames = if(is.character(x))
                  x
               else if(is(x, "SchemaCollection"))
                  unlist(lapply(x, names))
               else if(is(x, "list")) {
                  if(all(sapply(x, is.character)))
                     unlist(x)
                  else
                     names(x)
                }

   classNames = computeOrder(classNames, where, ...)

   txt = lapply(names(classNames), exportClassDef, where)
   if(!is.null(file))
      cat(sapply(txt, paste, collapse = " "), file = file, sep = "\n")
   txt
}
  

exportClassDef =
function(def, con, where = globalenv())
{
    #XXX prototype
  if(is.character(def))
    def = getClassDef(def)

  if(is.null(def))
    return(FALSE)

  if(is(def, "ClassUnionRepresentation")) {
    c(paste("setClassUnion(", sQuote(def@className), ", " ),
      paste("\tc(", paste(sQuote(names(def@subclasses)), collapse = ", "), ")"),
      ")")

  } else
  
     c(paste("setClass(", sQuote(def@className)),
        if(length(def@slots)) 
           paste(",\n\t   representation(\n\t\t",
                   paste(names(def@slots), sQuote(unlist(def@slots)), sep = " = ", collapse = ",\n\t\t"),
                ")", sep = ""),
        if(length(def@contains))
           paste(",\n\t   contains = c(", paste(sQuote(names(def@contains)[1]), collapse = ", "), ")"),
        ")")
}


writeClassDef =
function(def, file = stdout(), where = globalenv())
{

   if(!is.character(def) && !is(def, "classRepresentation"))
       return()

   if(is.character(def))
     def = getClassDef(def,  where = where)

   if(is.null(def))
     return(FALSE)
   
    cat(exportClassDef(def), "\n", file = file)

    className = def@className

    coerceTypes = c("XMLAbstractNode", "XMLInternalElementNode", "character", "integer", "numeric", "logical", "list")

    for(type in coerceTypes) {
      m <- selectMethod('coerce', c(type, className), optional = TRUE)
      if(!is.null(m)) {
         cat(sprintf("setAs('%s', %s,\n", type, sQuote(className)), file = file)
         m = as(m, "function")
         if(length(vars <- ls(environment(m), all = TRUE)) > 0 && !identical(environment(m), globalenv())) {
#           browser()
            warning("removing environment from coercion method with objects ", paste(vars, collapse = ", "))
          }

#         environment(m) = globalenv()
         cat("\n", deparse(discardEnvironment(m)), file = file, "\n", sep = "\n")
         cat(")\n", file = file)                                                   
       }
    }

}



setGeneric("expandS4",
            function(x)
              standardGeneric("expandS4"))

setMethod("expandS4", "list",
function(x) {

  e = quote(list())
  for(i in seq(along = x))
    e[[i + 1]] = expandS4(x[[i]])

  if(length(names(x)))
    names(e) = c("", names(x))

  e
})



setMethod("expandS4", "ANY",
function(x)
{
   if(!isS4(x))
      return(x)

   e = substitute(new(k), list(k = class(x)))
   addName = logical(length(slotNames(x)))
   for(i in seq(along = slotNames(x))) {
      val = slot(x, slotNames(x)[i])
      if(is.function(val)) {
         val = discardEnvironment(val)
      }
      if(!is.null(val)) {
         e[[ i + 2 ]] = expandS4(val)
         addName[i] = TRUE
      }
   }
   names(e) = c("", "", slotNames(x)[addName])
   e
})



discardEnvironment =
function(fun)
{
  e = environment(fun)
  if(is.null(e) || identical(e, globalenv()) || identical(e, emptyenv()))
    return(fun)
  
  vars = ls(e, all = TRUE)

  vars = intersect(vars, findGlobals(fun, FALSE)$variables)
  
  if(length(vars)) {
    formals(fun)[vars] = lapply(vars, get, e)
  }

  fun
}


computeOrder =
function(classes, where = globalenv(), numTries = 10)
{
  deps = lapply(classes, getDependencies, unlist(classes), where)
  names(deps) = classes
  lens = sapply(deps, length)

  ans = deps[order(lens)]
  for(i in 1:numTries) {
     if(all(checkOrder(ans)))
       return(ans)
     ans = permuteOrder(ans)
  }
  w = checkOrder(ans)
  warning(sum(!w), " not in correct order: ", paste(names(deps)[!w], collapse = ", "))
  ans
}

getDependencies =
function(class, allClasses, where = globalenv())
{
   def = getClassDef(class, where)
   if(is.null(def))
     return(character())
   v = if(is(def, "ClassUnionRepresentation"))
          names(def@subclasses)
       else {
         subs = intersect(names(def@contains), allClasses)

            # for regular classes, we may have another class in the contains
            # due to a setClassUnion() that has this class as a possible element
            # This information is stored in this class, not the ClassUnion definition(!)
            # So we have to recognize these.
            # It is possible to have a regular ClassUnion as a regular super class
            # and we check this by looking at the SClassExtension. But that is not enough!!!
         if(length(subs)) 
           ok = sapply(subs, function(k) {
                              d = getClassDef(k, where)
                              if(!is(d, "ClassUnionRepresentation"))
                                  return(TRUE)

                              !( class %in% names(d@subclasses) && is(d@subclasses[[class]], "SClassExtension"))
                              
                          })
         else
           ok = logical()
         c(subs[ok], unlist(def@slots))
       }
  intersect(v, allClasses)   
  # c(intersect(v, allClasses), def@className)
}
         

permuteOrder =
function(deps, browse = FALSE)
{
#  cat("starting permuteOrder\n")
  ids = names(deps)
  perm = 1L # integer()
  for(i in seq(along = deps)) {
     if( all(deps[[i]] %in% ids[perm]) )
       perm = c(perm, i)
     else {
if(browse) browser()       
        j = match(deps[[i]], names(deps))
        j = j[ j >= i]
        perm = c(perm, j, i)
     }
  }
  deps = deps[ perm ]
#  cat("ending permuteOrder\n")  
  deps[unique(names(deps))]
}


checkOrder =
function(deps)
{
  ids = names(deps)
  ok = logical(length(deps))
  for(i in seq(along = deps)) {
     ok[i] = all(deps[[i]] %in% ids[1:i])
  }
  ok
}
