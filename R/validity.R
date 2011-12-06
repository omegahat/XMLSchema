setGeneric("makeValidity",
             function(type, ...) {

               standardGeneric("makeValidity")

             })

makeListValidity =
  #
  #  Check if all elements of a list are from a fixed set of types
  #
function(type, types, ...)
{

  f = function(object) {
    if(!all(w <- sapply(object, function(x) is(x, "type") )))
      return(paste("not all elements are of the appropriate types:", paste(which(!w), collapse = ", ")))

    TRUE
  }
  environment(f) = globalenv()
  k = makeValidity(type@elType)

     #   body, if,   !   all(, <-, func
  body(f)[[2]][[2]][[2]][[2]][[3]][[3]] = k  

  f
}

makeCheckTypes =
  #
  #  Buid a function that checks if an object is one of several possible types
  #
  #
function(types, varName = 'object', asFunction = TRUE)
{
  k = substitute(is(x, type), list(x = as.name(varName), type = types[1]))  
  if(length(types) > 1) {
    q = quote(a || b)
    for(i in types[-1]) {
      q[[2]] = substitute(is(x, type), list(type = i, x = as.name(varName)))
      q[[3]] = k
      k = q
    }
  }

 if(!asFunction)
   return(k)
  
 f = function(object) {
        x
     }
 environment(f) = globalenv()
 body(f)[[2]] = k
 names(formals(f)) = varName
 f
}


if(FALSE)
makeTypesValidity =
function(types, ...)
{
  f = function(object) {
        if(check)
           return("")
        TRUE
  }

  f
}


setMethod("makeValidity", "UnionDefinition",
          function(type) {
            types = sapply(type@slotTypes, XMLSchema:::computeName)
            test = makeCheckTypes(types, asFunction = FALSE)
            f = function(object) {
                   if(test)
                      msg
                   else
                      TRUE
                }
            environment(f) = globalenv()
            body(f)[[2]][[2]] = test
            body(f)[[2]][[3]] = paste("object is not one of ", paste(types, collapse = ", "))
            f
          })

setMethod("makeValidity", "SimpleSequenceType",
           function(type, ...) {
             makeListValidity(type)
           })
