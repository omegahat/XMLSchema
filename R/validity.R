setGeneric("makeValidity",
             function(type, ...) {

               standardGeneric("makeValidity")

             })

# Reconcile the two functions - this one and makeListValidityFun
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

makeListValidityFun =
function(i, typeName = i@elType@Rname, count = numeric(0))
{
      valid = function(object) {
                 if(!all(w <- sapply(object, is, elName)))
                    return(paste("not all elements are of type ", elName, ": problems with element(s) ", paste(which(!w), collapse = ", ")))
                 else
                   TRUE
              }
         # replace elName with typenamae and also the elName in
         # the return() string.
      body(valid)[[2]][[2]][[2]][[2]][[3]][[4]] = typeName
      body(valid)[[2]][[3]][[2]][[3]] = typeName

      
      if(length(count) && (count != c(0, Inf))) {

        if(max(count) == Inf)
           e = substitute(if(length(object) < min) "too few elements" else TRUE, list(min = count[1]))
        else
           e = substitute(if(length(object) < min || length(object) > max)
                                             "incorrect number of elements"
                                         else
                                             TRUE, list(min = count[1], max = count[2]))

         body(valid)[[2]][[4]] = e
      }
      
      valid
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
        object
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
            types = sapply(type@slotTypes, computeName)
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



makeRestrictedPatternStringValidity =
function(pattern, name)
{
  f = function(object) {
     if(grepl(pattern, object, perl = TRUE))
        TRUE
     else
       sprintf("%s doesn't match pattern %s for class %s", object, pattern, name)
  }
  environment(f) = globalenv()

#  pattern = gsub("\\\\", "\\\\\\", pattern)
  
  body(f)[[2]][[2]][[2]] = pattern
  f
}


makeRestrictedStringValidityFunction =
function(types)
{
  types = lapply(types, function(x)
                           if(is(x, "ExtendedClassDefinition"))
                             x@baseType  # Do we need more than this? i.e. any restrictions, any extra pieces
                           else
                             x)
  
  pattern = sapply(types, is, "RestrictedStringPatternDefinition")
  rx = sapply(types[pattern], slot, "pattern")

  enums = sapply(types, is, "RestrictedStringDefinition")  
  values = unique(unlist(lapply(types[enums], slot, "values")))

  unrestrictedString = any(!pattern & !enums)

  if(unrestrictedString)
     return(function(object) TRUE)
  
  f = function(object) {
            values = .vals
            if(grepl(.rx, object))
               TRUE
            else if(x %in% values)
               TRUE
            else
              stop("value is not one of the valid values or patterns")
           }

  body(f)[[2]][[3]] = values
  body(f)[[3]][[2]][[2]] = paste(rx, collapse = "|")
  
  f
}

