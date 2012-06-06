setMethod("show", "ClassDefinition",
           function(object) {
             cat(getShowMessage(object), "\n")
             tmp = data.frame(name = names(object@slotTypes),
                              Rtype = sapply(object@slotTypes, getName),
                              min = sapply(object@slotTypes, function(x) if(is(x, "SchemaType")) x@count[1] else NA),
                              max = sapply(object@slotTypes, function(x) if(is(x, "SchemaType")) x@count[2] else NA),              
                              descriptionClass  = sapply(object@slotTypes, class))
             print(tmp)
           })


setMethod("show", "Element",
           function(object) {
              if(is.na(object@name))
                object = object@type
              cat("<", object@name, ">\n", sep = "")
           })

setMethod("show", "SchemaTypeReference",
           function(object) {
              cat("-> ", object@name, "(", object@nsuri, ") (reference)\n", sep = "")
           })



setMethod("show", "AttributeDef",
           function(object) {
              cat("@", object@name, if(!is.null(object@type)) c(" ", object@type@name),
                   "\n", sep = "")
           })

setGeneric("getShowMessage", function(object) standardGeneric("getShowMessage"))


setMethod("getShowMessage", "ExtendedClassDefinition",
           function(object) {
             paste(object@name, "extends", object@base, "(schema class Definition)")
           })

setMethod("getShowMessage", "ClassDefinition",
           function(object) {
             paste(object@name, "(schema class Definition)")
           })




                   
