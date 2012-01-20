resolveError =
  #
  # A function to raise a special error for resolving types within the WSDL schema.
  #
function(..., class = "SOAPResolveError")
{
   x = simpleError(paste(..., sep = ""))
   class(x) = c(class, class(x))
   stop(x)
}


# Resolve finds the definition and its contents within a context
# It can resolve at one level or all the way down.
# The definitions here are typically SOAP types.

DefaultPending = list(names = character(), types = list())

setGeneric("resolve", function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
                       obj = findSelfRefs(obj)
                       
#if(is(obj, 'GenericSchemaType') && obj@name == "description") browser()
                       
                       if(is.null(xrefInfo) && is(context, "SchemaCollection"))
                          xrefInfo = context@circularDefs

                       ans = standardGeneric("resolve")
                       if(!is.null(ans) && is(obj, "GenericSchemaType") && is(ans, "GenericSchemaType")) {
                          ans@default = optionalDefaultValue(ans, obj@default)
                        }
                       
                       ans
                     })

setMethod("resolve", c("SelfRef", "SchemaCollection"),
          function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
             i = match(obj@nsuri, names(context))
             if(is.na(i)) {
               stop("Can't find the schema with UR")
             }
             context[[i]][[obj@name]]
          })


setMethod("resolve", c("RestrictedStringPatternDefinition", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
              obj
           })


setMethod("resolve", c("AttributeDef", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
              ans = resolve(obj@type, context, namespaces, recursive, raiseError, xrefInfo)
              ans@default = obj@default
              ans
           })

setMethod("resolve", c("NULL", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
             NULL
           })

setMethod("resolve", c("BasicSOAPType", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
             stop("No method for this type ", class(obj))
           })


#XXX
# Here for StringArray in processWSDL("MassSpecAPI.wsdl", port = 3)
# That resolves to a ComplexType with a SimpleSequenceType
setMethod("resolve", c("SOAPComplexType", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
             if(length(obj@content) == 1)
                 return(resolve(obj@content, context, namespaces, recursive, raiseError, xrefInfo))
#             browser()
warning("probably need to do something different in resolve(SOAPComplexType, SchemaCollection) ", obj@name)
         #XXXX not appropriate
             resolve(obj@name, context, namespaces, recursive, raiseError, xrefInfo)                                      
           })

if(FALSE) {
setMethod("resolve", c("SOAPTypeReference", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
             
             resolve(obj@name, context, namespaces, recursive, raiseError, xrefInfo)
           })


#XXX Remove.Replaced by one directly below.
setMethod("resolve", c("SOAPTypeReference", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {

             if(!is.null(xrefInfo) && !is.null(xrefInfo$crossRefNames) && sprintf("%s:%s", obj@nsuri, obj@name) %in%  xrefInfo$crossRefNames) {
                   tp = xrefInfo$types
                   w = sapply(tp, function(tt) {
                                      i = match(obj@name, tt@subclasses)
                                      !is.na(i) && obj@nsuri == names(tt@subclasses)[i]
                                   })
                   return(tp[w][[1]])
             }
                
             
             ans = resolve(obj@name, context, namespaces, recursive, raiseError, xrefInfo)
             ans@default =  optionalDefaultValue(ans, obj@default)
             ans
           })
}

setMethod("resolve", c("SOAPTypeReference", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {

             if(!is.null(xrefInfo) && !is.null(xrefInfo$crossRefNames) && sprintf("%s:%s", obj@nsuri, obj@name) %in%  xrefInfo$crossRefNames) {
#browser()               
                   tp = xrefInfo$types
                   w = sapply(tp, function(tt) {
                                      i = match(obj@name, tt@subclasses)
                                      !is.na(i) && obj@nsuri == names(tt@subclasses)[i]
                                   })
                   return(tp[w][[1]])
             }

             if(length(context) == 0) 
               return(obj)             

                 # check if the reference is to one of the built-in types in XSD and if so, resolve it directly.
             if(obj@nsuri %in% getXSDSchemaURIs()) {
                 return(SOAPType(obj@name, nsuri = obj@nsuri, namespaceDefs = namespaces))
             }


             # find which context element corresponds to the URI we have in the object.
             i = match(obj@nsuri, names(context))
             if(is.na(i)) {
                   stop("can't find namespace '", obj@nsuri, "' of SOAPTypeReference ", obj@name,
                                " in context ", paste(names(context), collapse = ", "))
             }

             ans = context[[i]][[ obj@name ]]
             ans = resolve(ans, context, namespaces, recursive, FALSE, xrefInfo)

          
               if(is.null(ans) &&  substring(obj@name, 1, 7) == "ArrayOf") {
                  elementType = substring(obj@name, 8)

                  ans = new("SimpleSequenceType",
                               name = obj@name,
                               elementType = elementType,
                               elType = resolve(elementType, context, namespaces, recursive, raiseError, xrefInfo))
                }

                ans
           })

setMethod("resolve", c("AttributeDef", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
             obj #XXX
           })


setMethod("resolve", c("UnionDefinition", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
#if(obj@name == "iType") browser()
             obj@slotTypes = lapply(obj@slotTypes, resolve, context, namespaces, recursive)
             obj
           })

setMethod("resolve", c("ClassDefinition", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {

             obj@isAttribute = as.logical(sapply(obj@slotTypes, is, "AttributeDef"))
             obj@slotTypes = lapply(obj@slotTypes, resolve, context, namespaces, recursive, raiseError, xrefInfo)

            
        # This can go but is an experiment  to reduce/simplify the type
                 # Attempt to reduce . See DbListType in eutils.wsdl. But the first slot has already
                 # been made into a SimpleSequenceType.
             if(length(obj@slotTypes) == 1 && is(obj@slotTypes[[1]], "LocalElement")) {
                tp = obj@slotTypes[[1]]
                if(length(tp@count) && max(tp@count) > 1) {
                   def = new("SimpleSequenceType", name = obj@name, elType = tp, count = tp@count, nsuri = obj@nsuri, documentation = obj@documentation)
                  return(def)
               }
             }
             
             obj
           })

setMethod("resolve", c("WSDLTypeDescription", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
             resolve(obj$definition, context, namespaces, recursive, raiseError, xrefInfo)
           })


setMethod("resolve", c("PrimitiveSOAPType", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
             obj
           })



setMethod("resolve", c("SOAPVoidType"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
             obj
           })


setMethod("resolve", c("ArrayType", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
             if(recursive && (is.null(obj@elType) || length(obj@elType@name) == 0)) {
               name = obj@elementType
               obj@elType = resolve(name, context, namespaces, recursive, raiseError, xrefInfo)
             }
             obj
           })

setMethod("resolve", c("SOAPVoidType", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) 
	       return(obj))

setMethod("resolve", c("SOAPVoidType"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) 
	       return(obj))



setMethod("resolve", c("character", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {

             els = strsplit(obj, ":")[[1]]

             if(length(els) > 1) {

                i = match(els[1], names(namespaces))
                if(!is.na(i)) {
                   tp = SOAPType(els[2], els[2], namespaces[[i]]$uri)
                   if(recursive)
                      tp = resolve(tp, context, namespaces, recursive, raiseError, xrefInfo)
                   return(tp)
                } else {
                  warning("namespace prefix doesn't match any in namespaces")
                  if(getOption("DEBUG_XMLSCHEMA", FALSE)) browser()
                }
             }
             
#             if(length(els) > 1)
#               obj = els[2]

                # look for the element by name in the different schemas.
             for(ctxt in context) {
                       w = if(inherits(ctxt, "SchemaTypes") || inherits(ctxt, "SchemaCollection")) {
#                              cat("***** Calling resolve recursively for", obj, "with class", class(obj), "in ", class(ctxt), "\n")
                               resolve(obj, ctxt, ctxt@namespaceDefs, recursive = FALSE, raiseError = FALSE, xrefInfo)  #XXX was namespaces
                          } else if(obj %in% names(ctxt))
                               ctxt[[obj]]
                          else
                            NULL
                        if(!is.null(w))
                          break
            }

             i = is.null(w)
             
             if(is.null(w)) {
	        if(raiseError)
           	    resolveError("Cannot resolve ", obj, " in ", class(context))
                 else
    	           return(NULL)
             } #else if(sum(!i) > 1)
                # warning("resolved ", obj, "in ", sum(!i), " schema/elements")

	     val = w  # w[!i][[1]]   # # ((context[w])[[1]])[[obj]]

             if(recursive) {
#                   cat("[recursive] Calling resolve recursively for", obj, "with class", class(val), "in", class(context), "\n")
    	        resolve(val, context, namespaces, recursive, raiseError, xrefInfo)
              }
             else
                 val
           })


asQName = function(x) strsplit(x, ":")[[1]]

setMethod("resolve", c("AnySOAPType", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
             obj
           })

setMethod("resolve", c("SimpleSequenceType", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
#if(obj@name == "iType") browser()             
              obj@elType = resolve(obj@elType, context, namespaces, recursive, raiseError, xrefInfo = xrefInfo)
              obj
            })



setMethod("resolve", c("SOAPType", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
               #XXX deal with the namespace.
               #!!! This will cause infinite recursion if there is no method for the specific type.
               # So now match the namespace....
              i = match(obj@nsuri, names(context))
              if(!is.na(i)) {
                val = context[[ i ]] [[ obj@name ]]
                if(identical(val, obj))
                  return(val)

                return(resolve(val, context, namespaces, recursive, raiseError, xrefInfo))
              } else {

              }

              stop("problem")


              resolve(obj@name, context, namespaces, recursive, raiseError, xrefInfo)
           })

setMethod("resolve", c("SOAPType", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
               #XXX deal with the namespace. 
              resolve(obj@name, context, namespaces, recursive, raiseError, xrefInfo)
            })

setMethod("resolve", c("EnumValuesDef", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
             obj
            })

setMethod("resolve", c("RestrictedSetInteger", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
             obj
            })




.tmp <- function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
#if(obj == "StringArray") browser()

              if(length(context) == 0) {
                if(raiseError)
                    resolveError("Cannot resolve SOAP type in empty context")
                else
                   return(NULL)
              }
              
                  # remove any [] literals and split the string into
                  # namespace prefix & label.
               els = strsplit(gsub("\\[\\]", "", obj), ":")[[1]]
               if(length(els) == 2) { # array
                 if(els[1] == "xsd") {   #XXX need to match the namespace URI!
                   return(SOAPType(els[2], els[1]))
                 }
                  # Work with just the label, ignoring the namespace prefix.
                 els = els[2]
               } else if(length(els) == 1) {
                    # see if we are in the default namespace of XML schema
                 if(!is.na( idx <- match("", names(namespaces))) && namespaces[[ idx ]]$uri %in% getXSDSchemaURIs(all = TRUE))
                   return(SOAPType(els, nsuri = namespaces[[ idx ]]$uri))

               }

                    # deal with the namespace in the schema names.
               el = which(els[1] == names(context))
               if(length(el) == 0)
                   el = which(els[1] == gsub(".*:", "", names(context)))
            
            
               if(length(el) == 0 || is.na(el))  {
                 is.schema = sapply(context, is, "SchemaTypes")
                 if(any(is.schema)) {
#                   cat("SchemaTypes in c('character', 'list')\n")
                    tmp = lapply(context[is.schema], function(x) resolve(obj, x, recursive = recursive, raiseError = FALSE, xrefInfo = xrefInfo))
                    i = sapply(tmp, is.null)
                    if(!all(i))
                      return(tmp[!i][[1]])
                 }

                 if(raiseError)
                     resolveError("Cannot resolve SOAPTypeReference ", els[1], " in list")
                 else
                     return(NULL)
               }
           
                  # if there are 2 or more entries with the same name, then pick the one that isn't an Element.
                  # i.e. get the SOAPType one.
               if(length(el) > 1) {
                  el = el[ !sapply(context[el], is, "Element") ]
               }

               tmp = context[[el]]
               if(is(tmp, "XMLNode") && xmlName(tmp) == "element") {
                  id = asQName(xmlGetAttr(tmp, "type"))[2]
                  i = match(id, names(context))
                  if(is.na(i)) {
                    stop("unexpected problems when dealing with an <element> node that has no corresponding type in the larger context", class = "UnresolvedElementRef")
                    return(NULL)
                  }
                  el = i
               }

               if("definition" %in% names(context[[el]]))
                  ans = context[[el]]$definition
               else
                  ans = context[[el]]

               if(is.null(ans)) { #XXX why will the object above not have a definition.
                if(raiseError)
                   resolveError("NULL value for resolved type")
                else
                   return(NULL)
               }
              
               if(recursive && (is(ans, "ClassDefinition") || !is(ans, "TerminalSOAPType"))) {
#                  cat("Calling resolve recursively for", obj, "with", class(ans), "\n")
                  ans = resolve(ans, context, namespaces, recursive, raiseError, xrefInfo)
                }

               ans
           }

setMethod("resolve", c("character", "list"), .tmp) # could be SchemaTypes rather than list.
setMethod("resolve", c("character", "SchemaTypes"), .tmp)


setMethod("resolve", c("SimpleElement", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
             ans = new("Element")
#             ans = obj
             if(length(obj@type))
                ans@type = resolve(obj@type, context, namespaces, recursive, raiseError, xrefInfo)
             ans@attributes = lapply(ans@attributes, resolve, context, namespaces, recursive, raiseError, xrefInfo)
             #ans@default = obj@default
             ans             
           })


setMethod("resolve", c("Element", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {

#XXX temporary test
    obj@type = resolve(obj@type, context, namespaces, recursive, raiseError, xrefInfo)
    if(is(obj@type, "Element")) {
      obj@type@default =  optionalDefaultValue(obj@type, obj@default)
      return(obj@type)
    }
    return(obj)

             
                   # could give infinite recursion. See msnSearch.wsdl and SearchResponse.
                   # So just look it up.
             if(!is(obj@type, "SOAPTypeReference"))
                return(obj@type)
             
             ans = lookupType(obj@type@name, context)
             ans = if(is.null(ans)) 
                     obj@type
                   else
                     ans

             if(recursive && is(ans, "SOAPTypeReference"))
                ans = resolve(ans, context, namespaces, recursive, raiseError, xrefInfo)
             #ans
             obj@type = ans
             obj
           })



setMethod("resolve", c("LocalElement", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
              tmp = resolve(obj@type, context, namespaces, recursive, raiseError, xrefInfo)
              obj@type = tmp
              if(!length(obj@type@default))
                  obj@type@default = obj@default
              obj
           })


setMethod("resolve", c("RestrictedStringDefinition", "list"), #XXX was list
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL) {
             obj
           })

