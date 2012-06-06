resolveError =
  #
  # A function to raise a special error for resolving types within the WSDL schema.
  #
function(..., class = "SchemaResolveError")
{
   x = simpleError(paste(..., sep = ""))
   class(x) = c(class, class(x))
   stop(x)
}


# Resolve finds the definition and its contents within a context
# It can resolve at one level or all the way down.
# The definitions here are typically SOAP types.

DefaultPending = list(names = character(), types = list())

setGeneric("resolve", function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL,
                                type = NA, depth = 1L, work = NULL,  ...)
                      {

                        if(is(obj, "AnyAttributeDef"))
                          return(obj)

                        if(is.null(obj))
                           return(NULL)
                        
                        ans = NULL
                        if(depth > 100)
                          stop("Probably infinite recursion. Stopping the resolve() call.")
                        
                        if(FALSE) #XXXX Temporary turn off May 5th 2012
                            obj = findSelfRefs(obj)

                        if(is.null(work))
                          work = list(pending = new.env(), resolved = new.env())
                        
                        if(!is.null(work)) {
                          id.name = if(is(obj, "GenericSchemaType") && !is.na(obj@name)) obj@name else "?"
                          id = sprintf("%s:%s:%s:%s", id.name, class(obj), getName(obj), class(context))
                          if(exists(id, work$pending, inherits = FALSE) && id.name != "?") {
                             cat("looking for", id, "and it is in pending, depth = ", depth, "\n")
#                            browser()
                          }

                          #XXX Activating this causes an element with a different name to be returned for something with the
                          # same structure, i.e. class & type. e.g. slots in LatLonBoxType in kml21.xsd
#                          if(exists(id, work$resolved))
#                             return(get(id, work$resolved))
                          
                          assign(id, obj, work$pending)
                          on.exit({ if(exists(id, work$pending, inherits = FALSE))
                                        remove(list = id, envir = work$pending)
#                                   cat("finished resolving", id, "\n")
                                   if(!is.null(ans))
                                      assign(id, ans, work$resolved)
                                  })
                        }
                       
                       if(is.null(xrefInfo) && is(context, "SchemaCollection"))
                          xrefInfo = context@circularDefs
#XXXXXXXX  was = We could break everything by using the  <- version as now the entire body is used and not just standardGeneric()
                       ans <- standardGeneric("resolve")
                       if(!is.null(ans) && is(obj, "GenericSchemaType") && is(ans, "GenericSchemaType")) 
                          ans@default = optionalDefaultValue(ans, obj@default)
                       
                       ans
                     })

setMethod("resolve", c("SelfRef", "SchemaCollection"),
          function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL,
                        type = NULL, depth = 1L, work = NULL, ...) {
             i = match(obj@nsuri, names(context))
             if(is.na(i)) 
                 stop("Can't find the schema with URI ", obj@nsuri)

             context[[i]][[obj@name]]
          })


setMethod("resolve", c("RestrictedStringPatternDefinition", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL,
                       type = NULL, depth = 1L, work = NULL, ...) {
              obj
           })


setMethod("resolve", c("AttributeDef", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL, type = NULL, depth = 1L, work = NULL, ...) {
              ans = resolve(obj@type, context, namespaces, recursive, raiseError, xrefInfo, type, depth = depth + 1L, work = work, ...)
              ans@default = obj@default
#             ans
#XXXX ADDING THIS TEMPORARILY               Fri May 4, 12:33 2012
              obj@type = ans
              obj
           })

setMethod("resolve", c("NULL", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL,
                            type = NULL, depth = 1L, work = NULL, ...) {
             NULL
           })

setMethod("resolve", c("BasicSchemaType", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL,
                       type = NULL, depth = 1L, work = NULL, ...) {
             stop("No method for this type ", class(obj))
           })


#XXX
# Here for StringArray in processWSDL("MassSpecAPI.wsdl", port = 3)
# That resolves to a ComplexType with a SimpleSequenceType
setMethod("resolve", c("SchemaComplexType", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL,
                       type = NULL, depth = 1L, work = NULL, ...) {
             if(length(obj@content) == 1)
                 return(resolve(obj@content, context, namespaces, recursive, raiseError, xrefInfo, type, depth = depth + 1L, work = work, ...))

warning("probably need to do something different in resolve(SchemaComplexType, SchemaCollection) ", obj@name)
         #XXXX not appropriate
             resolve(obj@name, context, namespaces, recursive, raiseError, xrefInfo, type, depth = depth + 1L, work = work,  ...)
       })

if(FALSE) {
setMethod("resolve", c("SchemaTypeReference", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL, type = NULL, depth = 1L, ...) {
             
             ans = resolve(obj@name, context, namespaces, recursive, raiseError, xrefInfo, type, depth = depth + 1L, ...)
#!!! May 10, 6:13             
             ans@count = obj@count
             ans
           })


#XXX Remove.Replaced by one directly below.
setMethod("resolve", c("SchemaTypeReference", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL,
                             type = NULL, depth = 1L, work = NULL, ...) {

             if(!is.null(xrefInfo) && !is.null(xrefInfo$crossRefNames) &&
                    length(xrefInfo$crossRefNames) &&  length(obj@nsuri) && 
                      sprintf("%s:%s", obj@nsuri, obj@name) %in%  xrefInfo$crossRefNames) {
                   tp = xrefInfo$types
                   w = sapply(tp, function(tt) {
                                      i = match(obj@name, tt@subclasses)
                                      !is.na(i) && obj@nsuri == names(tt@subclasses)[i]
                                   })
                   return(tp[w][[1]])
             }
                
             ans = resolve(obj@name, context, namespaces, recursive, raiseError, xrefInfo, type, depth = depth + 1L, work = work, ...)
             ans@default = optionalDefaultValue(ans, obj@default)
             ans
           })
}

setMethod("resolve", c("AnyAttributeDef", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE,
                      xrefInfo = NULL, type = NULL, depth = 1L, work = NULL,  ...) {
             obj
           })

setMethod("resolve", c("AttributeGroup", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL,
                        type = NULL, depth = 1L, work = NULL,  ...) {

               obj@attributes = lapply(obj@attributes, resolve, context, namespaces, recursive, raiseError, xrefInfo, depth = depth + 1L, work = work, ...)
               obj
              })

setMethod("resolve", c("SchemaTypeReference", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL, type = NULL, depth = 1L, work = NULL,  ...) {

             if(!is.null(xrefInfo) && !is.null(xrefInfo$crossRefNames) &&
                    length(xrefInfo$crossRefNames) &&  length(obj@nsuri) && 
                      sprintf("%s:%s", obj@nsuri, obj@name) %in%  xrefInfo$crossRefNames) {            

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
             if(length(obj@nsuri) && obj@nsuri %in% getXSDSchemaURIs()) 
                 return(SchemaType(obj@name, nsuri = obj@nsuri, namespaceDefs = namespaces))


                 # find which context element corresponds to the URI we have in the object.
             if(length(obj@nsuri))
                i = match(obj@nsuri, names(context))
             else
                i = which(sapply(context, function(x) obj@name %in% names(x)))[1]
             
             if(is.na(i)) {
                   if(length(context) == 1 && names(context) == "" && is.na(obj@nsuri))
                      i = 1L
                   else
                      stop("can't find namespace '", obj@nsuri, "' of SchemaTypeReference ", obj@name,
                                " in context ", paste(names(context), collapse = ", "))
             }

#XXX original May 6, 2012 was just   ans = context[[i]][[ obj@name ]]
#browser()
   m <- which(obj@name == names(context[[i]]))
   if(length( m ) == 1) 
       ans = context[[i]][[ m ]]
   else {
      if(is.null(type))
         type = notElementFun                  

      m =  sapply(context[[i]][m], type)
      if(!any(m))
        stop("failed to find suitable object")
      else if(sum(m) > 1)
        warning("matched more than one schema entries named ", obj@name)
     ans = context[[i]][m][[ which(m)[1] ]]
   }

   if(recursive == FALSE)
      return(ans)
             
   ans = resolve(ans, context, namespaces, recursive, FALSE, xrefInfo, type, depth = depth + 1L, work = work, ...)

          
             if(is.null(ans) && length(obj@name) && !is.na(obj@name) && substring(obj@name, 1, 7) == "ArrayOf") {
                  elementType = substring(obj@name, 8)

                  ans = new("SimpleSequenceType",
                               name = obj@name,
                               elementType = elementType,
                               elType = resolve(elementType, context, namespaces, recursive, raiseError, xrefInfo, depth = depth + 1L, work = work, ...))
              }

                ans
           })


setMethod("resolve", c("AttributeDef", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL, type = NULL, depth = 1L, work = NULL,  ...) {
                 # just return the attribute
              obj@type = resolve(obj@type, context, namespaces, recursive, raiseError, xrefInfo, type, depth + 1L, work = work, ...)
              obj #XXX
           })


setMethod("resolve", c("UnionDefinition", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL, type = NULL, depth = 1L, work = NULL,  ...) {
#if(obj@name == "iType") browser()
             obj@slotTypes = lapply(obj@slotTypes, resolve, context, namespaces, recursive, depth = depth + 1L, work = work, ...)
             obj
           })

setMethod("resolve", c("ClassDefinition", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL,
                     type = NULL, depth = 1L, work = NULL,  ...) {

             obj@isAttribute = as.logical(sapply(obj@slotTypes, is, "AttributeDef"))
             obj@slotTypes = lapply(obj@slotTypes, resolve, context, namespaces, recursive,
                                                      raiseError, xrefInfo, depth = depth + 1L, work = work, ...)

            
         # This can go but is an experiment  to reduce/simplify the type
                 # Attempt to reduce . See DbListType in eutils.wsdl. But the first slot has already
                 # been made into a SimpleSequenceType.
             if(length(obj@slotTypes) == 1 && is(obj@slotTypes[[1]], "LocalElement")) {
                tp = obj@slotTypes[[1]]
                if(length(tp@count) && max(tp@count) > 1) {
                   def = new("SimpleSequenceType", name = obj@name, elType = tp, count = tp@count,
                                 nsuri = obj@nsuri, documentation = obj@documentation)
                  return(def)
               }
             }
             
             obj
           })

setMethod("resolve", c("WSDLTypeDescription", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL,
                      type = NULL, depth = 1L,  work = NULL, ...) {
             resolve(obj$definition, context, namespaces, recursive, raiseError, xrefInfo, type, depth = depth + 1L, work = work, ...)
           })


setMethod("resolve", c("PrimitiveSchemaType", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE,
                      xrefInfo = NULL, type = NULL, depth = 1L, work = NULL, ...) {
             obj
           })



setMethod("resolve", c("SchemaVoidType"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE,
                     xrefInfo = NULL, type = NULL, depth = 1L, work = NULL,  ...) {
             obj
           })


setMethod("resolve", c("ArrayType", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE,
                      xrefInfo = NULL, type = NULL, depth = 1L, work = NULL, ...) {
             if(recursive && (is.null(obj@elType) || length(obj@elType@name) == 0)) {
               name = obj@elementType
               obj@elType = resolve(name, context, namespaces, recursive, raiseError, xrefInfo, depth = depth + 1L, work = work, ...)
             }
             obj
           })

setMethod("resolve", c("SchemaVoidType", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE,
                      xrefInfo = NULL, type = NULL, depth = 1L, ...) 
	       return(obj))

setMethod("resolve", c("SchemaVoidType"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE,
                      xrefInfo = NULL, type = NULL, depth = 1L, work = NULL, ...) 
	       return(obj))



setMethod("resolve", c("character", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE,
                      xrefInfo = NULL, type = NULL, depth = 1L, work = NULL, ...) {
#if(!is.na(type)) browser()
    browser()
             els = strsplit(obj, ":")[[1]]

             if(length(els) > 1) {

               i = match(els[1], names(namespaces))
                if(!is.na(i)) {
                   tp = SchemaType(els[2], els[2], namespaces[[i]]$uri)
                   if(recursive)
                      tp = resolve(tp, context, namespaces, recursive, raiseError, xrefInfo, depth = depth + 1L, work = work, ...)
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
                               resolve(obj, ctxt, ctxt@namespaceDefs, recursive = FALSE, raiseError = FALSE, xrefInfo, depth = depth + 1L, work = work, ...)  #XXX was namespaces
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
    	        resolve(val, context, namespaces, recursive, raiseError, xrefInfo, depth = depth + 1L, work = work, ...)
              }
             else
                 val
           })


asQName = function(x) strsplit(x, ":")[[1]]

setMethod("resolve", c("AnySchemaType", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL, type = NULL, depth = 1L, work = NULL, ...) {
             obj
           })

setMethod("resolve", c("SimpleSequenceType", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL, type = NULL, depth = 1L, work = NULL, ...) {
#if(obj@name == "iType") browser()             
              obj@elType = resolve(obj@elType, context, namespaces, recursive, raiseError, xrefInfo = xrefInfo, type, depth = depth + 1L, work = work, ...)
              obj
            })



setMethod("resolve", c("SchemaType", "SchemaCollection"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL, type = NULL, depth = 1L, work = NULL, ...) {
               #XXX deal with the namespace.
               #!!! This will cause infinite recursion if there is no method for the specific type.
               # So now match the namespace....
              i = match(obj@nsuri, names(context))
              if(!is.na(i)) {
                val = context[[ i ]] [[ obj@name ]]
                if(identical(val, obj))
                  return(val)

                return(resolve(val, context, namespaces, recursive, raiseError, xrefInfo, type, depth = depth + 1L, work = work, ...))
              } else {

              }

              stop("problem")


              resolve(obj@name, context, namespaces, recursive, raiseError, xrefInfo, type, depth = depth + 1L, work = work, ...)
           })

setMethod("resolve", c("SchemaType", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL, type = NULL, depth = 1L, work = NULL,  ...) {
               #XXX deal with the namespace. 
              resolve(obj@name, context, namespaces, recursive, raiseError, xrefInfo, type, depth = depth + 1L, work = work, ...)
            })

setMethod("resolve", c("EnumValuesDef", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL, type = NULL, depth = 1L, work = NULL,  ...) {
             obj
            })

setMethod("resolve", c("RestrictedSetInteger", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL, type = NULL, depth = 1L, work = NULL,  ...) {
             obj
            })




.tmp <- function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL, type = NULL, depth = 1L,  work = NULL, ...) {
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
                   return(SchemaType(els[2], els[1]))
                 }
                  # Work with just the label, ignoring the namespace prefix.
                 els = els[2]
               } else if(length(els) == 1) {
                    # see if we are in the default namespace of XML schema
                 if(!is.na( idx <- match("", names(namespaces))) && namespaces[[ idx ]]$uri %in% getXSDSchemaURIs(all = TRUE))
                   return(SchemaType(els, nsuri = namespaces[[ idx ]]$uri))

               }

                    # deal with the namespace in the schema names.
               el = which(els[1] == names(context))
               if(length(el) == 0)
                   el = which(els[1] == gsub(".*:", "", names(context)))
            
            
               if(length(el) == 0 || is.na(el))  {
                 is.schema = sapply(context, is, "SchemaTypes")
                 if(any(is.schema)) {
#                   cat("SchemaTypes in c('character', 'list')\n")
                    tmp = lapply(context[is.schema], function(x) resolve(obj, x, recursive = recursive, raiseError = FALSE, xrefInfo = xrefInfo, type, depth = depth + 1L, ...))
                    i = sapply(tmp, is.null)
                    if(!all(i))
                      return(tmp[!i][[1]])
                 }

                 if(raiseError)
                     resolveError("Cannot resolve SchemaTypeReference ", els[1], " in list")
                 else
                     return(NULL)
               }
           
                  # if there are 2 or more entries with the same name, then pick the one that isn't an Element.
                  # i.e. get the SchemaType one.
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
              
               if(recursive && (is(ans, "ClassDefinition") || !is(ans, "TerminalSchemaType"))) {
#                  cat("Calling resolve recursively for", obj, "with", class(ans), "\n")
                  ans = resolve(ans, context, namespaces, recursive, raiseError, xrefInfo, type, depth = depth + 1L, work = work, ...)
                }

               ans
           }

setMethod("resolve", c("character", "list"), .tmp) # could be SchemaTypes rather than list.
setMethod("resolve", c("character", "SchemaTypes"), .tmp)


setMethod("resolve", c("SimpleElement", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL, type = NULL, depth = 1L, work = NULL, ...) {
             ans = new("Element")
             ans@name = obj@name ; ans@count = obj@count; ans@Rname = obj@Rname 
             ans@nsuri = obj@nsuri; ans@ns = obj@ns; ans@default = obj@default
             ans@documentation = obj@documentation
             
#             ans = obj
             if(length(obj@type))
                ans@type = resolve(obj@type, context, namespaces, recursive, raiseError, xrefInfo, type, depth = depth + 1L, work = work, ...)
             ans@attributes = lapply(obj@attributes, resolve, context, namespaces, recursive, raiseError, xrefInfo, depth = depth + 1L, work = work, ...)
             #ans@default = obj@default
             ans             
           })


setMethod("resolve", c("Element", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL, type = NULL, depth = 1L, work = NULL,  ...) {

#XXX temporary test
orig = obj  
    obj@type = resolve(obj@type, context, namespaces, recursive, raiseError, xrefInfo, type = notElementFun, depth = depth + 1L, work = work, ...)
    if(is(obj@type, "Element")) {
      obj@type@default = optionalDefaultValue(obj@type, obj@default)
      if(length(obj@count))
          obj@type@count = obj@count
      return(obj@type)
    }
    return(obj)

             
                   # could give infinite recursion. See msnSearch.wsdl and SearchResponse.
                   # So just look it up.
             if(!is(obj@type, "SchemaTypeReference"))
                return(obj@type)
             
             ans = lookupType(obj@type@name, context)
             ans = if(is.null(ans)) 
                     obj@type
                   else
                     ans

             if(recursive && is(ans, "SchemaTypeReference"))
                ans = resolve(ans, context, namespaces, recursive, raiseError, xrefInfo, type, depth = depth + 1L, work = work, ...)
             #ans
             obj@type = ans
             obj
           })



setMethod("resolve", c("LocalElement", "list"),
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE,
                         xrefInfo = NULL, type = NULL, depth = 1L, work = NULL,  ...)
           {
              tmp = resolve(obj@type, context, namespaces, recursive, raiseError,
                             xrefInfo, type, depth = depth + 1L, work = work, ...)
              obj@type = tmp
              if(!length(obj@type@default))
                  obj@type@default = obj@default

               sequenceOrAsIs( obj )
           })


setMethod("resolve", c("RestrictedStringDefinition", "list"), #XXX was list
           function(obj, context, namespaces = character(), recursive = TRUE, raiseError = TRUE, xrefInfo = NULL, type = NULL, depth = 1L, work = NULL,  ...) {
             obj
           })



notElementFun =
function(x)
   !is(x, "SchemaElement") && !is(x, "SchemaVoidType")
