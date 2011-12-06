SOAP.logical <-
function(x)
{
  val <- as.logical(x)

  if(is.na(val)) {
   val <- as.logical(as.integer(x))
  }

  val
}  


getNodeById <-
  #
  # Find the top-level node identified by id=value of id
  # Strip the `#' prefix off `id' first.
function(id, root)
{
  id <- gsub("^#", "", id)

  for(i in xmlChildren(root)) {
    k <- xmlAttrs(i)
    if(!is.null(k) && !is.na(match("id", names(k))))
      if(id == k[["id"]])
        return(i)
  }

  return(NULL)
}


      # Also need general converters that work on nodes
setGeneric("fromXML", function(node, root = NULL, converters = SchemaPrimitiveConverters,
                                append = TRUE, type = NULL, multiRefs = list(), namespaces = gatherNamespaceDefs(node))
                       {
	                    if(is(type, "BasicSOAPType") && length(formals(type@fromConverter)))
                              return(type@fromConverter(node))

                           standardGeneric("fromXML")
                       })


# Regardles of what the node is, convert a SOAPVoidType to NULL.
setMethod("fromXML", c(type="SOAPVoidType"),
 function(node, root = NULL, converters = SchemaPrimitiveConverters,
           append = TRUE, type = NULL, multiRefs = list(), namespaces = gatherNamespaceDefs(node))          
  {
     NULL
  }
)

setMethod("fromXML", "NULL",
 function(node, root = NULL, converters = SchemaPrimitiveConverters,
           append = TRUE, type = NULL, multiRefs = list(), namespaces = gatherNamespaceDefs(node))          
 {
   NULL
 })

setMethod("fromXML", c("XMLAbstractNode", type = "NULL"),
 function(node, root = NULL, converters = SchemaPrimitiveConverters,
           append = TRUE, type = NULL, multiRefs = list(), namespaces = gatherNamespaceDefs(node))          
 {
    xmlToList(node)
 })


setMethod("fromXML", c("XMLAbstractNode", type = "ArrayType"),
 function(node, root = NULL, converters = SchemaPrimitiveConverters,
           append = TRUE, type = NULL, multiRefs = list(), namespaces = gatherNamespaceDefs(node))          
 {
    fromSOAPArray(node, type = type, root, converters, multiRefs = multiRefs)
 })


if(FALSE) {
  # Take out for now.
setMethod("fromXML", c("character", "PrimitiveSOAPType"),
          function(node, root = NULL, converters = SchemaPrimitiveConverters, append = TRUE,
                    type = NULL, multiRefs = list(), namespaces = gatherNamespaceDefs(node))
          {
             node <- parseSOAP(node, asText = TRUE)
             fromXML(node[[1]], root = root, converters = converters, append = append, type = type,
                       multiRefs = multiRefs, namespaces = namespaces)
          })
}

# setOldClass("XMLNode")

setMethod("fromXML",
          c(node = "XMLAbstractNode", type = "PrimitiveSOAPType"), 
          function(node, root = NULL, converters = SchemaPrimitiveConverters, append = TRUE,
                    type = NULL, multiRefs = list(), namespaces = gatherNamespaceDefs(node)) {

            if(type@nsuri %in% getXSDSchemaURIs(all = TRUE)) 
              which <- match(type@name, names(converters))
            else
              which <- NA
              
            if(!is.na(which))
              val <- converters[[which]](xmlValue(node))
            else {
              val <- xmlValue(node)
              warning("Don't understand the primitive SOAP type `", type, "' yet")
            }

            val
          })


fromXML.SimpleSequence =
function(node, root = NULL, converters = SchemaPrimitiveConverters, append = TRUE, type = NULL,  multiRefs = list(), namespaces = gatherNamespaceDefs(node))
{
  tt = if(is.list(node))
          sapply(node,  fromXML, type = type@elType, namespaces = namespaces, converters = converters)
       else
          xmlSApply(node, fromXML, type = type@elType, namespaces = namespaces, converters = converters)

  if(is.atomic(tt))
     structure(tt, names = NULL) # strip the names.
  else
    tt
}  
setMethod("fromXML", c("XMLAbstractNode", type = "SimpleSequenceType"), fromXML.SimpleSequence)



  # Top-level entry point to convert a top-level SOAP XML node to an R object.
  #
fromXML.default = 
function(node, root = NULL, converters = SchemaPrimitiveConverters, append = TRUE, type = NULL,  multiRefs = list(), namespaces = gatherNamespaceDefs(node))
{

  if(is(type, "SimpleSequenceType")) {
     return(fromXML.SimpleSequence(node, root, converters, append, type, multiRefs, namespaces))
  } 

  
     # merge the specified converters with the standard ones.
     # This allows the caller either to bypass the standard converters with append = FALSE
     # or to merely add their own, overriding individual elements, if desired.
  if(!missing(converters) && append) {
    SchemaPrimitiveConverters[names(converters)] <- converters
    converters <- SchemaPrimitiveConverters
  }
  

  a <- xmlAttrs(node)
  origType = type
  if(!is.null(a)) {
    if(!is.na(match("null", names(a))))
      return(NULL)

    if(is.null(type) && !is.na(match("type", names(a))))
      type <- a[["type"]]

    if(!is.na(match("href", names(a)))) {
      id = substring(a[["href"]], 2)

      if(!is.na(match(id, names(multiRefs))))
        n <- multiRefs[[id]]
      else {
        n <- getNodeById(a[["href"]], root)
        if(is.null(n))
          stop("Can't find element ", a[["href"]])
      }
      
      return(fromXML(n, root = root, converters = converters, multiRefs = multiRefs, namespaces = namespaces))
    }

#    if(is.null(origType) && "arrayType" %in% names(a))
#      return(fromSOAPArray(node, root = root, converters = converters, multiRefs = multiRefs))
  }

  if(is.character(type)) {
    if(type %in% names(converters))
       return(converters[[type]](xmlValue(node)))
  }

  
  realType = is(type, "GenericSchemaType")

    # Dispatch to another method if possible.
  if(realType && type@name == "ArrayOfString" && type@nsuri == "http://www.w3.org/2001/XMLSchema" ) {
     return(unlist(xmlApply(node, xmlValue)))
  } else if(realType && type@name == "ArrayOfDouble" && type@nsuri == "http://www.w3.org/2001/XMLSchema" ) {
     return(as.numeric(unlist(xmlApply(node, xmlValue))))
  } else if(realType && type@name == "ArrayOfInt" && type@nsuri == "http://www.w3.org/2001/XMLSchema" ) {
     return(as.integer(unlist(xmlApply(node, xmlValue))))
  } else if(xmlName(node) == "Array" || (!is.null(type) && is.character(type) && type %in% c("SOAP-ENC:Array", "soapenc:Array"))) {
    return(fromSOAPArray(node, root = root, converters = converters, type = NULL, multiRefs = multiRefs, namespaces = namespaces)) # type = type
  } else if(xmlSize(node) > 1) {
    return(fromSOAPStruct(node, root = root, converters = converters, type = type, multiRefs = multiRefs, namespaces = namespaces))
  } else if(!is(type, "character"))
    return(fromXML(node, root, converters, type = type, multiRefs = multiRefs, namespaces = namespaces))




  if(is.null(type)) {
     # Want to check namespace is SOAP-ENC
     # and ideally don't want to prefix it with xsd:
     # but probably the most usual case is to get the typ
     # as xsd:type from the attribute and so that is the
     # default.
    type <- paste("xsd", xmlName(node), sep=":")
  } else if(is.na(match(type, names(converters))) && strsplit(type,":")[[1]][1] != "soapenc"){
     #XX Map soapenc:type to xsd:type
    type <- gsub("^soapenc:", "xsd:", type)
  }

  
   # This should now be done in the method for PrimitiveSOAPType
   # but we leave it here as it may get called from a path such as
   # for dates, etc.
  which <- match(type, names(converters))
  if(!is.na(which))
    val <- converters[[which]](xmlValue(node))
  else {
    val <- xmlValue(node)
    warning("Don't understand the SOAP type `", type, "' yet")
  }

  val
}

setMethod("fromXML", c(type="missing"), fromXML.default)
setMethod("fromXML", c(type="NULL"), fromXML.default)
setMethod("fromXML", c(type="character"), fromXML.default)
setMethod("fromXML", c(type="ANY", "ANY", "ANY"), fromXML.default)

setMethod("fromXML", c(type = "ArrayType"),
function(node, root = NULL, converters = SchemaPrimitiveConverters, append = TRUE, type = NULL, multiRefs = list(), namespaces = gatherNamespaceDefs(node))
{
  fromSOAPArray(node, type, root, converters, multiRefs = multiRefs, namespaces = namespaces)
})

setMethod("fromXML", c("XMLAbstractNode", type = "ClassDefinition"),
function(node, root = NULL, converters = SchemaPrimitiveConverters, append = TRUE, type = NULL, multiRefs = list(), namespaces = gatherNamespaceDefs(node))
{
    # Need to handle name here to get into S style?
  newSOAPClass(node, type@name, converters, type = type)
})



setMethod("fromXML", c("XMLAbstractNode", type = "Element"),
function(node, root = NULL, converters = SchemaPrimitiveConverters, append = TRUE, type = NULL,  multiRefs = list(), namespaces = gatherNamespaceDefs(node))
{

   # We should be able to dispatch to the SimpleSequenceType here, but it is not being picked up!
  # selectMethod("fromXML", c(class(node[[1]])[1], type = class(type@type)))
    # node or node[[1]] - was node[[1]] with type being a SimpleSequenceType.

   # Make certain to peel of the child that corresponds to the element.
   # We want to pass the node on to fromXML and not its contents
   # so that xmlSApply() for example can be used on the children in the SimpleSequenceType
  kid = node
  if(xmlName(kid) != type@name)
     kid = node[[type@name]]
  fromXML(kid, root, converters, append, type@type, multiRefs, namespaces = namespaces)
})



# Parse the type and dimensions of an array declaration. 

parseArrayType =
  # The goal here is to process a string giving a declaration for an array
  # and to extract the dimensions and the type
  # We need to be able to handle declarations of the form:
  #   xsd:string[0]
  #   xsd:string[]  
  #   xsd:string[2,3]
  #   xsd:string[2,3,]
  #   xsd:string[2,,]
  #   xsd:string[2,,3,,]    
  #   xsd:string[2][3]
  #  and xsd:string[2][]
  # We can use the ArrayType class to represent the result.

  #XXX Generate the elType for the ArrayType.
function(type, ns = character(), namespaces = list(), obj = new("ArrayType"),  targetNamespace = NA, elementFormDefault = NA)
{
  if(is.null(type) || length(type) == 0 || is.na(type) || type == "") {
     obj@elType = new("SOAPAnyType")
     return(obj)
  }
          
  
   # Get the type name, e.g. xsd:string
 # typeName = gsub("^\([^\[]*\)\\\[.*\\\]", "\\1", type, perl = TRUE)
 typeName = gsub("^([^[]*)\\[.*\\]", "\\1", type, perl = TRUE)
 
    # Get rid of the type name and work on the dimensions.
 dims = gsub("^[^[]*(\\[.*\\])", "\\1", type, perl = TRUE) 

    # Collapse [num][num][num] to num,num,num
    #XXX and also therefore [num,,num][num,,,num] to the concatenation
    #  num,,num,num,,,num
    # which is not necessarily the same thing.
 dims = sapply(strsplit(dims, "\\]"), function(x) paste(gsub("\\[", "", x), collapse= ","))

   # Get the individual dimension elements and fill in the missing ones with NAs
 els =
   lapply(dims, function(dim) {
      numCommas = sum("," == strsplit(dim, "")[[1]])
      els = strsplit(dim, ",")[[1]]
      els[els == ""] <- NA
      els = as.integer(els)
      if(numCommas == length(els))
        els <- c(els, NA)

      els
   })


   # Should we be returning a vector of dimensions
   # rather than a list of ArrayTypes.

    # Done here, and skip the last command - old version.
  elType = SOAPType(gsub("\\[.*$", "", type), namespaceDefs = namespaces)

  obj@elType = elType
  obj@elementType = typeName
  obj@dims = unlist(els)
  obj@nsuri = as.character(targetNamespace)

  return(obj)
}




fromSOAPArray <-
  # Need to handle the partial arrays
  # where individual elements are specified.
  #
  #  This doesn't handle multi-dimensional arrays, yet. We have the information now from the dimensions 
  #  of the type.
  #
  #  When we have a class defined for the array, make certain to return an instance of that class and not
  #  just list().  E.g. in the KEGG.wsdl, we have a case of SubType
  #
  #
function(node, type = NULL, root = NULL, converters = SchemaPrimitiveConverters, multiRefs = list(), simplify = TRUE,
           namespaces = gatherNamespaceDefs(node))
{
   # Get to the return value.
   #XXX "return" may not be the name of container node. We can read this from the WSDL.
  if(xmlSize(node) > 0 && xmlName(node[[1]]) == "return") 
     node = node[[1]]

  a <- xmlAttrs(node)  
  len = NA

  if(length(a) > 0 && "null" %in% names(a))
    return(NULL)
  
    # If we are not given a type, try to infer it from the attribute arrayType.
  if(is.null(type)) {
    type <- a[["arrayType"]]
    if(!is.null(type)) {

        #XXX  warning("Is xmlNamespace correct here")
      type = parseArrayType(type, namespaces = namespaces)   # ns = xmlNamespace(type))
      len = type@dims[length(type@dims)]  # Get the last one since this is the one that applies to this element.

      elType = type@elType
        #XXX We may want to leave this as an ArrayType, but we need parseArrayType() to create its elType
#      type = type@elementType
    }
  }

  origType = type
  classDef = NULL
  
  if(is(type, "ArrayType")) {
    type = type@elType
        # Probably want to capitalize this.
    classDef = getClassDef(paste("ArrayOf", type@name, sep = ""))
  }

  if(is(type, "PrimitiveSOAPType")) {
    type = if(length(type@ns) && nchar(type@ns[1])) paste(type@ns, type@name, sep = ":") else type@name
#XXX
    type = gsub("^soapenc:", "xsd:", type)
  }
  
  if(is.na(len))
    len = xmlSize(node)
  
  if(len == 0) {
        # Return, e.g., character(0)
    if(is.character(type) && type %in% names(zeroLengthArrays))
      return(zeroLengthArrays[[type]])
    else
      return(list())
  }

   # This is the general mechanism for dealing with offsets.
   # It doesn't work in S-Plus since it uses lexical scoping.
   # Need an OOP object for that.
  offset <- 1 
  if(!is.na(match("offset", names(a)))) {
    tmp <- gsub("\\[([0-9]+)\\]", "\\1", a[["offset"]])
    offset <- as.integer(tmp)
  }


    # Loop over the nodes and put them into a list. We have to respect 
    # any  position attributes in the nodes and place the elements into
    # the list at the corresponding  position. We use a closure to handle
    # the currrent offset. This could be done with 2 passes.

  
  ans <- vector("list", len)
  xmlApply(node, function(x, type = NULL, root = NULL, converters = SchemaPrimitiveConverters) {

                            # Convert the element.
                           z <- fromXML(x, type = type, root = root, converters = converters, multiRefs = multiRefs, namespaces = namespaces)

                            # Now figure out at what index it should be placed. 
                           a <- xmlAttrs(x)
                           if(!is.null(a) && !is.na(match("position", names(a)))) {
                             offset <- as.integer(a[["position"]]) + 1
                           }                           

                           ans[[offset]] <<- z

                            # Do we need to differentiate between this coming from
                            # the position or the global offset.
                            # In the case of position, each item will provide 
                            # its own position value.
                           offset <<- offset + 1

                           NULL
                        }, type = type, converters = converters, root = root)


   if(simplify && is.character(type) && mapsToRPrimitiveType(type)) {
      ans = unlist(ans)
      ans = setArrayClass(ans, origType)      
   } else  {
      if(!is.null(classDef))
         ans = new(classDef@className, ans)
        # the following should most likely go!!!!
       else
          ans = setArrayClass(ans, origType)
   }
  
  ans
}

setArrayClass =
  #
  # This attempts to make an object of the specified type
  # from the object ans. It basically takes the 
  # object ans and use the more specific class.
function(ans, origType)
{
   if(is(origType, "ArrayType") && length(origType@name) > 0) {
              # warning("Tell me that this happened")
      if(length(getClassDef(origType@name)))
         ans = as(ans, origType@name)
      else
         class(ans) = origType@name
    }

   ans
}


mapsToRPrimitiveType =
function(type)
{
  type %in% names(SchemaPrimitiveConverters)
}  
  

setGeneric("fromSOAPStruct",
           function(node, root = NULL, converters = SchemaPrimitiveConverters, type = NULL, multiRefs = list(), namespaces = gatherNamespaceDefs(node))
           standardGeneric("fromSOAPStruct"))


#
# We want to allow users to provide methods to control things.
# But we just have a SOAPType as the target. So if we create an instance of this
# and have methods defined for those types, then we will get the regular S4 dispatch.
#


setMethod("fromSOAPStruct", c("ANY", type = "SOAPType"),
function(node, root = NULL, converters = SchemaPrimitiveConverters, type = NULL, multiRefs = list(), namespaces = gatherNamespaceDefs(node)) {
   fromSOAPStruct(node, root, converters, type@name, multiRefs, namespaces)
})

setMethod("fromSOAPStruct", c("ANY", type = "character"),
function(node, root = NULL, converters = SchemaPrimitiveConverters, type = NULL, multiRefs = list(), namespaces = gatherNamespaceDefs(node))          
{
  typeName = type
  if(!is.null(getClassDef(typeName))) 
      fromSOAPStruct(node, root, converters, new(typeName), multiRefs, namespaces)
  else
      fromSOAPStruct_default(node, typeName, root, converters, multiRefs, namespaces)
})  

setMethod("fromSOAPStruct", c("ANY", type = "NULL"),
function(node, root = NULL, converters = SchemaPrimitiveConverters, type = NULL, multiRefs = list(), namespaces = gatherNamespaceDefs(node))
{
     a <- xmlAttrs(node)  
     if(!is.null(a) && !is.na(match("type", names(a)))) {
       typeName <- a[["type"]]
     } else
       typeName <- xmlName(node)

     fromSOAPStruct_default(node, typeName, root, converters, multiRefs, namespaces)
})


if(FALSE)
setMethod("fromSOAPStruct", "ANY",
function(node, root = NULL, converters = SchemaPrimitiveConverters, type = NULL, multiRefs = list(), namespaces = gatherNamespaceDefs(node))
{
   # See if there is a type="value" attribute which we will use
   # for the class.

  if(!is.null(type)) {
     typeName = type@name  
  } else {
     a <- xmlAttrs(node)  
     if(!is.null(a) && !is.na(match("type", names(a)))) {
       typeName <- a[["type"]]
     } else
       typeName <- xmlName(node)
  }

  # Now lookup the converters to see if there is an appropriate
  # handler for this type.

  # Otherwise, just use the default mechanism.
  fromSOAPStruct_default(node, typeName, root, converters, multiRefs)  
})

# Generic S4 fill in.
setMethod("fromSOAPStruct", "ANY",
function(node, root = NULL, converters = SchemaPrimitiveConverters, type = NULL, multiRefs = list(), namespaces = gatherNamespaceDefs(node))
{  
    val <- xmlApply(node, fromXML, root = root, converters = converters, multiRefs = multiRefs, namespaces = namespaces)
    obj = type
    for(i in names(val))  {
         # the names may differ because S4 has certain reserved words such as names which gets mapped to NAMES.
      slotName = if(i %in% ReservedSlotNames) toupper(i) else i

        # It is possible that the conversion has not been specific enough.
        # For example, the XML may tell us we have an array of string elements, e.g. xsd:string[5]
        # But in fact, we expect an instance of the class ArrayOfstring. So we do the conversion here.
      if(!is(val[[i]], class(slot(obj, slotName))))
         val[[i]] = as(val[[i]], class(slot(obj, slotName)))
      slot(obj, slotName) = val[[i]]
    }

    return(obj)
})

fromSOAPStruct_default =
function(node,  typeName, root = NULL, converters = SchemaPrimitiveConverters, multiRefs = list(),
           namespaces = gatherNamespaceDefs(node))
{
  val <- xmlApply(node, fromXML, root = root, converters = converters, multiRefs = multiRefs, namespaces = namespaces)
  class(val) <- gsub("^[a-zA-Z]+:", "", typeName)
  val  
}  
