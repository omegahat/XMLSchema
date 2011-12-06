
SOAPType =
  #
  # Map a name such as xsd:string or xsd:date to a SOAPType object
  #
function(name, ns = "", nsuri = "", namespaceDefs = list(), counts = numeric(), obj = new("SOAPTypeReference"), targetNamespace = "") # was SOAPType
{
  if(is.null(name)) 
    return(new("SOAPVoidType"))


   # If the name is in the form of namespace:type, split it
   # and assign to name and ns separately.
   # e.g. xsd:int will become name = "int", ns = "xsd"
  name = strsplit(name, ":")[[1]]
  
  if(length(name) == 2) {
    if(missing(ns))
       ns = name[1]
    name = name[2]        
  } #else if(missing(nsuri)) 
    # nsuri = targetNamespace
    

        # resolve namespace. Check if the nsuri we are passed
        # corresponds to that in the name.
        # Possibly not as the nsuri comes from the defn of the element
        # and the the name comes from the type="name" of the element.
    # if we have a non-trivial ns and no nsuri, then resolve the nsuri.
  if(missing(nsuri) && length(namespaceDefs)) {  #XXX do we want ns != ""     && ns != ""
     idx = match(ns, names(namespaceDefs))
     if(is.na(idx) && any(names(namespaceDefs) == ""))
        idx = which(names(namespaceDefs) == "")

     if(!is.na(idx)) 
         nsuri = namespaceDefs[[idx]]$uri
   }

#######  Should be removed if(!is.character(nsuri))
   nsuri = as(nsuri, "character")

      #XXXX if we don't know the mapping of xsd (and xsi?) at run time
      # let them pass through here!
  if(length(nsuri) && !is.na(name) && nsuri %in% c(getXSDSchemaURIs(all = TRUE), "xsd", "http://schemas.xmlsoap.org/soap/encoding/")) {
       # tmp = paste(ns, name, sep=":")
    which = sapply(XMLSchemaTypes, function(x) name == x[["type"]])
    if(any(which)) {
       k = XMLSchemaTypes[[which(which)]][["soapClass"]]
       obj = new(if(!is.null(k)) k else "PrimitiveSOAPType")
       funName = paste("as", names(XMLSchemaTypes)[which], sep=".")
       if(!exists(funName))
         funName = names(XMLSchemaTypes)[which]
       if(exists(funName))
         obj@toConverter = get(funName)
       tmp = XMLSchemaTypes[which][[1]]
       if("from" %in% names(tmp)) {

           if(is.character(tmp$from)) {
              if(length(tmp$from) > 1) {
                tmp$from = get(tmp$from[1], paste("package", tmp$from[2], sep = ":"))
              } else
                tmp$from = get(tmp$from)   
           }          
           obj@fromConverter = tmp$from
       }
    }
  } else {
     # Should check that this is referring to the target namespace of the
     # document, e.g. targetNamespace and xmlns:[targetNamespace] in the root node
#XXX
     # obj = new("SOAPTypeReference")
  }

  obj@name = name
  obj@ns = ns
  obj@nsuri = nsuri
  obj@count = counts

  obj
}



discardNamespace <-
  # utility function that removes the namespace prefix from a string.
  # e.g.  foo:tagName returns tagName.
  #
  # Or  gsub("^.*:", "", type)
function(str)
{
  x = strsplit(str,":")[[1]]
  if(length(x) > 1)
    x[2]
  else
    x
}  


ArrayType =
function(elementType, ns = character(), namespaceDefs = list(), obj = new("ArrayType"),
          targetNamespace = NA, elementFormDefault = NA)
{
  return(parseArrayType(elementType, ns = ns, namespaces = namespaceDefs, obj = obj, targetNamespace = targetNamespace, elementFormDefault = elementFormDefault))
                            
}  

ReservedSlotNames = c("names", "class")

ClassDef =
  #
  #  Fill in obj with the details of a ClassDefinition or a UnionDefinition
  # 
function(name, slotTypes, uris = as.character(rep(NA, length(slotTypes))),
          obj = new("ClassDefinition"), targetNamespace = NA, elementFormDefault = NA)
{
  obj@name = name

  ids = names(slotTypes)
  if(length(slotTypes) && any(is.na(ids)))
         # Potentially need a ListOf rather than the element types here.
      names(slotTypes)[is.na(ids)] = sapply(slotTypes[is.na(ids)], getName, compute = TRUE)

  obj@slotTypes = slotTypes


  if(!is.na(elementFormDefault) && (elementFormDefault || elementFormDefault == "qualified"))
     obj@uris = rep(as.character(targetNamespace), length(slotTypes))
  else
     obj@uris = as(uris, "character")

   obj@nsuri = as.character(targetNamespace)

    # fix up reserved S4 slot identifiers, e.g. names()
  i = match(names(obj@slotTypes), ReservedSlotNames)
  if(any(!is.na(i)))
     names(obj@slotTypes)[!is.na(i)] = toupper(names(obj@slotTypes)[!is.na(i)])
  
  obj
}

UnionDef =
function(name, slotTypes, uris = as.character(rep(NA, length(slotTypes))), obj = new("UnionDefinition"),
         targetNamespace = NA, elementFormDefault = NA)
{
   ClassDef(name, slotTypes, uris, obj = obj, targetNamespace = targetNamespace, elementFormDefault = elementFormDefault)
}





####################################
# For raising typed conditions (errors and warnings)
if(FALSE) {
stop =
function(..., class = character())
{
   base::stop(...)  
   e = simpleError(paste(..., collapse = ""))
#   class(e) = c(class, class(e))
   base::stop(e)
}

warning =
function(..., class = character())
{
  base::warning(...)
  return(FALSE)
   e = simpleWarning(paste(..., collapse = ""))
#  class(e) = c(class, class(e))
   base::warning(e)
}
}



