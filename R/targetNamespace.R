findTargetNamespace =
  #
  #  search for the targetNamespace attribute in the node and its ancestors.
  #
function(node, default = NA)
{
   while(!is.null(node)) {
     ans = xmlGetAttr(node, "targetNamespace", NA)
     if(!is.na(ans))
       return(ans)
     node = xmlParent(node)
  }

   default
}



getTargetNamespace =
  #
  # For a given XML node given by type, find the 
  # target namespace from the associated <schema>
  # as both the URI and the prefix.
function(type)
{
   ns = getNodeSet(type, ".//ancestor::xs:schema",  c(xs="http://www.w3.org/2001/XMLSchema"))
   if(length(ns) == 0)
       return(as.character(NA))
   
   tns = xmlGetAttr(ns[[length(ns)]], 'targetNamespace')

   if(length(tns) == 0)
       return(character())
   
   ns = xmlSearchNs(type, tns, asPrefix = FALSE)
   if(length(ns))
     names(tns) = names(ns)

   tns
}




getTypeNamespace =
function(typeNames, node)
{
  if(length(typeNames) == 0)
     return(as.character(NA))
  
  els = strsplit(unlist(typeNames), ":")
  prefixes = sapply(els, function(x) if(length(x) > 1) x[1] else NA)

  if(all(i <- is.na(prefixes)))
    return(prefixes)

  uris = as.character(rep(NA, length(prefixes)))

  uris[!i] = sapply(prefixes[!i], findNamespaceDefnByPrefix, node)

  names(uris) = prefixes
  uris
}

###############

findNamespaceDefnByPrefix =
function(prefix, node)
{
    #XXX Currently have to assume the namespace is on the schema element
  sc = getNodeSet(node, sprintf("./ancestor::xs:schema[namespace::%s]", prefix),
                    namespaces = c(xs="http://www.w3.org/2001/XMLSchema"))
  if(length(sc) == 0)
    return(as.character(NA))

    # Using the last node found.  Not guaranteed.
  defs = xmlNamespaceDefinitions(sc[[length(sc)]], simplify = TRUE)
  defs[prefix]
}

