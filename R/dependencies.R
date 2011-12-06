findTypeLoops =
  #
  # Find circularities in the definitions of the types, e.g.
  #  when A is defined as having an element of type B
  #  and B is defined as having an element of type A.
  #
  # This only finds direct cross-references, not via intermediate
  # classes.  We might implement this in the future.
  #
function(doc, deps = directDependencies(doc, namespaceDefs, ...), namespaceDefs = NULL, ...)
{
   if(is.character(doc))
      doc = XMLSchema:::parseSchemaDoc(doc)
  
    # Get the names in the form uri:name for both the names of the types being defined
    # and then of their dependencies.
   ids = deps$nameURIs
   ids = sprintf("%s:%s", names(ids), ids)

   dep = lapply(deps[[1]], function(x) sprintf("%s:%s", names(x), x))
   names(dep) = ids

   
   # Now loop over each type and return the names of the sub-elements
   # which are also names of types being defined at the top-level
   elOf = mapply(function(id, dep) intersect(dep, ids), ids, dep)

if(FALSE) {
  # make an incidence matrix
g = matrix(0, length(ids), length(ids), dimnames = list(ids, ids))
mapply(function(id, d) g[id, d] <<- g[id, d] + 1, names(elOf), elOf)
}


    # Now let's find the loops by seeing if any of the sub-elements
    # are defined so that they also have a sub element which is this one being defined.
   loops = mapply(function(id, els) {
                    if(length(els) > 0)
                       els[sapply(dep[els], function(x) id %in% x)]
                    else
                       character()
                  }, names(elOf), elOf)

    xloops = loops [ sapply(loops, length) > 0 ]
          
    xloops
}

directDependencies =
  #
  #  For all the top-level elements in any of the <schema> nodes, 
  #  find which types are used within those types, i.e. the classes/types we will need
  #  to define these. 
  #
function(doc, namespaces = XMLSchema:::gatherNamespaceDefs(its[[1]]),
          targetNamespace = xmlGetAttr(xmlRoot(doc), "targetNamespace")) # have to worry about nested schema
{
   q = "//xs:schema/*[not(local-name() = 'schema') and not(local-name() = 'annotation')] | //wsdl:types/*[not(local-name() = 'schema') and not(local-name() = 'annotation')]"
   
   its  = getNodeSet(doc, q, c("xs" = "http://www.w3.org/2001/XMLSchema", wsdl = "http://schemas.xmlsoap.org/wsdl/" ))
#  if(is(doc, "XMLInternalElementNode") && xmlName(doc) == "types") {
#      # handle the case there are type definitions in <types> ...</types> in a
#  }

   names(its) = sapply(its, xmlGetAttr, "name")
   deps = lapply(its, function(x) xpathSApply(x, ".//xs:*[@type]", getSchemaNodeType, namespaces = c("xs" = "http://www.w3.org/2001/XMLSchema")))

   nameURIs = mapply(function(node, id) mapName(id, namespaces, findTargetNamespace(node)),
                      its, names(its))
   names(nameURIs) = sub("^[^.]*\\.", "", names(nameURIs))
   
   deps = lapply(deps, function(x) {
                         v = mapply(function(x, ns) {
                                      mapName(x, namespaces, ns)
                                    }, x, names(x))
                         i = !duplicated(unlist(v))
                         structure(unlist(v[i]), names = sapply(v[i], names))})
   
   list(dependencies = deps, nameURIs = nameURIs)
}

getSchemaNodeType =
function(node)
{
   type = xmlGetAttr(node, "type")
   el = strsplit(type, ":")[[1]]
   if(length(el) == 1)
     el = c("", el)
   
   ns = gatherNamespaceDefs(node)
   i = match(el[1], names(ns))
   if(FALSE && is.na(i)) {
      if(interactive())
         browser()
      else
        stop("NA computed")
    }

   structure(type, names = if(is.na(i)) "" else ns[[i]]$uri)
}

mapName =
function(id, namespaces, localNs = NA, defaultNS = "")
{
   els = strsplit(id, ":")[[1]]
   if(length(els) == 1) {
     return(structure(id, names = defaultNS))
     els = c(defaultNS, els)
   }

   i = match(els[1], names(namespaces))
   if(is.na(i)) {
     if(is.na(localNs))
        stop("Problems matching namespace ", sQuote(els[1]))
     else
        ns = localNs
   } else
      ns = namespaces[[i[1]]]$uri

  structure(els[2], names = ns)
}


makeCrossRefType =
function(els, namespaceDefs = NULL)
{
  els = structure(gsub(".*:(.*)", "\\1", els), names = gsub("(.*):.*", "\\1", els))
  id = paste(els, collapse = ".")
  new("CrossRefType", name = id, subclasses = els)                                        
}


makeCrossRefTypes =
function(deps)
{
  ans = groupCrossRefTypes(deps)

  ans = lapply(ans, makeCrossRefType)
  names(ans) = sapply(ans, slot, "name")
  
  list(types = ans, crossRefNames = names(deps))
}

groupCrossRefTypes =
function(deps)
{  
   ans = list()
   for(i in names(deps)) {
      if(i %in% unlist(ans))
        next
      
      ans[[i]] = i
      for(j in names(deps)) {
         if(!all(j %in% unlist(ans))) {
            if(i %in% deps[[j]])
               ans[[i]] = c(ans[[i]], j)
         }
      }
   }

   lapply(ans, unique)
}
