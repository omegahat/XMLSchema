findTypeLoops =
  #
  # Find circularities in the definitions of the types, e.g.
  #  when A is defined as having an element of type B
  #  and B is defined as having an element of type A.
  #
  # What about A referring to A?
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

SchemaNSDefs = c("xs" = "http://www.w3.org/2001/XMLSchema", wsdl = "http://schemas.xmlsoap.org/wsdl/" )

directDependencies =
  #
  #  For all the top-level elements in any of the <schema> nodes, 
  #  find which types are used within those types, i.e. the classes/types we will need
  #  to define these.
  #  For each of these types, put the URI of the namespace in/for which they are defined
  #  so we can uniquely identify them if there are two types with the same name.
  #
function(doc, namespaces = XMLSchema:::gatherNamespaceDefs(its[[1]]),
          targetNamespace = xmlGetAttr(xmlRoot(doc), "targetNamespace")) # have to worry about nested schema
{
   q = "//xs:schema/*[not(local-name() = 'schema') and not(local-name() = 'annotation')] | //wsdl:types/*[not(local-name() = 'schema') and not(local-name() = 'annotation')]"
   
   its  = getNodeSet(doc, q, SchemaNSDefs)
#  if(is(doc, "XMLInternalElementNode") && xmlName(doc) == "types") {
#      # handle the case there are type definitions in <types> ...</types> in a
#  }

   if(is.null(namespaces))
     namespaces = XMLSchema:::gatherNamespaceDefs(its[[1]])

   names(its) = sapply(its, xmlGetAttr, "name")
    # end up with a list with each element being a character string containing the names of the elements
    # and the names of these elements the URI of the namespace in which they are defined.
   deps = lapply(its, function(x) xpathSApply(x, ".//xs:*[@type or @ref]", getSchemaNodeType,
                                               namespaces = SchemaNSDefs["xs"]))

   nameURIs = mapply(function(node, id) mapName(id, namespaces, findTargetNamespace(node)),
                      its, names(its))
   names(nameURIs) = sub("^[^.]*\\.", "", names(nameURIs))
        # nameURIs is just a character vector of the names of the data type and the names
        # on the vector are the namespace URIs.

        # Now loop over each data type and look at its dependencies and map these
# deps = lapply(deps, function(x) {
#                       v = mapply(function(x, ns) {
#                                    mapName(x, namespaces, ,ns)
#                                  }, x, names(x))
#                       i = !duplicated(unlist(v))
#                       structure(unlist(v[i]), names = sapply(v[i], names))
#                     })

   deps = lapply(deps, getUniqueTypes)

      # nameURIs
   list(dependencies = deps, nameURIs = nameURIs)
}

getUniqueTypes =
function(x)
{
  i = !duplicated(sprintf("%s:%s", names(x), x))
  x[i]
}


getSchemaRefNodeType =
function(node)
{
  ref = xmlGetAttr(node, "ref")
  els = getNodeSet(node, sprintf("//xs:*[name = %s]", ref), namespaces = SchemaNSDefs)
  
}

getSchemaNodeType =
function(node)
{
   type = xmlGetAttr(node, "type", xmlGetAttr(node, "ref"))
#   if(is.null(type))
#     return(getSchemaRefNodeType(node))
   el = strsplit(type, ":")[[1]]
   if(length(el) == 1)
     el = c("", el)
   
   ns = XMLSchema:::gatherNamespaceDefs(node)
   i = match(el[1], names(ns))
   if(FALSE && is.na(i)) {
      if(interactive())
         browser()
      else
        stop("NA computed")
    }

   structure(el[2], names = if(is.na(i)) "" else ns[[i]]$uri)
}

mapName =
function(id, namespaces, localNs = NA, defaultNS = "")
{
   els = strsplit(id, ":")[[1]]
   if(length(els) == 1) {
     return(structure(id, names = localNs)) # was defaultNS
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
  ns = names(els)
  id = paste(els, collapse = ".")
  new("CrossRefType", name = id, subclasses = els, nsuri = ns)
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
