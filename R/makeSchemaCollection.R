#############################################

processSchemaTypes =
  #
  # This is a different version than in processSchemaTypes.R and is currently experimental (Fri Aug 19 12:43:27 2011)
  # The idea is to look at the nodes and split them into schema and just types and the process the
  # schema to get SchemaTypes objects and
  #
  # XXX Have to handle the case where introduce 2 types for one node. Doesn't happen for <schema> nodes
  # so can still do the loop over the other elements
  #
function(node, doc = xmlDoc(node), namespaceDefs = gatherNamespaceDefs(node), createConverters = FALSE, verbose = FALSE,
          types = NULL, elementFormDefault = TRUE, targetNamespace = findTargetNamespace(node),
            substitutionGroups = getSubstitutionGroups(doc), checkCircularTypes = TRUE)
{

  if(is.null(node))
    return(new("SchemaCollection"))

  
  if(xmlName(node) == "schema") {
        # what about if nested schema. Shall we flatten the hierarchy?    
      def = processSchemaNodes(xmlChildren(node), namespaceDefs = namespaceDefs,
                                createConverters = createConverters, verbose = verbose,
                                 types = types, elementFormDefault = xmlGetAttr(node, "elementFormDefault", TRUE, function(x) x == "qualified"),
                                  targetNamespace = xmlGetAttr(node, "targetNamespace"))
   } else {

       ids = names(node)
       isSchema = ids %in% c("schema", "import")
       tns = sapply(node[isSchema], xmlGetAttr, "targetNamespace")
       schema = mapply(function(node, tns)
                         processSchemaTypes(node, targetNamespace = tns, checkCircularTypes = checkCircularTypes),
                       node[ names(node) == "schema"], tns)
       schema = new("SchemaCollection", schema)

       other = processSchemaNodes(node[!isSchema], namespaceDefs, createConverters, verbose, NULL, elementFormDefault,
                                   targetNamespace, substitutionGroups = substitutionGroups)
       if(length(other)) {
          ns = findTargetNamespace(node)
          el = new("SchemaTypes", other, namespaceDefs = namespaceDefs, targetNamespace = ns)
          schema[[ ns  ]] = el
       }

       def = schema
   }

   if(FALSE && is(def, "SchemaTypes") && any(w <- sapply(def, is, "SchemaTypes"))) {
       co = new("SchemaCollection", def[w])
#browser()       
       co[[ findTargetNamespace(node) ]] = new("SchemaTypes", def[ !w ], namespaceDefs = def@namespaceDefs, targetNamespace = def@targetNamespace)
       
       def = co
   }

   def
}


processSchemaNodes =
function(node, namespaceDefs = gatherNamespaceDefs(node), createConverters = FALSE, verbose = FALSE,
          types = NULL, elementFormDefault = xmlGetAttr(node, "elementFormDefault", "qualified") == "qualified",
          targetNamespace = xmlGetAttr(node, "targetNamespace", as.character(NA)),
          substitutionGroups = getSubstitutionGroups(xmlDoc(nodes[[1]])),
          nodes = xmlChildren(node), checkCircularTypes = checkCircularTypes)
{
  if(length(nodes) == 0)
     return(new("SchemaTypes"))
  
  ans = vector("list", length(nodes))
  names = character(length(ans))

  doc = xmlDoc(nodes[[1]])  

  for(i in seq(along = ans)) {
                   el = nodes[[i]]

                   if(inherits(el, c("XMLCommentNode", "XMLInternalCommentNode"))
                         || xmlName(el) %in% c("import", "include"))
                     next

                   if(xmlName(el) %in% c("annotation", "schema"))
                     next
                   
                   if(xmlName(el) == "schema") {
                         # pubmed from NCBI has a <schema><schema targetNamespace="">...
                      if(xmlSize(el) == 1 && names(el) == "schema")
                        el = el[[1]]
                     
                      ns = xmlGetAttr(el, "targetNamespace")
                      if(verbose)
                         cat("processing (sub) schema", ns, "\n")

                      qualified = xmlGetAttr(el, "elementFormDefault", NA, function(x) x == "qualified")
                                 #XXX removed the namespaceDefs in the call so we compute them for this schema.
                      o = processSchemaTypes(el, doc, createConverters = FALSE, verbose = verbose, types = ans,
                                               targetNamespace = ns, elementFormDefault = qualified, checkCircularTypes = checkCircularTypes)

                      if(!is(o, "SchemaTypes") && !is(o, "SchemaColection"))
                          o = new("SchemaTypes", o, namespaceDefs = namespaceDefs) #, targetNamespace = targetNamespace)
                      
                      o@elementFormQualified = qualified
                      if(length(o)) {
                          ans[[i]] <-  o
                          if(length(ns) || length(ns <- xmlGetAttr(el, "namespace"))) { #??? targetNamespace or namespace?
                             names[i] <- ns
                             names(ans) <- names
                          }
                      }
                   } else {
                      n = xmlGetAttr(el, "name", as.character(NA))
                      if(verbose)
                         cat(i,")", n, " (", xmlName(el), ")\n")

                      o = processSchemaType(el, substitutionGroups = substitutionGroups,
                                            namespaceDefs = namespaceDefs,
                                            types = ans, targetNamespace = targetNamespace,
                                            elementFormDefault = elementFormDefault)
                      
                      if(is.null(o))
                         next
                      
                      if(FALSE && createConverters && is(o, "BasicSOAPType"))
                         o@fromConverter = createSOAPConverter(o, ans)

                      ans[[i]] <- o
                      names[i] <- n # o@name By using n and not o@name, we avoid the case where we are
                                    # working on a complexType that is returned in o as an Element/LocalElement.
                                    # o@name would then be the name of that element. This happens in pugi_soap_cgi from NIH
                                    # for AnyKeyType which is a <complexType><sequence><element/></sequence></complexType>
                      names(ans) <- names

                      if(FALSE && is(o, "Element") && xmlName(el) == "complexType") {
                        #browser()
                          ans[[ length(ans) + 1]] <- o
                          names[length(names)+1] = n
                          names(ans) <- names
                       }
                      
                    }


                   NULL
        }

   names(ans) = names
   ans = ans[ ! sapply(ans, is.null) ]
   s = new("SchemaTypes", ans, namespaceDefs = namespaceDefs, elementFormQualified = elementFormDefault)
   if(!is.null(targetNamespace)) s@targetNamespace = targetNamespace
   s
}


processSchemaTypes = 
function(node, doc = xmlDoc(node), namespaceDefs = gatherNamespaceDefs(node), createConverters = FALSE, verbose = FALSE,
          types = NULL, elementFormDefault = TRUE, targetNamespace = findTargetNamespace(node),
            substitutionGroups = getSubstitutionGroups(doc), checkCircularTypes = TRUE, ...)
{

  if(is.null(node))
    return(new("SchemaCollection"))

  schemaNodes = getNodeSet(node, ".//xsd:schema[@targetNamespace]", c(xsd = "http://www.w3.org/2001/XMLSchema"))
  
  els = lapply(schemaNodes, processSchemaNodes, createConverters = createConverters, verbose = verbose, substitutionGroups = substitutionGroups, checkCircularTypes = checkCircularTypes)
  names(els) = ids = sapply(schemaNodes, xmlGetAttr, "targetNamespace")

  if(is(node, "XMLInternalElementNode") && xmlName(node) == "types" && any(w <- !(names(node) %in% c("text", "schema")))) {
     tns = findTargetNamespace(node)
     loose = processSchemaNodes(node, nodes = node[w], namespaceDefs = gatherNamespaceDefs(node), targetNamespace = tns,
                                 createConverters = createConverters, verbose = verbose,
                                substitutionGroups = substitutionGroups, checkCircularTypes = checkCircularTypes)
     if(tns %in% names(els))
       els[[tns]][names(loose)] =  loose
     else
       els[[tns]] = loose
  }
    

  if(xmlName(node) == "schema") {
     local = processSchemaNodes(node, createConverters = createConverters, verbose = verbose, substitutionGroups = substitutionGroups, checkCircularTypes = checkCircularTypes)
     els[[ xmlGetAttr(node, "targetNamespace", length(els) + 1L) ]]  = local
     if(length(els) > 1)
        els = els[c(length(els), 1:(length(els)-1))]
  }

#??? Should we return a SchemaTypes if there is just one. Will fixTypeNames handle just one. (Typically) No need to do it?

  ans = new("SchemaCollection", els)
  if(checkCircularTypes && length(els) > 0) {
    nsDefs = unlist(lapply(ans, function(x) x@namespaceDefs), recursive = FALSE)
    names(nsDefs) = sapply(nsDefs, function(x) x$id)
    ans@circularDefs = makeCrossRefTypes(findTypeLoops(node, namespaceDefs = nsDefs))

    ans = fixTypeNames(ans, ...)
  }
  

  ans

}

