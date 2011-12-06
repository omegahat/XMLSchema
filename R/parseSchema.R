importSchema =
function(node, baseURL, previouslyRead = NULL, verbose = FALSE)
{
   u =  xmlGetAttr(node, "schemaLocation", NA)
   if(is.na(u))
     return(NULL)

# if(length( grep("smil20", u )))   browser()
   
   u = getRelativeURL(u, baseURL)
   u = collapseURLPath(u)

   if(verbose)
     cat("schema", u, "\n")
   
   if(!is.null(previouslyRead) && exists(u, previouslyRead)) {
      if(verbose)
        cat("already read", u, "\n")
      return(get(u, previouslyRead))
   } else {
      assign(u, structure(u, class = "XMLSchemaURL"), previouslyRead) # need to put in a promise.
   }
   
   ans = parseSchemaDoc(u, prevSchema = previouslyRead)

   if(!is.null(previouslyRead))
       assign(u, ans, previouslyRead)
   ans
}

parseSchemaDoc =
  # "http://www.ncbi.nlm.nih.gov/entrez/eutils/soap/v2.0/efetch_pubmed.wsdl"
  #
  # Use to use the following two lines:
  #  (with handlers = WSDLParseHandlers(fileName))
  #  library(XML)
     # Parse the WSDL file into a tree which we will pick apart.
     # Could put handlers to convert the elements on the fly.
     #XXX Use internal nodes with xmlInternalTreeParse
  # doc = xmlRoot(xmlTreeParse(fileName, handlers = handlers,  asTree = TRUE, fullNamespaceInfo = TRUE))
  #
function(url, removeComments = TRUE, namespaces = c(xs = "http://www.w3.org/2001/XMLSchema"),
           followImports = TRUE, followIncludes = followImports,
            prevSchema = new.env())
{
  if(is(url, "XMLAbstractDocument")) {
     doc = url
     baseURL = dirname(docName(url))
   } else {
     doc = xmlParse(url)
     baseURL = dirname(docName(doc))
   }

      # Force the prefix of the namespaces to be xs
  if(length(namespaces) == 0 || !("xs" %in% names(namespaces)))
      names(namespaces)[1] = "xs"


  
  if(followImports) {
     imports = getNodeSet(doc, "//xs:schema/xs:import", namespaces)

     imports = lapply(imports, function(node) {
                              xdoc = importSchema(node, baseURL, prevSchema)
                              if(is.null(xdoc)) {
                                 removeNodes(node)
                                 return(NULL)
                              }
                              if(is(xdoc, "XMLSchemaURL"))
                                 return(xdoc)
                              if(!is.null(xmlRoot(xdoc))) {
                                 schema = getNodeSet(xdoc, "//xs:schema", namespaces)
                                       # The xmlClone below ensures that we leave the
                                       # document in tact and since we put it in prevSchema
                                       # this is a good thing. Otherwise that would be an empty
                                       # XML document in prevSchema.s
                                 sapply(schema, function(s) replaceNodes(node, xmlClone(xmlRoot(s))))
                              }
                            })
  }

  if(followIncludes) {

    includes = getNodeSet(doc, "//xs:schema/xs:include", namespaces)
    if(length(includes)) {
        sapply(includes, function(node) {
                             xdoc = importSchema(node, baseURL, prevSchema)
                             if(is(xdoc, "XMLSchemaURL"))
                               return()
                             schema = getNodeSet(xdoc, "//xs:schema", c(xs = "http://www.w3.org/2001/XMLSchema"))
                             p = xmlParent(node)
                             sapply(schema, function(s) addChildren(p, kids = xmlChildren(s)))
                         })
        removeNodes(includes) # , TRUE)
      }
  }

  if(removeComments) {
     comments = getNodeSet(doc, "//comment()", c(xs="http://www.w3.org/2001/XMLSchema"), noMatchOkay = TRUE)
     if(length(comments))
        removeNodes(comments)
   }
  
  doc
}


collapseURLPath =
function(u)
{
   n = nchar(u)
   while(TRUE) {
       u = gsub("/\\./", "/", u)
       if(nchar(u) == n) {
         break
       }
       n = nchar(u)
   }
   u
}
