importSchema =
function(node, baseURL, previouslyRead = NULL, verbose = FALSE, inline = TRUE)
{
   u =  xmlGetAttr(node, "schemaLocation", NA)
   if(is.na(u))
     return(NULL)

   u = getRelativeURL(u, baseURL)
   u = collapseURLPath(u)

   if(verbose)
     cat("processing schema", u, "from within", docName(node), "\n")
   
   if(!is.null(previouslyRead) && exists(u, previouslyRead)) {
      if(verbose)
        cat("already read", u, "\n")
      return(get(u, previouslyRead))
   } else {
      assign(u, structure(u, class = "XMLSchemaURL"), previouslyRead) # need to put in a promise.
   }
   
   ans = parseSchemaDoc(u, prevSchema = previouslyRead, verbose = verbose, inline = inline)

   if(!is.null(previouslyRead)) {
       assign(u, ans, previouslyRead)
       if(!is.null(tns <- xmlGetAttr(node, "namespace"))) {
#        cat("adding target namespace", tns, "for", u, "\n")
         val = get(".targetNamespaces", previouslyRead)
         val[u] = tns
         assign(".targetNamespaces", val, previouslyRead)
       }
   }
   ans
}

parseSchemaDoc =
  # e.g. "http://www.ncbi.nlm.nih.gov/entrez/eutils/soap/v2.0/efetch_pubmed.wsdl"
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
            prevSchema = new.env(), verbose = FALSE, inline = TRUE)
{
  if(is(url, "XMLAbstractDocument")) {
     doc = url
     baseURL = docName(url)
   } else {
     doc = xmlParse(url)
     baseURL = docName(doc)
   }

      # Force the prefix of the namespaces to be xs
  if(length(namespaces) == 0 || !("xs" %in% names(namespaces)))
      names(namespaces)[1] = "xs"


  
  if(followImports) {
     imports = getNodeSet(doc, "//xs:schema/xs:import", namespaces)

     imports = lapply(imports, processImport, baseURL, prevSchema, namespaces, verbose, inline = inline)
  }

  if(followIncludes) {

    includes = getNodeSet(doc, "//xs:schema/xs:include", namespaces)
    if(length(includes)) {
        sapply(includes, processInclude, baseURL, prevSchema, verbose)
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

# The next two functions are only called from the function above - parseSchemaDoc
# They were inlined, but easier to debug them as stand-alone.
processImport =
function(node, baseURL, prevSchema, namespaces, verbose = FALSE, inline = FALSE)
{
  xdoc = importSchema(node, baseURL, prevSchema, verbose = verbose, inline = inline)
  if(is.null(xdoc)) {
    removeNodes(node)
    return(NULL)
  }
  
  if(is(xdoc, "XMLSchemaURL"))
    return(xdoc)
  
  if(inline && !is.null(xmlRoot(xdoc))) {
    schema = getNodeSet(xdoc, "//xs:schema", namespaces)
       # The xmlClone below ensures that we leave the
       # document in tact and since we put it in prevSchema
       # this is a good thing. Otherwise that would be an empty
       # XML document in prevSchema.s
    sapply(schema, function(s) replaceNodes(node, xmlClone(xmlRoot(s))))
  } else
    xdoc
}

processInclude =
function(node, baseURL, prevSchema, verbose = FALSE)
{

    # Read the document in the <schema> node and then
    # replace any of the schema nodes in that document
  xdoc = importSchema(node, baseURL, prevSchema, verbose = verbose) #XXX No inline here!
  if(is(xdoc, "XMLSchemaURL"))
    return()
  schema = getNodeSet(xdoc, "//xs:schema", c(xs = "http://www.w3.org/2001/XMLSchema"))
  p = xmlParent(node)
  sapply(schema, function(s)
         addChildren(p, kids = xmlChildren(s)))
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
