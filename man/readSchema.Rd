\name{readSchema}
\alias{readSchema}
\title{Read the given XML schema and process the contents}
\description{
 This is the top-level function for reading an XML schema.
 It takes the name of the file to read or an already parse
 XML document (e.g. via \code{\link[XML]{xmlParse}} or
 \code{\link[XML]{xmlTreeParse}}) and 
  processes the nodes in the document to create R-level
  descriptions of the  schema's contents.
 
  The function can be given a WSDL document (Web Service Language Description)
  used for describing SOAP servers.  It then extracts the types node
  and uses that.
}
\usage{
readSchema(filename, createConverters = FALSE, verbose = FALSE,
            namespaces = c(ws = "http://schemas.xmlsoap.org/wsdl/",
                           xs = "http://www.w3.org/2001/XMLSchema"),
           followImports = TRUE, followIncludes = followImports,
           asNode = is(filename, "XMLInternalNode") &&
                     (is(filename, "AsIs") || xmlName(filename) == "schema"), 
            checkCircularTypes = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{the name of the XML document containing the schema or a previously
    parsed XML document. If this is a character string, it is passed to 
    \code{\link{parseSchemaDoc}} and on to \code{\link[XML]{xmlParse}}.
    This allows the value to be the name of a file, the contents of a file, a compressed file or a 
    URL, including a "GET" URL, i.e. a URL with parameters specified via \code{?} and separated
    by \code{&}.}
  \item{createConverters}{a logical value indicating whether, 
      for each type in the schema, to 
     create the functions that convert an XML node to a corresponding
      R object.
  }
  \item{verbose}{ a logical value indicating whether to emit information about
    the element currently being processed. This is passed to \code{\link{processSchemaTypes}}.}
  \item{namespaces}{ a character vector giving the \code{prefix = URI}
     pairs for namespaces to be used in searching the XML document
     for a schema types node.}
   \item{followImports,followIncludes}{see \code{\link{parseSchemaDoc}}.}
   \item{asNode}{a logical value that specifies whether the
     \code{filename} argument is an XML node or external content in a file
     or is an in memory string or is a schema XML node.}
   \item{\dots}{additional parameters that are passed on to }
   \item{checkCircularTypes}{a logical value that controls whether we
     check for circular references in the defitions of data types, e.g.
      where type B has a field of type A and type A has a field of type
      B or perhaps more indirectly via a separate type C.
    }
}
\value{
  A list containing the elements. 
  If the schema contains other schema, the result is a
  SchemaCollection.
}
\references{ 
 The XML schema specification at \url{http://www.w3.org/XML/Schema}.
 A tutorial at \url{http://www.w3schools.com/Schema/default.asp}.
 Book ``The definitive XML Schema'', Priscilla Walmsley, Prentice Hall.
}
\author{Duncan Temple Lang}
\seealso{ 
 \code{\link{processSchemaTypes}}
 \code{\link[XML]{xmlParse}}
}
\examples{
 f = system.file("samples", "egquery.xsd", package = "XMLSchema")
 x = readSchema(f, createConverters = FALSE)
 names(x)

 sapply(x, class)

 dir = system.file("samples", package = "XMLSchema")
 files = list.files(dir, pattern = "\\\.xsd$", full.names = TRUE)
 els = lapply(files, function(x) {
                        print(x)
                        readSchema(x, createConverters = FALSE, verbose = TRUE)
                      })
 sapply(els, length)
 sapply(els, names)

 library(RCurl)
 if(url.exists("http://www.weather.gov/forecasts/xml/SOAP_server/ndfdXMLserver.php?wsdl"))
    z = readSchema("http://www.weather.gov/forecasts/xml/SOAP_server/ndfdXMLserver.php?wsdl")
}
\keyword{IO}
\keyword{programming}
\concept{schema}

