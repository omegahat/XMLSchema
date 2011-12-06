\name{parseSchemaDoc}
\alias{parseSchemaDoc}
\title{Parse an XML schema document and replace the include and import nodes}
\description{
  This function parses an XML document and then looks for
  import and include nodes that are part of an XML schema
  (i.e. in the appropriate namespace identified by \code{namespaces}).
  For any such nodes, it "substitutes" those with the contents
  of the referenced schema in the \code{schemaLocation} attribute
  (assuming there is one).
}
\usage{
parseSchemaDoc(url, removeComments = TRUE,
               namespaces = c(xs = "http://www.w3.org/2001/XMLSchema"),
               followImports = TRUE, followIncludes = followImports,
               prevSchema = new.env())
}
\arguments{
  \item{url}{the location or content of the XML document.
      This can be a URL, name of a regular or compressed file, or the
      XML content itself.}
    \item{removeComments}{ this controls whether comment nodes are
      removed from the document after it is read in.
      This can simplify the processing of the schema.
     }
  \item{namespaces}{a character vector giving the name space definitions which are
     used to identify the standard XML schema. Since there are different versions of
     the XML schema specification, i.e. 1999 and 2001, we allow the user to specify
     the namespace URI. The prefix/name is irrelevant as we change this to "xs"
    }
  \item{followImports}{a logical value indicating whether to process 
      xs:import nodes within the resulting document 
      by reading the referenced document and inserting its contents into this document}
  \item{followIncludes}{a logical value indicating whether to process 
      xs:import nodes within the resulting document 
      by reading the referenced document and inserting its contents into
      this document}
   \item{prevSchema}{an environment which is used to contain the type
     definitions for other schema being processed}
}

\value{
  An XMLInternalDocument.
}
\author{Duncan Temple Lang}
\seealso{
 \code{\link[XML]{xmlParse}}
}
\examples{
}
\keyword{IO}
\keyword{programming}
