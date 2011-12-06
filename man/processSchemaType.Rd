\name{processSchemaType}
\alias{processSchemaType}
\alias{processSchemaTypes}
\title{Assemble R descriptions of XML Schema types}
\description{
 These functions take an XML document or node and 
 process the details in the XML schema  to create an
 R description of the information about the types or
 type described with the schema. 
}
\usage{
processSchemaType(type, types, substitutionGroups = NULL, namespaceDefs = list(),
                   targetNamespace = character(), elementFormDefault = NA,
                    localElements = FALSE)
processSchemaTypes(node, doc = xmlDoc(node), namespaceDefs = gatherNamespaceDefs(node),
                    createConverters = FALSE, verbose = FALSE, types = NULL,
                     elementFormDefault = TRUE, targetNamespace = findTargetNamespace(node),
                      substitutionGroups = getSubstitutionGroups(doc),
                       checkCircularTypes = TRUE, ...)
}
\arguments{
  \item{type}{an XML node describing the schema type to be processed}
  \item{types}{the collection of all XML nodes in the schema which are used to 
   to find/resolve references in \code{type} and its sub-nodes to other types within the schema.}
  \item{node}{the top-leve XML node of the schema to be processed.}
  \item{doc}{the parsed XML document in which the schema are
    defined. This is a reference to a C-level document. See the XML
    package and \code{\link[XML]{xmlParse}}.}
  \item{substitutionGroups}{XXX}
  \item{namespaceDefs}{the name space \code{prefix = URI} pairs that are in effect for the document and its nodes.
     This can be a named character vector or it might also be a list of \code{XMLNamespaceDefinition} objects
     The nodes may (re)define prefixes, but these give us the top-level pairs.
   }
  \item{createConverters}{a logical value indicating whether, 
      for each type in the schema, to 
     create the functions that convert an XML node to a corresponding
      R object.
  }
  \item{verbose}{ a logical value indicating whether to emit information about
    the element currently being processed. This is passed to
    \code{\link{processSchemaTypes}}.}
  \item{targetNamespace}{the name of the default namespace for the
    schema. This is used when creating the nodes for elements coming
    from this schema.
   }
  \item{elementFormDefault}{a logical value or string such as
    "qualified" that comes from the schema-level node and indicates
    whether the elements should  use the targetNamespace or not.
  }
  \item{localElements}{a logical value that controls whether element
    terms in the schema are considered to be global or local
    (\code{TRUE}).
     Local elements are those that are not at the top-level of the schema.
   }
   \item{\dots}{additional arguments passed on to
     \code{\link{readSchema}}.}
   \item{checkCircularTypes}{a logical value that controls whether we
     check for circular references in the defitions of data types, e.g.
      where type B has a field of type A and type A has a field of type
      B or perhaps more indirectly via a separate type C.
    }
}
\details{

}
\value{
 \code{processSchemaType} returns an object derived from \code{SOAPType}.

 \code{processSchemaTypes} returns a list.
}
\references{
 The XML schema specification at \url{http://www.w3.org/XML/Schema}.
 A tutorial at \url{http://www.w3schools.com/Schema/default.asp}.
 Book ``The definitive XML Schema'', Priscilla Walmsley, Prentice Hall.
} 
\author{Duncan Temple Lang}
\seealso{
 \code{\link{readSchema}}
 The \code{SSOAP} package and processing a WSDL document.
}
\examples{

}
\keyword{IO}
\keyword{programming}
\keyword{data}
\concept{XML}
\concept{Meta-programming}
