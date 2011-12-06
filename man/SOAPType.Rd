\name{SOAPType}
\alias{SOAPType}
\title{Creates a SOAPType object}
\description{
  This is a constructor function for filling in the
  general/common details  of the \code{\link{SOAPType-class}}
  objects.
}
\usage{
SOAPType(name, ns = "", nsuri = "", namespaceDefs = list(), counts = numeric(),
          obj = new("SOAPTypeReference"), targetNamespace = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{the name of the schema type (i.e. of the type or element),
    usually the value of the \code{name} attribute}
  \item{ns}{the namespace prefix from the XML type}
  \item{nsuri}{the namespace URI}
  \item{namespaceDefs}{the namespace definitions, i.e. \code{prefix =
      URI} values in effect for the XML element. This allows us to use
    and resolve  the regular XSD schema, etc. URIs}
  \item{counts}{a vector giving the minOccurs and maxOccurs values for
    the type.  See \code{getElementCount} which is a non-exported
    convenience function.}
  \item{obj}{the object whose slots are to be filled in. The user can
    pass this when creating an instance of a class derived from
    \code{SOAPType}.}
  \item{targetNamespace}{a string providing the value of the targetNamespace attribute
    of the schema being processed.}
}
\value{
  The value \code{obj} with slots updated.
}
\author{Duncan Temple Lang}
\seealso{
  \code{\link{SOAPType-class}}
  \code{\link{processSchemaTypes}}
  \code{\link{readSchema}}  
}
\examples{
  SOAPType("foo", count = c(1, Inf))
}
\keyword{programming}
