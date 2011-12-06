\name{createSOAPConverter}
\alias{createSOAPConverter}
\alias{createSOAPConverter,ANY-method}
\alias{createSOAPConverter,ArrayType-method}
\alias{createSOAPConverter,ClassDefinition-method}
\alias{createSOAPConverter,Element-method}
\alias{createSOAPConverter,RestrictedListType-method}
\alias{createSOAPConverter,RestrictedStringDefinition-method}
\alias{createSOAPConverter,SimpleSequenceType-method}
\alias{createSOAPConverter,SOAPComplexType-method}
\alias{createSOAPConverter,UnionDefinition-method}
\alias{createSOAPConverter,SchemaCollection-method}
\alias{createSOAPConverter,SimpleElement-method}

\title{Define the conversion function for an XML representation of a
  SOAP type to the corresponding R type}
\description{
  This generic function and associated methods are used to
  create R functions that convert a SOAP data type given
  as XML to an R object.
}
\usage{
createSOAPConverter(type, namespaces, defs = NULL, types = list(), ...)
}
\arguments{
  \item{type}{the description of the SOAP type}
  \item{namespaces}{appears not to be used}
  \item{defs}{appears to be never used.}
  \item{types}{the descriptions of other data types in the schema(s)}
  \item{\dots}{additional parameters for methods}
}
\details{

}
\value{
}
\references{
}
\author{
Duncan Temple Lang
}

\seealso{
}
\examples{
}
\keyword{programming}
\keyword{interface}

