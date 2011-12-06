\name{coerceArgumentCode}
\alias{coerceArgumentCode}
\title{Create R code to coerce an R value to a value appropriate for an
  XML Schema type}
\description{
  This function is used to generate an R expression/command
  that will coerce an R value to another R value
  that corresponds to an XML schema type.
  The resulting value can then be serialized to XML, e.g. in a Web
  service (e.g. SSOAP) request

  This function is used when programmatically generating
  R functions to interface to Web services and other
  facilities which involve serializing R values from users
  to XML.
}
\usage{
coerceArgumentCode(id, type)
}
\arguments{
  \item{id}{the name of the variable to be coerced}
  \item{type}{the \code{\link{SOAPType-class}} describing the XML schema
    type expected. }
}
\value{
  A string or R expression giving the  R command to convert
  \code{id}.
}
\author{Duncan Temple Lang}
\seealso{

}
\examples{
  tt = SOAPType("int", nsuri = "http://www.w3.org/2001/XMLSchema", obj = new("PrimitiveSOAPType"))
  coerceArgumentCode("x", tt)

  tt = SOAPType("string", nsuri = "http://www.w3.org/2001/XMLSchema", obj = new("PrimitiveSOAPType"))
  coerceArgumentCode("x", tt)

  tt = SOAPType("Hybrid", nsuri = "http://www.w3.org/2001/XMLSchema", obj = new("SOAPType"))
  coerceArgumentCode("x", tt)
}
\keyword{programming}
\keyword{IO}
