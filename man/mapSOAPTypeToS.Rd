\name{mapSOAPTypeToS}
\alias{mapSOAPTypeToS}
\title{Determine name of R class for an XML data type description}
\description{
  This class is used to map a description of an XML schema type
  to the name of an R class, either an existing class or one
  that would be defined to implement the XML data type.
  These are used to compute the name of a new class and
  the types of the slots for a new class class definition.
}
\usage{
mapSOAPTypeToS(type, types = list(), namespaceDefs = list())
}
\arguments{
  \item{type}{an instance of a class derived from
    \code{GenericSchemaType}  that describes the XML data type.}
  \item{types}{a \code{SchemaCollection} object that is a collection of
    schema descriptions, each containing a collection of
    GenericSchemaType objects.}
  \item{namespaceDefs}{a collection of namespace definitions of prefix
    and URIs. This now comes from each \code{SchemaTypes} object in the \code{SchemaCollection}.}
}
\value{
 a character string
}
\author{
Duncan Temple Lang
}


\seealso{}

\examples{
 def = new("ClassDefinition", name = "Foo",
                              slotTypes = list(a = new("PrimitiveSOAPType",  name = "string"),
                                               b = new("PrimitiveSOAPType",  name = "float") ))               

  mapSOAPTypeToS(def)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{data}
\keyword{programming}
\keyword{interface}


