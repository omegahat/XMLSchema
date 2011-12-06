\name{XMLSchemaTypes}
\alias{XMLSchemaTypes}

\alias{coerce,Base64Encoded,character-method}
\alias{coerce,Base64Encoded,raw-method}
\alias{coerce,SOAPElementConverter,AsFunction-method}
\alias{coerce,character,NMTOKEN-method}
\alias{coerce,character,XMLTypeConverter-method}
\alias{coerce,integer,Base64Encoded-method}
\alias{coerce,numeric,nonNegativeInteger-method}
\alias{coerce,numeric,positiveInteger-method}


\docType{data}
\title{Table of information for built-in primitive data types in XML schema}
\description{
  This is a "table" or "database" that provides information
  for the different built-in primitive data types defined within the
  XML schema specification and how these relate to R data types.
  This is a list with entries for each of the primitive data types
  and identifies the corresponding R class/type (e.g. "character",
  "numeric",  "SOAPDate") that can be used as the type for a slot in an R class
  to represent that type, and a mapping between the
  XSD schema type name and

  This needs to be formalized more.
}
%\usage{data(XMLSchemaTypes)}
\format{
  The format is a named list with each element itself being a named
  list.
  The top-level names provide the name of the R class that corresponds
  to the XML schema type.
  Each element is a list that can contain the elements
  "xsi:type", "type", "soapClass",  "from" and very specially "xsi:null".
  \code{from} is a function object that can be called with a value as a
  string (or potentially XML node if we extend the scope of this table)
  and converts that to the corresponding R value of the appropriate R type.
}
\references{
 The XML schema specification at \url{http://www.w3.org/XML/Schema}.
}
\examples{
data(XMLSchemaTypes)
}
\keyword{datasets}
