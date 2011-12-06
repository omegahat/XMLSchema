\name{BasicSOAPType-class}
\docType{class}
\alias{GenericSchemaType-class}
\alias{BasicSOAPType-class}
\alias{SOAPType-class}
\alias{SOAPTypeReference-class}
\alias{SOAPVoidType-class}
\alias{SOAPTypeOrList-class}
\alias{SOAPTypeOrNULL-class}
\alias{NMTOKEN-class}
\alias{NCName-class}
\alias{date-class}
\alias{dateTime-class}
\alias{time-class}
\alias{nonNegativeInteger-class}
\alias{ID-class}
\alias{gYear-class}
\alias{gYearMonth-class}
\alias{positiveInteger-class}

\alias{coerce,character,gYear-method}
\alias{coerce,character,gYearMonth-method}
\alias{coerce,Date,date-method}
\alias{coerce,POSIXct,date-method}
\alias{coerce,POSIXlt,date-method}
\alias{coerce,character,date-method}

\alias{as.SOAPDateTime}
\alias{as.SOAPDate}
\title{Class "SOAPType" }
\description{
  This collection of classes is used to represent the different
  types in an XML schema.

  There are many different classes in this class hierarchy.
  We represent both data types (e.g. complexType elements in a schema)
  and element definitions which do not correspond to actual types, but
  the markup useed to represent data in XML.
  We also represent XML attribute definitions. In addition to these
  types of data type descriptions, we also have support for
  data types that are "built-in" or defined in the XML schema definition
  as being primitive types, e.g. boolean, string, integer, date, time,
  etc.

  The GenericSchemaType class is the root class in this hierarchy.
  

  
  Note that the names will change to refer to schema types rather than
  SOAP types. The SOAP reference is related to the fact that the code
  was originally in the \code{SSOAP} package.
}
\section{Objects from the Class}{
  One can use these classes directly to represent your own
  types and construct them directly within R.
  However, for the most part, instances of these classes will
  be created by processing an XML schema via
  \code{\link{readSchema}} and/or \code{\link{processSchemaTypes}}.
}
\section{Slots}{
	 \describe{
    \item{\code{fromConverter}:}{Object of class \code{"function"} ~~ }
    \item{\code{toConverter}:}{Object of class \code{"function"} ~~ }
    \item{\code{name}:}{Object of class \code{"character"} ~~ }
    \item{\code{ns}:}{Object of class \code{"character"} ~~ }
    \item{\code{nsuri}:}{Object of class \code{"character"} ~~ }
    \item{\code{count}:}{Object of class \code{"numeric"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{SOAPType}"}, directly.
Class \code{"\linkS4class{SOAPTypeOrList}"}, by class "SOAPType", distance 2.
Class \code{"\linkS4class{SOAPTypeOrNULL}"}, by class "SOAPType", distance 2.
}
\section{Methods}{
No methods defined with class "BasicSOAPType" in the signature.
}
\references{
}
\author{Duncan Temple Lang}

\seealso{
  \code{\link{readSchema}}
  \code{\link{processSchemaTypes}}.
}
\examples{
showClass("BasicSOAPType")
}
\keyword{classes}
