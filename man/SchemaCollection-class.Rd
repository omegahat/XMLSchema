\name{SchemaCollection-class}
\Rdversion{1.1}
\docType{class}
\alias{SchemaCollection-class}
\alias{SchemaTypes-class}
%\alias{resolve,AnySOAPType,SchemaCollection-method}
%\alias{resolve,AttributeDef,SchemaCollection-method}
%\alias{resolve,character,SchemaCollection-method}
%\alias{resolve,RestrictedStringPatternDefinition,SchemaCollection-method}
%\alias{resolve,SelfRef,SchemaCollection-method}
%\alias{resolve,SimpleSequenceType,SchemaCollection-method}
%\alias{resolve,SOAPComplexType,SchemaCollection-method}
%\alias{resolve,SOAPType,SchemaCollection-method}
%\alias{resolve,SOAPTypeReference,SchemaCollection-method}
%\alias{resolve,SOAPVoidType,SchemaCollection-method}
%\alias{resolve,UnionDefinition,SchemaCollection-method}

\title{Class \code{"SchemaCollection"} and \code{"SchemaTypes"}}
\description{
  These classes are used to represent data types described in XML
  schema.
  \code{SchemaTypes} is used to store the data type descriptions from a
  single schema.
  The \code{SchemaCollection} class is used to represent a collection of
  one or more related schema, often where types from one are used in the
  other(s).  Each element of a \code{SchemaCollection} is a
  \code{SchemaTypes} object. The \code{SchemaCollection} is a list
  and its names are the URIs giving the target namespace of the
  different schemas.
}
\section{Objects from the Class}{
  Objects of these classes are typically generated via calls to
  \code{\link{readSchema}} or from other higher-level code such
  as in the \code{SSOAP} package for reading schema with in
  Web Service Description Language (WSDL) files.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}. For
      \code{SchemaCollection}, this is the collection
      of \code{SchemaTypes} elements.  For \code{SchemaTypes}, the
      elements are the data descriptions, each an object of some
      sub-class derived from \code{\link{GenericSchemaType-class}}}
  }
}
\section{Extends}{
Class \code{"\linkS4class{list}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "list", distance 2.
Class \code{"\linkS4class{SOAPTypeOrList}"}, by class "list", distance 2.
}
\section{Methods}{
  \describe{
    \item{createSOAPConverter}{\code{signature(type = "SchemaCollection")}: ... }
    \item{resolve}{\code{signature(obj = "AnySOAPType", context = "SchemaCollection")}: ... }
    \item{resolve}{\code{signature(obj = "AttributeDef", context = "SchemaCollection")}: ... }
    \item{resolve}{\code{signature(obj = "character", context = "SchemaCollection")}: ... }
    \item{resolve}{\code{signature(obj = "RestrictedStringPatternDefinition", context = "SchemaCollection")}: ... }
    \item{resolve}{\code{signature(obj = "SelfRef", context = "SchemaCollection")}: ... }
    \item{resolve}{\code{signature(obj = "SimpleSequenceType", context = "SchemaCollection")}: ... }
    \item{resolve}{\code{signature(obj = "SOAPComplexType", context = "SchemaCollection")}: ... }
    \item{resolve}{\code{signature(obj = "SOAPType", context = "SchemaCollection")}: ... }
    \item{resolve}{\code{signature(obj = "SOAPTypeReference", context = "SchemaCollection")}: ... }
    \item{resolve}{\code{signature(obj = "SOAPVoidType", context = "SchemaCollection")}: ... }
    \item{resolve}{\code{signature(obj = "UnionDefinition", context = "SchemaCollection")}: ... }
	 }
}
\references{
  XML Scheam
}
\author{
Duncan Temple Lang
}

\seealso{
  \code{\link{readSchema}}
  \code{\link{GenericSchemaType-class}}
    
}
\examples{
showClass("SchemaCollection")
}
\keyword{classes}
