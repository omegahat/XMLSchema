\name{VirtualXMLSchemaClass-class}
\docType{class}
\alias{VirtualXMLSchemaClass-class}

\title{Class "VirtualXMLSchemaClass"}
\description{
  This is a simple virtual class that is used as a base class of  R
 class definitions corresponding to  data types defined in XML schema.
 The idea is that we can then define general methods, 
 e.g. documentation, for these classes and instances with a single 
 method that dispatches on this class. Essentially this class is a label 
 with no fields.

 When one creates the classes, a different base class can be specified.
 It can extend this or replace it. For example, one might want to use
 a class name that identifies the context of the schema, e.g. KEGG or NOAA.
 One might then define a class with
 \code{setClass("NOAASchemaClass", contains = "VirtualXMLSchemaClass")}
 to inherit any general methods for \code{VirtualXMLSchemaClass}
  but to provide the opportunity of identifying the source/context of the
  new classes.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Methods}{
No methods defined with class "VirtualXMLSchemaClass" in the signature.
}

\author{Duncan Temple Lang}

\seealso{
 \code{\link[SSOAP]{processWSDL}}
 The internal functions \code{defineClasses} and \code{defClass} and
 \code{createArrayClass}.
}
\examples{
showClass("VirtualXMLSchemaClass")
}
\keyword{classes}
\concept{XML schema}
