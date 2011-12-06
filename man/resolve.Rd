\name{resolve}
\alias{resolve}
\alias{resolve}
\alias{resolve,ArrayType,list-method}
\alias{resolve,AttributeDef,list-method}
\alias{resolve,BasicSOAPType,list-method}
\alias{resolve,ClassDefinition,list-method}
\alias{resolve,Element,ANY-method}
\alias{resolve,EnumValuesDef,list-method}
\alias{resolve,PrimitiveSOAPType,list-method}
\alias{resolve,SOAPType,SchemaCollection-method}
\alias{resolve,SOAPType,list-method}
\alias{resolve,SOAPTypeReference,SchemaCollection-method}
\alias{resolve,SOAPTypeReference,list-method}
\alias{resolve,SOAPVoidType,ANY-method}
\alias{resolve,SOAPVoidType,SchemaCollection-method}
\alias{resolve,SimpleElement,ANY-method}
\alias{resolve,WSDLTypeDescription,list-method}
\alias{resolve,character,SchemaCollection-method}
\alias{resolve,character,list-method}
\alias{resolve,Element,list-method}
\alias{resolve,SimpleElement,list-method}
\alias{resolve,SimpleSequenceType,SchemaCollection-method}
\alias{resolve,NULL,list-method}
\alias{resolve,RestrictedStringDefinition,list-method}
\alias{resolve,RestrictedSetInteger,list-method}
\alias{resolve,RestrictedStringPatternDefinition,SchemaCollection-method}
\alias{resolve,UnionDefinition,SchemaCollection-method}
\alias{resolve,AnySOAPType,SchemaCollection-method}
\alias{resolve,AttributeDef,SchemaCollection-method}
\alias{resolve,LocalElement,list-method}
\alias{resolve,SelfRef,SchemaCollection-method}
\alias{resolve,character,SchemaTypes-method}
\alias{resolve,XMLAbstractNode,ANY,ANY,ANY,Element-method}
\alias{resolve,XMLAbstractNode,ANY,ANY,ANY,SimpleSequenceType-method}
\alias{resolve,AnySOAPType,SchemaCollection-method}
\alias{resolve,SOAPComplexType,SchemaCollection-method}

\title{Lookup the definition of a type by name}
\description{
  This takes the name of a XML schema type and looks
  through the schema information to find that type
  and return an R description for that type.
}
\usage{
resolve(obj, context, namespaces = character(), recursive = TRUE,
        raiseError = TRUE, xrefInfo = NULL)
}
\arguments{
  \item{obj}{the name of the type to search for}
  \item{context}{the overall schema/set of type information in which to
    lookup the name.}
  \item{namespaces}{a character vector of \code{prefix = URI} pairs
      allowing the name spaces from the XML nodes and type definitions to be resolved correctly}
  \item{recursive}{a logical value controlling whether sub-elements or
    slots within the resolved type are also resolved or left as names/references.}
  \item{raiseError}{a logical value controlling whether failure to
    find the type leads to an error or just  \code{NULL} being returned.
    This gives the caller control when a type is not in the schema.}
%  \item{pending}{unused at present. Will be used to detect infinite
%    recursion/circular references.}
  \item{xrefInfo}{an object representing the cross-references between
    data types.  This is almost never specified by the R user but passed
    recursively within the calls to resolve and from higher-level
    functions that call resolve.}
}
\value{
 An object of class \code{\link{SOAPType-class}}.
}
\author{Duncan Temple Lang}
\seealso{
\code{\link{processSchemaTypes}}
}
\examples{

}
\keyword{programming}
\concept{meta-programming}

