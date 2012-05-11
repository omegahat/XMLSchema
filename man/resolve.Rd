\name{resolve}
\alias{resolve}
\alias{resolve}
\alias{resolve,ArrayType,list-method}
\alias{resolve,AttributeDef,list-method}
\alias{resolve,BasicSchemaType,list-method}
\alias{resolve,ClassDefinition,list-method}
\alias{resolve,Element,ANY-method}
\alias{resolve,EnumValuesDef,list-method}
\alias{resolve,PrimitiveSchemaType,list-method}
\alias{resolve,SchemaType,SchemaCollection-method}
\alias{resolve,SchemaType,list-method}
\alias{resolve,SchemaTypeReference,SchemaCollection-method}
\alias{resolve,SchemaTypeReference,list-method}
\alias{resolve,SchemaVoidType,ANY-method}
\alias{resolve,SchemaVoidType,SchemaCollection-method}
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
\alias{resolve,AnySchemaType,SchemaCollection-method}
\alias{resolve,AttributeDef,SchemaCollection-method}
\alias{resolve,LocalElement,list-method}
\alias{resolve,SelfRef,SchemaCollection-method}
\alias{resolve,character,SchemaTypes-method}
\alias{resolve,XMLAbstractNode,ANY,ANY,ANY,Element-method}
\alias{resolve,XMLAbstractNode,ANY,ANY,ANY,SimpleSequenceType-method}
\alias{resolve,AnySchemaType,SchemaCollection-method}
\alias{resolve,SchemaComplexType,SchemaCollection-method}

\title{Lookup the definition of a type by name}
\description{
  This takes the name of a XML schema type and looks
  through the schema information to find that type
  and return an R description for that type.
}
\usage{
resolve(obj, context, namespaces = character(), recursive = TRUE,
        raiseError = TRUE, xrefInfo = NULL, type = NA, depth = 1L, ...)
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
  \item{type}{either a function or one of \code{NULL} or \code{NA}.  If
    this is a function, this is used to filter possible matches. The
    primary
      purpose is to avoid matching Element objects with the same name as
      a data type definition in the schema. Without this, we would get
      into infinite loops.
     More generally, this  allows us to select a particular entry in a schema in preference to
     others all with the same names. See \code{XMLSchema:::notElementFun}.
   }
   \item{depth}{an integer that indicates the depth of the recursion.
     This is used for aiding debugging infinite loops.}
   \item{\dots}{additional parameters to resolve that the methods can add}
}
\value{
 An object of class \code{\link{SchemaType-class}}.
}
\author{Duncan Temple Lang}
\seealso{
\code{\link{processSchemaTypes}}
}
\examples{

}
\keyword{programming}
\concept{meta-programming}

