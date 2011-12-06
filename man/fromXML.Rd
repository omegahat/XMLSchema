\name{fromXML}
\alias{fromXML}
\alias{fromXML,ANY,ANY,ANY,ANY,ANY-method}
\alias{fromXML,ANY,ANY,ANY,ANY,ArrayType-method}
\alias{fromXML,ANY,ANY,ANY,ANY,NULL-method}
\alias{fromXML,ANY,ANY,ANY,ANY,character-method}
\alias{fromXML,ANY,ANY,ANY,ANY,missing-method}
\alias{fromXML,XMLAbstractNode,ANY,ANY,ANY,ArrayType-method}
\alias{fromXML,ANY,ANY,ANY,ANY,SOAPVoidType-method}
\alias{fromXML,XMLAbstractNode,ANY,ANY,ANY,PrimitiveSOAPType-method}
\alias{fromXML,XMLAbstractNode,ANY,ANY,ANY,ClassDefinition-method}
\alias{fromXML,XMLAbstractNode,ANY,ANY,ANY,NULL-method}
\alias{fromXML,NULL,ANY,ANY,ANY,ANY-method}
\alias{fromXML,ANY,ANY,ANY,ANY,SimpleSequenceType-method}
\alias{fromXML,XMLAbstractNode,ANY,ANY,ANY,Element-method}
\alias{fromXML,XMLAbstractNode,ANY,ANY,ANY,SimpleSequenceType-method}
\title{Convert an XML node to an R type}
\description{
  Convert an XML node representing
  a SOAP value to an S object.
  The different functions handle different
  types of SOAP objets.
  This also works on an XML string or filename.
  The \code{fromSOAPArray} supports the \code{offset}
  and \code{position} attributes for partially
  transmitted and sparse  arrays.
  Currently, there are limits on the ....
}
\usage{
fromXML(node, root = NULL, converters = SchemaPrimitiveConverters,
        append = TRUE, type = NULL, multiRefs = list(),
         namespaces = gatherNamespaceDefs(node))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node}{the XML node (and sub-nodes) giving the SOAP content.
    The \code{fromSOAP} function works recursively, operating on
    sub-nodes, etc. This argument is the individual node.
  }
  \item{root}{the top-most node of the XML ``document''
    which is used for resolving \code{href} references
    within elements to other nodes.
  This doesn't change across the recursive calls.}
\item{converters}{a named-list of functions.
  The names are used to find the appropriate converter
  for a non-array or non-struct SOAP object.
  The name comes from the value of the ``xsi:type'' attribute
  or from the string ``xsd:<element name>''
  where \code{element-name} is the name of the XML node.
  This will become more advanced in the future.
  This allows user  to provide call-specific customizations
  to the conversion rather than just relying on
  "global" method definitions.  
 }
 \item{append}{a logical value indicating whether to merge the
   converters with the default SOAPPrimitiveConverters,
   or use them as is.
   This is available for covenience so that the caller doesn't have to
   perform the two-step merging herself.}
 \item{type}{an optional type identifier
   that if not specified is computed from the
   \code{xsi:type} attribute of the XML node (\code{node})
   if available.}
%\item{findReturn}{a logical value indicating whether to recursively
%  descend the XML node to find the \code{<return>} element and process
%  that rather than  working directly from \code{node}.
%  }
 \item{multiRefs}{?}
%%\item{simplify}{a logical value that, if \code{TRUE},
%%  causes \code{fromSOAPArray} to try to convert the
%%  list of elements into a vector. It does this only if the
%%  element type of the SOAP array is "primitive",
%%  defined currently as being in the names of the
%%  \code{SOAPPrimitiveConverters} list.
%%  This is similar to the simplification step
%%  \code{\link[base]{sapply}}
%%  performs after calling \code{\link[base]{lapply}}.}
 \item{namespaces}{a list of namespace information providing the
   mappings of the prefix to the namespace URI so that we can correctly
   resolve   types  such as "xsd:string".
  }
}
\value{
  An  R object corresponding to the information in the
  XML node. If there is a converter to create an instance
  of a class defined from the XML schema, then this is what is returned.
}
\references{
  \url{http://www.w3.org/TR/SOAP/}
  \url{http://www.omegahat.org/SSOAP},
  \url{http://www.omegahat.org/bugs}.
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
 The \code{SSOAP} package.
}

\examples{

}
\keyword{interface}
\keyword{connection}
