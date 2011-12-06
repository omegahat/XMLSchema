\name{defineClasses}
\alias{defineClasses}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Define R classes corresponding to XML schema types}
\description{
  This function maps \code{\link{SOAPType-class}}
  objects describing XML schema types to R class definitions
  within the existing R session.
}
\usage{
defineClasses(types, where = globalenv(), namespaceDefs = list(),
               verbose = FALSE, baseClass = "VirtualSOAPClass", force = FALSE,
               opts = new("CodeGenOpts"),
                pending = new.env(hash = TRUE, emptyenv()),
                classes = new.env(hash = TRUE, emptyenv()))
}
\arguments{
  \item{types}{a list of \code{\link{SOAPType-class}} objects
   describing the XML schema types.}
  \item{where}{the environment in which the new R class definitions
    should be defined and assigned. This is passed as the value of
    the \code{where} parameter in the call to \code{\link[methods]{setClass}}.}
  \item{namespaceDefs}{a character vector giving the \code{prefix = URI}
    pairs which are used to compare the namespaces in the
    \code{\link{SOAPType-class}} objects with those in the standard
    schema, i.e. XSD.
  }
  \item{verbose}{a logical value indicating whether to emit messages on
    the console as classes are defined.}
  \item{baseClass}{the name of  the base class which is added to the
    \code{contains} vector for each new class. This allows all the
    newly defined classes to have a common base class, perhaps with
    multiple inheritance. This can be used to define methods for all
    these classes and instances. For example, we might provide a
    documentation method  that provides the help from the
    XML schema's annotation nodes.
  }
  \item{force}{a logical value.  By default, if there is an existing R class 
    with a name corresponding to a \code{\link{SOAPType-class}} object,
    this function does not redefine it.
    This means that when one runs the function a second time, the
    classes are not redefined. To override this, use \code{force = TRUE}.
  }
  \item{opts}{an instance of \code{CodeGenOpts} that controls how the
    sub-functions generate code.  This class can be sub-classed/derived
    and new options introduced.}
  \item{pending,classes}{environments which are updated by the different
  functions and methods we call to register the names of the classes
  that are being processed and defined.  We allow these to be specified
  by the caller so that she can control and monitor some of the processing.}
}
\value{
  A list with the class definitions.
  This function is run more for its side-effect of defining new classes.
}
\author{Duncan Temple Lang}
\seealso{}
\examples{
}
\keyword{IO}
\keyword{programming}

