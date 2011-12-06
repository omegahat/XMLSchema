\name{exportClassDefs}
\alias{exportClassDefs}
\alias{exportClassDefs}
\alias{expandS4}
\alias{expandS4,ANY-method}
\alias{expandS4,list-method}
\alias{exportClassDef}
\alias{writeClassDef}

\title{Write R code corresponding to existing class definitions.}
\description{
  This function and the related helper functions
  is used to generate R code that can be used
  in another R session or package to define
  classes and coercion methods that currently exist in this session.
  The idea is that we use this to create explicit R code that will
  recreate the classes and methods we define via
  \code{\link{defineClasses}} corresponding to the types in our XML schema.

  This function also makes an effort to get the correct order in which the
  classes should be defined.
  
  \code{expandS4} takes care of serializing S4 objects
  which does not currently happen with \code{deparse}.

  \code{exportClassDef} generates the code for the class.
  
}
\usage{
exportClassDefs(x, file = NULL, where = globalenv(), ...)
}

\arguments{
  \item{x}{the collection of class names.}
  \item{file}{the name of a file or a connection to which the generated code
    should be written. If this is \code{NULL}, the code is not written but
     returned and the caller can do with it what she wants, e.g. write
  it to a file later, parse and evaluate it, etc..}
  \item{where}{the environment in which to look for the class definitions.}
  \item{\dots}{additional parameters passed to sub-functions}
}
\value{

}
\references{
}
\author{
  Duncan Temple Lang
}
\seealso{
\code{\link{defineClasses}}
}
\examples{

}
\keyword{programming}
\keyword{interface}
