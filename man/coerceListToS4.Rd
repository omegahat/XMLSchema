\name{coerceListToS4}
\alias{coerceListToS4}
\title{Simple utility function to convert an list to an S4 object}
\description{
  This is a utility function that allows us to convert
  a named collection of values in the form of a list (or vector)
  to an S4 object. This is used to simplify calls to
  SOAP  methods that expect a structured object
  of an S4 class by allowing us to group the slots
  in a list.
}
\usage{
coerceListToS4(.x = list(...), .obj, ..., .partialMatch = TRUE, .coerce = TRUE)
}
\arguments{
  \item{.x}{the R list containing the named elements that map to the
    slots in the S4 object}
  \item{.obj}{an instance of the S4 object or the name of an S4 class.}
  \item{\dots}{named elements which act as slots. This allows the caller
    to avoid collecting these elements into a list.}
  \item{.partialMatch}{a logical value that controls whether we match
     the names of the elements in the list to slot names in the class
     using partial matching or exact matching.}
   \item{.coerce}{a logical value that controls whether we coerce each
     slot to its defined type.}
}
\details{

}
\value{
  The \code{obj} object with any slots filled in from the
  the list \code{x}.
}
%\references{}
\author{
Duncan Temple Lang
}

\seealso{}
\examples{
   setClass("Bar", representation(x = "logical", y = "data.frame"))
   setClass("Foo", representation(a = "numeric", b = "integer", c = "character",
                                   d = "Bar"))

   coerceListToS4(list(a = 1, b = 4:9), new("Foo"))
   coerceListToS4(list(a = 1, b = 4:9, d = coerceListToS4(list(y = mtcars), new("Bar"))),  new("Foo"))

     # here we drop the new() and the list() but note that we have to
     # indicate that the class name should match the second parameter
     # with an extra ,
   coerceListToS4(a = 1, b = 4:9, d = coerceListToS4(y = mtcars, , "Bar"), , "Foo")
}
\keyword{programming}
\keyword{interface}
