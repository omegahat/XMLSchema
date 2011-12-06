\name{convertToSName}
\alias{convertToSName}
\alias{simple.dQuote}
\alias{discardNamespace}
\title{Utility functions used when generating code}
\description{
  These are simple utility functions that we use when processing types and 
  Web service operations, etc.  to create corresponding R code.
  These are exported for use in other packages such as SSOAP but 
  are quite trivial.

  \code{convertToSName} can be used to change the name of a variable to 
  camel-back notation, i.e. removing \_ characters and capitalizing the first
  letter of the next "word".

 \code{simple.dQuote} merely surrounds the string with quotes (\"). It avoids
  using fancyQuotes which can happen with \code{\link[base]{dQuote}} in base.

 \code{discardNamespace} ensures that the name from the XML element or attribute 
  is the regular name with no namespace prefix.
}
\usage{
convertToSName(name,
               useDash = if("UseDashInSOAPNames" \%in\% names(options()))
                             getOption("UseDashInSOAPNames")
                         else
                             TRUE)
discardNamespace(str)
simple.dQuote(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name,x,str}{the string to be processed}
  \item{useDash}{a logical value, and if \code{TRUE} then \code{name} is returned unaltered.}
}
\value{
  A string.
}
\author{Duncan Temple Lang}

\examples{
}
\keyword{programming}
