\name{normalizedCharacter-class}
\Rdversion{1.1}
\docType{class}
\alias{StringEnum-class}
\alias{AttributeType-class}
\alias{normalizedCharacter-class}
\alias{coerce,character,normalizedCharacter-method}

\title{Class "normalizedCharacter" with newlines, tabs and line feeds removed}
\description{
  This is a character vector which has any newline, tab and line feeds
  removed from the contents.

  The \code{StringEnum} class is a (virtual) class that is used as
  the base class for a restricted string that is an enumeration,
  i.e. one of a fixed set of strings.
}
\section{Objects from the Class}{
Objects can be created by calls of the form
\code{new("normalizedCharacter", ...)}
or via \code{as(vector, "normalizedCharacter")}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"character"}, inherited
      from the base \code{character} class.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{character}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "character", distance 2.
Class \code{"\linkS4class{data.frameRowLabels}"}, by class "character", distance 2.
Class \code{"\linkS4class{SuperClassMethod}"}, by class "character", distance 2.
}
\section{Methods}{
No methods defined with class "normalizedCharacter" in the signature.
}
\author{
Duncan Temple Lang
}

\seealso{
}
\examples{
showClass("normalizedCharacter")
}
\keyword{classes}
