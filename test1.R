library(XMLSchema)
library(XML)
library(SSOAP)
b = readSchema("biblioSimple.xsd", FALSE, TRUE)

# When defining Bibitem, the author field is not correct.
# It is an NA for the name and type NULL
# This is with the choice of authors and author.
# Simplify this for testing. And make up a name in general.

o = defineClasses(b, force = TRUE)

