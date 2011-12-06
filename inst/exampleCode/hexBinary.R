library(XMLSchema)
d = readSchema(system.file("samples", "kml21.xsd", package = "XMLSchema"))
XMLSchema:::defClass(d$color)
try(as("red", "color")) # Fails, wrong format
as("00ffb456", "color")

