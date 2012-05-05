library(XMLSchema); sch = readSchema("~/GitWorkingArea/XMLSchema/inst/samples/biblio.xsd")
defineClasses(sch)

tt = newXMLNode("Authors",
                 newXMLNode("author", newXMLNode("firstname", "Duncan"),
                                      newXMLNode("lastname", "Temple Lang")),
                 newXMLNode("author", newXMLNode("firstname", "Deb"),
                                      newXMLNode("lastname", "Nolan")))

as(tt, "Authors")

fromXML(tt)

