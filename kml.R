library(XMLSchema)
sch = readSchema("~/GitWorkingArea/XMLSchema/inst/samples/kml21.xsd")
defineClasses(sch)


library(XML)
a = newXMLNode("angle180", 45)
as(a, "angle180")


fromXML(a, type = "angle180")

fromXML(a, type = sch[[1]]$angle180)
# used to return a string, not a number. Now fails with infinite recursion.
