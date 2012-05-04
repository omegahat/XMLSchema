library(XMLSchema)
sch = readSchema("~/GitWorkingArea/XMLSchema/inst/samples/kml21.xsd")
invisible(defineClasses(sch))


library(XML)
a = newXMLNode("angle180", 45)
as(a, "angle180")

v = fromXML(a, type = "angle180")

v = fromXML(a, type = sch[[1]]$angle180)

sch[[1]]$angle180@toConverter(v)
# Fixed (up)
# used to return a string, not a number. Now fails with infinite recursion.
#  Appears the method for XMLAbstractNode, RestrictedDouble is not being used.
# Works now if we add the root = "missing".


XMLSchema:::defClass(sch[[1]][[5]])
a = newXMLNode("altitudeModeEnum", "clampToGround")
v = as(a, "altitudeModeEnum")

 # use a wrong value
b = newXMLNode("altitudeModeEnum", "clampToGroundXXX")
try(as(b, "altitudeModeEnum")) # fails

sch[[1]][[5]]@toConverter(v)


v = new("color", "ff00ffaa")
try(new("color", "ff00ffaax"))
sch[[1]]$color@toConverter(v)


sch[[1]][["CoordinatesType"]]@fromConverter
XMLSchema:::defClass(sch[[1]][["CoordinatesType"]])
 # check validity
getValidity(getClass("CoordinatesType"))
new("CoordinatesType", c("abc", "def"))

ct = newXMLNode("CoordinatesType", newXMLNode("string", "foo"), newXMLNode("string", "bar"))
as(ct, "CoordinatesType")


########
XMLSchema:::defClass(sch[[1]]$itemIconStateType, types = sch)
as(newXMLNode("itemIconStateEnum", "open"), "itemIconStateEnum")
new("itemIconStateEnum", "open")
try(new("itemIconStateEnum", "openx"))

nd = newXMLNode("itemIconStateType",
                 newXMLNode("string", "open"),
                 newXMLNode("string", "closed"))

as(nd, "itemIconStateType")



nd = newXMLNode("itemIconStateType",
                 newXMLNode("string", "openx"),
                 newXMLNode("string", "closed"))
try(as(nd, "itemIconStateType"))

