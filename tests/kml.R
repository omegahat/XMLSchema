
if(FALSE) {
library(XMLSchema)
k = readSchema("../XMLSchema/inst/samples/kml21.xsd")
o = defineClasses(k, baseClass = "VirtualXMLSchemaClass")

classNames = XMLSchema:::computeOrder(unlist(names(o[[1]])))

f = tempfile()
f = "/tmp/bob.R"
con = file(f, "w")
invisible(sapply(classNames, writeClassDef, file = con, where = globalenv()))
close(con)

source(f)

}
