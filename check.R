library(XMLSchema)
ff = list.files("~/GitWorkingArea/XMLSchema/inst/samples", pattern = "\\.xsd$", full = TRUE)
ff = grep("(ogckml22|xAL.xsd)", ff, invert = TRUE, value = TRUE)

schema = lapply(ff, readSchema)



all = unlist(unlist(schema, recursive = FALSE), recursive = FALSE)
table(sapply(all, class))


XMLSchema:::defClass(all[["orderidtype"]])

try(new("orderidtype", "bob"))

TRUE




