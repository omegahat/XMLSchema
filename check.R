library(XMLSchema)
ff = list.files("~/GitWorkingArea/XMLSchema/inst/samples/", pattern = "\\.xsd$", full = TRUE)
schema = lapply(ff, readSchema)

all = unlist(unlist(schema, recursive = FALSE), recursive = FALSE)
table(sapply(all, class))


