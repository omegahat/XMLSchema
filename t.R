library(XMLSchema)
dir = system.file("samples", package = "XMLSchema")
files = list.files(dir, pattern = "\\.xsd$", full.names = TRUE)
els = lapply(files[c(12:13)], readSchema, createConverters = FALSE, verbose = TRUE, followImports = FALSE)


els = lapply(files[-c(16, 17)], readSchema, createConverters = FALSE, verbose = TRUE, followImports = FALSE)

els = readSchema(files[[12]], createConverters = FALSE, verbose = TRUE, followImports = FALSE)
# Problem is kml21.xsd with FeatureType and processExtension( FeatureType[["complexContent"]])

els = lapply(files[16], readSchema, createConverters = FALSE, verbose = TRUE, followImports = FALSE)
