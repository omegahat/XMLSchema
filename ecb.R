library(XML)
library(XMLSchema)
library(RCurl)

urls = c("https://stats.ecb.int/stats/vocabulary/bsi/2005-07-01/sdmx-compact.xsd",
         "https://stats.ecb.europa.eu/stats/vocabulary/sdmx/2.0/SDMXMessage.xsd")
docs = lapply(urls, function(u) {
                     doc = xmlParse(getURLContent(urls[1]))
                     docName(doc) = u
                     doc
                   })

types = lapply(docs,
                 function(d)
                   as.character(unlist( xmlSApply(xmlRoot(d2), xmlGetAttr, "name"))))

readSchema(docs[[1]])
