library(SSOAP)
library(XML)

u = "http://sdw-ws.ecb.europa.eu/services/SDMXQuery?wsdl"

if(file.exists("~/Projects/SDMX")) {
    # resolve locally to that directory.
  catalogResolve("foo")
  catalogAdd("http://sdw-ws.ecb.europa.eu/services/", "/Users/duncan/Projects/SDMX/")
  catalogAdd("http://sdw-ws.ecb.europa.eu:80/services/", "/Users/duncan/Projects/SDMX/")
  catalogResolve(u)
}

w = processWSDL(u)

#iface = genSOAPClientInterface(, w)
