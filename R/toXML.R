#
# This creates code based on the SOAPType to convert an R object to an XML node.
#
setGeneric("toXML",
           function(type, types, namespaceDefs = NULL, env = getNamespace("XML"))
              standardGeneric("toXML"))

setMethod("toXML", "ClassDefinition",
           function(type, types, namespaceDefs = NULL) {

             f = function(obj, parent = NULL) {
                     attrs = character()
                     node = newXMLNode(name, parent = parent, attrs = attrs)
                     
                     node
             }
             body(f)[[3]][[3]][[2]] = type@name
             
             if(any(type@isAttribute)) {
                 # write code 
             }

             environment(f) = env
             f
           })
