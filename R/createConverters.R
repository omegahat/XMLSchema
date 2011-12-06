
#
# This file is related to the processing of WSDL documents and the code here
# is for creating the converters for the different SOAP types to R.
#
# Take a look at the bottom of KEGG.S and the converters we define there to see how
# this should work, e.g. the PathwayElement and ArrayOfPathwayElement objects.

#
# We could also provide as(XMLNode, "TargetType") in our code. That might be much more
# elegant and extensible.
#
# Map the elements to positions if possible to avoid lookup.
# Take care of minOccurs = 0 for optional elements.
#
#

if(FALSE) {
setAs("XMLAbstractNode", "PathwayElement", 
function(from)
{
   obj = new("PathwayElement")
   obj@element_id = as.integer(xmlValue(from[["element_id"]]))
   obj@type = xmlValue(from[["type"]])
   obj@NAMES = new("ArrayOfstring", xmlSApply(from[["names"]], xmlValue))
   obj@components = new("ArrayOfint", as.integer(xmlSApply(from[["components"]], xmlValue)))
   obj
})


setAs("XMLAbstractNode", "ArrayOfPathwayElement", 
function(from)
{
  tmp = xmlApply(from, as, "PathwayElement")
  new("ArrayOfPathwayElement", tmp)
})
}


# Was SOAPTypeConverter and used setOldClass.
setClass("XMLTypeConverter", contains = "function")
setClass("XMLArrayConverter", contains = "XMLTypeConverter")
setClass("XMLChoiceConverter", contains = "XMLTypeConverter")


# defs are to store the pending entries and the ones that have already been defined.

#
#  Convert from SOAP to R.
#
setGeneric("createSOAPConverter",
            function(type, namespaces, defs = NULL, types = list(), ...) {
               standardGeneric("createSOAPConverter")
            })

classTemplate =
function(from)
{
  obj = new("type")

}

setMethod("createSOAPConverter",
           "RestrictedStringDefinition",
            function(type, namespaces, defs = NULL, types = list(), ...) {
              function(node, ...) {
                 xmlValue(node)
              }
            })


setMethod("createSOAPConverter",
           "ClassDefinition",
            function(type, namespaces, defs = NULL, types = list(), ...) {

               gen.tmpl = expression(obj@slotId <- as(from, "?"))[[1]]
               prim.tmpl = expression(obj@slotId <- as(xmlValue(from[[""]]), "?"))[[1]]

               f = classTemplate
               bd = body(f)
               bd[[2]][[3]][[2]] = type@name
               slotIds = names(type@slotTypes)
               b = lapply(seq(along = slotIds),
                            function(i) {
                               elType = type@slotTypes[[i]]
                               id = slotIds[i]
                               
                               if(is(elType, "PrimitiveSOAPType")) {

                                 # Deal with minOccurs, etc. for optional
                                 # Also, if all are required (or up to before the first optional node)
                                 # we can use numeric indices rather than names.
                                   tmpl = prim.tmpl
                                   tmpl[[3]][[2]][[2]][[3]] = id
                                      # for "string", we don't need the extra as(, "character")
                                   if(elType@name == "string")
                                     tmpl[[3]] = tmpl[[3]][[2]]
                                   else
                                     tmpl[[3]][[3]] = getRTypeFromSOAP(elType@name, "type")
                               } else {
                                  tmpl = gen.tmpl               
                                  tmpl[[3]][[3]] = if(is(elType, "GenericSchemaType")) elType@name else elType #XXXXXXX
                               }

                                # Convert names to NAMES, etc.
                               tmpl[[2]][[3]] = as.name(id)
                                 #XXX would like to return tmpl and then insert these into the body.

                               # Want to deal with optional  elements, e.g. minOccurs = "0"
                               # and put tmpl in an if(!is.null(from[[id]]) )
                               
                               bd[[i + 2]] <<- tmpl
                            })
                   # Add the expression that returns the object.
               bd[[length(bd) + 1]] = quote(obj)

               body(f) = bd
               new("XMLTypeConverter", f)
            })

arrayTemplate =
function(from)
{
  tmp = xmlApply(from, as, "PathwayElement")
  new("???", tmp)
}

setMethod("createSOAPConverter", "ANY",
                      function(type, namespaces, defs = NULL, types = list(), ...) {
                         warning("No method for createSOAPConverter for type ", class(type), " ", type@name)
                        function(x, ...) {x}
                      })

#XXX Handle substitutionGroup attributes in the parent type and the simpleElement.
#
# In the case of KML and PolyStyle, for example, we have
#  <element name="PolyStyle" type="kml:PolyStyleType"
#                           substitutionGroup="kml:Object"/>
# The definition of this is in <complexType name="PolyStyleType">... below
#
# In this case, we don't want a SimpleElement for PolyStyle, but to follow the @type attribute
# and get the definition from there
#
#

#if(FALSE)
#setMethod("createSOAPConverter", "SimpleElement",
#                      function(type, namespaces, defs = NULL) {
#
#                      })


setMethod("createSOAPConverter", "UnionDefinition",
                      function(type, namespaces, defs = NULL, types = list(), ...) {
                        # Want to generate a function that
                        # determines what the name of the node is and
                        # uses that as the target class.
                        # The concept is that there will be a coerce()
                        # method for an XMLAbstractNode to each of the given
                        # classes that make up the choice/union and we need
                        # to determine this at run time.
                        # It is the same as
                        # as(x, if(xmlName(x) == "a") "a" else if(xmlName(x) == "b") ...)

                        # We do have to deal with the case where there are no elements perhaps
                        # But that shouldn't arise here, should it ? i.e. minOccurs inside the choice?????
                        #
                        f = function(x, ...) {
                                as(x, xmlName(x))
                            }
                        new("XMLChoiceConverter", f)                        
                      })

# Recognize the primitive types.

setMethod("createSOAPConverter",
           "ArrayType",
            function(type, namespaces, defs = NULL, types = list(), ...) {
               f = arrayTemplate
               b = body(f)

               b[[2]][[3]][[4]] = type@elType@name
#               b[[3]][[3]] = type@name
               b[[3]][[2]] = type@name
               body(f) = b
               new("XMLArrayConverter", f)
            })


setMethod("createSOAPConverter",
          "Element",
          function(type, namespaces, defs = NULL, types = list(), ...) {
              schemaTypes = types

              # What about anonymous/unnamed elements/types.  We need to know the
              # R class name.
              # Attributes, including conversion to numbers, enums, etc.
              # For sequences with an unknown number of elements, have to put them into a list.
              # They won't have names.
             if(is(type@type, "ClassDefinition")) {
               elNames = names(type@type@slotTypes)
               types = type@type@slotTypes
               if(is.null(elNames)) elNames = character()
               body = sapply(elNames[!is.na(elNames)],
                             function(id) {
                                # Worry about duplicate element names so x[[id]] won't work.

                               type = types[[id]]
                               type = if(is.character(type))
                                       schemaTypes[[id]] # lookupDef(id, schemaTypes)  # id or types[[id]]

                               typeName = sQuote(mapSOAPTypeToS(type@name, schemaTypes)) #XXX want the types in this call.
                               if(length(type@count)) {
                                          # lapply or sapply ? 
                                 paste("obj@", id, " <- lapply(x[names(x) == ", sQuote(id), "], as, ", typeName, ")", sep = "")
                               } else
                                 paste("obj@", id, " <- as(x[[", sQuote(id), "]], ", typeName, ")", sep = "")
                             })
             } else if(is(type@type, "SimpleSequenceType")) {
               body = paste("lapply(xmlChildren(x), as,", sQuote(type@type@elType@name), ")")
             }

             if(length(type@attributes)) {
               getAttrs = sapply(type@attributes,
                                   function(x) paste("obj@`", x@name, "` <- xmlGetAttr(x,", sQuote(x@name), ", character(0))", sep = ""))
             } else
               getAttrs = character()
             txt = c(paste("function(x, ..., obj = new(", sQuote(type@name), ")) {"),
                      body,
                      getAttrs,

                      "obj",
               
                     "}")

             
            f = eval(parse(text = paste(txt, collapse = "\n")), globalenv())
#            class(f) = c("SOAPElementConverter", "SOAPTypeConverter", "function")
             
            f = new("SOAPElementConverter", f)

            f
          })



setMethod("createSOAPConverter",
           "RestrictedListType",
            function(type, namespaces, defs = NULL, types = list(), ...) {

              txt = c("function(x, ...) {",
                      "vals = strsplit(xmlValue(x), '[[:space:]]+')",
                           # Check all values are legitimate
                      "as.character(vals)",  # convert to element type.
                           # Create a new instance of the defined class based on type@name
                      "}"
                     )

              as(txt, "XMLTypeConverter")
            })

setAs("character", "XMLTypeConverter",
        function(from) {
              f = eval(parse(text = paste(from, collapse = "\n")), globalenv())
              attr(f, "source") = NULL
              new("XMLTypeConverter", f)
  })


setMethod("createSOAPConverter",
           "SimpleSequenceType",
            function(type, namespaces, defs = NULL, types = list(), ...) {
              type@fromConverter
            })


setMethod("createSOAPConverter", "SOAPComplexType",
     function(type, namespaces, defs = NULL, types = list(), ...) {

   ids = names(type@attributes)
   if(length(ids) == 0 || any(ids == ""))
      ids = sapply(type@attributes, slot, "name")
   attr.code =  mapply(createConvertAttributeFromSOAP, ids, type@attributes, MoreArgs = list(target = "obj", src = "x"))
     # If we have one of each, i.e. no variable numbers, then we can assign the results of converting each node
     # to a slot. If we have variable number, then we should loop over them and return a list of the converted values.
     # If we have not children, we shouldn't do this, but it will be harmless.
     #XXX if the complexType has @mixed = 'true', then allow for character content.
   node.code = "mapply(as, xmlChildren(x), names(x))"
   code = c("function(x, ...) {", sprintf("obj = new('%s')", type@name), "",
                   attr.code, node.code, "}")
   as(code, "XMLTypeConverter")
})

createConvertAttributeFromSOAP = 
function(attrName, def, types = list(), target = "obj", src = "x")
{
  if(def@use == "optional")
      sprintf("tmp <- xmlGetAttr(%s, '%s')\nif(!is.null(tmp)) %s@%s = as(tmp, '%s')",
                 src, attrName, target, attrName, mapSOAPTypeToS(def@type, types))
  else
      sprintf("%s@%s = as(xmlGetAttr(%s, '%s'), '%s')",
                 target, attrName, src, attrName, mapSOAPTypeToS(def@type, types))
}


setMethod("createSOAPConverter",
           "SchemaCollection",
     function(type, namespaces, defs = NULL, types = list(), ...) {
        types = unlist(type, recursive = FALSE)
        names = unlist(lapply(type, names))
        invisible(
           structure(lapply(types, createSOAPConverter, namespaces, defs, type),
                   names = names))
     })


setMethod("createSOAPConverter",
           "SimpleElement",
     function(type, namespaces, defs = NULL, types = list(), ...) {
       
       f = function(node, obj = new(type)) {

       }
       environment(f) = globalenv()
       formals(f)[[2]][[2]] = computeName(type)
       
       if(length(type@type)) {
         
       }

       if(length(type@attributes)) {
         e = makeAttributeCode(type@attributes, 'obj')
         body(f)[seq(length(body(f)) + 1, length = length(e))] = e
       }

       body(f)[[length(body(f)) + 1L]] = quote(obj)

       f
  })


makeAttributeCode =
  #
  # See createConvertAttributeFromSOAP above
  #
function(attributeDefs, varName = 'obj')
{
  for(i in attributeDefs) {
    e = quote(obj@foo <- xmlGetAttr(node, ""))
    e[[2]][[1]] = as.name(varName)
    e[[2]][[3]] = as.name(i@name)
    e[[3]][[3]] = i@name           
    
    body(f)[[length(body(f)) + 1L]] = e
  }
}
