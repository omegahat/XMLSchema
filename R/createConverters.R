
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


# Was SchemaTypeConverter and used setOldClass.
setClass("XMLTypeConverter", contains = "function")
setClass("XMLArrayConverter", contains = "XMLTypeConverter")
setClass("XMLChoiceConverter", contains = "XMLTypeConverter")


# defs are to store the pending entries and the ones that have already been defined.

#
#  Convert from SOAP to R.
#
setGeneric("createFromXMLConverter",
            function(type, namespaces, defs = NULL, types = list(), allowMissingNodes = FALSE, ...) {
               standardGeneric("createFromXMLConverter")
            })

classTemplate =
function(from)
{
  obj = new("type")

}

setMethod("createFromXMLConverter",
           "RestrictedStringDefinition",
            function(type, namespaces, defs = NULL, types = list(), allowMissingNodes = FALSE, ...) {
              function(node, ...) {
                 xmlValue(node)
              }
            })


                # create template expressions for setting the slot
                # both by as(node, ) and by as( xmlValue(), ).
gen.tmpl = expression(obj@slotId <- as(from, "?"))[[1]]
prim.tmpl = expression(obj@slotId <- as(xmlValue(from[[""]]), "?"))[[1]]
                # templat for attribute values
attr.tmp = quote(obj@slotId <- as(xmlGetAttr(from, "attrName"), type))


setGeneric("genSlotFromConverterCode",
             function(elType, id, allowMissingNodes = FALSE, ...)
                  standardGeneric("genSlotFromConverterCode"))

            # Want to deal with optional  elements, e.g. minOccurs = "0"
            # and put tmpl in an if(!is.null(from[[id]]) )

    # Note that if we know the elements have to come in a particular order
    # we can index them by number.

setMethod("genSlotFromConverterCode", "SimpleSequenceType",
   function(elType, id, allowMissingNodes = FALSE, ...) {

     e = quote(obj@id <- new(type, lapply(from[names(from) == name], as, type)))
     e[[2]][[3]] = id
     e[[3]][[3]][[2]][[3]][[3]] = elType@elementType # elType@elType@name

     e[[3]][[3]][[4]] = elType@elType@name
     e[[3]][[2]] = elType@name
     
     e
   })

setMethod("genSlotFromConverterCode", "ANY",
   function(elType, id, allowMissingNodes = FALSE, ...)
   {
#if(id == "ResultItem") browser()
#if(id == "child") browser()

      orig = elType
      isElement = is(elType, "Element")
      if(isElement) {
         defaultValue = elType@default
         elType = elType@type
      } else {
         defaultValue = if(is(elType, "GenericSchemaType"))
                           elType@default
                        else
                           NULL
      }
      elName = elType@name
      
      optional = allowMissingNodes || is(elType, "CrossRefType")  ||
                     ((is(elType, "SchemaType") || is(elType, "Element")) &&
                       length(elType@count) && (0 %in% elType@count))
      
      if(!optional && isElement && is(orig, "Element"))
          optional = length(orig@count) && (0 %in% orig@count)

      if(is(elType, "PrimitiveSchemaType")) {

                # Deal with minOccurs, etc. for optional
                # Also, if all are required (or up to before the first optional node)
                # we can use numeric indices rather than names.
         tmpl = prim.tmpl
         tmpl[[3]][[2]][[2]][[3]] = id
                     # for "string", we don't need the extra as(, "character")
         if(elType@name == "string")
            tmpl[[3]] = tmpl[[3]][[2]]
         else
            tmpl[[3]][[3]] = getRTypeFromSOAP(elType, "type")

          if(optional) {
             prim.tmpl[[3]][[2]][[2]] = as.name("tmp")
          }
      } else {
          tmpl = gen.tmpl               
          tmpl[[3]][[3]] = getRTypeFromSOAP(elType, "type")
          tmpl[[3]][[2]] = substitute(from[[name]], list(name = id)) # elName)) #??? Should this be id or elName? Was elName. Jun 6 2012
                                             # was if(is(elType, "GenericSchemaType")) elType@name else elType #XXXXXXX
          if(optional) 
             tmpl[[3]][[2]] = as.name("tmp")

      }
                 # Convert names to NAMES, etc.
      tmpl[[2]][[3]] = as.name(id)

      if(optional) {
        e = quote(if(!is.null(tmp <- from[[""]])) do)
        e[[2]][[2]][[2]][[3]][[3]] = id
        e[[3]] = tmpl

        if(length(defaultValue)) {
            els = quote(obj@id <- as(val, type))
            els[[2]][[3]] = as.name(id)
            els[[3]][[2]] = defaultValue
            # In some cases, we can compute the value now rather than at run time.
            # e.g. altitude in kml21.xsd defauting to 0 and having a type "numeric".
            rtype = getRTypeFromSOAP(elType, "type")
#if(id %in% c("altitude")) browser()                    
            if(rtype %in% c("integer", "logical", "numeric", "character"))
               els[[3]] = as(defaultValue, rtype)
            else 
               els[[3]][[3]] = rtype
            e[[4]] = els
        }
        e
      }  else
         tmpl
})

#XXXXXXXXXXXXXXXXXXXXXXXXXX
# Deal with
# [Done] optional elements
# [Done] extended classes
# [Done] with optional attributes
# Check all

setMethod("genSlotFromConverterCode", "AttributeDef",
   function(elType, id, allowMissingNodes = FALSE) {
     
        attr.tmp[[2]][[3]] = as.symbol(id)
        attr.tmp[[3]][[2]][[3]] = id
        attr.tmp[[3]][[3]] = getRTypeFromSOAP(elType@type@name, "type") 

        if(elType@use == "optional") {
           e = quote(if(!is.null(tmp <- xmlGetAttr(from, name))) do)
           e[[2]][[2]][[2]][[3]][[3]] = id
           attr.tmp[[3]][[2]] = as.symbol("tmp")
           e[[3]] = attr.tmp
           e
        }  else
           attr.tmp
        
   })


setMethod("createFromXMLConverter",
           "ExtendedClassDefinition",
            function(type, namespaces, defs = NULL, types = list(), allowMissingNodes = FALSE, ...) {
                   # use the inherited method for ClassDefinition and then
                   # change the first expression which creates the new object
                   # by first coercing the input object to the base
                   # and then to the target type.
                                                           
               fun = callNextMethod()
               
               if(type@base %in% c("character", "string")) {
                 e = quote(obj <- new("", XMLSchema:::getNodeText(from)))
                 e[[3]][[2]] = getRTypeFromSOAP(type@name, "type")
               } else {
                 e = quote(obj <- as(as(from, base), type))
                 e[[3]][[2]][[3]] = type@base
                 e[[3]][[3]] = getRTypeFromSOAP(type@name, "type")
               }
               
               body(fun)[[2]] = e
               fun
            })

getNodeText =
   # gets the text nodes from this XML node as a character vector
function(from)
{
   isText = xmlSApply(from, is, "XMLInternalTextNode")
   if(any(isText))
     sapply(xmlChildren(from)[isText],  xmlValue)
   else
     character()
}

setMethod("createFromXMLConverter",
           "ClassDefinition",
            function(type, namespaces, defs = NULL, types = list(), allowMissingNodes = FALSE, ...) {

               f = classTemplate
               bd = body(f)
               bd[[2]][[3]][[2]] = type@name

               if(containsCrossRef(type, type@name, type@nsuri))
                  bd[[3]] = quote(if(xmlSize(from) == 0 || xmlGetAttr(from, "nil", "") == "true") return(obj))
               
               slotIds = names(type@slotTypes)
               b = mapply(genSlotFromConverterCode, type@slotTypes, slotIds, MoreArgs = list(allowMissingNodes = allowMissingNodes, ...))

               bd[seq(length(bd) + 1L, length = length(slotIds))] = b
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

# The following is a No-Op.
setMethod("createFromXMLConverter", "SchemaGroupType",
                      function(type, namespaces, defs = NULL, types = list(), allowMissingNodes = FALSE, ...) {
                            function(x, ...) {x}
                      })

setMethod("createFromXMLConverter", "ANY",
                      function(type, namespaces, defs = NULL, types = list(), allowMissingNodes = FALSE, ...) {
                         warning("No method for createFromXMLConverter for type ", class(type), " ", type@name)
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
#setMethod("createFromXMLConverter", "SimpleElement",
#                      function(type, namespaces, defs = NULL) {
#
#                      })


setMethod("createFromXMLConverter", "UnionDefinition",
                      function(type, namespaces, defs = NULL, types = list(), allowMissingNodes = FALSE, ...) {
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
                        f = function(from) { # used to have ...
                                as(from, xmlName(from))
                            }
                        new("XMLChoiceConverter", f)                        
                      })

# Recognize the primitive types.

setMethod("createFromXMLConverter",
           "SimpleSequenceType",
            function(type, namespaces, defs = NULL, types = list(), allowMissingNodes = FALSE, ...) {
              # was      type@fromConverter

                makeSequenceXMLConverter("list",  type@elType@Rname, type)
           })

# See createArrayClass in genCode.R  Compute builtinClass and elName there.
makeSequenceXMLConverter =
function(builtinClass, elName, type)
{

  if(is(type@elType, "SchemaStringType")) {
    fun = function(from)
              getChildrenStrings(from)
  } else  {
   fun = function(from)
            xmlSApply(from, as, "x")
   if(builtinClass == "list")
        body(fun)[[1]] = as.name("xmlApply")

   body(fun)[[4]] = elName # if(!is.na(which)) builtinClass else elName
 }
 environment(fun) = globalenv()
 fun
}

setMethod("createFromXMLConverter",
           "SchemaGroupType",
            function(type, namespaces, defs = NULL, types = list(), allowMissingNodes = FALSE, ...) {
#              warning("no method for createFromXMLConverter for SchemaGroupType named ", type@name)
              function() NULL
            })

setMethod("createFromXMLConverter",
           "ArrayType",
            function(type, namespaces, defs = NULL, types = list(), allowMissingNodes = FALSE, ...) {
               f = arrayTemplate
               b = body(f)

               b[[2]][[3]][[4]] = type@elType@name
#               b[[3]][[3]] = type@name
               b[[3]][[2]] = type@name
               body(f) = b
               new("XMLArrayConverter", f)
            })


setMethod("createFromXMLConverter",
          "Element",
          function(type, namespaces, defs = NULL, types = list(), allowMissingNodes = FALSE, ...) {
              schemaTypes = types

              # What about anonymous/unnamed elements/types.  We need to know the
              # R class name.
              # Attributes, including conversion to numbers, enums, etc.
              # For sequences with an unknown number of elements, have to put them into a list.
              # They won't have names.
              body = character()
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

                               typeName = sQuote(mapSchemaTypeToS(type@name, schemaTypes)) #XXX want the types in this call.
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
#            class(f) = c("SchemaElementConverter", "SchemaTypeConverter", "function")
             
            f = new("SchemaElementConverter", f)

            f
          })



setMethod("createFromXMLConverter",
           "RestrictedListType",
            function(type, namespaces, defs = NULL, types = list(), allowMissingNodes = FALSE, ...) {

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




setMethod("createFromXMLConverter", "SchemaComplexType",
     function(type, namespaces, defs = NULL, types = list(), allowMissingNodes = FALSE, ...) {

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
                 src, attrName, target, attrName, mapSchemaTypeToS(def@type, types))
  else
      sprintf("%s@%s = as(xmlGetAttr(%s, '%s'), '%s')",
                 target, attrName, src, attrName, mapSchemaTypeToS(def@type, types))
}


setMethod("createFromXMLConverter",
           "SchemaCollection",
     function(type, namespaces, defs = NULL, types = list(), allowMissingNodes = FALSE, ...) {
        types = unlist(type, recursive = FALSE)
        names = unlist(lapply(type, names))
        invisible(
           structure(lapply(types, createFromXMLConverter, namespaces, defs, type, allowMissingNodes, ...),
                   names = names))
     })


setMethod("createFromXMLConverter",
           "SimpleElement",
     function(type, namespaces, defs = NULL, types = list(), allowMissingNodes = FALSE, ...) {
       
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
