
W3SchemaURIs =
list( "1.1" =
        c('xsi'="http://www.w3.org/1999/XMLSchema-instance",
          'xsd'="http://www.w3.org/1999/XMLSchema"),
      "1.2" =
       c( 'xsi'="http://www.w3.org/2001/XMLSchema-instance",
          'xsd'="http://www.w3.org/2001/XMLSchema"))

DefaultFunctionNamespace = getNamespace("XML")

#???  should this be in another package, e.g. XML?
setClass("URI", contains = "character")
setAs("character", "URI",
        function(from) new("URI", from))

setClass("string", contains = "character")
setAs("XMLAbstractNode", "string",
      function(from)
        new("string", xmlValue(from)))


setClass("ListOfAnySimpleType", contains = "list",
           validity = function(object) if(!all(sapply(object, is, "AnySimpleType")))
                                            "not all elements are simple"
                                       else
                                           TRUE)
        

# A class to map the names of elements to the R class to which they correspond.
# e.g. <kml> - KmlType
setClass("XMLElementTypeMap", representation(names = "character"), contains = "character")


setClass("ListOfANY", contains = "list")

################################################################

# Description of a SOAP type 
# Later in the hierarcahy, we include a function that can convert to and from a SOAP representation and R.

# Corresponds to anySimpleType in XSD.

setClass("CodeGenOpts", representation(makePrototype = "logical", defineEnumVars = "logical",
                                       allowMissingNodes = "logical", verbose = "logical"),
           prototype = prototype(makePrototype = TRUE, defineEnumVars = FALSE, allowMissingNodes = FALSE, verbose = FALSE))

setClass("Base64Encoded", contains = "character")
setAs("Base64Encoded", "raw",
        function(from) {
           library(RCurl)
           base64Decode(from, "raw")
        })

setAs("Base64Encoded", "character",
        function(from) {
          library(RCurl)
          base64Decode(from)
        })


setClassUnion("AnySimpleType",  c("logical", "integer", "numeric", "character"))

setClass("GenericSchemaType", representation(name="character",
                                             ns="character",
                                             nsuri="character",
                                             default = "ANY", # "character",                                             
                                             Rname = "character", # the name of the type in R that corresponds to this class. Needed if we have to manually specify it.
                                             documentation = "character"))


setClass("SchemaType", representation(count = "numeric", # use numeric rather than integer because of Inf for unbounded1.
                                      abstract = "logical"
                                    ), contains = "GenericSchemaType",
                     prototype = prototype(default = NA_character_))


 # used for a slot that has a type that is the same as the parent type being defined
 # i.e. a forward reference to the same type, e. g. ItemType in eutils.wsdl
 # or struct  Foo {  struct Foo *f;}
 # What this to extend SchemaType so we can put it into an Element
setClass("SelfRef", contains = "SchemaType")


# The following are for types that involve recursive cross-references.
# The CrossRefType is for representing a SOAP/XMLSchema type.
# The CrossRefClass is for defining a class corresponding to a CrossRefType induced in an XMLSchema
setClass("CrossRefType", representation(subclasses = "character"), contains = "SchemaType",
                          prototype = prototype(default = NULL))
setClass("CrossRefClass")




# For representing arbitrary content, e.g. <complexType/>
#XXX What is the difference between this and SchemaAnyType
setClass("AnySchemaType", contains = "SchemaType",
            prototype = list(name = "AnySchemaType", nsuri = W3SchemaURIs[['1.2']]["xsd"], Rname = "ANY"))
#setClass("SchemaAnyType", representation("SchemaType"),
#            prototype = list(name = "any", nsuri = W3SchemaURIs[['1.2']]["xsd"], Rname = "ANY"))


setClass("SchemaVoidType", representation("SchemaType"),
            prototype = list(name = "null", ns = "xsi", Rname = "NULL"))

setClass("SchemaTypeReference", representation("SchemaType"))
setClass("AttributeGroupReference", contains = "SchemaTypeReference")


setClass("TerminalSchemaType", representation("VIRTUAL"))

setClass("BasicSchemaType", representation("SchemaType",
                                         fromConverter = "function",  # From XML to R
                                         toConverter = "function"))   # to XML from R

setClassUnion("SchemaTypeOrList", c("SchemaType", "list", "NULL"))

setClass("SchemaComplexType",
          representation(attributes = "list", content = "SchemaTypeOrList", xmlAttrs = "character"),
          contains = "BasicSchemaType")
            


#XXX Is this a setClassUnion ?
setClass("PrimitiveSchemaType", contains = c("BasicSchemaType", "TerminalSchemaType"))

setClass("SchemaIntType", contains = "PrimitiveSchemaType",
                 prototype = list(name = "int", Rname = "integer"))
setClass("SchemaDoubleType", contains = "PrimitiveSchemaType",
                 prototype = list(name = "double", Rname = "numeric"))
setClass("SchemaFloatType", contains = "PrimitiveSchemaType",
                 prototype = list(name = "float", Rname = "numeric"))

setClass("SchemaStringType", contains = "PrimitiveSchemaType",
           prototype = list(name = "string", # Rname = "string",
                            fromConverter = function(from) xmlValue(from)))


  # Should we drop the Type from the following class names.
  #  No probably leave gYear, etc. for the actual values, not the types
  #  See basicTypes.R setClass("gYear", contains= "integer") # "PrimitiveSchemaType")
setClass("SchemaDateType", contains = "PrimitiveSchemaType")
setClass("SchemaDateTimeType", contains = "PrimitiveSchemaType")
setClass("SchemaTimeType", contains = "PrimitiveSchemaType")

setClass("gYearType", contains = "PrimitiveSchemaType")
setClass("gYearMonthType", contains = "PrimitiveSchemaType")






setClass("SchemaCollection", representation(circularDefs = "list"), contains = "list")
setClass("SchemaTypes", representation(elementFormQualified = "logical",
                                       targetNamespace = "character",
                                       namespaceDefs = "XMLNamespaceDefinitions"), contains = "list")

setClassUnion("SchemaTypeOrNULL", c("SchemaType", "NULL"))


     # xmlAttrs here are for the attributes on the XML node that we don't know how to process in place so hold on to.
     #  e.g. nillable = "true" on anyType in DailValues.asmx?wsdl
setClass("SchemaElement", representation(name = "character",
                                         attributes = "list", count = "numeric",
                                         xmlAttrs = "character"), contains = "GenericSchemaType") # was BasicSchemaType - added Sep 18 as an experiment.
                                                                                                       # GenericSchemaType added 28 Oct.
setClass("SimpleElement", representation(type = "character", nsuri = "character"), contains = "SchemaElement")

setClassUnion("SchemaTypeOrNULLOrElement", c("SchemaTypeOrNULL", "SchemaElement"))
setClass("Element", representation(type = "SchemaTypeOrNULLOrElement"),
                     contains = "SchemaElement")

setClass("LocalElement", contains = "Element") # representation(count = "numeric"),


#setClass("EnumValuesDef",  contains = "character")
setClass("EnumValuesDef",  representation(values = "character"), contains = "SchemaType")




setClass("XMLSchemaComponent")


# Was SchemaType and not BasicSchemaType. But needed the fromConverter. See 
#   http://www.weather.gov/forecasts/xml/DWMLgen/wsdl/ndfdXML.wsdl
setClass("RestrictedStringType", contains = "BasicSchemaType") #  , prototype = list(Rname = "string"))
#              prototype = list(ns = "xsd", nsuri = c(xsd = "http://www.w3.org/2001/XMLSchema")))
setClass("RestrictedStringDefinition", representation(values = "character"), contains = "RestrictedStringType")
setClass("RestrictedStringPatternDefinition", representation(pattern = "character"), contains = "RestrictedStringType")



setClass("RestrictedNumber", representation(range = "numeric", inclusive = "logical"), contains = "BasicSchemaType")
setClass("RestrictedDouble", contains = "RestrictedNumber")
# Don't define these                  prototype = list(Rname = "numeric"))
setClass("RestrictedInteger", contains = "RestrictedNumber")
#                         prototype = list(Rname = "integer"))

setClass("RestrictedHexBinary", representation(length = "integer", pattern = "character"), # pattern is a regular expression.
                                  contains = "RestrictedStringType")


setClassUnion("AttributeType", c("RestrictedStringType", "EnumValuesDef", "character", "GenericSchemaType"))

 # We have two types of attributes - an actual one and a wildcard one.
 # So we use a common base class.
 #  We don't actually need the name for the AnyAttributeDef, but we share it here for now
 # since the code that processes the nodes in the schema wants to set the name.
setClass("GenericAttributeType", contains = "GenericSchemaType") # was "XMLSchemaComponent"

setClass("AnyAttributeDef", contains = "GenericAttributeType")
setClass("AttributeDef", representation(type = "AttributeType",
                                        use = "character",  # prohibited, optional, required
                                        default = "character",
                                        fixed = "character"
                                        ),
                         prototype = list(use = "optional"), contains = "GenericAttributeType")

setClass("AttributeGroup", representation(attributes = "list"), contains = "GenericAttributeType")


setClassUnion("SchemaTypeOrElement", c("SchemaType", "Element")) # Don't need if elType in SimpleSequenceType is not this union.

setClass("SimpleSequenceType", representation("BasicSchemaType", elementType = "character", elType = "SchemaTypeOrNULLOrElement")) # OrElement???

# White space spearated values
setClass("ListType", contains = "SimpleSequenceType")
setClass("RestrictedListType", representation(elements = "character",  # is elements somehow already in elType and elementType
                                              baseType = "character"), # baseType - either list or a vector type, e.g. character,
                                                                       #       numeric, integer, logical
                               contains = "ListType",
                            prototype = list(baseType = "list"))

                                              
setClass("ArrayType", representation("SimpleSequenceType", dims = "integer"))




# A definition for a complex data type defined in a WSDL file.
setClass("CompositeTypeDefinition", representation("BasicSchemaType",
                                                    slotTypes = "list",
                                                    isAttribute = "logical", # indicates which slots are attributes.
                                                    uris = "character"), # URIs identify the namespace in which the types were defined
                                                                         # which we need as we just have the names in slotTypes,
                                                                         # along with the namespace prefix.
                                    contains = "TerminalSchemaType")
setClass("ClassDefinition",  contains = "CompositeTypeDefinition")
setClass("UnionDefinition", contains = "CompositeTypeDefinition")
setClass("ArrayClassDefinition",  contains = "CompositeTypeDefinition")


# We might try to extend CompositeTypeDefinition below.
setClass("SchemaGroupType", representation(slotTypes = "list"), contains = "CompositeTypeDefinition")
setClass("SchemaGroupRefType", contains = "SchemaType")




setClass("RestrictedSetInteger", representation(values = "integer"), contains = "BasicSchemaType")

setClass("ExtendedClassDefinition", representation(base = "character", baseType = "GenericSchemaType"), contains = "ClassDefinition")


#
# When we generate an S4 class to represent a SOAP compound data type,
# we have it extend this degenerate class. This allows us to
# provide methods that dispatch on that class such as converting from
# S to SOAP values and vice-verse.
setClass("VirtualXMLSchemaClass")




#XX need validity methods to ensure it is a proper restriction of a character.
setClass("token", contains = "character")
#XXX we won't leave this as a character
setClass("duration", contains = "character")

setClass("URIString", contains = "URI")

setOldClass("WSDLTypeDescription")

# A description of a SOAP method in a server.
# See http://www-128.ibm.com/developerworks/webservices/library/ws-whichwsdl/ for
# information about bindingStyle and use fields.




# Class for a single string. This is to be sub-classed.
setClass("StringEnum", contains = "character")



