
DefaultFunctionNamespace = getNamespace("XML")


################################################################

# Description of a SOAP type 
# Later in the hierarcahy, we include a function that can convert to and from a SOAP representation and R.

# Corresponds to anySimpleType in XSD.

setClass("CodeGenOpts", representation(makePrototype = "logical"),
           prototype = prototype(makePrototype = TRUE))

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


setClass("SOAPType", representation(count = "numeric", # use numeric rather than integer because of Inf for unbounded1.
                                    abstract = "logical"
                                    ), contains = "GenericSchemaType",
                     prototype = prototype(default = NA_character_))


 # used for a slot that has a type that is the same as the parent type being defined
 # i.e. a forward reference to the same type, e. g. ItemType in eutils.wsdl
 # or struct  Foo {  struct Foo *f;}
 # What this to extend SOAPType so we can put it into an Element
setClass("SelfRef", contains = "SOAPType")


# The following are for types that involve recursive cross-references.
# The CrossRefType is for representing a SOAP/XMLSchema type.
# The CrossRefClass is for defining a class corresponding to a CrossRefType induced in an XMLSchema
setClass("CrossRefType", representation(subclasses = "character"), contains = "GenericSchemaType")
setClass("CrossRefClass")




# For representing arbitrary content, e.g. <complexType/>
setClass("AnySOAPType", contains = "SOAPType", prototype = list(name = "AnySOAPType"))

setClass("SOAPVoidType", representation("SOAPType"),
            prototype = list(name = "null", ns = "xsi"))
setClass("SOAPAnyType", representation("SOAPType"))

setClass("SOAPTypeReference", representation("SOAPType"))


setClass("TerminalSOAPType", representation("VIRTUAL"))

setClass("BasicSOAPType", representation("SOAPType",
                                         fromConverter = "function",  # From SOAP to R
                                         toConverter = "function"))   # to SOAP from R

setClassUnion("SOAPTypeOrList", c("SOAPType", "list", "NULL"))

setClass("SOAPComplexType",
          representation(attributes = "list", content = "SOAPTypeOrList", xmlAttrs = "character"),
          contains = "BasicSOAPType")
            


#XXX Is this a setClassUnion ?
setClass("PrimitiveSOAPType", contains = c("BasicSOAPType", "TerminalSOAPType"))

setClass("SOAPStringType", contains = "PrimitiveSOAPType")


#setClass("SOAPDate", contains = "PrimitiveSOAPType")
#setClass("SOAPDateTime", contains = "PrimitiveSOAPType")
#setClass("SOAPTime", contains = "PrimitiveSOAPType")

setClass("SOAPDateType", contains = "PrimitiveSOAPType")
setClass("SOAPDateTimeType", contains = "PrimitiveSOAPType")
setClass("SOAPTimeType", contains = "PrimitiveSOAPType")

setClass("gYearType", contains = "PrimitiveSOAPType")
setClass("gYearMonthType", contains = "PrimitiveSOAPType")

# See basicTypes.R setClass("gYear", contains= "integer") # "PrimitiveSOAPType")




setClass("SchemaCollection", representation(circularDefs = "list"), contains = "list")
setClass("SchemaTypes", representation(elementFormQualified = "logical",
                                       targetNamespace = "character",
                                       namespaceDefs = "XMLNamespaceDefinitions"), contains = "list")

setClassUnion("SOAPTypeOrNULL", c("SOAPType", "NULL"))


     # xmlAttrs here are for the attributes on the XML node that we don't know how to process in place so hold on to.
     #  e.g. nillable = "true" on anyType in DailValues.asmx?wsdl
setClass("SchemaElement", representation(name = "character",
                                         attributes = "list",
                                         xmlAttrs = "character"), contains = "GenericSchemaType") # was BasicSOAPType - added Sep 18 as an experiment.
                                                                                                       # GenericSchemaType added 28 Oct.
setClass("SimpleElement", representation(type = "character", nsuri = "character"), contains = "SchemaElement")

setClassUnion("SOAPTypeOrNULLOrElement", c("SOAPTypeOrNULL", "SchemaElement"))
setClass("Element", representation(type = "SOAPTypeOrNULLOrElement"),
                     contains = "SchemaElement")

setClass("LocalElement", representation(count = "numeric"), contains = "Element")


#setClass("EnumValuesDef",  contains = "character")
setClass("EnumValuesDef",  representation(values = "character"), contains = "SOAPType")




setClass("XMLSchemaComponent")


# Was SOAPType and not BasicSOAPType. But needed the fromConverter. See 
#   http://www.weather.gov/forecasts/xml/DWMLgen/wsdl/ndfdXML.wsdl
setClass("RestrictedStringType", contains = "BasicSOAPType")
#              prototype = list(ns = "xsd", nsuri = c(xsd = "http://www.w3.org/2001/XMLSchema")))
setClass("RestrictedStringDefinition", representation(values = "character"), contains = "RestrictedStringType")
setClass("RestrictedStringPatternDefinition", representation(pattern = "character"), contains = "RestrictedStringType")


setClass("RestrictedNumber", representation(range = "numeric", inclusive = "logical"), contains = "SOAPType")
setClass("RestrictedDouble", contains = "RestrictedNumber")
setClass("RestrictedInteger", contains = "RestrictedNumber")

setClass("RestrictedHexBinary", representation(length = "integer", pattern = "character"), # pattern is a regular expression.
                                  contains = "RestrictedStringType")


setClassUnion("AttributeType", c("RestrictedStringType", "EnumValuesDef", "character", "GenericSchemaType"))

setClass("AttributeDef", representation(name = "character",
                                        type = "AttributeType",
                                        use = "character",  # prohibited, optional, required
                                        default = "character",
                                        fixed = "character"
                                        ),
                         prototype = list(use = "optional"), contains = "XMLSchemaComponent")


setClassUnion("SOAPTypeOrElement", c("SOAPType", "Element")) # Don't need if elType in SimpleSequenceType is not this union.

setClass("SimpleSequenceType", representation("BasicSOAPType", elementType = "character", elType = "SOAPTypeOrNULLOrElement")) # OrElement???

# White space spearated values
setClass("ListType", contains = "SimpleSequenceType")
setClass("RestrictedListType", representation(elements = "character"),  # is elements somehow already in elType and elementType
                               contains = "ListType")

                                              
setClass("ArrayType", representation("SimpleSequenceType", dims = "integer"))




# A definition for a complex data type defined in a WSDL file.
setClass("CompositeTypeDefinition", representation("BasicSOAPType",
                                                    slotTypes = "list",
                                                    isAttribute = "logical", # indicates which slots are attributes.
                                                    uris = "character"), # URIs identify the namespace in which the types were defined
                                                                         # which we need as we just have the names in slotTypes,
                                                                         # along with the namespace prefix.
                                    contains = "TerminalSOAPType")
setClass("ClassDefinition",  contains = "CompositeTypeDefinition")
setClass("UnionDefinition", contains = "CompositeTypeDefinition")
setClass("ArrayClassDefinition",  contains = "CompositeTypeDefinition")


# We might try to extend CompositeTypeDefinition below.
setClass("SOAPGroupType", representation(slotTypes = "list"), contains = "CompositeTypeDefinition")
setClass("SOAPGroupRefType", contains = "SOAPType")




setClass("RestrictedSetInteger", representation(values = "integer"), contains = "BasicSOAPType")

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



