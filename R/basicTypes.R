XMLbase64Decode =
function(x) {
  library(RCurl)
  base64Decode(xmlValue(x))
}

# Named list of mappings from primitive SOAP types to S data types.
# From http://www.w3.org/TR/xmlschema-2/#built-in-primitive-datatypes
# d means done
# w means "needs more work"
##  d    3.2.1 string
##  d    3.2.2 boolean
##  d    3.2.3 decimal
##  d    3.2.4 float         
##  d    3.2.5 double
##       3.2.6 duration
##  d    3.2.7 dateTime
##  d    3.2.8 time
##  d    3.2.9 date
##  d    3.2.10 gYearMonth
##  d    3.2.11 gYear
##       3.2.12 gMonthDay
##       3.2.13 gDay
##       3.2.14 gMonth
##  w    3.2.15 hexBinary
##  w    3.2.16 base64Binary
##       3.2.17 anyURI
##       3.2.18 QName
##       3.2.19 NOTATION

# Is there an int? Yes, it is restricted from other types.
# See the tree at the top of section 3.2 of the document above.

XMLSchemaTypes <-
  list("character" = list("xsi:type" = "xsd:string", type = "string"),
       "normalizedCharacter" = list("xsi:type" = "xsd:string", type = "normalizedString"),
       
       "numeric"   = list("xsi:type" = "xsd:float", type = "float"),
       "numeric"   = list("xsi:type" = "xsd:long", type = "long"),
       "numeric"   = list("xsi:type" = "xsd:decimal", type = "decimal"),
#?      "double"   = list("xsi:type" = "xsd:float", type = "float"),
       "numeric"   = list("xsi:type" = "xsd:double", type = "double"),       
       "integer"   = list("xsi:type" = "xsd:int", type = "int"),
       "integer"   = list("xsi:type" = "xsd:short", type = "short"),
#??? Should this be  a character vector of length 1 with an element of nchar(1).
#  or a raw?       
       "integer"   = list("xsi:type" = "xsd:unsignedByte", type = "unsignedByte"),              
       "integer"   = list("xsi:type" = "xsd:unsignedShort", type = "unsignedShort"),       
       "positiveInteger" = list("xsi:type" = "xsd:int", type = "positiveInteger",
                                  from = function(x) { if(x <= 0) stop("must be positive") ; as.integer(x)}),
       "nonNegativeInteger" = list("xsi:type" = "xsd:int", type = "nonNegativeInteger",
                                  from = function(x) { if(x < 0) stop("must be non-negative") ; as.integer(x)}),              
# xsd:integer allows for a wider range than R's integer.
       "numeric"   = list("xsi:type" = "xsd:integer", type = "integer"),
       "logical"   = list("xsi:type" = "xsd:boolean", type = "boolean"),

       "character"   = list("xsi:type" = "xsd:ID", type = "ID"),
       "character"   = list("xsi:type" = "xsd:NCName", type = "NCName"),              

#TMP       "POSIXct"   = list("xsi:type" = "xsd:date", type = "date"),
       # Use SOAPDate for the as.SOAPDate to be able to allow both strings and POSIXt types.
#       "SOAPDate"   = list("xsi:type" = "xsd:date", type = "date", soapClass = "SOAPDate"),       
#       "SOAPDateTime"   = list("xsi:type" = "xsd:dateTime", type = "dateTime", soapClass = "SOAPDateTime"),
#       "SOAPTime"   = list("xsi:type" = "xsd:time", type = "time", soapClass = "SOAPTime"),       
       "date"   = list("xsi:type" = "xsd:date", type = "date", soapClass = "SOAPDateType", useCoerce = TRUE,
                           to = function(x) format(x, "%Y-%m-%d")),       
       "dateTime"   = list("xsi:type" = "xsd:dateTime", type = "dateTime", soapClass = "SOAPDateTimeType", useCoerce = TRUE),       
       "time"   = list("xsi:type" = "xsd:time", type = "time", soapClass = "SOAPTimeType", useCoerce = TRUE),

       gYear = list('xsi:type' = 'xsd:gYear', type = "gYear", soapClass = "gYearType", useCoerce = TRUE),
       gYearMonth = list('xsi:type' = 'xsd:gYearMonth', type = "gYearMonth", soapClass = "gYearMonthType", useCoerce = TRUE),       

       "duration" = list("xsi:type" = "xsd:duration", type = "duration"),

       "Base64Encoded"       = list("xsi:type" = "xsd:base64Binary", type = "base64Binary",
                                    to = function(x) as(x, "character"),
                                    from = function(x) new("Base64Encoded", xmlValue(x))),
       "raw"       = list("xsi:type" = "xsd:hexBinary", type = "hexBinary"),

       "URI"       = list("xsi:type" = "xsd:anyURI", type = "anyURI", from = parseURI),
       "token"       = list("xsi:type" = "xsd:token", type = "token"),

       "ANY"   = list("xsi:type" = "xsd:anyType", type = "anyType"),
#QName
#NOTATION
       "NULL"      = list("xsi:null" = 1, "xsi:type" = "null", type = "null"),

       "NMTOKEN" = list("xsi:NMTOKEN" = "NMTOKEN", type = "NMTOKEN")
    )


setClass("dateTime", contains = "POSIXct")
setClass("date", contains = "Date")
setClass("time", contains = "POSIXct")

as.date = function(from) {
  if(is.character(from))
     from = as.Date(from)   # we may want to try several different formats here.
  new("date", as.Date(from))
}

setAs("character", "date", as.date)
setAs("Date", "date", as.date)
setAs("POSIXct", "date", as.date)
setAs("POSIXlt", "date", as.date)


as.dateTime = function(from) {
  if(is.character(from)) {
         # try different formats until we get a non NA
     for(fmt in c("%Y-%m-%dT%H:%M:%S"))
        if(!is.na(strptime(from, fmt))) {
          from = strptime(from, fmt)
          break
        }
     if(is.character(from))
       stop("couldn't convert string to dateTime.  Create a POSIXct object directly and pass that instead")
  } 
  from = as(from, "POSIXct")
  new("dateTime", from)
}

setAs("character", "date", as.dateTime)
setAs("Date", "date", as.dateTime)
setAs("POSIXct", "date", as.dateTime)
setAs("POSIXlt", "date", as.dateTime)




checkNCName =
  function(object) {
    if(!grepl("^[A-Za-z_][A-Za-z0-9.-_]*", object))
      return("invalid string for NCName")
    TRUE
  }
setClass("NCName", contains = "character", validity = checkNCName)
setClass("ID", contains = "NCName", validity = checkNCName)
      

setClass("gYear", contains = "POSIXct")
setClass("gYearMonth", contains = "POSIXct")
setAs("character", "gYear",
       function(from) {
          from = gsub("Z$", "", from)

          val = gsub("(^[0-9]{4})(.*)", "\\1/01/01\\2", from)
          fmt = if(grepl("\\+", val))
                   "%Y/%d/%m+%H:%M"
                else
                   "%Y/%d/%m"
          tm = strptime(val, fmt)
          r = seq(tm, length = 2, by = "year")
          new("gYear", c(min(r), max(r) - 1))          
       })
#as("2010", "gYear")
#as("2010+02:00", "gYear")
#as("2010+00:00", "gYear")
# as("2010Z", "gYear")
# as("-2010", "gYear")




setAs("character", "gYearMonth",
       function(from) {
          from = gsub("Z$", "", from)
          from = gsub("((\\+)|$)", "-01\\2", from)
          fmt = if(grepl("\\+", from))
                   "%Y-%d-%m+%H:%M"
                else
                   "%Y-%d-%m"
          if(grepl("^-", from))
             fmt = sprintf("-%s", fmt)
          
          val = strptime(from, fmt)
          r = seq(val, length = 2, by = "month")
          new("gYearMonth", c(min(r), max(r) - 1))
       })

#as("2010-10", "gYearMonth")
#as("2010-10+02:00", "gYearMonth")
#as("2010-10+00:00", "gYearMonth")
# as("2010-10Z", "gYearMonth")
# as("-2010-10", "gYearMonth")




setClass("NMTOKEN", contains = "character",
          validity =
             function(object) {
                length(grep("[[:space:]]", object)) == 0
             })

trim =
function(x)
  gsub("^[[:space:]]*(.*)[[:space:]]*$", "\\1", x)


setAs("character", "NMTOKEN",
      function(from) {
         new("NMTOKEN", trim(from))
      })

setClass("positiveInteger", contains = "integer",
           validity = function(object) {
                if(object <= 0)
                   "value must be positive"
                else
                  TRUE
             })

setClass("nonNegativeInteger", contains = "integer",
           validity = function(object) {
                if(object < 0)
                   "value must be non-negative"
                else
                  TRUE
             })

setClass("normalizedCharacter",
           contains = "character",
          validity = function(object) {
            num = length(grep("[\\\n\\\t\\\r]", object))
            if(num == 0)
              TRUE
            else
               "string contains one or more newlines, tabs or line feeds"
          })

setAs("character", "normalizedCharacter",
      function(from) {
       new("normalizedCharacter", gsub("[\\\n\\\t\\\r]", " ", from))
      })

# as(c("abc\tdef", "abc\ndef"), "normalizedCharacter")

setAs("numeric", "positiveInteger", 
function(from)
{
   from = as.integer(from)  
   if(from <= 0)
     stop("value must be positive")

   from
})

setAs("numeric", "nonNegativeInteger", 
function(from)
{
   from = as.integer(from)  
   if(from < 0)
     stop("value must be non-neative")

   from
})

as.SOAPDateTime = as.SOAPDate =
function(x)
{
  if(inherits(x, "POSIXt"))
    return(x)

  x = as.character(x)
  
#  if(is.character(x))
       # check the format heuristically.
  x
}

token =
function(x)
{
  x = gsub(" +", " ", x)
  x = gsub("(\\\t|\\\r\\\n)", "", x)
    # trim the beginning and end
  x = gsub("(^ +| +$)", "", x, )  
  x
}  



# These correspond to the XMLSchema namespace (xsd in .SOAPDefaultNameSpaces)
#
SchemaPrimitiveConverters <-
                list("timeInstant" = as.POSIXct,
                     "int" = as.integer,
                     "float" = as.numeric,
                     "double" = as.numeric,
                     "string" = as.character, # function(x)  if(length(x) == 0) "" else as.character(x),
                     "boolean" = SOAP.logical,
# Extensions from Datatypes schema                     
                     "decimal" = as.numeric,
                     anyType = function(x) x)

  # Double up for the moment with xsd: prefixes to duplicate the existing entries.
SchemaPrimitiveConverters[paste("xsd", names(SchemaPrimitiveConverters), sep = ":")] <- 
                  SchemaPrimitiveConverters[names(SchemaPrimitiveConverters)]

zeroLengthArrays <-
                list("xsd:timeInstant" = as.POSIXct(character(0)),
                     "xsd:float" = numeric(0),
                     "xsd:int" = integer(0),
                     "xsd:float" = numeric(0),
                     "xsd:double" = numeric(0),                     
                     "xsd:string" = character(0),
                     "xsd:boolean" = logical(0),
# Extensions from Datatypes schema                     
                     "xsd:decimal" = numeric(0))



processSimpleList =
function(type, name, namespaceDefs = NULL)
{
  type = xmlGetAttr(type, "itemType")
  elType = SOAPType(type, namespaceDefs = namespaceDefs)
  new("RestrictedListType", name = name, elType = elType, elementType = type)
}




mapSOAPTypeToS =
  #
  # Take a SOAP type (by name or as a SOAPType object)
  # and find the name of the R data type that it maps to 
  # so that data type can be used in, e.g., a class representation
  # e.g. "int" goes to "integer".
  #
  #XXX Need to deal with namespaces. Tricky because they may have been
  # defined elsehwere, not in the top of the document. Need to match
  # the prefix to the actual URI and then compare URIs.
function(type, types = list(), namespaceDefs = list())
{
   if(is(type, "Element")) {
     return(mapSOAPTypeToS(type@type, types, namespaceDefs))
   }
   
   if(is(type, "GenericSchemaType") && length(type@Rname))
     return(type@Rname)

   if(is(type, "SelfRef"))
      return(type@name) # Use the URI


   if(is(type, "SimpleSequenceType")) {
       if(!is.na(type@name))
         return(type@name)
       
       tmp = mapSOAPTypeToS(type@elType, types, namespaceDefs)
       if(tmp %in%  c("character", "integer", "logical", "numeric"))
         return(tmp)
       else {
          if(!is.na(tmp) && tmp != "")
            return(sprintf("ListOf%s", tmp))  #XXX should constrain the element types.
         else
            return("list")
       }
   }
      
  
    if(is(type, "ClassDefinition")) {
       if(is.na(type@name)) {
         #XXX creat a representation
         warning("this isn't right - giving a ClassDefinition an R type of numeric.")
         return("numeric")
       } else
         return(type@name)
    }
  
    if(is(type, "AttributeDef") || is(type, "Element"))
       type = type@type

     if(is(type, "SOAPTypeReference"))
        type = resolve(type, types, namespaceDefs)

     if(length(type@Rname))
       return(type@Rname)

    if(is(type, "EnumValuesDef")) {
      if(length(type@name))
         return(type@name)
      # define a class based on the names and use that.
      # But we need a where.
    }

    if(is(type, "UnionDefinition") && (length(type@name) == 0 || is.na(type@name) || type@name == "")) {
        els = sapply(type@slotTypes, mapSOAPTypeToS, types = types, namespaceDefs = namespaceDefs)
        return(paste(els, collapse= "Or"))
        
    }   
      
  
    if(is(type, "GenericSchemaType")&& !is.na(type@name) )
       type = type@name
  

    if(is.na(type) || !is.character(type)) {
      stop("Don't have a meaningful value for type")
    }

    if(length(type) == 1) { # just one entry
      type =  strsplit(type, ":")[[1]]
      if(length(type) == 2)  # so we have a namespace as well as the type name
               # call this function again with these two elements.
        return(mapSOAPTypeToS(type, types, namespaceDefs))
    }

    if(length(type) == 2) {
      ns = type[1]
      type = type[2]
    }

    if(length(grep("^ArrayOf", type)) > 0 && type %in% unlist(c(sapply(types, names)))) {
        # just give back the name of the R type (to be)
      return(type)
      # return(mapSOAPTypeToS(gsub("^ArrayOf", "", type), types, namespaceDefs))
    }

    if(is.na(type)) 
      return(NA)
          

    which = sapply(XMLSchemaTypes, function(x) x[["type"]] == type) #is this now correct?
    if(any(which))
      return((names(XMLSchemaTypes)[which])[1])

#    if(length(grep(":", type)))
#      type = gsub("^.*:", "", type)
    
    if(length(types)) {

      if(is(types, "SchemaCollection")) {
        for(i in types) {
           which = match(type, sapply(names(i), discardNamespace))
#XXXif(length(which) > 1) browser()
           if(!is.na(which))
               return(discardNamespace(names(i)[which]))
        }
      } else {

        which = match(type, names(types))
        if(!is.na(which))
            return(discardNamespace(names(types)[which]))        
      }
    }

   if(type == "anySimpleType")
       return("AnySimpleType") #XXXX  want a type that is the union of the primitives, not anything.



  if(getOption("SSOAP_DEBUG", FALSE))  browser()

   if(length(type) && !is.na(type) && type != "")
       #??? Check it is a valid name in types?
      type
    else {
      warning("Can't match XML Schema type ", type)
     "XMLInternalNode"
    }
}


setAs("integer", "Base64Encoded",
      function(from) {
        bytes = writeBin(from, raw(), 1L)
        new("Base64Encoded", as.character(base64Encode(bytes)))
      })


