
         
createRestrictionType =
function(name, rnode, namespaces = list(), targetNamespace = NA, base = xmlGetAttr(rnode, "base"))
{
     #XXX need to figure out the namespace and check it is an XSD double or integer
  
 if(length(base) > 1) base = base[2]

   if(base == "double" || base == "integer") {
      r = getRestrictedRange(rnode)
      className = switch(base, double = "RestrictedDouble", integer = "RestrictedInteger")
      def = new(className, name = name, range = r$range, inclusive = r$inclusive)

         # make the converter
      def@fromConverter =  makeValueFromConverter(def@name)
      def@toConverter =  makeValueToConverter(def@name, targetNamespace)      
      
   } else if (base == "hexBinary") {
      if("length" %in% names(rnode))
        len = xmlGetAttr(rnode[["length"]], "value", NA)
      else
        len = NA
      rx = if(!is.na(len)) sprintf("^([a-fA-F0-9]{2}){%d}$", as.integer(len)) else "^([a-fA-F]){2}$"
      
      def = new("RestrictedHexBinary", length = as.integer(len), pattern = rx, name = name)

      def@fromConverter = makeValueFromConverter(def@name)
      def@toConverter = makeValueToConverter(def@name, targetNamespace)      

   } else {
                                        #XXX Assume enumeration values for present.
     if(!(base %in% c("string", "token", "NMTOKEN", "Name", "ID", "IDREF"))) {
        warning("treating ", name, " as an enumeration type")
     }


     if(base == "string")
        def = createRestrictedStringDefinition(rnode, name, targetNamespace)
     else
        def <- new("EnumValuesDef", name = name, values = xmlSApply(rnode,  xmlGetAttr, "value"))
     if(length(name) == 0)  #XXX
              def@name = paste(def@values, collapse = ",")
   }

   def@nsuri = targetNamespace
   def@name = name
   
   def
}

getRestrictedRange =
function(node)
{
  ids = names(node)

  range = structure(as.numeric(c(NA, NA)), names = c("min", "max"))
  inc =  structure(logical(2), names = c("min", "max"))
  for(w in c("min", "max")) {
     i = grep(sprintf("^%s", w), ids)
     if(length(i)) {
        range[w] = xmlGetAttr(node[[i]], "value", , as.numeric)
        inc[w] = ids[i] == sprintf("%sInclusive", w)
     }
  }

  
  list(range = range, inclusive = inc)
}


makeRestrictedFunc =
function(className, base, range, inc)
{

   f = function(from) {
         from = as(from, base)
         if(length(from) && from <= range["min"])
             stop("")

         if(length(from) && from >= range["max"])
             stop("")
         
         new(className, from)
    }

    body(f)[[2]][[3]][[3]] = base

    body(f)[[3]][[2]][[3]] = range["min"]
    body(f)[[3]][[3]][[2]] = paste("value is too small. Must be larger than", range["min"])


    body(f)[[4]][[2]][[3]] = range["max"]
    body(f)[[4]][[3]][[2]] = paste("value is too large. Must be less than", range["max"])

   if(inc["min"])
          body(f)[[3]][[2]][[1]] = as.name("<")
   if(inc["max"])
          body(f)[[4]][[2]][[1]] = as.name(">")
   
   body(f)[[5]][[2]] = className

   
   if(all(is.na(range)))
       body(f) = body(f)[[4]]
   else if(is.na(range["min"]))
     body(f) = body(f)[-2]
   else if(is.na(range["max"]))  # could have removed the two.
     body(f) = body(f)[-3]   


   environment(f) = globalenv()
   
   f
}



createRestrictedStringDefinition =
function(type, name, nsuri = character())
{
  if(xmlName(type) != "restriction")
    res = type[[1]]
  else
    res = type
  
           #This is wrong!
           # new("EnumValuesDef", name = name, values = xmlSApply(type[[1]],  xmlGetAttr, "value"))
#  kids = xmlChildren(type[[1]])[ ! xmlSApply(type[[1]], is, "XMLInternalTextNode") ]
  if(xmlName(res) != "restriction") 
    warning("createRestrictedStringDefinition passed a node ", xmlName(res), " that is not a <restriction> node")


  
  kids = res[ ! xmlSApply(res, is, "XMLInternalTextNode") ]
  names = sapply(kids, xmlName)

  if(all(names == "enumeration")) {
    vals = if(length(kids))
              sapply(kids,  xmlGetAttr, "value")
            else
              character()
#     fun = function(from) as(from, name)
#     body(fun)[[3]] = name
     fun = function(from) new(name, xmlValue(from))
     body(fun)[[2]] = name    
     environment(fun) = DefaultFunctionNamespace

      # need the namespace on the node.
     toFun = makeValueToConverter(name, nsuri, DefaultFunctionNamespace)
    
     new("RestrictedStringDefinition", name = name, values = vals,
                  Rname = name,
                  ns = "xsd", nsuri = c(xsd = "http://www.w3.org/2001/XMLSchema"),
                  toConverter = toFun,
                  fromConverter = fun)
  } else {

    if(any(names == "pattern")) {
       pattern = xmlGetAttr((kids[names(kids) == "pattern"])[[1]], "value")
       pattern = sprintf("^%s$", pattern)
       def = new("RestrictedStringPatternDefinition", name = name, pattern = pattern,
                     ns = "xsd", nsuri = c(xsd = "http://www.w3.org/2001/XMLSchema"))
       def@fromConverter = function(from) {
                              if(is(from, "XMLAbstractNode"))
                                 from = xmlValue(from)
                              as(from, "character")
                           }
       environment(def@fromConverter) = DefaultFunctionNamespace
       
       epattern = paste("^", pattern, "$", sep = "")
       def@toConverter = function(from, epattern = "") {
                             x = as(from, "character")
                             if(length(grep(epattern, x)) == 0)
                               stop("Invalid string: doesn't match expected pattern")
                             x
                         }
          # clean up the environment.
       environment(def@toConverter) = DefaultFunctionNamespace
       formals(def@toConverter)[["epattern"]] = epattern
       
       def
    }
  }
}

makeValueFromConverter =
function(className)
{
    fun = function(from)  {}
    body(fun)[[2]] = substitute(as(xmlValue(from), name), list(name = className))
    environment(fun) = globalenv()
    fun
}

makeValueToConverter =
function(nodeName, namespace = character(), env = globalenv())
{
  toFun = function(from) newXMLNode(name, as.character(from))
  body(toFun)[[2]] = nodeName
  if(length(namespace) && !is.na(namespace))
       body(toFun)[["namespaceDefinitions"]] = namespace
  environment(toFun) = env

  toFun
}
