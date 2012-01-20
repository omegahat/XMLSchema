verbose = FALSE


capitalizeFirstLetter =
function(word)
{
  els = strsplit(word, "")[[1]]
  els[1] = toupper(els[1])
  paste(els, collapse="")
}

convertToSName =
  #
  #  Convert a string to a "legitimate" name in the S/R language.
  #
function(name, useDash = if("UseDashInSOAPNames" %in% names(options()))
                            getOption("UseDashInSOAPNames")
                         else
                            TRUE)
{
  if(useDash)
    return(name)
  
  els = strsplit(name, "_")[[1]]
  if(length(els) == 1)
    return(els)
  
  els[2:length(els)] = sapply(els[-1], capitalizeFirstLetter)

  paste(els, collapse="")
}


defineClasses =
  #
  # namespaceDefs is used in mapSOAPTypeToS as the third argument.
  #
function(types,  where = globalenv(), namespaceDefs = list(), verbose = FALSE,
          baseClass = "VirtualSOAPClass", force = FALSE, opts = new("CodeGenOpts"),
          pending = new.env(hash = TRUE, emptyenv()), classes = new.env(hash = TRUE, emptyenv())  )
{
  if(is(types, "SchemaTypes"))
    types = structure(list(types), class = "SchemaCollection")
  
       # for each schema, define each type.
  lapply(types,  function(schema)  lapply(schema, defClass, where, namespaceDefs, verbose, pending, classes, types, baseClass, force, opts = opts))
  ls(classes)
}

showDefClassTrace = FALSE

setGeneric("computeName", function(type, ...) standardGeneric("computeName"))

setMethod("computeName", "SchemaElement",
            function(type, ...)
                type@name)

setMethod("computeName", "UnionDefinition",
            function(type, ...)
               paste( sapply(type@slotTypes, getName), collapse = "."))

setMethod("computeName", "AttributeDef",
            function(type, ...)
               type@name)

setMethod("computeName", "SOAPType",
            function(type, ...) {
              id = type@name
              if(length(id) == 0 || is.na(id) || id == "") stop("Problem with empty name")
              id
        })

setMethod("computeName", "SimpleSequenceType",
            function(type, ...) {
              id = type@elType@name
              sprintf("ListOf%s", id)
        })

setMethod("computeName", "SOAPType",
            function(type, ...) {
              type@name
        })

setMethod("computeName", "Element",
            function(type, ...) {
             if(is.na(type@name) || type@name == "")
                return(computeName(type@type, ...))
             type@name
        })


setMethod("computeName", "ClassDefinition",
            function(type, ...) {
              if(is.na(type@name) || type@name == "")
                return(paste(names(type@slotTypes), collapse = "."))
              else
                type@name
        })

if(FALSE)
setMethod("computeName", "NULL",
            function(type, ...) {
               "" # or NA or character()
             })

getName =
function(i, compute = FALSE)
{
    # e.g. GetDatabases in MassSpecAPI is an empty element so type is NULL
  ans = if(is(i, "Element") && length(i@type))  {
           if(length(i@type@Rname)) i@type@Rname else i@type@name
        } else if(is(i, "GenericSchemaType") || is(i, "XMLSchemaComponent")) {
           if(length(i@Rname)) i@Rname else i@name
        } else
           i$name

  if(is.na(ans) && compute)
    computeName(i)
  else
    ans
}

setGeneric("defClass",
function(i, where = globalenv(),
         namespaceDefs = list(),
         verbose = FALSE,
         pending = new.env(hash = TRUE, emptyenv()),
         classes = new.env(hash = TRUE, emptyenv()),
         types = NULL,
         baseClass = "VirtualSOAPClass", force = FALSE,
         name = getName(i),
         ignorePending = FALSE, opts = new("CodeGenOpts"))
{
  orig = i

    if(is.null(i))
      return(FALSE)

    if(is(i, "AttributeDef")) {
      i = i@type
    }

  if(length(i@nsuri) && !is.na(i@nsuri) && i@nsuri %in% "http://www.w3.org/2001/XMLSchema") {
           # the type refers to a type defined in the XML schema language itself.
      return(getSchemaClass(i, types))
    
  }
    
    
#    if(name %in% c("eComp", "ECompression")) {cat("Hey", name, "\n"); browser()}
    while(is(i, "Element"))
      i = i@type

    if(is.null(i))
      return(FALSE)

    if(is(i, "SOAPTypeReference"))
       i = resolve(i, types, namespaceDefs, recursive = TRUE, xrefInfo = types@circularDefs)    
    
    if(is(i, "SchemaTypes"))
     return(standardGeneric("defClass"))
    
    type = i
    if(is(i, "XMLSchemaComponent"))
       type = i@type

    if(length(name) == 0 || is.na(name) || name == "")
      name = computeName(i)

#if(name %in%c("uType", "iType", "bType")) browser()
  
    if(verbose) {
      cat("<defClass>", name, "\n")
      on.exit(cat("finished", name, "\n"))
    }

   
           # if it is already defined, skip this unless force is TRUE.
   if(!force && !is.null(defn <- getClassDef( name, where )))
      return(defn) # structure(FALSE, names = "class already exists"))

           # if it is currently pending, then don't do it as we will get recursive calls.
   if(!ignorePending && exists(name, pending))
      return(structure(NA, names = "pending definition"))

           # register the type as pending since we are about to define it.
     assign(name, "TRUE", pending)
    
     on.exit({   # arrange to clean this up.  ??? Should we do this if overridePending = TRUE
               if(verbose && exists(name, envir = pending, inherits = FALSE)) {
                  cat("Removing ", name, "from pending\n")
               remove(list = name, envir = pending, inherits = FALSE)
              }
             })    
    
  
    def = standardGeneric("defClass")
    
    if(!is.null(def)) {

      if(is(type, "BasicSOAPType") && length(formals(type@fromConverter)) > 0) {
        if(verbose)
            cat("defining setAs() for", type@name, "\n")
        if(is(type@fromConverter, "SOAPElementConverter"))
           cvt = as(type@fromConverter, "AsFunction")
        else
           cvt = type@fromConverter
        setAs("XMLAbstractNode", type@name, cvt, where = where)
      }         

      assign(name, def, classes)

      def
    } else
      NULL    
})

if(FALSE) {
# If we enable this, we lose the ArrayOfInt definition in MassSpecAPI.asmx
setMethod("defClass", "LocalElement",
          function(i, where = globalenv(),
                   namespaceDefs = list(),
                   verbose = FALSE,
                   pending = new.env(hash = TRUE, emptyenv()),
                   classes = new.env(hash = TRUE, emptyenv()),
                   types = NULL,
                   baseClass = "VirtualSOAPClass",
                   force = FALSE,
                   name = getName(i),
                   ignorePending = FALSE, opts = new("CodeGenOpts")) {

            defClass(i@type, where, namespaceDefs, verbose, pending, classes, types, baseClass, force, name, ignorePending, opts)
          })
}

setMethod("defClass", "SchemaTypes",
          function(i, where = globalenv(),
                   namespaceDefs = list(),
                   verbose = FALSE,
                   pending = new.env(hash = TRUE, emptyenv()),
                   classes = new.env(hash = TRUE, emptyenv()),
                   types = NULL,
                   baseClass = "VirtualSOAPClass", force = FALSE,
                   name = if(is(i, "GenericSchemaType") || is(i, "XMLSchemaComponent")) i@name else i$name,
                   ignorePending = FALSE, opts = new("CodeGenOpts")) {

    lapply(i, defClass, where, namespaceDefs, verbose, pending, classes, types, baseClass, force, name, ignorePending, opts)
   })


setMethod("defClass", "Element",
          function(i, where = globalenv(),
                   namespaceDefs = list(),
                   verbose = FALSE,
                   pending = new.env(hash = TRUE, emptyenv()),
                   classes = new.env(hash = TRUE, emptyenv()),
                   types = NULL,
                   baseClass = "VirtualSOAPClass", force = FALSE,
                   name = getName(i),
                   ignorePending = FALSE, opts = new("CodeGenOpts")) {
            defClass(i@type, where, namespaceDefs, verbose, pending, classes, types, baseClass, force, name, ignorePending, opts)
      })

setMethod("defClass", "ANY",
          function(i, where = globalenv(),
                   namespaceDefs = list(),
                   verbose = FALSE,
                   pending = new.env(hash = TRUE, emptyenv()),
                   classes = new.env(hash = TRUE, emptyenv()),
                   types = NULL,
                   baseClass = "VirtualSOAPClass", force = FALSE,
                   name = getName(i),
                   ignorePending = FALSE, opts = new("CodeGenOpts")) {
  def = NULL

  
#if(name == "ResourceIdSetType") {
#  unlockBinding("showDefClassTrace", getNamespace("XMLSchema"))
#  showDefClassTrace <<- TRUE
#}

if(showDefClassTrace)
  print(sys.calls())

 
         if(is(i, "XMLAbstractNode") || is.null(i)) {
           return(NA)
         }

         type = i

  
         if(is(i, "AnySOAPType")) {
             if(verbose)
                 cat("defining", name, "\n")
             setClass(name, where = where)
     	     return(TRUE)
	 }
          

         o = i
         if(!is(i, "GenericSchemaType"))
           i = i$definition

         if(is(i, "RestrictedStringDefinition")) {

            valid = function(object) {
               values = ""
               if(!any(object == values))
                  stop(object, " is not a recognized value ", paste(sQuote(values), collapse = ', '))
               TRUE
            }
            body(valid)[[2]][[3]] = i@values
            def = setClass(name, contains = "character", validity = valid, where = where)
            

         } else if(is(i, "RestrictedSetInteger")) {

            # add a validity
           def = setClass(name, "integer", where = where)
             # coercion method.
           fun = function(from)
                     asIntegerSetValue(from, 'a', 'b')
           body(fun)[[3]] = i@values
           body(fun)[[4]] = name
           environment(fun) = emptyenv()
           setAs("numeric", name, fun, where = where)
           setAs("character", name, fun, where = where)           

         } else if(is(i, "EnumValuesDef")) {

           elName = paste(name, "Values", sep = "_")
           assign(elName, as.character(i@values), envir = where)
           #XXX No ZZ and no validateEnum!
#           f = function(object) validateEnum(object, ZZ)
# See RGCCTranslationUnit and RAutoGenRunTime....           
#           body(f)[[3]] = as.character(i@values)
           f = NULL

            if(verbose)
                cat("defining class", name, "\n")           

           def = setClass(name, contains = c("character", baseClass), validity = f, where = where)
           
         } else if(is(i, "ClassDefinition")) {

               #   def <- createTypeClass(i, types, where = where)                       

           def = defineClassDefinition(i, types, namespaceDefs, name, classes, pending, baseClass, where, verbose, force, opts = opts)
           
         } else if(is(i, "Element")) {
               # recursively define the type, using the Element's name.
if(verbose) cat("<defClass>element", type@name, "\n")

if(TRUE) {
           tmp = if(class(i@type) %in% c("SOAPType", "SOAPTypeReference"))
                     resolve(i@type, types)
                 else
                    i@type
} else tmp = i@type           

           def = defClass(tmp, where, namespaceDefs, verbose, pending, classes, types, baseClass, force, name = type@name, ignorePending = TRUE, opts = opts)
           return(def)
         } else if(is(i, "SimpleSequenceType")) {   # XXX was "ArrayType" Nov 6.
            def = createArrayClass(i, types, name, where = where, verbose = verbose)
         } else if(is(i, "SOAPComplexType")) {
             # attributes and content
             #XXX We should convert the SOAPComplexType to a class definition before we get to this stage.
             #  i.e. in processWSDL()
           if(verbose)
              cat("defining", i@name, " (temporary solution)\n")
           setClass(i@name, where = where)
           return()
         }  else if(is(i, "UnionDefinition")) {
            defUnionClass(i, types, nsURI = i@uris, name = name, where, verbose = verbose, force = force,
                            classes = classes, pending = pending, baseClass = baseClass, opts = opts, namespaceDefs = namespaceDefs)
         } else if(is(i, "SimpleElement")) {
            warning("defClass: no code to handle ", class(i), " for ", i@name)#XXX fix this.
         } else if(is(i, "PrimitiveSOAPType")) {
            # no problem, these are built-in
         } else if(class(i) == "SOAPType") {
              tmp = resolve(i, types, namespaceDefs)
              defClass(tmp, where, namespaceDefs, verbose, pending, classes, types, baseClass, force, name, ignorePending, opts)
         } else {
           warning("defClass: no code to handle ", class(i), " for ", i@name)
#           browser()
           def = NULL
         }
    })



setMethod("defClass", "CrossRefType",
          function(i, where = globalenv(),
                   namespaceDefs = list(),
                   verbose = FALSE,
                   pending = new.env(hash = TRUE, emptyenv()),
                   classes = new.env(hash = TRUE, emptyenv()),
                   types = NULL,
                   baseClass = "VirtualSOAPClass",
                   force = FALSE,
                   name = getName(i),
                   ignorePending = FALSE, opts = new("CodeGenOpts")) {
            
            setClass(i@name, contains = "CrossRefClass", where = where)
          })

setMethod("defClass", "SOAPGroupType",
          function(i, where = globalenv(),
                   namespaceDefs = list(),
                   verbose = FALSE,
                   pending = new.env(hash = TRUE, emptyenv()),
                   classes = new.env(hash = TRUE, emptyenv()),
                   types = NULL,
                   baseClass = "VirtualSOAPClass",
                   force = FALSE,
                   name = getName(i),
                   ignorePending = FALSE, opts = new("CodeGenOpts")) {

          defClass(i@slotTypes[[1]], where, namespaceDefs, verbose, pending, classes, types, baseClass, force, i@name, ignorePending = TRUE, opts)

          })

tmp = function(i, where = globalenv(),
                namespaceDefs = list(),
                verbose = FALSE,
                pending = new.env(hash = TRUE, emptyenv()),
                classes = new.env(hash = TRUE, emptyenv()),
                types = NULL,
                baseClass = "VirtualSOAPClass", force = FALSE,
                name = getName(i),
                ignorePending = FALSE, opts = new("CodeGenOpts")) {
                 return(FALSE)
             }
setMethod("defClass", "NULL", tmp)
setMethod("defClass", "SOAPVoidType", tmp)



setMethod("defClass", "AttributeDef",
          function(i, where = globalenv(),
                   namespaceDefs = list(),
                   verbose = FALSE,
                   pending = new.env(hash = TRUE, emptyenv()),
                   classes = new.env(hash = TRUE, emptyenv()),
                   types = NULL,
                   baseClass = "VirtualSOAPClass", force = FALSE,
                   name = getName(i),
                   ignorePending = FALSE, opts = new("CodeGenOpts")) {

                 defClass(i@type, where, namespaceDefs, verbose, pending, classes, types, baseClass, force, name, ignorePending, opts)
               })



setMethod("defClass", "SOAPTypeReference",
          function(i, where = globalenv(),
                   namespaceDefs = list(),
                   verbose = FALSE,
                   pending = new.env(hash = TRUE, emptyenv()),
                   classes = new.env(hash = TRUE, emptyenv()),
                   types = NULL,
                   baseClass = "VirtualSOAPClass", force = FALSE,
                   name = if(is(i, "GenericSchemaType") || is(i, "XMLSchemaComponent")) i@name else i$name,
                   ignorePending = FALSE, opts = new("CodeGenOpts")) {

     def = getClassDef(i@name)
     if(length(def) == 0) {
         # then resolve and define
       def = resolve(i, types, namespaceDefs)
#       def = lookupType(i@name, types)
       if(!is.null(def))
          def = defClass(def, where, namespaceDefs, verbose, pending, classes, types, baseClass, force, ignorePending = TRUE, opts = opts)
       # stop("Need to define the reference ", i@name)
     }
     def
    })


setMethod("defClass", "RestrictedStringPatternDefinition",
#XXX See also createRestrictedStringDefinition in processSchemaTypes.R
          function(i, where = globalenv(),
                   namespaceDefs = list(),
                   verbose = FALSE,
                   pending = new.env(hash = TRUE, emptyenv()),
                   classes = new.env(hash = TRUE, emptyenv()),
                   types = NULL,
                   baseClass = "VirtualSOAPClass", force = FALSE,
                   name = getName(i),
                   ignorePending = FALSE, opts = new("CodeGenOpts")) {

        #Set the validity to enforce the pattern is met.
        #??? Can we use i@fromConverter
        valid = makeRestrictedPatternStringValidity(i@pattern, i@name)
        def = setClass(name, contains = "character", where = where, validity = valid)
       
        def
     })

makeRestrictedPatternStringValidity =
function(pattern, name)
{
  function(object) {
     if(grepl(pattern, object))
        TRUE
     else
       paste("%s doesn't match pattern %s for class %s", object, pattern, name)
  }
}

setClass("SOAPElementConverter", contains = "function")
setClass("AsFunction", contains = "function")

setAs("SOAPElementConverter", "AsFunction",
       function(from) {
         params = formals(from)
         formals(from) = alist(from =)
         b = expression({ x = from; obj = new("GetSitesXml")})
         ob = body(from)[-1]
         b[[1]][1:length(ob) + 4] =  ob
         body(from) = b
         from
       })
       

getDefaultValue =
function(type)
{
 #XXX remove this when we compute the default correctly earlier when creating the type descriptions.
#    if(0 %in% type@count)
#       vector(class(type@default), 0)
#    else
      type@default
}

defineClassDefinition =
function(i, types, namespaceDefs, name, classes, pending, baseClass, where = globalenv(), verbose = FALSE, force = FALSE, extendList = FALSE,
          opts = new("CodeGenOpts"))
{

#if(name == "RequestData") browser()
  
     i@slotTypes = lapply(i@slotTypes, resolve, types, namespaceDefs)

         # Handle any SOAPGroupType and make certain those classes are defined and then
         # use them as base classes to extend and remove from the slotTypes.
     isGroup = sapply(i@slotTypes, is, "SOAPGroupType")
     if(any(isGroup)) {
       extraBaseClasses = sapply(i@slotTypes[isGroup], computeName)
       forceClassDefs(extraBaseClasses, i@slotTypes[isGroup], types, namespaceDefs, where, classes = classes, baseClass = baseClass,
                             pending = pending, verbose = verbose, force = force, opts = opts)
       i@slotTypes = i@slotTypes[!isGroup]
     } else
       extraBaseClasses = character()


    defaultValues = lapply(i@slotTypes, getDefaultValue)


     if(is(i, "ExtendedClassDefinition") && length(i@slotTypes) == 1 && is(i@slotTypes[[1]], "SimpleSequenceType"))  {
             # This is the  case where the extended class is just a sequence and so we want to extend list
             # AND the regular base class in that order. 
          extendList = TRUE
          extraBaseClasses = c("list", extraBaseClasses)
          repn = representation()
          
     } else if(FALSE && is(i, "ClassDefinition") && length(i@slotTypes) > 1 && sum(w <- sapply(i@slotTypes, is, "SimpleSequenceType")) == 1)  {
          # here we have a class definition with more than one slot and with exactly one slot that is a sequence
          # So we can  extend list.

       extendList = TRUE # unused.
       listType = i@slotTypes[w]
       i@slotTypes = i@slotTypes[!w]
       repn = createClassRepresentation(i, types, namespaceDefs)
       clasDefs = forceClassDefs(repn, i@slotTypes, types, namespaceDefs, where, classes = classes, baseClass = baseClass,
                             pending = pending, verbose = verbose, force = force, opts = opts)
#XXXrepn
       prot = if(opts@makePrototype) makePrototype(repn, i@slotTypes, "list", i@name, defaultValues) else NULL
       def = setClass(i@name, repn, contains = "list", where = where, prototype = prot)
       return(def)

     } else {
  
                 # make certain the types for the fields are defined
            repn = createClassRepresentation(i, types, namespaceDefs)
            classDefs = forceClassDefs(repn, i@slotTypes, types, namespaceDefs, where, classes = classes,
                                        baseClass = baseClass,  pending = pending, verbose = verbose, force = force, opts = opts)
     }


     if(is(i, "ExtendedClassDefinition")) {

       baseType = resolve(i@baseType, types)
       xbaseClass = mapSOAPTypeToS(baseType, types = types)
       if(is.null(getClassDef(xbaseClass))) {

#         w = sapply(types, function(x) xbaseClass %in% names(x))
#         if(any(w))
              defClass(baseType, where, namespaceDefs, verbose, pending = pending,
                         classes, types, baseClass, force, opts = opts)
       }
       
       super = names(getClass(xbaseClass)@contains)
       if(!(baseClass %in% super))
         baseClass = c(xbaseClass, baseClass)
       else
         baseClass = xbaseClass
     }

      baseClass = c(baseClass, extraBaseClasses)

       if(verbose)
                cat("defining class", name, "\n")

       if(extendList) 
            baseClass = unique(c("list", baseClass))
#XXXrepn
         prot = if(opts@makePrototype) makePrototype(repn, i@slotTypes, baseClass, i@name, defaultValues) else NULL     
         def = setClass(name, representation = repn, where = where, contains = baseClass, prototype = prot)

         if(is(i, "CompositeTypeDefinition"))
              createListCoercion(name, repn, where = where)
            
          def
}


createListCoercion =
function(name, representation = list(), where = globalenv())
{
  f = function(from)
        coerceListToS4(from,  new(name))
  body(f)[[3]][[2]] =  as.character(name)
#  environment(f) = globalenv()  # don't set environment
  setAs("list", name, f, where = where)
}




W3SchemaURIs =
list( "1.1" =
        c('xsi'="http://www.w3.org/1999/XMLSchema-instance",
          'xsd'="http://www.w3.org/1999/XMLSchema"),
      "1.2" =
       c( 'xsi'="http://www.w3.org/2001/XMLSchema-instance",
          'xsd'="http://www.w3.org/2001/XMLSchema"))

getXSDSchemaURIs =
function(version = "1.2", all = FALSE) {
  if(all)
    unlist(W3SchemaURIs)
  else
    W3SchemaURIs[[version]]
}  


coerceArgumentCode =
  #
  # This is the code that 
  #
function(id, type)
{

  if(is.null(type))
    return(id)

orig = type

  end = type
  while(is(end, "Element")) {
    end = end@type
  }
  type = end

  if(is.null(type))
      return(id)

  
  
  name = convertToSName(id)
  default = paste("as(", id, ", '", type@name, "')", sep = "")
  
  if(length(type@nsuri) && type@nsuri %in% getXSDSchemaURIs(all = TRUE)) {
     which = match(type@name, sapply(XMLSchemaTypes, "[[", "type"))
     if(!is.na(which) && type@name != "anyType") {
        if("useCoerce" %in% names(XMLSchemaTypes[[which]]) && XMLSchemaTypes[[which]][["useCoerce"]])
          sprintf("as(%s, '%s')", id, names(XMLSchemaTypes)[which])
        else
           paste("as.",  names(XMLSchemaTypes)[which], "(", id, ")", sep = "")
     } else
          default
  } else if(is(type, "ArrayType")) {

     if(is(type@elType, "PrimitiveSOAPType")) {
        coerceArgumentCode(id, type@elType)
     } else {
          # Or we could go straight to R nodes
                                           #\/ Make sure this is the the name of the R class!
       paste("lapply(", name, ", as, ", type@elType@name, ")") 
     }
    
  } else {
    default
  }
}


setGeneric("getRClassName",
           function(id, ns, types)
            standardGeneric("getRClassName"))

setMethod("getRClassName", "SOAPType",
  # See mapSOAPTypeToS
function(id, ns, types)
{
    id@name
})

setMethod("getRClassName", "character",
  # See mapSOAPTypeToS
function(id, ns, types)
{
  #XXX Deal with the builtin types in XSD
  if(!is.na(ns) && ns %in% getXSDSchemaURIs(all = TRUE)) {
    mapSOAPTypeToS(id)
  } else {
     q = asQName(id)
     q[length(q)]
  }
})
  

defUnionClass =
function(type, types = NULL, nsURI = rep(NA, length(type)),
          name = type@name, where = globalenv(), verbose = FALSE, force = FALSE,
           classes = list(), pending = new.env(hash = TRUE, emptyenv()), baseClass = "VirtualSOAPClass",
            opts = new("CodeGenOpts"), namespaceDefs = list())
{

#if(name == "any-referenceType"  ) browser()

      # Loop over the types and get the names of the corresponding R classes
#   elTypes = mapply(getRClassName, unlist(type@slotTypes), nsURI, types)
  slotTypes = lapply(type@slotTypes, resolve, types)

  
# It would be convenient to have an additional argument passed through to 
# all the  functions as we define classes that map the name to the
# R name and then we could just look that up.
# Some types will be anonymous and so not be in the already defined.  
   #elTypes = mapply(getRClassName, slotTypes)
   elTypes = mapply(mapSOAPTypeToS, slotTypes, MoreArgs = list(types = types) )

   klasses = forceClassDefs(elTypes, slotTypes, types, namespaceDefs, where, verbose = verbose,
                             force = force, pending = pending, classes = classes, baseClass = baseClass, opts = opts)

  if(all(sapply(slotTypes, is, "SimpleSequenceType"))) {
    def =  setClass(name, contains = "list", where = where)
    elTypes = sapply(slotTypes , function(x) mapSOAPTypeToS(x@elType, types = types))
    f = function(object) {
      XMLSchema:::checkHomogeneousList(object, elTypes)
    }
    setValidity(name, f, where = where)
  } else {
   def = setClassUnion(name, elTypes, where = where)
   assign(name, def, classes)
 }
  
   name
}


forceClassDefs =
function(repn, slotTypes, types, namespaceDefs = list(), 
         classes = new.env(hash = TRUE, emptyenv()), pending = new.env(hash = TRUE, emptyenv()), baseClass = NULL, 
          where = globalenv(), verbose = FALSE, force = FALSE, opts = new("CodeGenOpts"))
{

   m = sapply(repn, function(x)  is.null(getClassDef(x)) )
   if(any(m)) {
      if(verbose) 
       cat("Digressing to define", paste(repn[m], collapse = ", ")) # , "for", name, "\n")


       # Now we recursively call this function to define these outstanding nodes.
       # We do have to worry about the depth of the call stack as this could grow
       # quite large if the classes are given in the wrong order.
       #XXX  only looking in first schema here.
     k = mapply(function(x, type) {

                if(!is.null(type))  {
                     def = defClass(type, where, namespaceDefs, classes = classes, pending = pending, baseClass = baseClass, 
                                     types = types, verbose = verbose, force = force, opts = opts)
                     if(is.null(def))
                        stop("Failed to define class ", x)
                 } else
                    stop("Couldn't define class for ", x)

                 def

                }, repn[m], slotTypes[m])

#      if(length(k) < sum(m) || any(sapply(k, is.null)))     browser()
    }

   if(getOption("CHECK_DEF_CLASS", FALSE)) {
    notDef = sapply(repn, function(x)  is.null(getClassDef(x)) )
    if(any(notDef))
       stop("failed to create all classes: ", paste(repn[notDef], collapse = ", "))
  }
    
}


simple.dQuote =
function(x)
  paste('"', x, '"', sep = "")



if(FALSE) {

  createTypeClass =
  # probably not called anymore as lifted into the defineClasses() function above.
    function(type, types, where = globalenv(), parentClass = "VirtualSOAPClass",
         namespaceDefs = list())
      {
        if(verbose)
          cat("[createTypeClass]", type$name, "\n")
        repn = createClassRepresentation(type$definition, types, namespaceDefs)
#XXXrepn        
        setClass(type$name, representation = repn, where = where, contains = parentClass)
      }
}


createArrayClass =
  # Should be merged into createTypeClass.sl
  #XX parentClass not used
function(type, types, name = NA, where = globalenv(), parentClass = "VirtualSOAPClass", verbose = FALSE)
{

#  name = type$definition@elementType
 if(is.na(name)) {  
  name = if(is(type, "GenericSchemaType")) 
            type@name
         else 
            type$name    
  }

  el = if(is(type, "GenericSchemaType")) 
          type@elType
       else 
          type$definition@elType


  elName = if(is(el, "GenericSchemaType")) el@name else el$name

  builtinClass = "list"
  which = NA

  if(FALSE && is(el, "Element")) # Shouldn't be 
    el = el@type
  
    # Want to see if this is a basic type in R, e.g. integer
  if(is(el, "UnionDefinition") || is(el, "ClassDefinition")) {
      
  } else if(el@nsuri %in% getXSDSchemaURIs(all = TRUE)
             ||  # here we check if this is a string or some type in the SOAP schema
                 # We should process that fully and directly using readSchema.
              (el@nsuri == "http://schemas.xmlsoap.org/soap/encoding/"
                   && !is.na(match(elName, sapply(XMLSchemaTypes, "[[", "type"))))) {
    # builtinClass = getArrayElementTypeFromName(name)
    which = match(elName, sapply(XMLSchemaTypes, "[[", "type"))
    if(!is.na(which))
      builtinClass = names(XMLSchemaTypes)[which]
     #    if(is.na(builtinClass))
     # stop("can't identify array element type for SOAP definition")
  }

  if(verbose)
    cat("defining class", name, "\n")
  
        #XXX want a typed list where the elements are checked. See DCOM code.
        # Use validity below(?).
  ans = setClass(name, contains = builtinClass, where = where)

  fun = function(from)
             xmlSApply(from, as, "x")
  body(fun)[[4]] = if(!is.na(which)) builtinClass else elName
  environment(fun) = DefaultFunctionNamespace
  setAs("XMLInternalElementNode", name, fun, where = where)

  if(builtinClass %in% RPrimitiveTypes) {
     createVectorCoercions(name, builtinClass, where)
  }

  if(is(el, "UnionDefinition") || is(el, "ClassDefinition")) {
    #XXX make the validity method.
#   valid = function(object)
#               all(sapply(obj, is, ))
#   setValidity(ans, valid, where = where)
  }

  ans
}

RPrimitiveTypes = c("logical", "integer", "numeric", "character")
createVectorCoercions =
function(className, base, where = globalenv())
{
  f = function(from)
         new(class, as(from, base))
  environment(f) = DefaultFunctionNamespace
  body(f)[[2]] = className
  for(i in setdiff(RPrimitiveTypes, base)) {
      body(f)[[3]][[3]] = i
      setAs(i, className, f, where = where)
  }
}

getRTypeFromSOAP =
function(el, col = "xsi:type", asIndex = FALSE)
{
 target = if(col == "xsi:type") paste("xsd:", el, sep = "") else el
 i = match(target, sapply(XMLSchemaTypes, function(x) x[[col]]))
 if(asIndex)
   return(i)
 
 if(!is.na(i))
   el = names(XMLSchemaTypes)[i]
 
 el
}

getArrayElementTypeFromName =
function(name, stripArray = TRUE, convertToRType = TRUE)
{
 els = strsplit(name, ":")[[1]]
 if(length(els) > 1) {
   els = els[2]
 }
 if(stripArray)
   els = gsub("\\[\\]$", "", els)

 if(!convertToRType)
   return(els)

 getRTypeFromSOAP(els)
}  


newSOAPClass =
  #
  # Creates a new instance of the specified class (className) and populates its
  # fields with the values from the XML node.
  #
  #XXX converters is not used here yet.
function(node, className, converters = SchemaPrimitiveConverters, type = NULL)
{
 obj = new(className)

 reg = !type@isAttribute

 rslotTypes = getClassDef(className)@slots
 
 for(i in slotNames(className)[reg]) {
   tmp = node[[i]]
   if(!is.null(tmp)) {
     if(!is.null(type))
       slotType = type@slotTypes[[i]]
     else
       slotType = NULL

     tmp <- fromXML(tmp, type = slotType) #XXX Need SOAP type here!
       # do the coercion ourselves if slotType is NULL.
     if(is.null(slotType) || !(is(tmp, rslotTypes[[i]])))
       tmp = as(tmp, rslotTypes[[i]])

     slot(obj, i) = tmp
   } 
 }

 if(any(!reg)) {
    klass = getClassDef(className)
    at = xmlAttrs(node)
    for(i in slotNames(className)[!reg]) {
      slot(obj, i) = as(at[i], klass@slots[[i]]) # fromXML(at[i], type@slotTypes[[i]]) 
    }
 }
 
 
  obj
}


createClassRepresentation =
 #
 # 
function(type, types, namespaceDefs = list())
{
   repn = lapply(type@slotTypes, mapSOAPTypeToS, types, namespaceDefs)
   nas = sapply(repn, is.na)
   if(any(nas))
     stop("problem resolving SOAP type ", names(type)[nas], class = "ResolveSOAPType")

   repn 
}




if(FALSE) {

# f = function(){}
# formals(f) <- alist(server=, kid=, threshold=, orgs=)
  
 body(f) <- substitute({
  val = .SOAP(server,
              .opName,
              action = .action,
              xmlns = .namespace,
              .types = ..types)
  }, 
  list(.opName= operation@name,
       .action = operation@action,
       .namespace = operation@namespace,
       ..types = operation@parameters))

  # now put the arguments in.
 e = body(f)[[2]]
 kk = e[4:length(e)]
# for(i in )
 k = body(f)[[2]][[3]]
}

# body(f) = substitute(body(f), list(.opname=operation@name))




asIntegerSetValue =
function(val, values, className)
{
   val = as.integer(val)  
   if(is.na(match(val, values)))
     stop("invalid integer value for class ", className)

   val
}



setMethod("defClass", "EnumValuesDef",
          function(i, where = globalenv(),
                   namespaceDefs = list(),
                   verbose = FALSE,
                   pending = new.env(hash = TRUE, emptyenv()),
                   classes = new.env(hash = TRUE, emptyenv()),
                   types = NULL,
                   baseClass = "VirtualSOAPClass", force = FALSE,
                   name = getName(i),
                   ignorePending = FALSE, opts = new("CodeGenOpts")) {

    defEnum(name, i@values, where = where)
          })




makePrototype =
function(repn, slots, base = NA, className = NA, defaults = NULL)
{
    
    if(length(repn) == 0) {
      if(any(base == "character"))
        return(prototype(""))
      else
        return(NULL)
    }
  
#    str = sapply(slots, function(x) is(x, "PrimitiveSOAPType") && x@name == "string")

    base = base[1]
    
    str = sapply(repn, function(x) x == "character")
    
    
   if(!all(nas <- sapply(defaults, function(x) is.null(x) || (length(x) == 1 && is.na(x))))) {

       values =  mapply(as, defaults[!nas], repn[!nas], SIMPLIFY = FALSE)
      # values[ names(str)[str & nas] ] = ""
       ans = do.call(prototype, values)
       return(ans)
    }
    

    ans = if(any(str)) {
               # call prototype with the name = value sequences
             do.call(prototype, structure( replicate(sum(str), "", simplify = FALSE),
                            names = names(slots)[str]))
           } else
             prototype()

#   if(!is.na(base) && (base %in% c("integer", "logical", "numeric", "character")))
#      get(sprintf("as.%s", base))(ans) # , base)
#   else
       ans

}



setMethod("defClass", "RestrictedNumber",
function(i, where = globalenv(),
         namespaceDefs = list(),
         verbose = FALSE,
         pending = new.env(hash = TRUE, emptyenv()),
         classes = new.env(hash = TRUE, emptyenv()),
         types = NULL,
         baseClass = "VirtualSOAPClass", force = FALSE,
         name = getName(i),
         ignorePending = FALSE, opts = new("CodeGenOpts"))
{

   base = switch(class(i), "RestrictedInteger" = "integer", "RestrictedDouble" = "numeric")
   def = setClass(i@name, contains = base, where = where)
   f = makeRestrictedFunc(i@name, base, i@range, i@inclusive)

   setAs(base, i@name, f, where = where)
   setAs("character", i@name, f, where = where)   
   def
})



setMethod("defClass", "RestrictedHexBinary",
function(i, where = globalenv(),
         namespaceDefs = list(),
         verbose = FALSE,
         pending = new.env(hash = TRUE, emptyenv()),
         classes = new.env(hash = TRUE, emptyenv()),
         types = NULL,
         baseClass = "VirtualSOAPClass", force = FALSE,
         name = getName(i),
         ignorePending = FALSE, opts = new("CodeGenOpts"))
  {


      def = setClass(i@name, contains = "character", where = where)
      valid = function(object) {
                 if(!grepl(pattern, object))
                   paste("doesn't match pattern of", len, "0-0A-F pairs")
                 else
                    TRUE
              }
      body(valid)[[2]][[2]][[2]][[2]] = i@pattern
      body(valid)[[2]][[3]][[3]] = i@length

      setValidity(i@name, valid, where = where)
      

      fun = function(from) {
                 if(!grepl(pattern, from))
                   stop("invalid hex binary of length ", len)
                 new(type, from)
              }
       body(fun)[[2]][[2]][[2]][[2]] = i@pattern
       body(fun)[[2]][[3]][[2]] = i@pattern
       body(fun)[[3]][[2]] = i@name
       environment(fun) = DefaultFunctionNamespace
      
      setAs("character", i@name, fun, where = where)
   
      def
  })



getSchemaClass =
  #
  # Get the class corresponding to a schema type itself.
  #
function(def, types)
{
   class = switch(def@name, schema = "SchemaTypes", "ANY")
   if(class != "ANY")
      getClassDef(class, package = "XMLSchema")
   else
      class
}
