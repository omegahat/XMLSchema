if(FALSE) {
processSchemaTypes =
   # Process all the elements in types (hopefully <schema> elements)
   # and merge. We may want to keep these as a list indexed by namespace/schema identifier.
   # Is this still true?
  
   # Originally, we just processed the first one.
   # types = processSchemaTypes(doc[["types"]][[1]], doc)


  # We are called with the doc[["types"]] argument
  # That should contain a schema.  The schema
  # can have elements within it (e.g. interop.wsdl) or
  # import statements which give other schema, e.g. eutils.wsdl
  # or both import statements and new type defintions (e.g. KEGG.wsdl).
  #

  # This can be called recursively for sub-schema, i.e. schema defined within the
  # <types><schema>...</schema></types>

  # This is (initially) called with the top-level <types> which should contain one
  # <schema> node and potentially have <import> and <include> nodes within this.
  # We will process these recursively.

function(node, doc, namespaceDefs = gatherNamespaceDefs(node), createConverters = FALSE, verbose = FALSE,
           types = NULL, elementFormDefault = NA, targetNamespace = NA)
{
  if(is.null(node))
      return(list())

  substGroups = getSubstitutionGroups(doc)


     # Loop over the children of this node and process each element.
  ans = vector("list", xmlSize(node))
  names = character(length(ans))

 for(i in seq(length = xmlSize(node)))  {
                   el = node[[i]]

                   if(inherits(el, c("XMLCommentNode", "XMLInternalCommentNode"))
                         || xmlName(el) %in% c("import", "include"))
                     next

                   if(xmlName(el) == "annotation")
                     next
                   
                   if(xmlName(el) == "schema") {
                         # pubmed from NCBI has a <schema><schema targetNamespace="">...
                      if(xmlSize(el) == 1 && names(el) == "schema")
                        el = el[[1]]
                     
                      ns = xmlGetAttr(el, "targetNamespace")
                      if(verbose)
                         cat("processing (sub) schema", ns, "\n")

                      qualified = xmlGetAttr(el, "elementFormDefault", NA, function(x) x == "qualified")
                                 #XXX removed the namespaceDefs in the call so we compute them for this schema.
                      o = processSchemaTypes(el, doc, createConverters = FALSE, verbose = verbose, types = ans,
                                               targetNamespace = ns, elementFormDefault = qualified)
                      if(!is(o, "SchemaTypes"))
                          o = new("SchemaTypes", o, namespaceDefs = namespaceDefs)
                      
                      o@elementFormQualified = qualified
                      if(length(o)) {
                          ans[[i]] <-  o
                          if(length(ns) || length(ns <- xmlGetAttr(el, "namespace"))) {
                             names[i] <- ns
                             names(ans) <- names
                          }
                      }
                   } else {
                      n = xmlGetAttr(el, "name", as.character(NA))
                      if(verbose)
                         cat(i,")", n, " (", xmlName(el), ")\n")

                      o = processSchemaType(el, substitutionGroups = substGroups,
                                            namespaceDefs = namespaceDefs,
                                            types = ans, targetNamespace = targetNamespace,
                                            elementFormDefault = elementFormDefault)
                      
                      if(is.null(o))
                         next
                      
                      if(FALSE && createConverters && is(o, "BasicSchemaType"))
                         o@fromConverter = createFromXMLConverter(o, ans)

                      ans[[i]] <- o
                      names[i] <- n # o@name By using n and not o@name, we avoid the case where we are
                                    # working on a complexType that is returned in o as an Element/LocalElement.
                                    # o@name would then be the name of that element. This happens in pugi_soap_cgi from NIH
                                    # for AnyKeyType which is a <complexType><sequence><element/></sequence></complexType>
                      names(ans) <- names

                      if(FALSE && is(o, "Element") && xmlName(el) == "complexType") {
                        browser()
                          ans[[ length(ans) + 1]] <- o
                          names[length(names)+1] = n
                          names(ans) <- names
                       }
                      
                    }

                   NULL
        }

         # Fix the names on these types to avoid the schema.
 #             types = unlist(types, recursive = FALSE)
 #names(types) = sapply(types, function(x) x$name)
 # types

      # if we have a collection of SchemaTypes, turn them into a SchemaTypes object
      # and if we have a collection of exclusively SchemaTypes  (i.e. separate Schema)
      # make a SchemaCollection. In between where we have separate types and
      # one or more schemas (containing types), we leave as is for now, i.e. just a list.
  if(!any(sapply(ans, inherits, "SchemaTypes")))
     ans = new("SchemaTypes", ans, namespaceDefs = namespaceDefs)
  else if(all(sapply(ans, inherits, "SchemaTypes"))) {
     ans = new("SchemaCollection", ans)
  }

  if(createConverters) 
     ans = addConverters(ans, ans)

  ans
}

} # end if(FALSE)

setGeneric("addConverters",
           function(x, types, ...)
             standardGeneric("addConverters"))

setMethod("addConverters",
           "list",
           function(x, types, ...) {
             lapply(x, addConverters, x)
           })

setMethod("addConverters",
           "list",
           function(x, types = x, ...) {
             lapply(x, createFromXMLConverter, types = types)
           })

setMethod("addConverters",
           "SchemaCollection",
           function(x, types = x, ...) {
             lapply(x, addConverters, types = types)
           })

sQuote =
function(x)
  sprintf("'%s'", x)


# This material relates to XML schema and processing of types defined within
# schema. Ideally, we will separate this into a separate package and use the
# results in different ways, not just for SOAP. And we will provide a more
# extensive framework for dealing with all of the schema details rather than
# the rather limited but common ones supported here.
#
#  See the O'Reilly book XML Schemas by Eric van der Vlist for more information
#  on schemas, or look on the Web (e.g. www.xml.com/schemas)
#
#  Priscilla Walmsley's Definitive XML Schema Prentice Hall
#  is much better. 

# Failed  complexType & attribute.
#   Assay_molTypeType
#   Assembly_currentType


processSchemaType =
  #
  # This is intended to create an R description for
  # XML Schema nodes that actually define a type.
  #
  #
  # This currently deals with a very small subset of XML schema
  # specifications.  These are the common ones, but more work 

  #

  #  simpleType
  #  complexType
  #  complexContent

   # sequence

  # Fix for
  #   simpleType with "attribute" child.
  #   simpleType with restriction.

  # Handle <group>
function(type, types, substitutionGroups = NULL, namespaceDefs = list(),
           targetNamespace = character(), elementFormDefault = NA, localElements = FALSE)
{
  tmp = NULL

  if(inherits(type, c("XMLInternalCommentNode", "XMLComment", "XMLInternalTextNode")) || xmlName(type) == "annotation")
      return(new("SchemaVoidType"))

  name = xmlGetAttr(type, "name", "")

  nsDefs = xmlNamespaceDefinitions(type, simplify = FALSE)
  if(length(nsDefs)) 
      namespaceDefs[ names(nsDefs) ] = nsDefs


# if(length(name) && !is.na(name)  && name == "GetDatabases") browser()
#if(length(name)   && !is.na(name) && name == "office-of-filingType") {debug(processSequence); on.exit(undebug(processSequence))}
# if(length(name) && !is.na(name)  && name == "Constant") browser()

   if(xmlName(type) == "complexType" && xmlSize(type) == 0)
       return(new("SchemaVoidType"))
  
   if(xmlName(type) == "attribute")
     return(processAttribute(type, name, namespaceDefs = namespaceDefs, targetNamespace = targetNamespace, elementFormDefault = elementFormDefault, localElements = TRUE, types = types))

   if(xmlName(type) == "anyAttribute")
       return(new("AnyAttributeDef"))

#if(name == "grPostal") browser()   #DDDD

   if(xmlName(type) == "attributeGroup" && !is.na(xmlGetAttr(type, "ref", NA)))
     return(getAttributeGroup(type, namespaceDefs, targetNamespace, elementFormDefault))

   if(xmlName(type) == "attributeGroup" && is.na(xmlGetAttr(type, "ref", NA)))
     return(makeAttributeGroup(type, types, namespaceDefs, targetNamespace,
                                 substitutionGroups = substitutionGroups,
                                  elementFormDefault = elementFormDefault, localElements = localElements)) 


   if(xmlName(type) == "any")  #XXX check the namespace
       return(new("AnySchemaType"))


   if(xmlName(type) == "group")  #XXX check the namespace
      return(processGroup(type,types, namespaceDefs, name, targetNamespace = targetNamespace, elementFormDefault = elementFormDefault))

   if(xmlName(type) == "sequence")
      return(processSequence(type, types, namespaceDefs, name, targetNamespace = targetNamespace, elementFormDefault = elementFormDefault))

   if(xmlName(type) == "choice")
      return(processChoice(type, types, namespaceDefs, targetNamespace = targetNamespace, elementFormDefault = elementFormDefault))
  
  docString = character()

  if(xmlSize(type) > 0 && "annotation" == xmlName(type[[1]])) { 
         # drop the annotation sub-node.
    docString = XML:::trim(xmlValue(type[[1]]))
       # if dealing with internal nodes.
    removeNodes(type[[1]])
    # xmlChildren(type) = xmlChildren(type)[-1]    
  }

   
     #   simpleType only and return.

  if(xmlName(type) == "element" && !is.na(xmlGetAttr(type, "type", NA)) && xmlSize(type) == 0) {

            #XXXX  can we just call processSchemaElement and ignore the remainder of this if() body
    return(processSchemaElement(type, name, namespaceDefs, types, targetNamespace = targetNamespace,
                                 elementFormDefault = elementFormDefault, localElements = localElements))
    
  } else if(name == "simpleType" || xmlName(type) == "simpleType") {
    done = TRUE

    if(xmlSize(type) > 0 && xmlName(type[[1]]) == "restriction") {  # need to account for the annotation. Removed above.

       if(xmlSize(type[[1]]) > 0) {
            # check if the base type is a primitive (e.g. a double, ...)
            # and take care of the namespace, e.g. xs:double
         base = asQName(xmlGetAttr(type[[1]], "base"))
         if(!is.na(getRTypeFromSOAP(base[2], asIndex = TRUE))) { #XXX what if no prefix? Make asQName() return a vector of length 2?
#XXXX is this the right thing to do here at all?
           if(base[2] == "string") {
              def = createRestrictedStringDefinition(type, name)
           } else {
#             if(xmlGetAttr(type, "name", "") == "NUMBER") browser()
              tp = SchemaType(base[2], base[1], counts = getElementCount(type), namespaceDefs = namespaceDefs)
              
              def = if(length(getNodeSet(type[[1]], "./*"))) {  # xmlSize(type[[1]])) {

                       if(base[2] == "integer") {
                         vals = xmlSApply(type[[1]],  xmlGetAttr, "value", converter = as.integer)
                         from = function(val) asIntegerSetValue(val, vals, name)
                         body(from)[[3]] = vals; body(from)[[4]] = name
                         new("RestrictedSetInteger", name = name, values = vals,
                                  toConverter = function(val) val,
                                  fromConverter = from)
                       } else
                         new("EnumValuesDef", name = name, values = xmlSApply(type[[1]],  xmlGetAttr, "value"))
                    } else
                       new("ExtendedClassDefinition", name = xmlGetAttr(type, "name", as.character(NA)), base = base[2], baseType = tp)
           }
         } else
           def = createRestrictionType(name, type[[1]], namespaceDefs, targetNamespace, base)

       } else {
                     # e.g. from eBaySvc.wsdl
                     # <xs:simpleType name="DisputeIDType">
                     #   <xs:restriction base="xs:string"/>
                     # </xs:simpleType>
         def = new("ExtendedClassDefinition", name = name, base = xmlGetAttr(type[[1]], "base"),
                       baseType = SchemaType(xmlGetAttr(type[[1]], "base"), namespaceDefs = namespaceDefs))
       }
       
   } else if(xmlSize(type) > 0 && xmlName(type[[1]]) == "list") {

       if(xmlSize(type[[1]]) > 0) {
          el = processSchemaType(type[[1]][[1]], types, namespaceDefs = namespaceDefs,
                                   targetNamespace = targetNamespace, elementFormDefault = elementFormDefault, localElements = TRUE)
          def = SchemaType(name, counts = getElementCount(type), obj = new("RestrictedListType"), namespaceDefs = namespaceDefs)
          def@elType = el
          if(is(el, "EnumValuesDef"))  # And is a string
              def@elements = el@values
       } else if(xmlName(type) == "simpleType" && xmlName(type[[1]]) == "list"){
           def = processSimpleList(type[[1]],  xmlGetAttr(type, "name", as.character(NA)), namespaceDefs, targetNamespace)
       } else
           stop("Not sure what to do here with ", xmlName(type))

   } else if(xmlName(type[[1]]) == "union") {  # kml21.xsd - dataTimeType.
     
      u = type[[1]]
      tp = xmlGetAttr(u, "memberTypes", "")
      tp = strsplit(tp, "[[:space:]]+")[[1]]
      els = lapply(tp, SchemaType, namespaceDefs = namespaceDefs, targetNamespace = targetNamespace)
    
      types = lapply(xmlChildren(u), processSchemaType, types = types, localElements = TRUE,
                       targetNamespace = targetNamespace, namespaceDefs = namespaceDefs, localElements = TRUE)

      def = new("UnionDefinition", name = name, slotTypes = c(types, els))
   } else
       def <- "xsd:string"

    
     if(done) {
       if(is(def, "GenericSchemaType")) 
          def@documentation = docString       
       return(def)
     }
  }

   if("complexType" %in% names(type) && xmlSize(type[["complexType"]]) == 0) {
     return(new("Element", name = name, attributes = lapply(xmlChildren(type)[names(type) == "attribute"],
                                                            processAttribute,
                                                            types = types,
                                                            namespaceDefs = namespaceDefs,
                                                            targetNamespace = targetNamespace,
                                                            elementFormDefault = elementFormDefault,
                                                            localElements = TRUE, nsuri = as.character(targetNamespace)),
                            type = new("SchemaVoidType"),
                            nsuri = targetNamespace))
     
   } else if(names(type)[1] == "complexType" && names(type[[1]]) == "simpleContent") {
      ext = type[[1]][[1]][[1]]
      kids = xmlChildren(ext)
      attrs = list()
      #!!! can use lapply now that we have general call to processSchemaTypes() rather than to different specific functions.
      for(i in kids[names(ext) %in% c("attribute", "attributeGroup")])
        attrs = c(attrs, processSchemaType(i, types, substitutionGroups, namespaceDefs = namespaceDefs,
                                              targetNamespace = targetNamespace,
                                              elementFormDefault = elementFormDefault, localElements = TRUE))
      names(attrs) = sapply(attrs, slot, "name")
#XXX Constant.
      ttype = xmlGetAttr(ext, "base", character())
      ttype = structure(gsub(".*:", "", ttype), names = lookupNamespace(ttype, ext))
     return(new("SimpleElement", name = xmlGetAttr(type, "name", as.character(NA)),
                                 attributes = attrs,
                                 xmlAttrs = as(xmlAttrs(type), "character"),
                                 type = ttype  #XXX type should be the extension type
                  #               count = getElementCount(type)
                ))
  } else if(xmlName(type) %in% c("complexContent", "element")) {
         
      tmp = type
      
  } else if(xmlName(type) == "complexType" && xmlSize(type) == 0) {
          #??? What do we do here.  See "http://www.ebi.ac.uk/ebisearch/service.ebi?wsdl" for example.
    return(new("AnySchemaType", name = if(name != "") name else "AnySchemaType"))
    
  } else if(xmlName(type) == "complexType" && xmlName(type[[1]]) == "all") {
     tmp = type
  } else if(xmlName(type) == "complexType" &&
            ( (xmlSize(type) == 1 && names(type) == "sequence")
                  || ("sequence" %in% names(type)  && (all(names(type) %in% c("attribute", "anyAttribute", "annotation", "sequence"))))))  {
# compare with condition at #377
            # we can just use xmlApply(, processSchemaType) then merge the slotTypes, etc.
if(xmlSize(type) > 1) {      # when seq is a SimpleSequenceType, need to do some surgery to add the extra elements.

      els = xmlApply(type, processSchemaType, types, substitutionGroups, namespaceDefs, targetNamespace, elementFormDefault, localElements)
      seq = els[[1]]

      if(length(els) == 1) {
        seq@documentation = docString
        return(seq)
      }
      

      if(!is(seq, "ClassDefinition") && (is(seq, "Element") || is(seq, "SchemaGroupRefType") || is(seq, "SimpleSequenceType"))) 
             seq = new("ClassDefinition", name = name, slotTypes = structure(list(seq), names = computeName(seq))) #XXX fill in the rest.
               
       seq@slotTypes = structure(append(seq@slotTypes, els[-1]),
                                    names = c(names(seq@slotTypes), as.character(sapply(els[-1], function(x) x@name))))

         # important we don't do this before putting the seq into the first element of the slot type.
       seq@name = name      

      return(seq)
    } else {
         # the original version. Doesn't necessarily return  a SimpleSequenceType. This can be collapsed, e.g. <sequence><element/></sequence>
      seq = processSequence(type[["sequence"]], types, namespaceDefs, name, targetNamespace = targetNamespace, elementFormDefault = elementFormDefault)
    return(seq)
    }
  } else if(xmlName(type) == "complexType" && (all(names(type) %in% c("attribute", "anyAttribute", "annotation", "sequence")))) {
            # so all attributes
            # could merge this with the case below, but for now just get it working.
#browser()     # xAL is defined here, PostalServiceElements
     kids = xmlChildren(type)[ !xmlSApply(type, function(x) xmlName(x) == "annotation") ]
     stypes = lapply(kids, processSchemaType, types, substitutionGroups, namespaceDefs = namespaceDefs,
                                              targetNamespace = targetNamespace,
                                              elementFormDefault = elementFormDefault, localElements = TRUE)

     if(length(name) == 0 || is.na(name))
        name = xmlGetAttr(xmlParent(type), "name")
     
     ClassDef(name, stypes, targetNamespace = targetNamespace, documentation = docString)
#     new("ClassDefinition", name = name, Rname = name, slotTypes = stypes, documentation = docString,
#                     isAttribute = rep(TRUE, length(stypes)), nsuri = targetNamespace)
    
  } else if(FALSE && xmlName(type) == 'complexType' && all(names(type) %in% c("attribute", "attributeGroup"))) {
      #XXX what about any attributes on the complexType such as mixed="true".
       # Just an attribute group within the complexType.
        # Make a separate function.
     attrs = list()
     for(i in xmlChildren(type))
        attrs = c(attrs, processSchemaType(i, types, namespaceDefs = namespaceDefs,
                                            targetNamespace = targetNamespace,
                                             elementFormDefault = elementFormDefault, localElements = TRUE))
     names(attrs) = sapply(attrs, slot, "name")

     return(new("SchemaComplexType",
                 name = xmlGetAttr(type, "name"),
                 attributes = attrs,
                 xmlAttrs = as(xmlAttrs(type), "character")))
     
  } else if(xmlName(type) == "attribute") {
       # Case where xmlSize() > 0. But this must be (?)
       # a simpleType with a restriction on the type and so would be
       # handled in processAttribute(). So this could be simplified.
       # 
       # XXX is this right ? Think this must be handled by processAttribute() correctly.
       # We may have a restriction on the value. e.g. from the pmml schema
# <xs:attribute name="type" use="required">
#  <xs:simpleType>
#    <xs:restriction base="xs:string">
#      <xs:enumeration value="int"/>
#      <xs:enumeration value="real"/>
#      <xs:enumeration value="string"/>
#    </xs:restriction>
#  </xs:simpleType>
# </xs:attribute>
     return(processAttribute(type, name, namespaceDefs = namespaceDefs, targetNamespace = targetNamespace,
                               elementFormDefault = elementFormDefault, localElements = TRUE), types = types)
  } else 
    tmp <- type[["complexContent"]]

  if(xmlName(type) == "attributeGroup") {
        # We'll deal with these when they are referenced.
        # We could compute them just once and access that but where do we put them so that we can access them.
        # An environment like RGCCTranslationUnit.
      return(NULL)
      
  } else if(xmlName(type) == "element") {
       ans = processSchemaElement(tmp, namespaceDefs = namespaceDefs, types = types, targetNamespace = targetNamespace,
                                    elementFormDefault = elementFormDefault)
       if(is(ans, "Element") && (length(ans@type@name) == 0 || is.na(ans@type@name) || ans@type@name == ""))
          ans@type@name = name
          ans@documentation = docString
       return(ans)
  } else if(!is.null(tmp) && !is.null(tmp[["all"]])) {
        # Connect this with the other case !is.null(tmp <- type[["all"]]) below.
    
             # a struct-like definition with slots.
     a = tmp[["all"]]

          # Should we use processSchemaType()
     slotTypes = xmlApply(a, function(x) {
                                   # just return the SOAP type. We'll resolve it later.
                                  tt = xmlGetAttr(x, "type")
                                  if(length(tt) == 0) {
                                     tt = xmlGetAttr(x, "ref")
                                  }
                                  SchemaType(tt, namespaceDefs = namespaceDefs, counts = getElementCount(a))
                               })

     names(slotTypes) = xmlSApply(a, getElementName)
     def = ClassDef(name, slotTypes, targetNamespace = targetNamespace, elementFormDefault = elementFormDefault)

  } else if(!is.null(tmp) && !is.null(tmp[["extension"]])) {
     def = processExtension(tmp, name, types, namespaceDefs, targetNamespace = targetNamespace, elementFormDefault = elementFormDefault)
  } else if(!is.null(tmp) && !is.null(tmp[["restriction"]])) {
     def = processRestriction(tmp, name, types, namespaceDefs, targetNamespace = targetNamespace, elementFormDefault = elementFormDefault) 
  } else if(!is.null(tmp <- type[["all"]])) {
    
                    # a struct-like definition with slots.
     slotTypes = xmlApply(tmp, xmlGetAttr, "type")
     names(slotTypes) = xmlSApply(tmp, getElementName) #??? xmlGetAttr, "name")
     def = ClassDef(name, slotTypes, new("ArrayClassDefinition"), targetNamespace = targetNamespace, elementFormDefault = elementFormDefault)
     
  } else if(!is.null(tmp <- type[["sequence"]])) {
    
                    # <complexType><sequence>
                    # Connect this with the code for the "all" case.
                    # Need to resolve the type if it is not a primitive.
                    # XXX also want the minOccurs and maxOccurs
#XXXX-XMCDA
      def = SchemaType(name, counts = getElementCount(type), obj = new("SchemaComplexType"), namespaceDefs = namespaceDefs)
      def@xmlAttrs = as(xmlAttrs(type), "character")
      def@content = processSequence(tmp, types, namespaceDefs, targetNamespace = targetNamespace, elementFormDefault = elementFormDefault)
               # Now process the attributes and attributes group
      def@attributes = lapply(xmlChildren(type)[names(type) == "attribute"], processAttribute,
                                     namespaceDefs = namespaceDefs, targetNamespace = targetNamespace,
                                     elementFormDefault = elementFormDefault, localElements = TRUE, types = types)

      ags = xmlChildren(type)[names(type) == "attributeGroup"]
      doc = as(type, "XMLInternalDocument")
      for(i in ags) {
          attrs = getAttributeGroup(i, namespaceDefs, targetNamespace, elementFormDefault, doc = doc)
          def@attributes[names(attrs)] = attrs
      }

      def = postprocessComplexType(def)
#XXX deal with attribute group.  
#    Also  xs:choice for an element with the <xs:all>

#      def = new("SimpleSequenceType", name = name, elType = slotTypes, elementType = slotTypes@name) 
#      def = ClassDef(name, slotTypes)
#      def@count = getElementCount(type)
      
  } else if (xmlName(type) == "choice") {
    
    def = processChoice(type, types, namespaceDefs, targetNamespace = targetNamespace, elementFormDefault = elementFormDefault)
    
  } else if(!is.null(tmp <- type[["choice"]])) {

      #XXX type or ref?
      # Handle local definitions
     # We can either get the names of the references to elemense
     # or get the actual types, assuming they have already been
     # processed.
    # "references-citedType" in ops.wsdl

     def = processChoice(tmp, types, namespaceDefs, name, targetNamespace = targetNamespace, elementFormDefault = elementFormDefault)

     if(xmlSize(type) > 1) {
        kids = xmlChildren(type)[-1]
        w = sapply(kids, xmlName) != "annotation"

        if(any(w)) {
          kids = kids[w]
          defs = lapply(kids, processSchemaType, types, substitutionGroups, namespaceDefs, targetNamespace, elementFormDefault, localElements = TRUE)

          defs = do.call(c, defs)
          names(defs) = sapply(defs, computeName)
          
          #XXX mege the defs into def, changing the class as necessary.
          if(!is(def, "ClassDefinition")) {
             def = new("ExtendedClassDefinition", name = name, baseType = def)
          }
          
          def@slotTypes[names(defs)] = defs
        }
     }

   } else if(!is.null(tmp <- type[["simpleContent"]])) {

     if(xmlName(tmp[[1]]) == "extension") {
              # We can call processExtension
         def = processExtension(tmp, name, types, namespaceDefs, targetNamespace, elementFormDefault)

    } else {
          warning("Unhandled code for simpleContent in ", xmlName(type))
          def <- NULL
    }
  } else if(xmlName(type) == "complexType" &&
            xmlSize(type) > 0 && all(xmlSApply(type, xmlName) %in% c("attribute", "anyAttribute", "attributeGroup", "annotation"))) {
#XXX See line 377. This is almost an identical condition. This has no sequence in it.

          # ???Can't this be merged with another mechanism to process the children???
        w = xmlSApply(type, xmlName) == "attribute"

        els = lapply(type[w], processAttribute, namespaceDefs = namespaceDefs,
                                  targetNamespace = targetNamespace, elementFormDefault = elementFormDefault,
                                  localElements = TRUE, types  = types)

        w =  which(xmlSApply(type, xmlName) == "attributeGroup")
        for(k in w) {
            tmp = processAttribute(type[[k]], namespaceDefs = namespaceDefs, targetNamespace = targetNamespace,
                                     elementFormDefault = elementFormDefault, localElements = TRUE, types  = types)
            els[names(tmp)] = tmp
        }

        
        name = xmlGetAttr(type, "name")
        if(length(name) == 0 || is.na(name))
            name = xmlGetAttr(xmlParent(type), "name")
        
           # we used to define the class here directly with new("ClassDefinition")
        def = ClassDef(name, slotTypes = structure(els, names = sapply(els, slot, "name")), # names = xmlSApply(type, xmlGetAttr, "name")),
                         documentation = docString, targetNamespace = targetNamespace, elementFormDefault = elementFormDefault,
                         uris = targetNamespace)        
     #   def = new("SchemaComplexType", name = name, attributes = els)
     #   def@xmlAttrs = as(xmlAttrs(type), "character")

       # XXX
#<xs:complexType name="ArrayType" mixed="true">
# <xs:attribute name="n" type="INT-NUMBER" use="optional"/>
# <xs:attribute name="type" use="required">
#   <xs:simpleType>
#     <xs:restriction base="xs:string">
#       <xs:enumeration value="int"/>
#       <xs:enumeration value="real"/>
#       <xs:enumeration value="string"/>
#     </xs:restriction>
#   </xs:simpleType>
# </xs:attribute>
#</xs:complexType>

# <xs:complexType>
#  <xs:attribute name="name" type="xs:string" use="required"/>
#  <xs:attribute name="optype" type="OPTYPE"/>
#  <xs:attribute name="dataType" type="DATATYPE"/>
# </xs:complexType> 
  } else if(xmlName(type) == "complexType" &&  (xmlSize(type) == 0 || trim(xmlValue(type)) == "")) {

    def = ClassDef(name, list(), targetNamespace = targetNamespace, elementFormDefault = elementFormDefault)
     # now handle the childen.

    kids = dropAnnotationNodes(type)
    els = lapply(kids, processSchemaType, types = types, namespaceDefs = namespaceDefs,
                                       targetNamespace = targetNamespace, elementFormDefault = elementFormDefault, localElements = TRUE)
    # if some of these came back as a list, e.g. <attributegroup ref>, then unravel these
    w = sapply(els, is.list)
    if(all(w) && length(els) == 1)
      els = els[[1]]
    else if(any(w))
      els = do.call(c, els)
    def@slotTypes = els
    #XXX names?
    
  } else if(xmlName(type) == "all") {
     return(xmlSApply(type, processSchemaType, types = types, namespaceDefs = namespaceDefs,
                                       targetNamespace = targetNamespace, elementFormDefault = elementFormDefault, localElements = TRUE))
  } else {

     warning("Failed to handle node ", name, " of type ", xmlName(type),
              if(xmlSize(type) > 0) c(" & ", xmlName(type[[1]])), " in processSchemaType. ",
                as(type, "character"),
                 class = "ProcessSchemaTypeError")
     return(NULL)
  }

  if(is(def, "GenericSchemaType")) {
     def@documentation = docString
     if(length(def@nsuri) == 0 || is.na(def@nsuri))
         def@nsuri = as.character(targetNamespace)
   }

  return(def)
}


processGroup =
function(node, types, namespaceDefs, name = "",  targetNamespace = NA, elementFormDefault = NA)
{

   ref = xmlGetAttr(node, "ref")
   if(!is.null(ref)) {
       els = strsplit(ref, ":")[[1]]
       ans = new("SchemaGroupRefType")
    } else {
       name = xmlGetAttr(node, "name")
       els = strsplit(name, ":")[[1]]
       ans = new("SchemaGroupType") #, name = els[length(els)], ns = if(length(els) > 1) els[1]) # nsuri
       ans@slotTypes = xmlApply(node, processSchemaType, types = types, namespaceDefs = namespaceDefs,
                                       targetNamespace = targetNamespace, elementFormDefault = elementFormDefault, localElements = TRUE)
   }

   ans@count = getElementCount(node)
   ans@name = els[length(els)]

   if(length(els) > 1) {
     i = match(els[1], names(namespaceDefs))
     if(!is.na(i))
        ans@nsuri = namespaceDefs[[i]]$uri
   } else  {
#     if(any(i <- names(namespaceDefs) == ""))
#       ans@nsuri = namespaceDefs[[ which(i)[1] ]]$uri
#     else
        ans@nsuri = targetNamespace
   }

   ans
}

elementToType =
  #XXX should we leave this to later processing to make more comprehensive reductions.
function(x) {

  if(is(x, "LocalElement")) {
     tmp = x@type
     tmp@count = x@count


         # If there is a non-trivial count, we make this Element into a SimpleSequenceType.
     tt = if(length(tmp@count) > 0 && !all(tmp@count == 1)  && max(tmp@count) > 1) {
            new("SimpleSequenceType", elType = tmp, count = tmp@count, name = as.character("<Anon>"))
          } else
            tmp
     x@type = tt
     x
   } else
     x
}

dropAnnotationNodes =
function(node)
   xmlChildren(node)[ !(names(node) %in% c("documentation", "annotation")) & !xmlSApply(node, is, "XMLInternalTextNode")]  

processChoice =
  #
  #  process a <choice> element, typically turning it into a UnionDefinition
  #  but sometimes a SimpleSequenceType.
  #
function(node, types, namespaceDefs, name = "",  targetNamespace = NA, elementFormDefault = NA)
{
  
           #??? Can we call processSchemaType instead of getType - Yes
      kids = dropAnnotationNodes(node)
      slotTypes = lapply(kids, processSchemaType, types, namespaceDefs = namespaceDefs, targetNamespace = targetNamespace,
                               elementFormDefault = elementFormDefault, localElements = TRUE)
      names(slotTypes) = sapply(kids,  getElementName) 

           # Any Element objects here should be mapped to their type.
           # We could keep the element if we had the type adequately resolved at that point.
      slotTypes = sapply(slotTypes, elementToType)
      
        # Want to find the namespaces to identify the origin of the definition
        # in case of ambiguities and  also built-in types.
     # If getType resolves the SchemaType, then we 
     #      uris = getTypeNamespace(slotTypes, tmp)
     #      uris = sapply(slotTypes, function(x) x@nsuri)
#XXX
uris = as.character(rep(NA, length(slotTypes)))
      
      count = getElementCount(node)

      if(length(name) == 0 || is.na(name) || name == "") {
         ids = names(slotTypes)
         if(any(is.na(ids)))
           ids[is.na(ids)] = sapply(slotTypes[is.na(ids)], computeName)
         name = paste(ids, collapse = "Or")
      }
      
      ans = if(xmlSize(node) == 1)
               ClassDef(name, slotTypes[1], uris, elementFormDefault = elementFormDefault)
            else    
               UnionDef(name, slotTypes, uris)


      if(length(count) > 1 && max(count) > 1)
          new("SimpleSequenceType", name = name, count = count, elType = ans, elementType = "<choice>") #XXX
      else
        ans
}

getType =
function(node, types, namespaceDefs = list(), targetNamespace = NA, elementFormDefault = NA)
{
  if(xmlName(node) == "sequence") {
    processSequence(node, types, namespaceDefs)
  } else if(xmlName(node) == "choice") {
    processChoice(node, types, namespaceDefs, "", targetNamespace = targetNamespace, elementFormDefault = elementFormDefault)
  } else if(xmlName(node) == "element") {
    id = xmlGetAttr(node, "type", xmlGetAttr(node, "ref", as.character(NA)))
    lookupType(id, types, namespaceDefs, node = node)
  } else
    stop("Handle this case in getType for ", xmlName(node))
}

setGeneric("getElementName",
  #
  # Handles an <element name="..."> and <element ref="...">
  #
function(node, keepNS = FALSE, ...)
      standardGeneric("getElementName"))

if(FALSE) {
{
   ans = xmlGetAttr(x, "name")
   if(length(ans))
     return(ans)

   ref = xmlGetAttr(x, "ref")
   if(length(ref)) {
      doc = as(node, "XMLInternalDocument")
      node = getNodeSet(doc, "//x:*[@name =", sQuote(ref), " or @name =", sQuote(discardNamespace(ref)), "]", "x")
      if(length(node))
        return(xmlName(node[[1]]))
   }

   NA
}
}

setMethod("getElementName", "ANY",
  #
  # And defined differently again!!!
  #
function(node, keepNS = FALSE, ...)
{  
  ans = xmlGetAttr(node, "name", xmlGetAttr(node, "ref", if(xmlName(node) == "any") "any" else as.character(NA)))

  if(is.na(ans)) {
      if(xmlName(node) %in% c("choice", "sequence") ) {
         kids = !(xmlSApply(node, xmlName) %in% c("annotation", "documentation"))
         ans = paste(sapply(xmlChildren(node)[kids], getElementName, FALSE),
                     collapse = switch(xmlName(node), sequence = ".", choice = "Or", "."))
     } else
        warning("NA from getElementName() for ", saveXML(node))
  }
  if(keepNS)
    ans
  else
    gsub(".*:", "", ans)
})

setMethod("getElementName", "AnySchemaType",
  #
  # And defined differently again!!!
  #
function(node, keepNS = FALSE, ...)
  "any"
)



asCount =
function(x)
{
   if(x == "unbounded")
     Inf
   else
     as.numeric(x)
}

getElementCount =
function(node)
{
  c(min = xmlGetAttr(node, "minOccurs", 1L, as.integer),
    max = xmlGetAttr(node, "maxOccurs", 1L, asCount))
}


processSequence =
  #
  # There are two basic kinds of sequences:
  #   1) an ordered collection of 1 or more different elements, some optional
  #   2) zero or more instances of the same element
  #
  #  2) maps to a list in R, but we might need to impose a constraint on the number of entries in the list.
  #  1) maps to a class definition. It is a structure, perhaps with missing/NULL/default values for  elements.
  #
  # There are also choice groups !!!! See SDMX
  #
  #
function(node, types, namespaceDefs = list(), name = getElementName(node), targetNamespace = NA, elementFormDefault = NA)
{

  if(xmlSize(node) == 1 && !is.na(xmlGetAttr(node[[1]], "maxOccurs", NA))) {

     elType = processSchemaType(node[[1]], types, namespaceDefs = namespaceDefs,
                                 targetNamespace = targetNamespace, elementFormDefault = elementFormDefault, localElements = TRUE)

     count = getElementCount(node[[1]])
     
     
      #??? We have  changed SimpleSequenceType to allow an element or a SchemaType in elType.

     
     if(any(count > 1) ) {
        if(is(elType, "Element"))
            elType = elType@type       
        ans = new("SimpleSequenceType", name = name, elType = elType, count = count)
        ans@nsuri = as.character(targetNamespace)
     
          # ??? where should it be - on the sequence or the element(s)
        if(is(ans@elType, "SchemaType"))
             ans@elType@count = ans@count
                                   

        if(is(ans@elType, "SchemaType") && all(!is.na(ans@elType@count)) && all(ans@elType@count %in% c(0, 1))) {
          # if the sequence has a single element and the minOccurs and maxOccurs are both 1,
          # then return just the element.
        #XXX have to be careful that we recognize that the content is within the outer node given by name
          tmp = ans@elType
          return(tmp)
        }
      } else
        ans = elType

     return(ans)
   }


     # Build a SchemaType for each of the slots.
     # ??? Should we use processSchemaType
if(FALSE) {  
  slotTypes = xmlApply(node, function(x) {
                              typeName =  xmlGetAttr(x, "type", xmlGetAttr(x, "ref"))
                              SchemaType(typeName,
                                       nsuri = lookupNamespace(typeName, x),
                                       namespaceDefs = namespaceDefs,
                                       count = getElementCount(x))
                            })
} else {
   kids = dropAnnotationNodes(node)
   slotTypes = lapply(kids, processSchemaType, types, namespaceDefs = namespaceDefs, targetNamespace = targetNamespace,
                        elementFormDefault = elementFormDefault, localElements = TRUE)
}

  names(slotTypes) = ids = as.character(sapply(kids, getElementName))
                       
  slotTypes = slotTypes[ ! sapply(slotTypes, is.null) ]

   if(length(name) == 0 || is.na(name) || name == "") {
if(any(is.na(ids))) browser()     #XXX
      name = paste(ids, collapse = ".")
   }
  
  if(TRUE || !is.na(name))
    ClassDef(name, slotTypes, elementFormDefault = elementFormDefault, targetNamespace = targetNamespace)
  else
    slotTypes
}


  

lookupNamespace =
function(id, node)
{
  els = strsplit(id, ":")[[1]]
  if(length(els) > 1)
     xmlSearchNs(node, els[1], asPrefix = TRUE)
  else
    getTargetNamespace(node)
}


# Children
# no children - simple element  esearch.xsd  Count.
# complexType with a sequence  egquery.xsd  Result
# simpletype



processSchemaElement =
  #
  # process a <element> into a SOAP type.
  #
function(element, name = xmlGetAttr(element, "name"), namespaceDefs = list(), types = NULL,
                   targetNamespace = NA, elementFormDefault = NA, localElements = FALSE)
{
#if(length(name) && !is.na(name) && name  == "altitude") browser()
#if(length(name) && name == "LookAt") browser()

  defaultValue = xmlGetAttr(element, "default", NA_character_) #XXX Immediate character()  
  attrs = xmlAttrs(element)

  count = getElementCount(element)

#if(length(name) && !is.na(name) && name == "row") browser()
  
  if(all(c("name", "type") %in% names(attrs)))  {
           #XXX test this instead of the remainder of the if() body
           #XXX need to process additional attributes such as nillable="true"
      ans = getElementRef(xmlGetAttr(element, "type"), element, types, namespaceDefs, targetNamespace, localElements)
      ans@default = optionalDefaultValue(ans, defaultValue)
      ans@count = count
      return(ans)

if(FALSE) {
      #??? Why should this always be a SimpleElement.
      # e.g. <element name="foo" type="xsd:string"/>
      # should map to <foo>
      #XXX deal with nillable="true"
    els = strsplit(attrs["type"], ":")[[1]]
    if(length(els) > 1) {
      uri = findNamespaceDefnByPrefix(els[1], element)
      ty = els[2]
    } else {
      uri = getTargetNamespace(element)
      ty = els
    }

    ans = new("SimpleElement", name = attrs["name"], type = ty, nsuri = uri)
    ans@default = optionalDefaultValue(ans, defaultValue)
      
    return(ans)
 } # FALSE
    }

  if(!is.null(ref <- xmlGetAttr(element, "ref"))) {
      ans = getElementRef(ref, element, types, namespaceDefs, targetNamespace, localElements)
      ans@default = optionalDefaultValue(ans, defaultValue)
      ans@count = count
      return(ans)
  }

  
  if(xmlSize(element) == 0) {
    
     obj =  new("SchemaVoidType")  # new("SimpleElement", name = name)
     
  } else if(names(element)[1] == "complexType" && all(names(element[[1]]) == "attribute")) {
    #XXXXX See line 362. Very similar code.
     # e.g. ParameterField, ArrayType in PMML.
     #XXXX what about attributeGroups giving rise to multiple attributes? Is this in resolve()
    attrs = xmlApply(element[[1]], processAttribute, namespaceDefs = namespaceDefs,
                                                                                      targetNamespace = targetNamespace,
                                                                                      elementFormDefault = elementFormDefault,
                                                                                      localElements = TRUE, types = types)
    names(attrs) = sapply(attrs, slot, "name")
#browser()    
    obj = new("SimpleElement", name = xmlGetAttr(element, "name", as.character(NA)),
                                attributes = attrs,
                                type = character(), #,   count = getElementCount(element)
                                default = defaultValue)

  } else {
      # complexType
    if(xmlName(element[[1]]) == "complexType") {
             #XXX what about if there are additional children and complexType is not the first one!!!!
             # should run all the children through processSchemaType.
        if(xmlSize(element) > 1)
           warning("currently skipping additional children within <element> definition for ", name)

        el = element[[1]]
         #XXX Forcing the name here.  But it doesn't seem to be used so can back this out.
#        if(is.null(xmlGetAttr(el, "name")))
#           xmlAttrs(el) = c(name = name)

        type = processSchemaType(el, types, namespaceDefs = namespaceDefs, targetNamespace = targetNamespace,
                                  elementFormDefault = elementFormDefault, localElements = TRUE)

        type@name = setNameIf(type@name, name)
        type@Rname = setNameIf(type@Rname, name)

        #XXX Forcing this here. Should be done in processSchemaType()
        if(is(type, "ClassDefinition")  && any(names(type@slotTypes) == "")) {
             names(type@slotTypes) = name # sapply(type@slotTypes, slot, "name")
#             type@slotTypes[[1]]@name = type@slotTypes[[1]]@Rname = name
         }
        
        obj = new("Element", name = name, type = type, Rname = name)

        i = xmlSApply(element[[1]], xmlName) == "attribute"
        if(any(i))  {
          obj@attributes = sapply(xmlChildren(element[[1]])[i], processAttribute, namespaceDefs = namespaceDefs, targetNamespace = targetNamespace, elementFormDefault = elementFormDefault, types = types)
          names(obj@attributes) = sapply(obj@attributes, slot, "name")
        }

        # obj@elements = xmlApply(element[[1]], processSchemaType, types, namespaceDefs = namespaceDefs, localElements = TRUE)
        
      } else if(xmlSize(element) == 1) {

       #??? for "priority-active-indicator", we end up with EnumValuesDef. Should this be a RestrictedStringDefinition
         tp = processSchemaType(element[[1]], types, namespaceDefs = namespaceDefs, localElements = TRUE)
#         if(length(tp@name) == 0 || is.na(tp@name))
#           tp@name = xmlGetAttr(element, "name", as.character(NA))

         obj = new(if(localElements) "LocalElement" else "Element", name = name, type = tp)
          
      } else {
         stop("Unhandled case for processSchemaElement!")
         obj = NULL
      }
  } 

# if(length(targetNamespace) < 1 || is.na(targetNamespace))      stop("hey")
  
  if(!is.null(obj))
     obj@nsuri = as.character(targetNamespace)

  if(!is.null(defaultValue))
      obj@default = defaultValue

   obj@default = optionalDefaultValue(obj)

   obj@Rname = name
   obj@count = count
  
  obj
}

optionalDefaultValue =
function(obj, default = obj@default)
{
#XXX
  if(is(obj, "SchemaTypeReference"))
    return(default)

  if(length(default) && !is.na(default))
    return(default)


   if(is(obj, "ClassDefinition") && typeof(default) %in% c("integer", "logical", "character", "numeric"))
      return(NULL)
  
  if(is(obj, "Element"))
    return(optionalDefaultValue(obj@type, default))

  if(is(obj, "SchemaType") && length(obj@count) > 0 && 0 %in% obj@count) {
     if(is(obj, "SimpleSequenceType"))
         NULL
     else
        vector(class(default), 0)
  } else
     default
}


getElementRef =
function(id, node, types = NULL, namespaceDefs = list(), targetNamespace = NA, localElements = FALSE)
{

  if(is.na(id) || id == "")
     id = xmlGetAttr(node, "name", xmlGetAttr(node, "ref", NA))

   els = strsplit(id, ":")[[1]]
   if(length(els) == 1)
      els = c("", els)
#   uri = findNamespaceDefnByPrefix(els[1], node)
 uri = NULL
   if(is.null(uri) || is.na(uri)) {
     i = match(els[1], names(namespaceDefs))
     if(!is.na(i))
       uri = namespaceDefs[[ i ]][["uri"]]
   }

   if(is.null(uri) || is.na(uri))
     uri = as.character(targetNamespace)
      
#XXX should be an element reference.
#XXX get the nsuri as the targetNamespace

   className = if(localElements) "LocalElement" else "Element"

    # if there is no name, but just a ref, don't we want to control how we generate this.

   tp = SchemaType(name = els[2], els[1],
                   nsuri = uri, namespaceDefs = namespaceDefs) # new("SchemaTypeReference", name = els[2], nsuri = uri, ns = els[1])

   if(FALSE && length(types))
     tp = resolve(tp, types)

   count = getElementCount(node)
   tp@count = count
   
   ans = new(className, name = xmlGetAttr(node, "name", as.character(NA)),
                        type = tp,
                        nsuri = as.character(targetNamespace))

   ans@default = xmlGetAttr(node, "default", character())

   if(localElements)
      ans@count = getElementCount(node)

#   elementToType(ans)
   
   ans
}

getAttributeGroup = 
function(refNode, namespaceDefs = list(), targetNamespace = NA, elementFormDefault = NA, localElements = FALSE, types = list(),
         doc = as(refNode, "XMLInternalDocument"))
{

        #XXX We take off any names space. We should be more careful here if there is more than one namespace.
     groupName = discardNamespace( xmlGetAttr(refNode, "ref") )
     agroup = getNodeSet(doc, paste("//xs:attributeGroup[@name=", sQuote(groupName), "]"), c(xs = "http://www.w3.org/2001/XMLSchema"))
     if(length(agroup) == 0) {
          # look for the explicit name in case it has a ns:name
      agroup = getNodeSet(doc, paste("//xs:attributeGroup[@name=", sQuote(xmlGetAttr(refNode, "ref")), "]"), c(xs = "http://www.w3.org/2001/XMLSchema"))
       
       if(length(agroup) == 0) {
browser()                 
          return(new("AttributeGroupReference", name = groupName))
#           stop("Cannot find attribute group named ", sQuote(groupName))
        }
     }
     processAttributeGroup(agroup[[1]], namespaceDefs, targetNamespace, elementFormDefault, localElements, types)
}

processAttributeGroup =
function(node, namespaceDefs = list(), targetNamespace = NA, elementFormDefault = NA, localElements = FALSE, types = list(), ...)
{
  tmp = sapply(xmlChildren(node)[names(node) == "attribute"], processAttribute, namespaceDefs = namespaceDefs, targetNamespace = targetNamespace, ...)
  names(tmp) = sapply(tmp, slot, "name")
  tmp
}



processAttribute =
function(node, name = xmlGetAttr(node, "name"), type = xmlGetAttr(node, "type", as.character(NA)),
          namespaceDefs = character(), targetNamespace = NA, elementFormDefault = NA, localElements = FALSE, types = list())
{
  if(xmlName(node) == "attributeGroup")
         # whoever calls processAttribute has to be able to handle 
     return(getAttributeGroup(node, namespaceDefs, targetNamespace, elementFormDefault, localElements, types))
  
   ns = character()
   
   if(xmlName(node) == "anyAttribute")
     return(new("AnyAttributeDef")) #XXX  Namespace

   if(!is.null(ref <- xmlGetAttr(node, "ref"))) {
          #XXX merge with getAttributeGroup
      tmp = getNodeSet(as(node, "XMLInternalDocument"), sprintf("//xsd:schema/xsd:attribute[@name='%s']",
                                                                gsub("[a-z0-9]+:", "", ref)),
                  c(xsd = "http://www.w3.org/2001/XMLSchema"))
      
      if(length(tmp) == 0 && grep(":", ref)) {
         els = strsplit(ref, ":")[[1]]
         ns = els[1]
         ref = els[2]
browser()         
         tmp = getNodeSet(as(node, "XMLInternalDocument"), sprintf("//xsd:schema/xsd:attribute[@name='%s']",
                                                                gsub("[a-z0-9]+:", "", ref)),
                           c(xsd = "http://www.w3.org/2001/XMLSchema"))
      }
      
      if(length(tmp) == 0) {
browser()
         nsuri = if(length(ns) && !is.na(i <- match(ns, names(namespaceDefs)))) 
                    namespaceDefs[[i]]$uri
                 else
                    character()
         return(new("AttributeGroupReference", name = ref, ns = ns, nsuri = nsuri)) #, namespace = as(targetNamespace, "character")))
         stop("Cannot find attribute reference for ", ref)
      }
      node = tmp[[1]]
   }
     
   if(is.na(type) && xmlSize(node)) {
     if(xmlName(node[[1]]) == "simpleType" &&
           xmlName(node[[1]][[1]]) == "restriction") {
                  # handle non-string types too in the restriction.
          type = processSchemaType(node[[1]], types, namespaceDefs = namespaceDefs, targetNamespace = targetNamespace,
                                     elementFormDefault = elementFormDefault, localElements = TRUE)
          if(is.na(type@name) || type@name == "")
              type@name = type@Rname = sprintf("%s.Enum", name)

     } else
        if(xmlSize(node) > 1 || xmlName(node[[1]]) != "annotation") {
          nodeNames = xmlSApply(node, xmlName)
          if(setdiff(nodeNames, "annotation") == "simpleType") {
             type = processSchemaType(node[["simpleType"]], types, namespaceDefs = namespaceDefs,
                                       targetNamespace = targetNamespace, elementFormDefault = elementFormDefault,
                                         localElements = TRUE)
          } else
             warning("<fixme> Skipping children ", paste(nodeNames, collapse = ", "),
                        " in <attribute> definition ", xmlGetAttr(node, "name"))
        }
   }


   if(is.character(type)) {
      if(!is.na(type))
         type = SchemaType(type, namespaceDefs = namespaceDefs)
      else
         type = new("SchemaStringType")  # should this be a simple string or a SchemaAnyType. 
    }

   new("AttributeDef", name = name,
                       type = type,
                       use = xmlGetAttr(node, "use", "optional"),
                       default = xmlGetAttr(node, "default", as.character(NA)),       
                       fixed = xmlGetAttr(node, "fixed", as.character(NA)))
}



getSubstitutionGroups =
  #
  # doc = xmlParse("../inst/samples/kml21.xsd")
  # g = getSubstitutionGroups(doc)
  # by(g, g$group, function(x) x)
  #
  # Need to include this information in the class definitions.
  #
  #  e.g. kml:Geometry is a kml:Geometry and there are
  # the following in the substitutionGroup
  #         name                  type        group  
  #MultiGeometry kml:MultiGeometryType kml:Geometry
  #        Point         kml:PointType kml:Geometry
  #   LineString    kml:LineStringType kml:Geometry
  #   LinearRing    kml:LinearRingType kml:Geometry
  #      Polygon       kml:PolygonType kml:Geometry
  #        Model         kml:ModelType kml:Geometry  
  #
  # XXX Check for nesting in groups
  # 
  #
function(doc)
{
   groups = getNodeSet(doc, "//*[@substitutionGroup]")
   type = sapply(groups, xmlGetAttr, "type", as.character(NA))
   data.frame(name = sapply(groups, xmlGetAttr, "name"),
              type = type,
              group = sapply(groups, xmlGetAttr, "substitutionGroup"))
}



processExtension =
function(type, name, types, namespaceDefs, targetNamespace = NA, elementFormDefault = NA)
{
     base = xmlGetAttr(type[["extension"]], "base")
                      # Now get the extensions.
        #??? do we need to fix the namespace prefix, e.g. kml:ObjectType

#if(name == "ContainerType") browser()
     
#     baseType = lookupType(base, types, namespaceDefs, node = type)
     baseType = NULL

     if(is.null(baseType))
         baseType = SchemaType(base, namespaceDefs = namespaceDefs) # resolve(base, types, namespaceDefs)

       # start with no slot types.
     def = ClassDef(name, list(), obj = new("ExtendedClassDefinition"))
     def@base = baseType@name
     def@baseType = baseType
     type = type[[1]]

     kids =  dropAnnotationNodes(type)
     def@slotTypes = lapply(kids, processSchemaType, types, namespaceDefs = namespaceDefs, targetNamespace = targetNamespace, elementFormDefault = elementFormDefault, localElements = TRUE)
#     if(length(def@slotTypes)
#       names(def@slotTypes) = sapply(kids, computeName)
     
     if(length(def@slotTypes) == 1 && is(def@slotTypes[[1]], "ClassDefinition"))
       def@slotTypes = def@slotTypes[[1]]@slotTypes
     else if(length(def@slotTypes) == 1 && is(def@slotTypes[[1]], "list")) { # corresponds to an <extensions><all></extension>
       def@slotTypes = def@slotTypes[[1]]
       names(def@slotTypes) = sapply(def@slotTypes, computeName)
     } else
       names(def@slotTypes) = sapply(kids, getElementName)
     
     def
}

lookupType =
  # getType is to avoid getting an Element
function(name, types, namespaceDefs = list(), getType = TRUE, node = NULL)
{
 #XXX Deal with distinguishing between elements/references and actual types.
  # getType is intended to control whether this happens or not.
  
     id = strsplit(name, ":")[[1]]
     if(length(id) == 2) {
        i = match(id[1], names(namespaceDefs))
        if(is.na(i)) {
          if(length(node))  {
              #???
             u = getTargetNamespace(node)
             if(length(u) == 0)
                stop("Need to determine URI")
          } else
          stop("Need to determine URI")
        } else
          u = namespaceDefs[[i]]$uri
        
        if(u %in% names(types))
           igetSchemaType(id[2], types[[u]])
        else if(!is.null(node) && id[2] %in% names(types) && length(xmlSearchNs(node, u, asPrefix = FALSE))) {
            # See if this is in the current target namespace
           igetSchemaType(id[2], types)
        } else {
             # why do we do this if we already have u
           if(is.na(u))
               u = xmlSearchNs(node, u, asPrefix = FALSE)

           new("SchemaTypeReference", nsuri = u, ns = id[1], name = name)
        }
     } else {
        igetSchemaType(id, types)
     }
}

igetSchemaType =
  # internal function.
function(name, schema, getType = TRUE)  
{
  if(is(schema, "SchemaCollection") && length(schema) == 1)
    schema = schema[[1]]
  
  i = match(name, names(schema))
  if(all(is.na(i)))
    return(NULL)

  if(sum(!is.na(i)) == 1)
     return(schema[[ i[!is.na(i)] ]])

  types = schema[!is.na(i)]
  w = ! sapply(types, inherits, c("Element", "SchemaTypeReference"))
if(!any(w))
  warning("Ooops")
  types[[w]]
}

processRestriction =
  # Is this still used. See 462, but perhaps createRestrictionType is more appropriate.
function(type, name, types, namespaceDefs, targetNamespace = NA, elementFormDefault = NA)
{  
                   # Currently we interpret this as an Array.
                   # So get the <restriction><attribute> element
                   # and take the arrayType and ref attribution from that.
            # In interop.wsdl, we also have a <sequence> <element> ... node within this restriction.
#cat("dealing with restriction\n");browser()    
    restriction = type[["restriction"]]
    a = restriction[["attribute"]]
    if(is.null(a)) {
          # See pmml-3-2.xsd and the "row" element.
          #XXXXXXXX
        base =  xmlGetAttr(restriction, "base")
        if(xmlSize(restriction) > 0) {
          def =  processSchemaType(restriction, types, namespaceDefs = namespaceDefs, targetNamespace = targetNamespace,
                                    elementFormDefault = elementFormDefault, localElements = TRUE)

          if(is(def, "SchemaComplexType") && is(def@content, "SimpleSequenceType"))  #??? perhaps add base is and Array - grepl("(^|:)Array", base)
            def = def@content

          def@name = name
          
        } else {
           warning("case not handled")
           def = NULL
        }
        
    } else {
       def = ArrayType(xmlGetAttr(a, "arrayType", addNamespace = FALSE), namespaceDefs = namespaceDefs)
       def@name = name
    }

    def
}




postprocessComplexType =
function(def)
{
  if(!( is(def@content, "ClassDefinition") || is(def@content, "SimpleSequenceType")))
    return(def)

  if(is(def@content, "ClassDefinition"))  {
    ans = def@content
  } else if(is(def@content, "SimpleSequenceType"))
if(FALSE) {    
    ans = new("ClassDefinition", slotTypes = list(def@content))
    
   ans@documentation = def@documentation
   ans@name = def@name
   ans@Rname = def@Rname
   nslot = length(ans@slotTypes)
   ans@isAttribute = c(rep(FALSE, nslot), rep(TRUE, length(def@attributes)))
   ans@slotTypes = c(ans@slotTypes, def@attributes)    
}
  ans =  ClassDef(def@name, c(def@content, def@attributes), targetNamespace = def@nsuri, documentation = def@documentation)

   ans
}


setNameIf =
function(name, other)
{
  if(length(name) == 0 || is.na(name) || name == "")
     other
  else
     name
}

makeAttributeGroup = 
function(node, context, namespaceDefs = list(), targetNamespace = NA, ...)
{
   ans = new("AttributeGroup", name = xmlGetAttr(node, "name"), nsuri = as(targetNamespace, "character"))
   ans@attributes = xmlApply(node, processSchemaType, types = context,
                                     namespaceDefs = namespaceDefs, targetNamespace = targetNamespace, ...)
   ans
}
