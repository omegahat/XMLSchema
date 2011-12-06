findSelfRefs =
function(type)
{
  if(!is(type, "CompositeTypeDefinition"))
    return(type)

  ref = new("SelfRef", name = type@name, nsuri = type@nsuri)
  type@slotTypes = lapply(type@slotTypes, asSelfRef, ref)
  type
}

asSelfRef =
function(type, ref)
{
  orig = NULL
  isElement = is(type, "Element")
  if(isElement) {
    orig = type
    type = orig@type
    type@default = orig@default
  }
  if(!is.na(type@name) && type@name == ref@name && !is.na(type@nsuri) && type@nsuri == ref@nsuri) {
    if(isElement) {
       orig@type = ref ### may need to create an array/list/SimpleSequenceType, etc.
       orig
    } else
        ref
  } else
    type
}
