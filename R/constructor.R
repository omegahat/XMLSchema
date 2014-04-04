genericConstructor =
  #
  # setClass("Foo", representation(x = "integer", y = "numeric", z = "character"))
  # genericConstructor(x = 1, y = 2L, z = 3, .class = "Foo")
  #
  # setClass("Foo", representation(a = "integer", b = "logical"), contains = "integer")
  # genericConstructor(c(1, 2), 3, b = 1L, .class = "Foo")
  # 
function(..., .obj = new(.class), .class)
{
  args = list(...)
  
  for(i in setdiff(names(args), ""))
    slot(.obj, i) = as(args[[i]], class(slot(.obj, i)))

    # if any or all of the arguments don't have a name
    # deal with those by matching the
  if(length(names(args)) == 0)
      # just assume the arguments correspond to the slots in order, up to how many we have.
    ids = slotNames(.obj)[seq(along = args) ]
  else if(any(w <- names(args) == "")) {
      # so some arguments with no names. We'll do our best
      # by matching them to the remaining slots, in order.
     ids = setdiff(slotNames(.obj), names(args))
     args = args[w]
  }

  for(i in seq(along = args)) {
      id = ids[i]
      slot(.obj, id) = as(args[[i]], class(slot(.obj, id)))
  }


  .obj
}

