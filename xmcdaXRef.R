setClass("string", contains = "character",
           validity = function(object) {
                      if(length(object) > 1)
                        "too many elements"
                      else
                        TRUE
                    })

setClass("author", contains = 'character')
setClass("user", contains = 'string')
setClass("version", contains = 'string')

setClass("description",
          representation(title = "string",
                         subTitle = "string",
                         subSubTitle = "string",
                         other = "list"))

setValidity("description",
            function(object) {

              if(!all(w <- sapply(object@other, function(x) is(x, "user") || is(x, "author") || is(x, "version") || is(x, "bibliography")))) # etc....
                 paste("not all elements in other are of the correct type:", paste(which(w), collapse = ", "))
              else 
                 TRUE
            })

setClass("bibliography",  contains = "list")
setValidity("bibliography", function(object) {

                              if(length(object) == 0)
                                return("must be at least one element")
                
                              if(!all(w <- sapply(object, function(x) is(x, "description") || is(x, "character")) ))
                                  paste("not all elements in other are of the correct type:", paste(which(w), collapse = ", "))
                            })



if(FALSE) {

d = new("description", other = list(new("user", "me"), new("author", c("Duncan Temple Lang", "Deb Nolan"))))

try(d <- new("description", other = list(new("user", c("me", "you")), new("author", c("Duncan Temple Lang", "Deb Nolan")))))

d = new("description", other = list(new("user", "me"), new("author", c("Duncan Temple Lang", "Deb Nolan"))))
d@other = c(d@other, new("bibliography"))

validObject(d)

e = d
e@other = c(d@other, new("string", "foo"))
validObject(e)  # no
}
