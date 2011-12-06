# Taken from the Definitive XML Schema book Chap 16 on substitution groups.
setClass("Product", "VIRTUAL")

setClass("Shirt", representation(size = "integer"), contains = "Product")
setClass("Umbrella", contains = "Product")
setClass("Hat", contains = "Product")


setClass("Blouse", contains = "Shirt")
setClass("TShirt", contains = "Shirt")

new("TShirt", size = 10L)


# With setClassUnion()

setClassUnion("UProduct", c("Shirt", "Umbrella", "Hat"))

setClass("Order", representation(product = "Product", count = "integer"))
setClass("UOrder", representation(product = "UProduct", count = "integer"))

new("Order", product =new("TShirt", size = 10L), count = 3L)
new("UOrder", product =new("TShirt", size = 10L), count = 3L)
