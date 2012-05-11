setClass("ObjectType", representation(id = "ID", targetId = "NCName"), contains= "VirtualXMLSchemaClass")
setClass("FeatureType",  representation(name = "character", visibility = "logical", open = "logical"), contains = "ObjectType",
                  prototype = prototype(visibility = TRUE, open = FALSE))
setClass("NetworkLinkType", representation(refreshVisibility = "logical", flyToView = "logical"), contains = "FeatureType",
              prototype = prototype(refreshVisibility = FALSE, flyToView = FALSE))


setClass("PlacemarkType", representation(Geometry = "GeometryType"), contains = "FeatureType")

#

setClass("ObjectType", representation(a = "logical"), contains= "VirtualXMLSchemaClass")
setClass("FeatureType",  representation(name = "character", open = "logical"), contains = "ObjectType")
setClass("NetworkLinkType", representation(flyToView = "logical"), contains = "FeatureType")
