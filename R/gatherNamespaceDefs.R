gatherNamespaceDefs =
  #
  # merge the namespace definitions in this node with those of the ancestor nodes.
  #XXX Move this to the XML package.
  #
function(node, ancestors = TRUE)
{
  if(ancestors) {
     nodes = append(rev(xmlAncestors(node)), node)
     ans = xmlNamespaceDefinitions(nodes[[1]])
     if(is.null(ans))
       ans = structure(list(), class = "XMLNamespaceDefinitions")
     for(i in nodes[-1]) {
       tmp = xmlNamespaceDefinitions(i)
       ans[names(tmp)] = tmp
     }
     ans  
    
  } else
    xmlNamespaceDefinitions(node)
}
