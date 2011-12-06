flattenSchema =
function(x)
{
   st = sapply(x, function(x) is(x, "SchemaTypes")
   sc = sapply(x, function(x) is(x, "SchemaCollection") 
   isAgg = st | sc
   if(!any(isAgg))
     return(isAgg)

   ans = new("SchemaCollection")
   
   n = lapply(x, flattenSchema)
   sapply(x, is, "SchemaCollection")
    
  
}
