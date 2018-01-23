#++++++++++++++++++++++++++++++++++
# Captação de erros de codificacao:
catch.error = function(x){
  # let us create a missing value for test purpose
  y = NA
  # Try to catch that error (NA) we just created
  catch_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  # check result if error exists, otherwise the function works fine.
  return(y)
}
#++++++++++++++++++++++++++++++++++