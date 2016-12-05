#
# Compute coefficient of determination (R-squared)
#
#   + object is the returned object of lm(..), glm(..) or rlm(..)
#     if a regress model object is given, r.square(..) will get y
#     and fitted.y from this model
#   + user can also pass their y and fitted.y instead of a regress
#     model object
#     
# for example:
#
#  r.square(y, fitted.y)
#  r.square(lm(y1~x1+x2+x3, data=dfm))
#  r.square(rlm(cbind(y1, y2, y3)~x1+x2+x3, data=dfm))
#
r.square = function(object=NULL, y, fitted.y)
{
  # get y and fitted.y from a lm or glm object
  if (! is.null(object)) {
    fitted.y = fitted(object)
    if (class(fitted.y) == "numeric")
      y = object$model[[1]]
    else
      y = object$model[,1]
  }
  
  # compute coefficient of determination
  if (class(fitted.y) == "numeric") {
    return(cor(y, fitted.y)^2)
  } else {
    R2 = double(ncol(y))
    for (ic in 1:ncol(y))
      R2[ic] = cor(y[,ic], fitted.y[,ic])^2
    return(R2)
  }
}