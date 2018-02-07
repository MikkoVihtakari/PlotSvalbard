##' @title Select an element of each vector from a list
##' @description Selects y'th element of each vector from a list
##' @param x list
##' @param y number of element. Must be numeric
##' @keywords internal
##' @export

select <- function(x,y) sapply(x, "[", y)
