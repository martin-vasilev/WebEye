validate <- function(
    x
    , name = NULL
    , check_class = NULL
    , check_mode = NULL
    , check_integer = FALSE
    , check_NA = TRUE
    , check_infinite = TRUE
    , check_length = NULL
    , check_dim = NULL
    , check_range = NULL
    , check_cols = NULL
) {
  if(is.null(name)) name <- deparse(substitute(x))
  
  if(is.null(x)) stop(paste("The parameter '", name, "' is NULL.", sep = ""))
  
  if(!is.null(check_dim) && !all(dim(x) == check_dim)) stop(paste("The parameter '", name, "' must have dimensions " , paste(check_dim, collapse=""), ".", sep = ""))
  if(!is.null(check_length) && length(x) != check_length) stop(paste("The parameter '", name, "' must be of length ", check_length, ".", sep = ""))
  
  if((is.null(check_class) || !check_class == "function") && any(is.na(x))) {
    if(check_NA) stop(paste("The parameter '", name, "' is NA.", sep = ""))
    else return(TRUE)
  }
  
  if(check_infinite && inherits(x, "numeric") && any(is.infinite(x))) stop(paste("The parameter '", name, "' must be finite.", sep = ""))
  if(check_integer && inherits(x, "numeric") && any(x %% 1 != 0)) stop(paste("The parameter '", name, "' must be an integer.", sep = ""))
  
  for(x.class in check_class) {
    if(!methods::is(x, x.class)) stop(paste("The parameter '", name, "' must be of class '", x.class, "'.", sep = ""))
  }
  
  for (x.mode in check_mode) {
    if(!check_mode %in% mode(x)) stop(paste("The parameter '", name, "' must be of mode '", x.mode, "'.", sep = ""))
  }
  
  if(!is.null(check_cols)) {
    test <- check_cols %in% colnames(x)
    
    if(!all(test)) {
      stop(paste0("Variable '", check_cols[!test], "' is not present in your data.frame.\n"))
    }
  }
  
  if(!is.null(check_range) && any(x < check_range[1] | x > check_range[2])) stop(paste("The parameter '", name, "' must be between ", check_range[1], " and ", check_range[2], ".", sep = ""))
  TRUE
}
