copy_package_function <- function(name, env) {
  package_function <- get(
    name,
    envir = asNamespace("ramp.xds"),
    inherits = FALSE
  )
  if (!is.function(package_function)) {
    stop("Package binding is not a function: ", name)
  }

  local_function <- eval(
    call("function", formals(package_function), body(package_function)),
    envir = env
  )
  assign(name, local_function, envir = env)
  local_function
}
