determine_package <- function(fun) {
	UseMethod("determine_package")
}

#' @export
determine_package.function <- function(fun) {
	env <- environment(fun)

	if (is.null(env)) {
		cli::cli_abort(c(
			"x" = "{.code {fun}} could not be found in any loaded packages.",
			"!" = "Do you need to load the package containing it?"
		))
	}

	environmentName(env)
}

#' @export
determine_package.character <- function(fun) {
	determine_package.function(eval(rlang::parse_expr(fun)))
}

#' @export
determine_package.default <- function(fun) {
	determine_package.character(as.character(fun))
}
