determine_package <- function(fun) {
	UseMethod("determine_package")
}

#' @export
determine_package.function <- function(fun) {
	env <- environment(fun)

	if (is.null(env)) {
		cli::cli_abort(c(
			"x" = "{.var {fun}} could not be found in any loaded packages.",
			"!" = "Do you need to load the package containing it?"
		))
	}

	environmentName(env)
}

#' @export
determine_package.character <- function(fun) {
	cli::cli_div(theme = cli_theme_caniuse())

	fun_text <- fun

	fun <- tryCatch(
		eval(rlang::parse_expr(fun)),
		error = function(e) {
			cli::cli_abort(c(
				"x" = "{.var {fun_text}} could not be found in any loaded packages.",
				"!" = "Do you need to load the package containing it?"
			))
		}
	)

	if (!is.function(fun)) {
		cli::cli_abort(c(
			"!" = "{.var {fun_text}} must be a {.cls function}.",
			"x" = "{.var {fun_text}} is an object of class {.cls {class(fun)}}."
		))
	}

	determine_package.function(fun)
}

#' @export
determine_package.default <- function(fun) {
	determine_package.character(as.character(fun))
}
