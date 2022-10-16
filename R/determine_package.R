determine_package <- function(fun) {
	UseMethod("determine_package")
}

#' @export
determine_package.function <- function(fun) {
	if (!identical(typeof(fun), "closure")) {
		# Normal functions (e.g. `mean`) and infix functions (e.g. `%in%`)
		# have class `function` and type `closure`.
		# Special operators (e.g. `+` or `<-`) also have class `function`,
		# but type `builtin` or `special`.
		# Special operators can't be exported from packages,
		# so if a function does not have type `closure`, it must be from `base`.
		return("base")
	}

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
	cli::cli_div(theme = cli_theme_wtfn())

	fun_text <- fun

	fun <- make_syntactic_name(fun)

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

make_syntactic_name <- function(fun) {
	namespace <- ""

	if (grepl("[[:alnum:]\\.]+:::?", fun)) {
		namespace <- regmatches(
			fun,
			regexpr("[[:alnum:]\\.]+:::?", fun)
		)
		fun <- sub("[[:alnum:]\\.]+:::?", "", fun)
	}

	fun <- rlang::expr_text(rlang::sym(fun))

	paste0(namespace, fun)
}
