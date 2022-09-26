can_i_use <- function(fun, fun_text = NULL) {
	if (is.null(fun_text)) {
		fun_text <- unquote(rlang::expr_text(rlang::enexpr(fun)))
	}
	pkg <- determine_package(fun)

	if (grepl("[[:alnum:]\\.]+:::?", fun_text)) {
		namespaced_fun <- fun_text
		bare_fun <- gsub("[[:alnum:]\\.]+:::?", "", fun_text)
	} else {
		namespaced_fun <- paste0(pkg, "::", fun_text)
		bare_fun <- fun_text
	}

	cli::cli_div(theme = cli_theme_caniuse())
	cli::cat_line()
	cli::cli_inform(c("i" = "{.strong {.var {fun_text}} is from {.pkg {pkg}}.}"))

	if (identical(pkg, desc::desc_get_field("Package"))) {
		cli::cli_inform(c(
			"i" = "{.pkg {pkg}} is the current package.",
			"v" = paste(
				"You can use {.var {fun_text}},",
				"because it's a function from {.pkg {pkg}},",
				"the package you're currently developing."
			)
		))

		return(invisible(TRUE))
	}

	if (identical(pkg, "base")) {
		cli::cli_inform(c(
			"i" = "{.pkg base} is an implicit dependency of all R packages.",
			"v" = "You can use {.var {fun_text}}, because it's a function from {.pkg base}.",
			"v" = "You don't even need to include a namespace!"
		))

		return(invisible(TRUE))
	}

	deps <- desc::desc_get_deps()

	if (!in_deps(pkg, deps = deps)) {
		cli::cli_inform(c(
			"i" = "{.pkg {pkg}} is not a declared dependency.",
			"x" = paste(
				"You can't use {.var {fun_text}},",
				"because your package doesn't depend on {.pkg {pkg}}."
			),
			"*" = 'Use {.run usethis::use_package("{pkg}")} to add it as a dependency.'
		))

		return(invisible(FALSE))
	}

	if (in_suggests(pkg, deps = deps)) {
		cli::cli_inform(c(
			"i" = "{.pkg {pkg}} is a suggested dependency.",
			"!" = paste(
				"You can use {.var {fun_text}} {.emph carefully},",
				"because your package suggests {.pkg {pkg}}."
			),
			"*" = paste(
				'In your package code, use {.code rlang::is_installed("{pkg}")}',
				'or {.code rlang::check_installed("{pkg}")} to test if {.pkg {pkg}} is installed.'
			),
			"*" = "Then refer to it with {.var {namespaced_fun}}."
		))

		return(invisible(TRUE))
	}

	if (is_imported(bare_fun, from = pkg)) {
		cli::cli_inform(c(
			"i" = "{.pkg {pkg}} is a declared dependency.",
			"v" = "You can use {.var {fun_text}}, because your package depends on {.pkg {pkg}}.",
			"v" = "You don't even need to include a namespace, because you used {.code importFrom}!"
		))

		return(invisible(TRUE))
	}

	cli::cli_inform(c(
		"i" = "{.pkg {pkg}} is a declared dependency.",
		"v" = "You can use {.var {fun_text}}, because your package depends on {.pkg {pkg}}.",
		"*" = "In your package code, refer to it with {.var {namespaced_fun}}."
	))

	invisible(TRUE)
}

unquote <- function(x) {
	gsub("[\"'`]", "", x, perl = TRUE)
}

cli_theme_caniuse <- function() {
	list(
		span.fun = list(color = "blue"),
		span.run = list(transform = function(x) {
			x <- cli::builtin_theme()$span.run$transform(x)
			cli::builtin_theme()$span.code$transform(x)
		}),
		span.var = list(color = "blue")
	)
}
