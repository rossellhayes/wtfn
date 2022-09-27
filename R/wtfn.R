wtfn <- function(fun, fun_text = NULL) {
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
			"i" = "{.strong {.pkg {pkg}} is the current package.}",
			"v" = "You can use {.var {fun_text}}. You don't even need to include a namespace!"
		))

		return(invisible(TRUE))
	}

	if (identical(pkg, "base")) {
		cli::cli_inform(c(
			"i" = "{.strong {.pkg base} functions can be used in all R packages.}",
			"v" = "You can use {.var {fun_text}}. You don't even need to include a namespace!"
		))

		return(invisible(TRUE))
	}

	dependency_type <- dependency_type(pkg)

	if (is.na(dependency_type)) {
		cli::cli_inform(c(
			"i" = "{.strong {.pkg {pkg}} is not a declared dependency.}",
			"x" = paste(
				"You can't use {.var {fun_text}},",
				"because your package doesn't depend on {.pkg {pkg}}."
			),
			"*" = 'Use {.run usethis::use_package("{pkg}")} to add it as a dependency.'
		))

		return(invisible(FALSE))
	}

	cli::cli_inform(
		c("i" = "{.strong {.pkg {pkg}} is declared in {.val {dependency_type}}.}")
	)

	if (dependency_type %in% c("Imports", "Depends")) {
		if (is_imported(bare_fun, from = pkg)) {
			cli::cli_inform(c(
				"i" = "{.strong {.var {fun_text}} is imported from {.pkg {pkg}} using {.var importFrom}.}",
				"v" = "You can use {.var {fun_text}}. You don't even need to include a namespace!"
			))

			return(invisible(TRUE))
		}

		cli::cli_inform(c(
			"v" = "You can use {.var {fun_text}}.",
			"*" = "In your package code, refer to it with {.var {namespaced_fun}}."
		))

		return(invisible(TRUE))
	}

	# If we reach this point,
	# `dependency_type` is "Suggests", "Enhances", "LinkingTo" or something weird
	cli::cli_inform(c(
		"!" = "You can use {.var {fun_text}} {.emph carefully}.",
		"*" = paste(
			'In your package code, use {.code rlang::is_installed("{pkg}")}',
			'or {.code rlang::check_installed("{pkg}")} to test if {.pkg {pkg}} is installed.'
		),
		"*" = "Then refer to it with {.var {namespaced_fun}}."
	))

	return(invisible(TRUE))
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
