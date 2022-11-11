wtfn <- function(fun) {
	description <- desc::description$new()$normalize()
	namespace_imports <- get_namespace_imports()
	fun <- wtfn_function$new({{fun}}, description, namespace_imports)

	cli::cli_div(theme = cli_theme_wtfn())
	cli::cli_inform(c("i" = "{.strong {fun$cli_name} is from {fun$cli_pkg}.}"))

	if (identical(fun$pkg, desc::desc_get_field("Package"))) {
		cli::cli_inform(c(
			"i" = "{.strong {fun$cli_pkg} is the current package.}",
			"v" = "You can use {fun$cli_name}. You don't even need to include a namespace!"
		))

		return(invisible(TRUE))
	}

	backports_message <- generate_backports_message(fun, description)

	if (identical(fun$pkg, "base")) {
		cli::cli_inform(c(
			"i" = "{.strong {.pkg base} functions can be used in all R packages.}",
			"v" = "You can use {fun$cli_name}. You don't even need to include a namespace!",
			backports_message
		))

		return(invisible(TRUE))
	}

	dependency_type <- dependency_type(fun$pkg)

	if (is.na(dependency_type)) {
		cli::cli_inform(c(
			"i" = "{.strong {fun$cli_pkg} is not a declared dependency.}",
			"x" = paste(
				"You can't use {fun$cli_name},",
				"because your package doesn't depend on {fun$cli_pkg}."
			),
			"*" = 'Use {.run usethis::use_package("{fun$pkg}")} to add it as a dependency.',
			backports_message
		))

		return(invisible(FALSE))
	}

	cli::cli_inform(
		c("i" = "{.strong {fun$cli_pkg} is declared in {.val {dependency_type}}.}")
	)

	if (dependency_type %in% c("Imports", "Depends")) {
		if (is_imported(fun$bare_name, from = fun$pkg)) {
			cli::cli_inform(c(
				"i" = "{.strong {fun$cli_name} is imported from {fun$cli_pkg} using {.var importFrom}.}",
				"v" = "You can use {fun$cli_name}. You don't even need to include a namespace!",
				backports_message
			))

			return(invisible(TRUE))
		}

		cli::cli_inform(c(
			"v" = "You can use {fun$cli_name}.",
			"*" = "In your package code, refer to it with {fun$cli_namespaced_name}.",
			backports_message
		))

		return(invisible(TRUE))
	}

	# If we reach this point,
	# `dependency_type` is "Suggests", "Enhances", "LinkingTo" or something weird
	cli::cli_inform(c(
		"!" = "You can use {fun$cli_name} {.emph carefully}.",
		"*" = paste(
			'In your package code, use {.code rlang::is_installed("{fun$pkg}")}',
			'or {.code rlang::check_installed("{fun$pkg}")} to test if {fun$cli_pkg} is installed.'
		),
		"*" = "Then refer to it with {fun$cli_namespaced_name}."
	))

	return(invisible(TRUE))
}

unquote <- function(x) {
	gsub("[\"']", "", x, perl = TRUE)
}

cli_theme_wtfn <- function() {
	list(
		span.fun = list(color = "blue"),
		span.run = list(transform = function(x) {
			x <- cli::builtin_theme()$span.run$transform(x)
			cli::builtin_theme()$span.code$transform(x)
		}),
		span.var = list(color = "blue")
	)
}
