get_wtfn_status <- function(fun, description, namespace_imports) {
	cli::cli_div(theme = cli_theme_wtfn())

	message <- c("i" = "{.strong {fun$cli_name} is from {fun$cli_pkg}.}")

	if (identical(fun$pkg, description$get_field("Package"))) {
		message <- c(
			message,
			"i" = "{.strong {fun$cli_pkg} is the current package.}",
			"v" = "You can use {fun$cli_name}."
		)

		return(list(message = message, can_use = TRUE))
	}

	if (identical(fun$pkg, "base")) {
		message <- c(
			message,
			"i" = "{.strong {fun$cli_pkg} functions can be used in all R packages.}",
			"v" = "You can use {fun$cli_name}."
		)

		return(list(message = message, can_use = TRUE))
	}

	dependency_type <- dependency_type(fun$pkg, description)

	if (is.na(dependency_type)) {
		message <- c(
			message,
			"i" = "{.strong {fun$cli_pkg} is not a declared dependency.}",
			"x" = paste(
				"You can't use {fun$cli_name},",
				"because your package doesn't depend on {fun$cli_pkg}."
			),
			"*" = 'Use {.run usethis::use_package("{fun$pkg}")} to add it as a dependency.'
		)

		return(list(message = message, can_use = FALSE))
	}

	message <- c(
		message,
		"i" = cli::format_inline(
			"{.strong {fun$cli_pkg} is declared in {.val {dependency_type}}.}"
		)
	)

	if (dependency_type %in% c("Imports", "Depends")) {
		if (is_imported(fun, namespace_imports)) {
			message <- c(
				message,
				"i" = "{.strong {fun$cli_name} is imported from {fun$cli_pkg} using {.var importFrom}.}",
				"v" = "You can use {fun$cli_name}."
			)

			return(list(message = message, can_use = TRUE))
		}

		message <- c(message, "v" = "You can use {fun$cli_name}.")

		if (fun$is_infix) {
			message <- c(
				message,
				"*" = paste(
					"For ease of use, consider importing it into your package with",
					'{.run usethis::import_from("{fun$pkg}", "{fun$bare_name}")}.'
				),
				"*" = "Then refer to it with {fun$cli_bare_name}."
			)

			return(list(message = message, can_use = TRUE))
		}

		message <- c(
			message,
			"*" = "In your package code, refer to it with {fun$cli_namespaced_name}."
		)

		return(list(message = message, can_use = TRUE))
	}

	# If we reach this point,
	# `dependency_type` is "Suggests", "Enhances", "LinkingTo" or something weird
	message <- c(
		message,
		"!" = "You can use {fun$cli_name} {.emph carefully}.",
		"*" = paste(
			'In your package code, use {.code rlang::is_installed("{fun$pkg}")}',
			'or {.code rlang::check_installed("{fun$pkg}")}',
			'to test if {fun$cli_pkg} is installed.'
		),
		"*" = "Then refer to it with {fun$cli_namespaced_name}."
	)

	list(message = message, can_use = TRUE)
}
