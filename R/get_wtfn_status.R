get_wtfn_status <- function(fun, description, namespace_imports) {
	cli::cli_div(theme = cli_theme_wtfn())

	message <- c("i" = "{fun$cli_name} is from {fun$cli_pkg}.")

	if (identical(fun$pkg, description$get_field("Package"))) {
		headline <- c("v" = "You can use {fun$cli_bare_name}.")

		message <- c(message, "i" = "{fun$cli_pkg} is the current package.")

		return(list(headline = headline, message = message, can_use = TRUE))
	}

	if (identical(fun$pkg, "base")) {
		headline <- c("v" = "You can use {fun$cli_bare_name}.")

		message <- c(
			message,
			"i" = "{fun$cli_pkg} functions can be used in all R packages."
		)

		return(list(headline = headline, message = message, can_use = TRUE))
	}

	dependency_type <- dependency_type(fun$pkg, description)

	if (is.na(dependency_type)) {
		headline <- c(
			"x" = paste(
				"You can't use {fun$cli_name},",
				"because your package doesn't depend on {fun$cli_pkg}."
			)
		)

		message <- c(
			message,
			"i" = "{fun$cli_pkg} is not a declared dependency.",
			"*" = 'Use {.run usethis::use_package("{fun$pkg}")} to import it as a dependency.',
			"*" = 'Or use {.run usethis::use_package("{fun$pkg}", "Suggests")} to suggest it
			  as a dependency.'
		)

		return(list(headline = headline, message = message, can_use = FALSE))
	}

	message <- c(
		message,
		"i" = paste0(
			cli::format_inline("{fun$cli_pkg} is declared in "),
			get_desc_declaration(fun$pkg, dependency_type), "."
		)
	)

	if (dependency_type %in% c("Imports", "Depends")) {
		if (is_imported(fun, namespace_imports)) {
			headline <- c("v" = "You can use {fun$cli_bare_name}.")

			message <- c(
				message,
				"i" = "{fun$cli_name} is imported from {fun$cli_pkg} using {.var importFrom}."
			)

			return(list(headline = headline, message = message, can_use = TRUE))
		}

		headline <- c("v" = "You can use {fun$cli_namespaced_name}.")

		if (fun$is_infix) {
			message <- c(
				message,
				"*" = paste(
					"For ease of use, consider importing it into your package with",
					'{.run usethis::import_from("{fun$pkg}", "{fun$bare_name}")}.'
				),
				"*" = "Then refer to it with {fun$cli_bare_name}."
			)

			return(list(headline = headline, message = message, can_use = TRUE))
		}

		message <- c(
			message,
			"*" = "In your package code, refer to it with {fun$cli_namespaced_name}."
		)

		return(list(headline = headline, message = message, can_use = TRUE))
	}

	# If we reach this point,
	# `dependency_type` is "Suggests", "Enhances", "LinkingTo" or something weird
	headline <- c(
		"!" = "You can use {fun$cli_namespaced_name} {.emph carefully}."
	)

	message <- c(
		message,
		"*" = paste(
			'In your package code, use {.code rlang::is_installed("{fun$pkg}")}',
			'or {.code rlang::check_installed("{fun$pkg}")}',
			'to test if {fun$cli_pkg} is installed.'
		),
		"*" = "Then refer to it with {fun$cli_namespaced_name}."
	)

	list(headline = headline, message = message, can_use = TRUE)
}

# @staticimports pkg:stringstatic
#   str_detect str_which

get_desc_declaration <- function(package, dependency_type) {
	desc_file <- fs::path(
		rprojroot::find_root(rprojroot::is_r_package, "."),
		"DESCRIPTION"
	)

	desc_lines <- readLines(desc_file)

	dependency_type_line <- str_which(
		desc_lines, sprintf("^%s:", dependency_type)
	)[[1]]

	package_line <- dependency_type_line + str_which(
		desc_lines[seq.int(dependency_type_line + 1, length(desc_lines))],
		sprintf("^\\s*%s,?\\s*", package)
	)

	cli::cli_div(theme = cli_theme_wtfn())

	cli::style_hyperlink(
		cli::col_blue(dependency_type),
		paste0("file://", desc_file),
		list(line = package_line)
	)
}
