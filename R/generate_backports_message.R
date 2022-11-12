generate_backports_message <- function(fun, description) {
	minimum_r_version <- get_package_minimum_r_version(description)

	backport <- backports[
		backports$fun == fun$bare_name &
			backports$package == fun$pkg &
			backports$version > minimum_r_version,
	]

	if (nrow(backport) > 0) {
		backport_version <- backport$version[[1]]

		cli::cli_div(theme = cli_theme_wtfn())

		c(
			"!" = cli::format_inline(
				"The current implementation of {fun$cli_name} ",
				"was introduced in R {.val {backport_version}}."
			),
			"*" = cli::format_inline(
				"For compatibility with R < {.val {backport_version}}, ",
				"consider using {.help [{.pkg backports}](backports::import)}."
			)
		)
	}
}

get_package_minimum_r_version <- function(description) {
	deps <- description$get_deps()
	r_version <- deps[deps$package == "R", "version"]
	if (length(r_version) == 0) return(as.numeric_version(0))
	r_version <- sub("^.*?(\\d+([.-]\\d+){0,2}).*?$", "\\1", r_version)
	r_version <- as.numeric_version(r_version)
	# If there is somehow more than one specified R version, take the largest
	max(r_version)
}
