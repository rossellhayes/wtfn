generate_backports_message <- function(fun) {
	minimum_r_version <- get_package_minimum_r_version()
	backport <- backports[
		backports$fun == fun$bare_name &
			backports$package == fun$pkg &
			backports$version > minimum_r_version,
	]

	if (nrow(backport) > 0) {
		c(
			"!" = paste(
				"The current implementation of {fun$cli_name}",
				"was introduced in R {.val {backport$version}}."
			),
			"*" = paste(
				"For compatibility with R < {.val {backport$version}},",
				"consider using {.help [{.pkg backports}](backports::import)}."
			)
		)
	}
}
