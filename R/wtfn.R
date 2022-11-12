wtfn <- function(fun) {
	description <- desc::description$new()$normalize()
	namespace_imports <- get_namespace_imports()
	fun <- wtfn_function$new({{fun}}, description, namespace_imports)

	backports_message <- generate_backports_message(fun, description)
	wtfn_status <- get_wtfn_status(fun, description, namespace_imports)

	cli::cli_div(theme = cli_theme_wtfn())
	cli::cli_inform(wtfn_status$message)
	if (length(backports_message) > 0) cli::cli_inform(backports_message)
	invisible(wtfn_status$can_use)
}

unquote <- function(x) {
	gsub("[\"']", "", x, perl = TRUE)
}

cli_theme_wtfn <- function() {
	list(
		span.blue = list(color = "blue"),
		span.run = list(transform = function(x) {
			x <- cli::builtin_theme()$span.run$transform(x)
			cli::builtin_theme()$span.code$transform(x)
		}),
		span.var = list(color = "blue")
	)
}
