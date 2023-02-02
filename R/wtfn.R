#' Where's this function?
#'
#' Prints information about the package containing a function and whether
#' you can use that function given the current dependencies of your package.
#'
#' @param fun A function, as either a character string or a symbol
#' @param verbose A [logical].
#'   If `verbose` is [`TRUE`], `wtfn()` prints all information about `fun`.
#'   If `verbose` is [`FALSE`], `wtfn()` prints a single line indicating if you
#'   can use `fun` in your package given its current dependencies.
#'   Defaults to [`TRUE`].
#'
#' @return Invisibly returns [`TRUE`] if you can use `fun` in your package
#'   given your current dependencies and [`FALSE`] if you cannot.
#' @export
#'
#' @examples
#' wtfn("paste")
#' wtfn("wtfn::wtfn")
wtfn <- function(fun, verbose = TRUE) {
	description <- desc::description$new()$normalize()
	namespace_imports <- get_namespace_imports()
	fun <- wtfn_function$new({{fun}}, description, namespace_imports)

	backports_message <- generate_backports_message(fun, description)
	wtfn_status <- get_wtfn_status(fun, description, namespace_imports)
	wtfn_status$headline <- paste0_preserve_names(
		"{.strong ", wtfn_status$headline, "}"
	)

	cli::cli_div(theme = cli_theme_wtfn())

	if (!verbose && cli::ansi_has_hyperlink_support()) {
		cli::cli_div(theme = list(
			span.run = list(transform = cli::builtin_theme()$span.run$transform)
		))
		wtfn_status$headline <- paste0_preserve_names(
			wtfn_status$headline, " ",
			'[{.run [more](wtfn::wtfn("', fun$name, '", verbose = TRUE))}]'
		)
	}

	cli::cli_inform(wtfn_status$headline)

	if (verbose) {
		cli::cli_inform(wtfn_status$message)
		if (length(backports_message) > 0) cli::cli_inform(backports_message)
	}

	invisible(wtfn_status$can_use)
}

paste0_preserve_names <- function(...) {
	res <- paste0(...)
	names(res) <- purrr::compact(purrr::map(list(...), names))[[1]]
	res
}

unquote <- function(x) {
	gsub("[\"']", "", x, perl = TRUE)
}

cli_theme_wtfn <- function() {
	list(
		span.href = list(color = "blue"),
		span.run = list(
			color = "blue",
			transform = function(x) {
				if (cli::ansi_has_hyperlink_support()) {
					x <- cli::builtin_theme()$span.run$transform(x)
				}
				cli::builtin_theme()$span.code$transform(x)
			}
		),
		span.var = list(
			color = "blue",
			transform = function(x) {
				if (cli::ansi_grepl("^`.*`$", x)) return(x)
				if (cli::ansi_grepl("`", x, fixed = TRUE)) return(paste0("`` ", x, " ``"))
				paste0("`", x, "`")
			}
		),
		`.bullets .bullet-?` = list(
			`text-exdent` = 2,
			before = function(x) {
				paste0(cli::col_cyan(cli::style_bold("?")), " ")
			}
		)
	)
}
