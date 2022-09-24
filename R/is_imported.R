is_imported <- function(fun, from) {
	import_froms <- get_import_froms()

	nrow(import_froms[import_froms$pkg == from & import_froms$fun == fun, ]) > 0
}

get_import_froms <- function(file = ".") {
	if (!identical(fs::path_file(file), "NAMESPACE")) {
		file <- fs::path(file, "NAMESPACE")
	}

	file_text <- readLines(file)
	exprs <- rlang::parse_exprs(file_text)
	importFrom_exprs <- purrr::keep(
		exprs, function(x) identical(x[[1]], rlang::expr(importFrom))
	)

	purrr::map_dfr(
		importFrom_exprs,
		function(x) c(pkg = as.character(x[[2]]), fun = as.character(x[[3]]))
	)
}
