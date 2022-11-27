get_namespace_imports <- function(file = ".") {
	namespace_file <- fs::path(file, "NAMESPACE")
	if (!fs::file_exists(namespace_file)) return(empty_imports())

	namespace <- readLines(namespace_file)
	namespace_exprs <- rlang::parse_exprs(namespace)
	importFrom_exprs <- purrr::keep(
		namespace_exprs,
		function(x) identical(x[[1]], rlang::sym("importFrom"))
	)

	purrr::map_dfr(
		importFrom_exprs,
		function(x) c(pkg = as.character(x[[2]]), fun = as.character(x[[3]]))
	)
}

empty_imports <- function() {
	dplyr::tibble(pkg = character(0), fun = character(0))
}
