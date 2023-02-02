get_namespace_imports <- function(file = ".") {
	namespace_file <- fs::path(file, "NAMESPACE")
	if (!fs::file_exists(namespace_file)) return(empty_imports())

	namespace <- readLines(namespace_file)
	namespace_exprs <- rlang::parse_exprs(namespace)
	importFrom_exprs <- purrr::keep(
		namespace_exprs,
		function(x) {
			identical(x[[1]], rlang::sym("importFrom"))
		}
	)

	dplyr::bind_rows(
		purrr::map(
			importFrom_exprs,
			function(x) {
				x <- as.character(x)
				x <- x[-1] # Remove function, leaving only arguments
				names(x) <- c("pkg", "fun")
				x
			}
		)
	)
}

empty_imports <- function() {
	dplyr::tibble(pkg = character(0), fun = character(0))
}
