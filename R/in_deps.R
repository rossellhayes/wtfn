in_deps <- function(package, file = ".", deps = NULL) {
	if (identical(package, "base")) {
		return(TRUE)
	}

	deps <- deps %||% desc::desc_get_deps(file)
	package %in% deps$package
}

in_suggests <- function(package, file = ".", deps = NULL) {
	deps <- deps %||% desc::desc_get_deps(file)
	deps <- deps[deps$type == "Suggests", ]
	package %in% deps$package
}
