dependency_type <- function(pkg, description) {
	deps <- description$get_deps()
	deps[deps$package == pkg, "type"][1]
}
