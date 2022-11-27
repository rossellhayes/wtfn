dependency_type <- function(pkg, description = desc::description$new()) {
	deps <- description$normalize()$get_deps()
	deps[deps$package == pkg, "type"][1]
}
