dependency_type <- function(pkg) {
	deps <- desc::desc_get_deps()

	# Reorder dependencies in order of highest to lowest dependency.
	# If, for some reason, a package appears more than once in `DESCRIPTION`,
	# this ensures we will return its highest level of dependency.
	type_order <- c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo")
	deps <- deps[order(match(deps$type, type_order)), ]

	deps[deps$package == pkg, ]$type[[1]]
}
