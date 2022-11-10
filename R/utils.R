get_package_minimum_r_version <- function() {
	deps <- desc::desc_get_deps()
	r_version <- deps[deps$package == "R", "version"]
	if (length(r_version) == 0) return(as.numeric_version(0))
	r_version <- sub("^.*?(\\d+([.-]\\d+){0,2}).*?$", "\\1", r_version)
	as.numeric_version(r_version)
}
