is_imported <- function(fun, namespace_imports) {
	nrow(
		namespace_imports[
			namespace_imports$pkg == fun$pkg &
			namespace_imports$fun == fun$bare_name,
		]
	) > 0
}
