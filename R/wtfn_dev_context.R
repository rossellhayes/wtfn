wtfn_dev_context <- R6Class(
	"wtfn_dev_context",
	public = list(
		file = NULL,
		pkg = NULL,

		initialize = function(file = ".") {
			self$file <- file
			self$pkg <- desc::desc_get_field("Package", file = self$file)
			self
		}
	),

	active = list(
		deps = function() {
			if (!is.null(private$deps_holder)) {
				return(private$deps_holder)
			}

			deps <- desc::desc_get_deps(file = self$file)
			type_order <- c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo")
			private$deps_holder <- deps[order(match(deps$type, type_order)), ]
			private$deps_holder
		},

		imports = function() {
			if (!is.null(private$imports_holder)) {
				return(private$imports_holder)
			}

			if (is.null(self$namespace)) {
				return(tibble::tibble(pkg = character(0), fun = character(0)))
			}

			namespace_exprs <- rlang::parse_exprs(self$namespace)
			importFrom_exprs <- purrr::keep(
				namespace_exprs,
				function(x) identical(x[[1]], rlang::expr(importFrom))
			)

			private$imports_holder <- purrr::map_dfr(
				importFrom_exprs,
				function(x) c(pkg = as.character(x[[2]]), fun = as.character(x[[3]]))
			)
			private$imports_holder
		},

		namespace = function() {
			if (!is.null(private$namespace_holder)) {
				return(private$namespace_holder)
			}

			namespace_file <- fs::path(self$file, "NAMESPACE")

			if (!fs::file_exists(namespace_file)) {
				return(NULL)
			}

			private$namespace_holder <- readLines(namespace_file)
			private$namespace_holder
		}
	),

	private = list(
		deps_holder = NULL,
		imports_holder = NULL,
		namespace_holder = NULL
	)
)
