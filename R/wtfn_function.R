wtfn_function <- R6Class(
	"wtfn_function",
	public = list(
		name = NULL,

		initialize = function(fun = NULL, name = NULL) {
			self$name <- name %||%
				unquote(rlang::expr_text(rlang::quo_get_expr(rlang::enquo(fun))))

			if (is.function(fun)) {
				private$fun_holder <- fun
			} else {
				self$name <- name %||% as.character(fun)
			}

			self
		}
	),

	active = list(
		fun = function(value) {
			if (!missing(value)) {
				private$fun_holder <- value
			}

			if (!is.null(private$fun_holder)) {
				return(private$fun_holder)
			}

			fun <- self$syntactic_name
		},

		cli_name = function(value) {
			if (!missing(value)) {
				private$cli_name_holder <- value
			}

			if (!is.null(private$cli_name_holder)) {
				return(private$cli_name_holder)
			}

			cli::cli_div(theme = cli_theme_caniuse())

			if (self$is_infix) {
				private$cli_name_holder <- cli::format_inline("{.var {self$name}}")
			} else {
				private$cli_name_holder <- cli::format_inline("{.fun {self$name}}")
			}

			private$cli_name_holder
		},

		pkg = function(value) {
			if (!missing(value)) {
				private$pkg_holder <- value
			}

			if (!is.null(private$pkg_holder)) {
				return(private$pkg_holder)
			}

			if (!self$is_closure) {
				# Normal functions (e.g. `mean`) and infix functions (e.g. `%in%`)
				# have class `function` and type `closure`.
				# Special operators (e.g. `+` or `<-`) also have class `function`,
				# but type `builtin` or `special`.
				# Special operators can't be exported from packages,
				# so if a function does not have type `closure`, it must be from `base`.
				return("base")
			}
			private$pkg_holder
		},

		cli_pkg = function(value) {
			if (!missing(value)) {
				private$cli_pkg_holder <- value
			}

			if (!is.null(private$cli_pkg_holder)) {
				return(private$cli_pkg_holder)
			}

			cli::cli_div(theme = cli_theme_caniuse())

			private$cli_pkg_holder <- cli::format_inline("{.pkg {self$pkg}}")
			private$cli_pkg_holder
		},

		bare_name = function(value) {
			if (!missing(value)) {
				private$bare_name_holder <- value
			}

			if (!is.null(private$bare_name_holder)) {
				return(private$bare_name_holder)
			}

			name <- self$name
			name <- sub("[[:alnum:]\\.]+:::?", "", name, perl = TRUE)
			name <- gsub("^`|`$", "", name, perl = TRUE)
			private$bare_name_holder <- name
			private$bare_name_holder
		},

		syntactic_name = function(value) {
			if (!missing(value)) {
				private$syntactic_name_holder <- value
			}

			if (!is.null(private$syntactic_name_holder)) {
				return(private$syntactic_name_holder)
			}

			name <- self$name
			namespace <- ""

			if (grepl("[[:alnum:]\\.]+:::?", name)) {
				namespace <- regmatches(
					name,
					regexpr("[[:alnum:]\\.]+:::?", name)
				)
				name <- sub("[[:alnum:]\\.]+:::?", "", name)
			}

			name <- rlang::expr_text(rlang::sym(name))

			private$syntactic_name_holder <- paste0(namespace, name)

			if (length(namespace) > 0) {
				self$namespaced_name <- private$syntactic_name_holder
			}

			private$syntactic_name_holder
		},

		namespaced_name = function(value) {
			if (!missing(value)) {
				private$namespaced_name_holder <- value
			}

			if (!is.null(private$namespaced_name_holder)) {
				return(private$namespaced_name_holder)
			}

			if (grepl("[[:alnum:]\\.]+:::?", self$syntactic_name)) {
				private$namespaced_name_holder <- self$syntactic_name
			} else {
				private$namespaced_name_holder <- paste0(
					self$pkg, "::", self$syntactic_name
				)
			}

			private$namespaced_name_holder
		},

		cli_namespaced_name = function(value) {
			if (!missing(value)) {
				private$cli_namespaced_name_holder <- value
			}

			if (!is.null(private$cli_namespaced_name_holder)) {
				return(private$cli_namespaced_name_holder)
			}

			cli::cli_div(theme = cli_theme_caniuse())

			if (self$is_infix) {
				private$cli_namespaced_name_holder <-
					cli::format_inline("{.var {self$namespaced_name}}")
			} else {
				private$cli_namespaced_name_holder <-
					cli::format_inline("{.fun {self$namespaced_name}}")
			}

			private$cli_namespaced_name_holder
		},

		is_infix = function() {
			if (!is.null(private$is_infix_holder)) {
				return(private$is_infix_holder)
			}

			private$is_infix_holder <-
				!self$is_closure ||
				grepl("%.*%", self$name, perl = TRUE)

			private$is_infix_holder
		},

		is_closure = function() {
			if (!is.null(private$is_closure_holder)) {
				return(private$is_closure_holder)
			}

			private$is_closure_holder <- identical(typeof(self$fun), "closure")
			private$is_closure_holder
		}
	),

	private = list(
		fun_holder = NULL,
		cli_name_holder = NULL,
		pkg_holder = NULL,
		cli_pkg_holder = NULL,
		bare_name_holder = NULL,
		syntactic_name_holder = NULL,
		namespaced_name_holder = NULL,
		cli_namespaced_name_holder = NULL,
		is_infix_holder = NULL,
		is_closure_holder = NULL
	)
)
