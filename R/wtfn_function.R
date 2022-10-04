wtfn_function <- R6Class(
	"wtfn_function",
	public = list(
		name = NULL,
		dev_context = NULL,

		initialize = function(fun = NULL, dev_context = NULL) {
			self$name <- unquote(
				rlang::expr_text(rlang::quo_get_expr(rlang::enquo(fun)))
			)

			if (is.character(fun)) {
				self$name <- fun
			}

			if (is.function(fun)) {
				self$fun <- fun
			}

			self$dev_context <- dev_context

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
			fun <- purrr::possibly(eval, otherwise = NULL)(rlang::parse_expr(fun))

			private$fun_holder <- fun
			private$fun_holder
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

			# If `fun` is imported using `importFrom()`, use that package
			if (
				!is.null(self$dev_context) &&
				self$bare_name %in% self$dev_context$imports$fun
			) {
				private$pkg_holder <- self$dev_context$imports[[
					match(self$bare_name, self$dev_context$imports$fun),
					"pkg"
				]]
				return(private$pkg_holder)
			}

			if (!is.null(self$fun)) {
				if (!self$is_closure) {
					# Normal functions (e.g. `mean`) and infix functions (e.g. `%in%`)
					# have class `function` and type `closure`.
					# Special operators (e.g. `+` or `<-`) also have class `function`,
					# but type `builtin` or `special`.
					# Special operators can't be exported from packages,
					# so if a function is not a `closure`, it must be from `base`.
					return("base")
				}

				# If `fun` can be evaluated with the current search path, use that package
				env <- environment(self$fun)

				if (rlang::is_namespace(env)) {
					private$pkg_holder <- environmentName(env)
					return(private$pkg_holder)
				}
			}

			# Find packages that have a help file for the function
			help_packages <- utils::help.search(
				paste0("^\\Q", self$bare_name, "\\E$"),
				fields = "alias",
				ignore.case = FALSE
			)$matches$Package

			if (length(help_packages) < 1) {
				cli::cli_abort(c(
					"x" = "{self$cli_name} could not be found in any installed packages.",
					"!" = "Do you need to install the package containing it?"
				))
			}

			# Reorder search results so packages that are also in `$dev_context$deps`
			# appear first
			help_packages <- help_packages[
				order(match(help_packages, self$dev_context$deps$package))
			]

			private$pkg_holder <- help_packages[1]
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
