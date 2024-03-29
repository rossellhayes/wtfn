# @staticimports pkg:stringstatic
#   str_detect str_extract str_remove str_remove_all

wtfn_function <- R6Class(
	"wtfn_function",
	public = list(
		description = NULL,
		namespace_imports = NULL,

		name = NULL,

		initialize = function(
		  fun = NULL,
		  description = desc::description$new(),
		  namespace_imports = get_namespace_imports()
		) {
			self$name <- unquote(
				rlang::expr_text(rlang::quo_get_expr(rlang::enquo(fun)))
			)

			if (is.character(fun)) {
				self$name <- fun
			}

			if (is.function(fun)) {
				self$fun <- fun
			}

			self$description <- description
			self$namespace_imports <- namespace_imports

			self
		}
	),

	active = list(
		fun = function(value) {
			if (!missing(value)) private$fun_holder <- value
			if (!is.null(private$fun_holder)) return(private$fun_holder)

			fun <- self$syntactic_name
			fun <- purrr::possibly(eval, otherwise = NULL)(rlang::parse_expr(fun))

			private$fun_holder <- fun
			private$fun_holder
		},

		cli_name = function(value) {
			if (!missing(value)) private$cli_name_holder <- value
			if (!is.null(private$cli_name_holder)) return(private$cli_name_holder)

			cli::cli_div(theme = cli_theme_wtfn())

			private$cli_name_holder <- if (self$is_function && !self$is_infix) {
				if (cli::ansi_has_hyperlink_support()) {
					cli::format_inline("{.var {.help [{self$name}]({self$help_topic})}()}")
				} else {
					cli::format_inline("{.var {self$name}()}")
				}
			} else {
				if (cli::ansi_has_hyperlink_support()) {
					# `.var` will not apply it's style to a lone hyperlink (r-lib/cli#541).
					# Adding empty braces resolves this.
					cli::format_inline("{.var {.topic [{self$name}]({self$help_topic})}{}}")
				} else {
					cli::format_inline("{.var {self$name}}")
				}
			}

			private$cli_name_holder
		},

		pkg = function(value) {
			if (!missing(value)) private$pkg_holder <- value
			if (!is.null(private$pkg_holder)) return(private$pkg_holder)

			# If `fun` is namespaced, use that package
			if (!is.na(self$namespace_pkg)) {
				private$pkg_holder <- self$namespace_pkg
				return(private$pkg_holder)
			}

			# If `fun` is imported using `importFrom()`, use that package
			if (
				nrow(self$namespace_imports) > 0 &&
				self$bare_name %in% self$namespace_imports$fun
			) {
				private$pkg_holder <- self$namespace_imports[[
					match(self$bare_name, self$namespace_imports$fun),
					"pkg"
				]]
				return(private$pkg_holder)
			}

			if (!is.null(self$fun)) {
				# If `fun` can be evaluated with the current search path, use that package
				env <- environment(self$fun)

				if (rlang::is_namespace(env)) {
					private$pkg_holder <- environmentName(env)
					return(private$pkg_holder)
				}

				# If you use `environment()` on a special operators (e.g. `+` or `<-`),
				# you get `NULL`.
				# Most special operators come from `base`,
				# so that's a good place to check
				# if `self$fun` exists but `env` isn't an environment.
				if (self$bare_name %in% ls(asNamespace("base"), all.names = TRUE)) {
					private$pkg_holder <- "base"
					return(private$pkg_holder)
				}
			}

			private$pkg_holder <- self$help_page$Package
			private$pkg_holder
		},

		cli_pkg = function(value) {
			if (!missing(value)) private$cli_pkg_holder <- value
			if (!is.null(private$cli_pkg_holder)) return(private$cli_pkg_holder)

			cli::cli_div(theme = cli_theme_wtfn())

			if (!cli::ansi_has_hyperlink_support()) {
				private$cli_pkg_holder <- cli::format_inline("{.pkg {self$pkg}}")
				return(private$cli_pkg_holder)
			}

			pkg_help_pages <- utils::help.search(
				paste0("^\\Q", self$pkg, "-package", "\\E$"),
				fields = "alias", ignore.case = FALSE, package = self$pkg
			)$matches$Topic

			if (length(pkg_help_pages) < 1) {
				private$cli_pkg_holder <- cli::format_inline("{.pkg {self$pkg}}")
				return(private$cli_pkg_holder)
			}

			pkg_help_page <- paste0(self$pkg, "::", pkg_help_pages[[1]])

			private$cli_pkg_holder <-
				cli::format_inline("{.help [{.pkg {self$pkg}}]({pkg_help_page})}")

			private$cli_pkg_holder
		},

		namespace_pkg = function(value) {
			if (!missing(value)) {
				private$namespace_pkg_holder <- value
			}

			if (!is.null(private$namespace_pkg_holder)) {
				return(private$namespace_pkg_holder)
			}

			private$namespace_pkg_holder <- str_extract(
				self$name,
				"[[:alnum:]\\.]+(?=:::?)"
			)
			private$namespace_pkg_holder
		},

		help_page = function(value) {
			if (!missing(value)) private$help_page_holder <- value
			if (!is.null(private$help_page_holder)) return(private$help_page_holder)

			help_pages <- utils::help.search(
				paste0("^\\Q", self$bare_name, "\\E$"),
				fields = "alias", ignore.case = FALSE, types = "help"
			)$matches

			if (nrow(help_pages) < 1) {
				help_pages <- utils::help.search(
					paste0("^\\Q", self$bare_name, "\\E$"),
					fields = "alias", ignore.case = FALSE, types = "help", rebuild = TRUE
				)$matches
			}

			if (nrow(help_pages) < 1) {
				cli::cli_div(theme = cli_theme_wtfn())

				cli::cli_abort(c(
					"x" = "{.var {self$name}} could not be found in any installed packages.",
					"!" = "Do you need to install the package containing it?"
				))
			}

			# Reorder search results
			# - First, prefer packages that the current package depends on
			#   - Excluding backports if backports is a dependency
			# - Then, prefer base packages (e.g. `base`, `tools`, `utils`)
			# - Then, backports if backports is a dependency
			# - Finally, all non-dependency and non-base packages
			help_pages <- help_pages[
				order(
					match(help_pages$Package, setdiff(self$description$get_deps()$package, "backports")),
					match(help_pages$Package, row.names(installed.packages(priority = "base"))),
					match(help_pages$Package, intersect(self$description$get_deps()$package, "backports"))
				),
			]

			if (!is.null(private$pkg_holder)) {
				help_pages <- help_pages[help_pages$Package == private$pkg_holder, ]
			}

			private$help_page_holder <- help_pages[1, ]
			private$help_page_holder
		},

		help_topic = function(value) {
			if (!missing(value)) private$help_topic_holder <- value
			if (!is.null(private$help_topic_holder)) return(private$help_topic_holder)

			private$help_topic_holder <- paste0(self$pkg, "::", self$help_page$Topic)
			private$help_topic_holder
		},

		bare_name = function(value) {
			if (!missing(value)) private$bare_name_holder <- value
			if (!is.null(private$bare_name_holder)) return(private$bare_name_holder)

			name <- self$name
			name <- str_remove(name, "[[:alnum:]\\.]+:::?")
			name <- str_remove_all(name, "^`|`$")
			private$bare_name_holder <- name
			private$bare_name_holder
		},

		cli_bare_name = function(value) {
			if (!missing(value)) private$cli_bare_name_holder <- value
			if (!is.null(private$cli_bare_name_holder)) {
				return(private$cli_bare_name_holder)
			}

			cli::cli_div(theme = cli_theme_wtfn())

			private$cli_bare_name_holder <-
				if (self$is_function && !self$is_infix) {
					if (cli::ansi_has_hyperlink_support()) {
						cli::format_inline(
							"{.var {.help [{self$bare_name}]({self$help_topic})}()}"
						)
					} else {
						cli::format_inline("{.var {self$bare_name}()}")
					}
				} else {
					if (cli::ansi_has_hyperlink_support()) {
						# `.var` will not apply it's style to a lone hyperlink (r-lib/cli#541).
						# Adding empty braces resolves this.
						cli::format_inline(
							"{.var {.topic [{self$bare_name}]({self$help_topic})}{}}"
						)
					} else {
						cli::format_inline("{.var {self$bare_name}}")
					}
				}

			private$cli_bare_name_holder
		},

		syntactic_name = function(value) {
			if (!missing(value)) private$syntactic_name_holder <- value
			if (!is.null(private$syntactic_name_holder)) {
				return(private$syntactic_name_holder)
			}

			name <- self$name
			namespace <- ""

			if (str_detect(name, "[[:alnum:]\\.]+:::?")) {
				namespace <- str_extract(name, "[[:alnum:]\\.]+:::?")
				name <- str_remove(name, "[[:alnum:]\\.]+:::?")
			}

			name <- rlang::expr_text(rlang::sym(name))

			private$syntactic_name_holder <- paste0(namespace, name)

			if (nzchar(namespace)) {
				self$namespaced_name <- private$syntactic_name_holder
			}

			private$syntactic_name_holder
		},

		namespaced_name = function(value) {
			if (!missing(value)) private$namespaced_name_holder <- value
			if (!is.null(private$namespaced_name_holder)) {
				return(private$namespaced_name_holder)
			}

			if (str_detect(self$syntactic_name, "[[:alnum:]\\.]+:::?")) {
				private$namespaced_name_holder <- self$syntactic_name
			} else {
				private$namespaced_name_holder <- paste0(
					self$pkg, "::", self$syntactic_name
				)
			}

			private$namespaced_name_holder
		},

		cli_namespaced_name = function(value) {
			if (!missing(value)) private$cli_namespaced_name_holder <- value
			if (!is.null(private$cli_namespaced_name_holder)) {
				return(private$cli_namespaced_name_holder)
			}

			cli::cli_div(theme = cli_theme_wtfn())

			private$cli_namespaced_name_holder <-
				if (self$is_function && !self$is_infix) {
					if (cli::ansi_has_hyperlink_support()) {
						cli::format_inline(
							"{.var {.help [{self$namespaced_name}]({self$help_topic})}()}"
						)
					} else {
						cli::format_inline("{.var {self$namespaced_name}()}")
					}
				} else {
					if (cli::ansi_has_hyperlink_support()) {
						# `.var` will not apply it's style to a lone hyperlink (r-lib/cli#541).
						# Adding empty braces resolves this.
						cli::format_inline(
							"{.var {.topic [{self$namespaced_name}]({self$help_topic})}{}}"
						)
					} else {
						cli::format_inline("{.var {self$namespaced_name}}")
					}
				}

			private$cli_namespaced_name_holder
		},

		is_function = function() {
			if (!is.null(private$is_function_holder)) {
				return(private$is_function_holder)
			}

			private$is_function_holder <- is.function(self$fun)
			private$is_function_holder
		},

		is_infix = function() {
			if (!is.null(private$is_infix_holder)) return(private$is_infix_holder)

			private$is_infix_holder <-
				!self$is_closure ||
				identical(self$bare_name, ":=") ||
				str_detect(self$name, "%.*%")

			private$is_infix_holder
		},

		is_closure = function() {
			if (!is.null(private$is_closure_holder)) return(private$is_closure_holder)

			private$is_closure_holder <- identical(typeof(self$fun), "closure")
			private$is_closure_holder
		}
	),

	private = list(
		fun_holder = NULL,
		cli_name_holder = NULL,
		pkg_holder = NULL,
		cli_pkg_holder = NULL,
		namespace_pkg_holder = NULL,
		help_page_holder = NULL,
		help_topic_holder = NULL,
		bare_name_holder = NULL,
		cli_bare_name_holder = NULL,
		syntactic_name_holder = NULL,
		namespaced_name_holder = NULL,
		cli_namespaced_name_holder = NULL,
		is_function_holder = NULL,
		is_infix_holder = NULL,
		is_closure_holder = NULL
	)
)
