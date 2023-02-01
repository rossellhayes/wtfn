wtfn_addin <- function() {
	wtfn(
		function_under_cursor(),
		verbose = getOption(
			"wtfn.verbose",
			default = !cli::ansi_has_hyperlink_support()
		)
	)
}

wtfn_addin_short <- function() {
	wtfn(function_under_cursor(), verbose = FALSE)
}

wtfn_addin_verbose <- function() {
	wtfn(function_under_cursor(), verbose = TRUE)
}

function_under_cursor <- function() {
	rlang::check_installed("rstudioapi", "to use the wtfn addin.")

	code <- rstudioapi::selectionGet()$value
	if (length(code) > 0 && any(nzchar(code))) return(code)

	# If nothing is selected, get code from current active RStudio context
	context <- rstudioapi::getActiveDocumentContext()

	cursor <- context$selection[[1]]$range$start

	# `getActiveDocumentContext()` and `getParseData()` handle tabs differently.
	# `getActiveDocumentContext()` treats a tab as one character,
	# but `getParseData()` treats a tab as eight characters.
	# That means if `context$contents` contains any tabs,
	# the position of the cursor in `getActiveDocumentContext()` won't match
	# the position of the expressions in `getParseData()`.
	# To fix this, we convert each tab to a single space,
	# which both functions count as one character.
	code <- gsub("\t", " ", context$contents, fixed = TRUE)

	parse_data <- parse(text = code, keep.source = TRUE)
	parse_data <- utils::getParseData(parse_data)

	found_symbol <- parse_data[
		parse_data$line1 == cursor["row"] &
			parse_data$col1 <= cursor["column"] &
			parse_data$col2 >= cursor["column"] &
			# An "expr" token is a container that holds other tokens.
			# We want the most specific token (probably a "SYMBOL_FUNCTION_CALL"),
			# not any exprs that contain that token.
			parse_data$token != "expr",
	]

	if (nrow(found_symbol) == 0) {
		cli::cli_div(theme = cli_theme_wtfn)
		cli::cli_inform(c("?" = "What's the function?"))
		code <- readline("> ")
		return(code)
	}

	# Found symbol may not contain the entire expression under the cursor.
	# For example, a namespaced function contains three symbols:
	# `namespace`, `::` and `function`.
	# It wouldn't be helpful if we returned "::"
	# when the cursor was over the colons in a namespaced function.
	# Instead, we return all symbols that share `found_symbol`'s parent expression.
	found_symbol_parent <- found_symbol[["parent"]]
	found_expr <- parse_data[parse_data$parent == found_symbol_parent, ]

	# Don't include `expr`s that share a parent expression
	# (This would include things like arguments to infixes)
	found_expr <- found_expr[found_expr$token != "expr", ]

	# Using the positions of the found expressions in the parse data,
	# extract the function under the cursor from `code`.
	found_start <- found_expr[found_expr$line1 == min(found_expr$line1), ]
	found_start <- found_start[found_start$col1 == min(found_start$col1), ]

	found_end <- found_expr[found_expr$line2 == max(found_expr$line2), ]
	found_end <- found_end[found_end$col2 == max(found_end$col2), ]

	code <- line_sub(
		code,
		line_start = found_start$line1,
		col_start  = found_start$col1,
		line_end   = found_end$line2,
		col_end    = found_end$col2
	)

	if (length(code) == 1) {
		return(code)
	}

	paste(code, collapse = "\n")
}

line_sub <- function(x, line_start, col_start, line_end, col_end) {
	if (line_start == line_end) {
		return(substr(x[[line_start]], col_start, col_end))
	}

	x <- x[seq.int(line_start, line_end)]
	x[[1]] <- substr(x, col_start, .Machine$integer.max)
	x[[length(x)]] <- substr(x, 0, col_end)
	x
}
