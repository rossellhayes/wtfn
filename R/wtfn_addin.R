wtfn_addin <- function() {
	wtfn(function_under_cursor())
}

function_under_cursor <- function() {
	rlang::check_installed("rstudioapi", "to use the caniusethis addin.")

	code <- rstudioapi::selectionGet()$value

	# If nothing is selected, get code from current active RStudio context
	if (length(code) > 0 && any(code != "")) {
		return(code)
	}

	context <- rstudioapi::getActiveDocumentContext()
	cursor <- context$selection[[1]]$range$start
	cursor <- adjust_for_tabs(cursor, context)

	parse <- parse(text = context$contents, keep.source = TRUE)
	parse_data <- utils::getParseData(parse)

	found_symbol <- parse_data[
		parse_data$line1 == cursor["row"] &
			parse_data$col1 <= cursor["column"] &
			parse_data$col2 >= cursor["column"] &
			parse_data$token != "expr",
	]

	found_symbol_parent <- found_symbol[["parent"]]
	found_expr <- parse_data[parse_data$parent == found_symbol_parent, ]
	code <- paste(found_expr[["text"]], collapse = "")
	code
}

adjust_for_tabs <- function(cursor, context) {
	# `rstudioapi::getActiveDocumentContext()` treats a tab as one character,
	# but `utils::getParseData()` treats a tab as eight characters.
	# To find the symbol under the cursor, we must adjust the cursor position
	# to account for the different way tabs are counted.

	text_on_cursor_line <- context$contents[cursor["row"]]
	text_before_cursor <- substr(text_on_cursor_line, 1, cursor["column"] - 1)

	tabs_before_cursor <- unlist(gregexpr("\t", text_before_cursor, fixed = TRUE))
	# If there are no matches, `gregexpr()` returns -1, which we want to exclude
	tabs_before_cursor <- tabs_before_cursor[tabs_before_cursor > 0]
	tabs_before_cursor <- length(tabs_before_cursor)

	if (tabs_before_cursor == 0) {
		return(cursor)
	}

	cursor["column"] <- cursor["column"] + 7 * tabs_before_cursor
	cursor
}

