library(backports)
library(dplyr)
library(purrr)
library(stringr)

base_packages <- row.names(installed.packages(priority = "base"))

backports <- utils::help.search(".", package = "backports", types = "help") %>%
	purrr::pluck("matches") %>%
	dplyr::filter(Field == "alias") %>%
	dplyr::select(fun = Entry, title = Title) %>%
	dplyr::filter(str_detect(title, fixed("Backport of"))) %>%
	dplyr::mutate(
		version = str_extract(title, "R( versions)? < \\d\\.\\d\\.\\d") %>%
			str_extract("\\d\\.\\d\\.\\d") %>%
			as.numeric_version()
	) %>%
	dplyr::rowwise() %>%
	dplyr::mutate(
		package = utils::help.search(
			pattern = paste0("^\\Q", fun, "\\E$"),
			package = base_packages,
			fields = "alias",
			agrep = FALSE
		) %>%
			purrr::pluck("matches") %>%
			dplyr::pull(Package)
	) %>%
	dplyr::select(-title) %>%
	dplyr::ungroup()

usethis::use_data(backports, internal = TRUE, overwrite = TRUE)
