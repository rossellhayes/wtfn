test_that("wtfn$pkg() handles reexports", {
	withr::local_dir("test-pkgs/test.pkg.a")
	expect_equal(
		wtfn_function$new("dplyr::tibble")$pkg,
		"dplyr"
	)
})

test_that("wtfn$pkg() picks the package that has `importFrom()`", {
	withr::local_dir("test-pkgs/test.pkg.a")
	withr::local_file(list("NAMESPACE" = writeLines("", "NAMESPACE")))

	writeLines("importFrom(dplyr,filter)", "NAMESPACE")
	expect_equal(
		wtfn_function$new("filter")$pkg,
		"dplyr"
	)

	writeLines("importFrom(stats,filter)", "NAMESPACE")
	expect_equal(
		wtfn_function$new("filter")$pkg,
		"stats"
	)
})
