test_that("wtfn$pkg() picks the package that has `importFrom()`", {
	withr::local_dir("test-pkgs/test.pkg.a")
	withr::local_file(list("NAMESPACE" = writeLines("", "NAMESPACE")))

	writeLines("importFrom(dplyr,filter)", "NAMESPACE")
	expect_equal(
		wtfn_function$new("filter", wtfn_dev_context$new())$pkg,
		"dplyr"
	)

	writeLines("importFrom(stats,filter)", "NAMESPACE")
	expect_equal(
		wtfn_function$new("filter", wtfn_dev_context$new())$pkg,
		"stats"
	)
})

test_that("wtfn$pkg() picks the package that has `importFrom()`", {
	withr::local_dir("test-pkgs/test.pkg.a")
	withr::local_file(list("NAMESPACE" = writeLines("", "NAMESPACE")))

	writeLines("importFrom(dplyr,filter)", "NAMESPACE")
	expect_equal(
		wtfn_function$new("filter", wtfn_dev_context$new())$pkg,
		"dplyr"
	)

	writeLines("importFrom(stats,filter)", "NAMESPACE")
	expect_equal(
		wtfn_function$new("filter", wtfn_dev_context$new())$pkg,
		"stats"
	)
})
