test_that("backports messages", {
	withr::local_dir("test-pkgs")
	withr::local_file(list(
		"test.pkg.b" = dir.create("test.pkg.b"),
		"test.pkg.b/DESCRIPTION" = writeLines("", "test.pkg.b/DESCRIPTION")
	))
	withr::local_dir("test.pkg.b")

	writeLines("Package: test.pkg.b", "DESCRIPTION")
	expect_true(
		!is.null(
			generate_backports_message(
				wtfn_function$new("stopifnot", wtfn_dev_context$new())
			)
		)
	)

	writeLines(c("Depends:", "    R (>= 2.10)"), "DESCRIPTION")
	expect_true(
		!is.null(
			generate_backports_message(
				wtfn_function$new("stopifnot", wtfn_dev_context$new())
			)
		)
	)

	file.remove("DESCRIPTION")
	writeLines(
		c("Package: test.pkg.b", "Depends:", "    R (>= 4.0.0)"),
		"DESCRIPTION"
	)
	expect_null(
		generate_backports_message(
			wtfn_function$new("stopifnot", wtfn_dev_context$new())
		)
	)
})
