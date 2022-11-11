test_that("backports messages", {
	withr::local_dir("test-pkgs")
	withr::local_file(list(
		"test.pkg.b" = dir.create("test.pkg.b"),
		"test.pkg.b/DESCRIPTION" = writeLines("", "test.pkg.b/DESCRIPTION")
	))
	withr::local_dir("test.pkg.b")

	desc <- desc::description$new("!new")
	desc$set(Package = "test.pkg.b")
	desc$write(file = "DESCRIPTION")
	expect_true(
		!is.null(
			generate_backports_message(
				wtfn_function$new("stopifnot", wtfn_dev_context$new())
			)
		)
	)

	desc$set(Depends = "R (>= 2.10)")
	desc$write(file = "DESCRIPTION")
	expect_true(
		!is.null(
			generate_backports_message(
				wtfn_function$new("stopifnot", wtfn_dev_context$new())
			)
		)
	)

	desc$set(Depends = "R (>= 4.0)")
	desc$write(file = "DESCRIPTION")
	expect_null(
		generate_backports_message(
			wtfn_function$new("stopifnot", wtfn_dev_context$new())
		)
	)
})
