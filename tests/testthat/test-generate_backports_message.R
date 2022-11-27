test_that("backports messages", {
	desc <- desc::description$new("!new")
	desc$set(Package = "test.pkg")
	expect_vector(
		generate_backports_message(wtfn_function$new("stopifnot", desc), desc),
		ptype = character(0),
		size = 2
	)

	desc$set(Depends = "R (>= 2.10)")
	expect_vector(
		generate_backports_message(wtfn_function$new("stopifnot", desc), desc),
		ptype = character(0),
		size = 2
	)

	desc$set(Depends = "R (>= 4.0)")
	expect_null(
		generate_backports_message(wtfn_function$new("stopifnot", desc), desc)
	)
})
