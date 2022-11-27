test_that("dependency_type() picks highest level of dependency", {
	desc <- desc::description$new("!new")
	desc$set(Package = "test.pkg")

	desc$set_deps(tibble::tribble(
		~ type,     ~ package, ~ version,
		"Suggests", "cli",     "*",
		"Imports",  "cli",     "*"
	))
	expect_equal(dependency_type("cli", desc), "Imports")

	desc$set_deps(tibble::tribble(
		~ type,     ~ package, ~ version,
		"Suggests", "cli",     "*",
		"Imports",  "cli",     "*",
		"Depends",  "cli",     "*"
	))
	expect_equal(dependency_type("cli", desc), "Depends")
})
