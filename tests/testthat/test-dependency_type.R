test_that("dependency_type() picks highest level of dependency", {
  withr::local_dir("test-pkgs/test.pkg.a")
	expect_equal(dependency_type("cli"), "Imports")
})
