test_that("makes good url", {
    expected = "https://biolockj-dev-team.github.io/BioLockJ/GENERATED/biolockj.module.diy/GenMod/"
    url = module_userguide_url("biolockj.module.diy.GenMod")
    expect_equivalent(url, expected)
})
