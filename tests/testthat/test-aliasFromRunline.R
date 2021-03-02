test_that("core concept works", {
  line = "#BioModule path.to.class.for.Dostuff AS MyStuff"
  expect_equivalent( aliasFromRunline( line ) , "MyStuff")
  
  line2 = "#BioModule path.to.class.for.Dostuff"
  expect_equivalent( aliasFromRunline( line2 ) , "Dostuff")
})
