test_that("gets correct name", {
    line = "#BioModule path.to.class.for.Dostuff AS MyStuff"
    expect_equivalent( classFromRunline( line ), "path.to.class.for.Dostuff")
    
    line2 = "#BioModule path.to.class.for.Dostuff"
    expect_equivalent( classFromRunline( line2 ) , "path.to.class.for.Dostuff")
})
