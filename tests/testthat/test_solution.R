# test_that("copying works", {
# 		cat(getwd(), file="~/Desktop/test_wd.txt")
# 		
# #		fpath <- system.file("extdata", "binary.phy", package="PhyloMethLikelihoodTrees")
# #		cat(fpath, file="~/Desktop/test_fpath.txt")
# 		system(paste("cp ", fpath, " ", "~/Desktop","/test_", input.file, sep=""))
# 		expect_equal(1,2)
# })


test_that("InferMorphologyTree_solution",{
	cat(getwd(), file="~/Desktop/test_wd.txt")
	
	results <- InferMorphologyTree_solution(input.path="../../inst/extdata", output.path=tempdir())
	expect_is(results$parsimony.tree, "phylo")
	expect_is(results$ml.tree, "phylo")
	expect_true(is.null(results$parsimony.tree$edge.length))
	expect_false(is.null(results$ml.tree$edge.length))
})

#test_that("InferDNATreeWithBootstrappingAndPartitions_solution",{
#	results <- InferDNATreeWithBootstrappingAndPartitions_solution(input.path="../../inst/extdata", output.path=temp.dir())
#	expect_is(results$ml.tree, "phylo")
#	expect_false(is.null(results$ml.with.bs.tree$node.label))
#	expect_equals(length(results[[3]]), 100)
#})

