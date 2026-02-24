test_that("generation of a Chu-Liu arborescence with known root is stable", {
  obs_group_df <- readr::read_tsv(
    system.file("extdata", "external_variables_df.tsv", package = "SH.misc")
  )
  value_df <- readr::read_tsv(
    system.file("extdata", "Sahul_structure_wide.tsv", package = "SH.misc")
  )
  dep_matrix <- dependency_matrix_cond_MI(value_df, obs_group_df)
  CLarbor <- CLarbor_from_dep_matrix_known_root(dep_matrix, 1)
  expect_true(is_tree(CLarbor$tree))
  expect_equal(ecount(CLarbor$tree), ncol(dep_matrix) - 1)
  expect_equal(CLarbor$redundant, 24.4919129)
})
