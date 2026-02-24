test_that("generation of a globally optimal Chu-Liu arborescence is stable", {
  obs_group_df <- readr::read_tsv(
    system.file("extdata", "external_variables_df.tsv", package = "SH.misc")
  )
  value_df <- readr::read_tsv(
    system.file("extdata", "Sahul_structure_wide.tsv", package = "SH.misc")
  )
  dep_matrix <- dependency_matrix_cond_MI(value_df, obs_group_df)
  CLarbors <- CLarbor_from_dep_matrix_check_all_roots(dep_matrix, return.all = T)
  best_arbor <- CLarbors$best.arbor
  expect_length(CLarbors$all.arbors$tree, ncol(dep_matrix))
  expect_length(CLarbors$all.arbors$redundant, ncol(dep_matrix))
  expect_equal(ecount(CLarbors$best.arbor), ncol(dep_matrix) - 1)
  expect_equal(max(unlist(CLarbors$all.arbors$redundant)), 25.3886447)
})
