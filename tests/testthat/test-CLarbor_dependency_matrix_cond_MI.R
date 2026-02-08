test_that("dependency matrix computation is stable", {
  obs_group_df <- readr::read_tsv(
    system.file("extdata", "external_variables_df.tsv", package = "SH.misc")
  )
  value_df <- readr::read_tsv(
    system.file("extdata", "Sahul_structure_wide.tsv", package = "SH.misc")
  )
  expect_snapshot(
    dependency_matrix_cond_MI(value_df)
  )
  expect_snapshot(
    dependency_matrix_cond_MI(value_df, obs_group_df)
  )
})
