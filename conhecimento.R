prob <- 0.10
# prob <- 0.25
# prob <- 0.75

thresholds <- platform_new %>%
  group_by(platform) %>%
  summarise(
    min_n_line_add = quantile(n_line_add, prob),
    min_n_line_del = quantile(n_line_del, prob),
    min_commits = quantile(commits, prob),
    min_files = quantile(files, prob))

pn2 <- platform_new %>%
  inner_join(thresholds, by="platform") %>%
  filter(n_line_add > min_n_line_add,
         n_line_del > min_n_line_del,
         commits > min_commits,
         files > min_files)