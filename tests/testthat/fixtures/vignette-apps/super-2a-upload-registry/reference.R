# Doc-only reference (ADR 0013 D3). Not parsed by tests.
# The all-defaults equivalent of super-2a-upload-registry/app.R with every
# placeholder replaced by its seeded default. df_main and df_aux are the
# uploaded datasets (companion-name args carried through ppUpload).
df_main |>
  dplyr::filter(hp >= 75) |>
  dplyr::mutate(adj = mpg / wt) |>
  ggplot(aes(x = mpg, y = adj, color = cyl)) +
  geom_point(aes(group = cyl), size = 2, alpha = 0.7) +
  geom_smooth(data = df_aux, aes(x = mpg, y = wt), method = "lm") +
  facet_wrap(vars(cyl)) +
  labs(title = "Title", subtitle = "")
