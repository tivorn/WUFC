library(ggplot2)

perg_df <- readr::read_csv(here::here("data/perg_data.csv"))

perg_left_eye <- perg_df |>
  ggplot(aes(x=time, y=PERG_left_eye, color=diagnosis)) +
  geom_line(alpha=.25, aes( group=id_record)) +
  stat_smooth(aes(group=1), se=FALSE, size=1.5) +
  facet_wrap(~diagnosis) +
  labs(x="Time", y="Amplitude (Left Eye)") +
  theme_bw() +
  theme(legend.position="none")

perg_right_eye <- perg_df |>
  ggplot(aes(x=time, y=PERG_right_eye, color=diagnosis)) +
  geom_line(alpha=.25, aes( group=id_record)) +
  stat_smooth(aes(group=1), se=FALSE, size=1.5) +
  facet_wrap(~diagnosis) +
  labs(x="Time", y="Amplitude (Right Eye)") +
  theme_bw() +
  theme(legend.position="none")

cowplot::plot_grid(perg_left_eye, perg_right_eye, ncol=1)

ggsave(here::here("docs", "report", "perg_plot.png"))
