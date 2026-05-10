library(tidyverse)

subsidy <- read_csv("~/Desktop/Subsidy/Gasoline Prices Master - subsidy_final.csv") %>%
  mutate(date = as.Date(paste0(date, "-01")),
         ross_only = ifelse(date <= as.Date("2015-06-01"), bmgap2015adj, NA),
         own_full  = bmgap_own)

ggplot(subsidy) +
  geom_line(aes(x = date, y = own_full, color = "Own calculations (2005–2022)"),
            linetype = "dashed", linewidth = 0.6) +
  geom_line(aes(x = date, y = ross_only, color = "Ross et al. (2017) (2005–2015)"),
            linewidth = 0.7) +
  geom_vline(xintercept = as.Date("2015-07-01"), linetype = "dotted",
             color = "gray40", linewidth = 0.5) +
  annotate("text", x = as.Date("2015-07-01"), y = -0.05,
           label = "Own calculations\nused from here", hjust = 0, size = 3,
           color = "gray40", family = "Times New Roman") +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.4) +
  scale_color_manual(
    values = c(
      "Ross et al. (2017) (2005–2015)" = "#185FA5",
      "Own calculations (2005–2022)"   = "#BA7517"
    ),
    breaks = c("Ross et al. (2017) (2005–2015)", "Own calculations (2005–2022)")
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    x = NULL,
    y = "Implicit subsidy gap\n(constant 2015 USD/liter)",
    color = NULL,
    caption = "Note: Negative values indicate implicit subsidy. Own calculations follow Ross et al. (2017) methodology.\nSource: Ross et al. (2017), Von Uexküll et al. (2024), Trading Economics, EIA, FRED."
  ) +
  theme_minimal(base_size = 11, base_family = "Times New Roman") +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 0,
                                family = "Times New Roman"),
    axis.title.y = element_text(size = 9, family = "Times New Roman"),
    axis.text = element_text(family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman")
  )