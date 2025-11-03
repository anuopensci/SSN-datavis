
# animation----------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(gganimate)
  library(showtext)
  library(scales)
})
#add font
font_add_google("DM Serif Display")
showtext_auto()
df<-readRDS("df_ma2.rds") 
df <- df %>%
  mutate(
    sombra = (Time %% 2 == 0),
    Year_f = factor(Time),                      
    Year_i = as.integer(as.character(Year_f))   
  )

# Watermark of year
year_tbl <- df %>%
  distinct(Year_f, Year_i) %>%
  arrange(Year_i) %>%
  mutate(year_size = 32)

p_anim <- ggplot() +
  geom_point(
    data = df,
    aes(x = Value_le, y = Value_tfr, color= Region, group = Location),
    size = 6,
    stroke = 0.8,
    alpha = 0.75
  ) +
  geom_text(
    data = year_tbl,
    aes(x = 90, y = 7.8, label = Year_i, size = year_size),
    family = "playfair", fontface = "bold",
    hjust = 1, vjust = 1,
    colour = scales::alpha("#0B132B", 0.10),
    show.legend = FALSE, inherit.aes = FALSE
  ) +
  scale_size_identity(guide = "none") +

  scale_colour_manual(values = c(
    "Caribbean"       = "#e41a1c",
    "Central America" = "#377eb8",
    "South America"   = "#4daf4a"
  ))             +
  geom_hline(yintercept = 2.1, linetype = "dotted", color = "grey34", linewidth = 0.8) +
  annotate("text", x = 31, y = 2.3, label = "Replacement Level 2.1", hjust = 0, size = 4, color = "grey34", family = "DM Serif Display") +
  labs(
    title = "Demographic Transition in Latin America and The Caribbean",
    subtitle = "from 1950 to 2025",
    x = "Life Expectancy at birth (boths sexes)",
    y = "Total Fertility Rate",
    caption = "Source: UN World Population Prospects"
  ) +
  coord_cartesian(xlim = c(30, 90), ylim = c(0, 8)) +
  guides(
    colour = guide_legend(
      nrow = 1, byrow = TRUE,
      title.position = "top",
      override.aes = list(size = 4, alpha = 1, stroke = 0.4)  
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "DM Serif Display"),        
    title = element_text(face = "bold"),
    legend.position      = "bottom",
    legend.box           = "horizontal",
    legend.title = element_text(hjust =0.5)  ,
    legend.spacing.x     = unit(20, "pt"),
    legend.key.width     = unit(35, "pt"),
    legend.key.height    = unit(30, "pt"),
    legend.text        = element_text(margin = margin(r = 20)),
    legend.margin        = margin(t = 4, r = 6, b = 0, l = 6),
    legend.box.spacing   = unit(18, "pt"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 1),
    axis.title = element_text(face = "bold", size = 14),
    axis.text  = element_text(size = 13, face = "bold"),
  ) +
  transition_states(
    states = Year_f,            
    transition_length = 3,
    state_length = 3,
    wrap = FALSE
  ) +
  ease_aes("cubic-in-out")

idx_text_layers <- which(sapply(ggplot_build(p_anim)$plot$layers,
                                function(l) inherits(l$geom, "GeomText")))
p_anim <- p_anim + shadow_mark(past = TRUE, future = FALSE, alpha = 0.2, size = 2,
                               exclude_layer = idx_text_layers)

animate(
  p_anim,
  width = 1200,
  height = 800,
  fps = 8,
  duration = 30,
  end_pause = 15,
  renderer = gifski_renderer("demographic transition LAC.gif")
)
