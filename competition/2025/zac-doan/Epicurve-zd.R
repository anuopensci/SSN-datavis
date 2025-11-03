# Require packages
pacman::p_load(tidyverse, readr, patchwork) 

# Import the csv file
# rain <- readr::read_csv(path = ...)
# current_monsoon_with_avg <- readr::read_csv(path = ...)

p1 <- ggplot(data = current_monsoon_with_avg, aes(x = episode_date)) +
  geom_col(aes(y = cases, fill = location), alpha = 1, width = 1) +
  geom_line(aes(y = daily_avg_cases, color = "5-year daily average (2020–2024)"), linewidth = .25, alpha = 2.5) +
  geom_point(aes(y = daily_avg_cases, color = "5-year daily average (2020–2024)"), size = 0.5, alpha = 0.5) +
  labs(title = "Number of cases of melioidosis notified to Queensland Health by episode date and HHS of residence /nand the average rainfall of Cairns, Ingham and Townville cities",
       subtitle = "Queensland, 01/11/2024 - 30/06/2025",
       fill = "",
       x = "",
       y = "Number of cases in Queensland") +
  scale_x_date(date_breaks = "2 week", date_labels = "%b %d", expand = expansion(add = c(0, 0))) +  
  scale_y_continuous(breaks = function(.) seq(0, range(.)[2], by = 2), 
                     expand = expansion(add = c(0, 0.3))) +
  scale_fill_viridis_d(direction = 1, option = "viridis") +
  scale_color_manual(name = "", values = c("5-year daily average (2020–2024)" = "red3")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
    plot.subtitle = element_text(hjust = 0.5, size = 18),
    
    axis.title = element_text(color = "black", face = "bold", size = 18),
    axis.text.y = element_text(color = "black", size = 18),
    axis.ticks.y = element_line(color = "black"),
    
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    panel.background = element_rect(fill = "white"),
    
    legend.position = "inside", 
    legend.justification = c("right", "top"), 
    legend.direction = "vertical",
    legend.text = element_text(size = 18)
  )
p1

p2 <- ggplot(data = rain, aes(x = date, y = average_rainfall)) +
  geom_line(linewidth = 1.5, linetype = "solid", alpha = 1.5, color = "navy") +
  labs(x = NULL, y = "Rainfall (mm)",
       color = ""
  ) +
  scale_x_date(date_breaks = "2 week", date_labels = "%b %d", expand = expansion(add = c(0, 0))) +  
  scale_y_continuous(breaks = c(0, 108, 173, 237)  , expand = expansion(add = c(10.5, 10.5))) +
  theme(
    axis.title.y = element_text(color = "navy", size = 18),
    axis.text.y = element_text(color = "navy", size = 18),
    axis.ticks.y = element_line(color = "navy"),
    axis.line.y = element_blank(),
    
    axis.line.x = element_blank(),
    axis.text.x = element_text(color = "navy", size = 18),
    axis.ticks.x = element_line(color = "navy", linewidth = .5),
    
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center"
  )
p2

p1 / p2 + plot_layout(heights = c(4, 1))

