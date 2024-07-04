data <- read_excel(path = file.path(here(srcDir), "qual_results.xlsx"))

data$earner <- factor(data$earner, levels = c("man", "woman", "equal"))

p1 <- data %>%
  filter(decision == "high") %>%
  ggplot(aes(x = ame, y = decider, fill = decider)) +
  geom_col(width = 0.8, position = position_dodge(0.7)) +
  geom_text(aes(x = ame + .01 * sign(ame), label = ifelse(significant == "yes", "*", " ")), 
            position = position_dodge(width = 0.9), 
            size = 3.5 , angle = 90) +
  facet_grid(reorder(topic, -ame) ~ earner,
             space = "free",
             switch = "y") +
  theme_minimal(13) +
  scale_fill_grey() +
  scale_y_discrete(labels = NULL, breaks = NULL) +
  scale_x_continuous(limits = c(-.22, .22)) +
  theme(strip.text.y.left = element_text(angle = 0),
        legend.position = "top") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs( x        = " ", 
        y        = " ",
        subtitle = "High-stakes decisions")

p2 <- data %>%
  filter(decision == "low") %>%
  ggplot(aes(x = ame, y = decider, fill = decider)) +
  geom_col(width = 0.8, position = position_dodge(0.7)) +
  geom_text(aes(x = ame + .01 * sign(ame), label = ifelse(significant == "yes", "*", " ")), 
            position = position_dodge(width = 0.9), 
            size = 3.5 , angle = 90) +
  facet_grid(reorder(topic, -ame) ~ earner,
             space = "free",
             switch = "y") +
  theme_minimal(13) +
  scale_fill_grey() +
  scale_y_discrete(labels = NULL, breaks = NULL) +
  scale_x_continuous(limits = c(-.22, .22)) +
  theme(strip.text.y.left = element_text(angle = 0),
        legend.position = "none") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs( x        = " ", 
        y        = " ",
        subtitle = "Low-stakes decisions")


library(gtable)
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g <- rbind(g1, g2, size = "first")
g$widths <- unit.pmax(g1$widths, g2$widths)
grid.newpage()
grid.draw(g)