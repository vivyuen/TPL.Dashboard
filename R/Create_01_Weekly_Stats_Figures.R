#### Title: TPL Weekly Stats Tranformation
#### Purpose: Clean and transform variables for visualization
#### Author: Vivien Yuen
#### Date: 19/11/2023

# Set Up -----------------------------------------------------------------------
# Load packages

create_weekly_stats_figures <- function(weekly_data_clean)
{
set.seed(12345)

pal <- paletteer_d("DresdenColor::paired")

# Plots ----------------------------------
# Game Involvement
gc_long <- pivot_longer(weekly_data_clean, cols = c(not_goal_contributions, goal_contributions), names_to = "GC", values_to = "Number")

gc_long$GC <- factor(gc_long$GC, levels = c("not_goal_contributions", "goal_contributions"))

plot_game_involvement <- ggplot(subset(gc_long, gameId %in% "49212"),
                                mapping = aes(x = reorder(name, -Number, sum),
                                              y = Number,
                                              fill = GC)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_fill_manual(values = c("#785596FF", "#513965FF"), name = "") +
    labs(title = "Game Involvement (Goal Contributions vs Touches)") +
    theme_classic() +
    theme(title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.text = element_text(size = 8),
          axis.text.x = element_text(angle = 90),
          legend.position = "top")

plot_game_involvement


# Turnovers
turnovers_long <- pivot_longer(weekly_data_clean, cols = c(turnovers, not_turnovers), names_to = "Turnovers", values_to = "Number")

plot_turnovers <- ggplot(subset(turnovers_long, gameId %in% "49212"),
                                mapping = aes(x = reorder(name, -Number, sum),
                                              y = Number,
                                              fill = Turnovers))   +
    geom_bar(position="stack", stat = "identity") +
    scale_fill_manual(values = c("#C2697FFF", "#9E4058FF"), name = "") +
    labs(title = "Turnovers to Touches") +
    theme_classic() +
    theme(title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.text = element_text(size = 8),
          axis.text.x = element_text(angle = 90),
          legend.position = "top")

plot_turnovers

# Team Contributions
plot_team_contribution <- ggplot(subset(weekly_data_clean, gameId %in% "49212"),
                                 mapping = aes(x = percent_of_team_touches,
                                               y = percent_of_team_contributions,
                                               label = name)) +
    geom_point() +
    labs(title = "Team Contribution",
         x = "Percent of Team Touches",
         y = "Percent of Team Goal Contributions") +
    geom_point(color = "#589E40FF", fill = "#589E40FF") +
    geom_text_repel(box.padding = 0.5, force = 5, segment.alpha = 0) +
    theme_classic() +
    theme(title = element_blank(),
          legend.text = element_text(size = 8)) +
    scale_x_continuous(labels = scales::percent_format(scale = 100),
                       expand = expansion(mult = c(0.05, 0.1))) +
    scale_y_continuous(labels = scales::percent_format(scale = 100),
                       expand = expansion(mult = c(0.05, 0.1)))

plot_team_contribution

}
