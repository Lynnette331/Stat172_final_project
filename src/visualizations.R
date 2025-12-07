source("src/cleaning.R")

nfl_model <- nfl %>%
  select(combineHeight, combineWeight, combineBMI, ageAtDraft,
         combine40yd, combineVert, combineBench,
         combineShuttle, combineBroad, combine3cone,
         region, top_three_round_bin)

summary(nfl_model)

###Visualizations for the top 3 picks#########

# Dataframe of variable importance
var_importance <- data.frame(
  Variable = c("40 yard dash","Weight","Age","Height","BMI",
               "3-cone","Vertical","Broad","Bench","Shuttle","Region"),
  Importance = 11:1  
)

# Make the barplot
ggplot(var_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Horizontal bars
  labs(title = "Variable Importance for Top 3-Round Picks",
       x = "Variable",
       y = "Importance") +
  theme_minimal()
ggsave("output/var_importance_plot.pdf")
#the bar chart shows the variables in order of importance  


#which regions produce more top 3 picks
ggplot(nfl, aes(x = region, fill = factor(top_three_round_bin))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values=c("0"="gray","1"="steelblue"), labels=c("Others","Top 3")) +
  labs(title="Proportion of Top 3 Picks by Region",
       x="Region", y="Proportion", fill="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))
#the south produces more top 3 picks than any other region

#heatmap of top3 picks by region
region_summary <- nfl %>%
  group_by(region) %>%   # keep both!
  summarise(
    top3_count = sum(top_three_round_bin == 1),
    total_players = n(),
    proportion_top3 = top3_count / total_players
  ) 
state_region_lookup <- nfl %>%
  select(homeState, region) %>%
  distinct()

state_summary_region <- state_region_lookup %>%
  left_join(region_summary, by = "region") %>%
  rename(state = homeState)

plot_usmap(data = state_summary_region, values = "proportion_top3", color = "white") +
  scale_fill_viridis_c(
    name = "Proportion of Top 3 Picks",
    label = scales::percent
  ) +
  labs(title = "Proportion of Top 3 NFL Draft Picks by Region") +
  theme(legend.position = "right")
ggsave("output/heatmap_top3picks_plot.pdf")


# Box plots of weight by region
ggplot(nfl, aes(x = region, y = combineWeight, fill = factor(top_three_round_bin))) +
  geom_boxplot() + 
  facet_wrap(~region, scales = "free") +
  scale_fill_brewer(name = "Top 3 Pick", palette = "Paired")
ggsave("output/boxplots_of_weight_by_region.pdf")
#players drafted in the top 3 from all regions consistently show higher median weights than those not drafted 
#especially in the midwest. 
#south region shows more variation in weight with outliers
#

#scatter plot for weight vs 40-yard 
ggplot(nfl, aes(x = combine40yd, y = combineWeight, color = factor(top_three_round_bin))) +
  geom_point(alpha = 0.6) +
  labs(title = "Top 3-Round Picks vs 40-Yard Dash and Weight",
       x = "40-Yard Dash (sec)",
       y = "Weight (lbs)",
       color = "Top 3-Round Pick") +
  theme_minimal() +
  scale_color_manual(values = c("0" = "#0173B2", "1" = "#DE8F05"))
ggsave("output/scatter_plot_for_weight_vs_40-yard.pdf")

#strong positive relationship. Heavier players tend to run slower and vice versa. 

#the highest top 3 picks appear in the (220-280)lbs and (4.4-5.0)seconds. Many fast light players weren't 
#drafted early (170-220lbs) and 4.6 seconds.

#scatter plot of age vs weight
ggplot(nfl, aes(x = ageAtDraft, y = combineWeight, color = factor(top_three_round_bin))) +
  geom_point(alpha = 0.7) +
  labs(title = "Top 3-Round Picks vs 40-Yard Dash and Weight",
       x = "ageAtDraft",
       y = "Weight (lbs)",
       color = "Top 3-Round Pick") +
  theme_minimal() + 
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"))
ggsave("output/age_vs_weight.pdf")

#not much insights


##histogram of age of drafted

ggplot(nfl_model %>% filter(top_three_round_bin == 1, ageAtDraft >= 19, ageAtDraft <= 27), 
       aes(x = ageAtDraft)) + 
  geom_histogram(binwidth = 0.17, fill = "steelblue", color = "white", size = 0.05) +
  labs(title = "Age at Draft ",
       x = "Age at Draft",
       y = "Count") +
  scale_x_continuous(breaks = 19:27) +  # Show all ages from 20-25
  theme_minimal()
ggsave("output/age_histogram.pdf")

#highest ages drafted range between 22 and 24

#########visualizations for top 3 for the defense #########
nfl$top_three_round_class <- factor(nfl$top_three_round_bin, 
                                    levels = c(0,1),
                                    labels = c("Not Drafted", "Drafted") )

def_positions <- c("DB", "DE", "DL", "DT", "LB", "OLB", "S")
nfl_defense <- nfl %>%
  filter(position %in% def_positions)

nfl_defense <- nfl_defense %>%
  select(combineHeight, combineWeight, combineBMI, ageAtDraft,
         combine40yd, combineVert, combineBench,
         combineShuttle, combineBroad, combine3cone,
         region, top_three_round_bin, top_three_round_class)
  

# 3-cone vs 40-yard dash (agility vs speed for defense)
ggplot(nfl_defense, aes(x = combine3cone, y = combine40yd, color = factor(top_three_round_bin))) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "Defensive Players: Agility vs Speed Combination") + 
  scale_color_manual(values = c("0" = "#0072B2", "1" = "#D55E00"))
ggsave("output/3_cone_vs_40-yard.pdf")

#agility and speed are essential for defense players

#age and weight of the defense positions
ggplot(nfl_defense, aes(x = ageAtDraft , y = combineWeight, color = factor(top_three_round_bin))) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "Defensive Players: weight vs Age") +
  scale_color_manual(values = c("0" = "#0072B2", "1" = "#D55E00"))
ggsave("output/age_and_weight.pdf")

#size/weight matters more than age

#for defense positions
offensive_positions <- c("C", "OG", "OL", "OT", "QB", "RB", "FB", "TE", "WR")
nfl_offense <- nfl %>%
  filter(position %in% offensive_positions)


nfl_offense <- nfl_offense %>%
  select(combineHeight, combineWeight, combineBMI, ageAtDraft,
         combine40yd, combineVert, combineBench,
         combineShuttle, combineBroad, combine3cone,
         region, top_three_round_bin, top_three_round_class)

#Age and 40yd for offense positions
ggplot(nfl_offense, aes(x = ageAtDraft, y = combine40yd, color = factor(top_three_round_bin))) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "Offensive Players: Age at Draft vs Speed Combination") +
  scale_color_viridis_d(option = "plasma", name = "Top 3 Pick")
ggsave("output/age_and_40-yd.pdf")

#most offensive players are between 21 and 24. And they are generally faster.

# 3-cone vs 40-yard dash (agility vs speed for offense)
ggplot(nfl_offense, aes(x = combine3cone, y = combine40yd, color = factor(top_three_round_bin))) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "Defensive Players: Agility vs Speed Combination") + 
  scale_color_manual(values = c("0" = "blue", "1" = "red"), name = "Top 3 Pick")
ggsave("output/agility_vs_speed.pdf")
#it is positively correlated. 



# age and if drafted top three histogram density 
ggplot(nfl_model, aes(x = ageAtDraft, color = top_three_round_bin)) +
  geom_density(linewidth = 1.2, alpha = 0.6) +
  labs(
    title = "Density of Age at Draft by Top 3 Round Draft Status",
    x = "Age at Draft",
    color = "Drafted in Top 3 Rounds?"
  ) +
  theme_minimal()

# boxplot 

# Create the data for the two groups
plot_data <- bind_rows(
  nfl %>% mutate(group = "All Players"),
  nfl %>% 
    filter(top_three_round_bin == 1) %>% 
    mutate(group = "Top 3 Round Picks")
)

# Make group a factor so ordering is correct
plot_data$group <- factor(plot_data$group, 
                          levels = c("All Players", "Top 3 Round Picks"))

# Plot
ggplot(plot_data, aes(y = group, x = ageAtDraft, fill = group)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c(
    "All Players" = "red",
    "Top 3 Round Picks" = "lightblue"
  )) +
  labs(
    title = "Age at Draft: All Players vs. Top 3 Round Picks",
    x = "Age at Draft",
    y = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 40yd and positions
agility_positions <- c("QB", "RB", "FB", "WR", "TE", 
                       "DB", "S", "LB", "OLB", "K", "P", "LS")

OD_line_positions <- c("C", "OG", "OT", "OL", "DE", "DT", "DL")

nfl <- nfl %>%
  mutate(
    position_group = case_when(
      position %in% agility_positions ~ "Agility Based Positions",
      position %in% OD_line_positions ~ "Strength Based Positions",
      TRUE ~ "Other"
    )
  )

plot_data <- nfl %>% 
  filter(position_group != "Other")


ggplot(plot_data, aes(x = position_group, y = combine40yd, fill = position_group)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.15, fill = "white", outlier.size = 0.8) +
  scale_fill_manual(values = c(
    "Agility Based Positions" = "red",
    "Strength Based Positions" = "lightblue"
  )) +
  labs(
    title = "40-yard Dash Times by Position Group",
    x = "Position Group",
    y = "40-yard Dash Time (seconds)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
=======
########Visualization to compare the 3 final forests for All Positions, Smaller Positions and QBs#############
# Load each final forest you saved as an RDS file
forest_all      <- readRDS("output/final_forest_ALL.rds")
forest_QBs  <- readRDS("output/final_forest_QBs.rds")
forest_small  <- readRDS("output/final_forest_smallerPositions.rds") 


# Takes a forest model + a label (model name)
# Returns a clean table with variable, importance, and model name
imp_df <- function(model, name) {
  as.data.frame(importance(model)) %>% 
    rownames_to_column("variable") %>% 
    mutate(model = name)
}

# Create cleaned importance tables for each forest
imp_all      <- imp_df(forest_all, "All Positions")
imp_QBs  <- imp_df(forest_QBs, "Offense")
imp_smaller  <- imp_df(forest_small, "Smaller Positions")

# Combine all importance tables into one dataset
imp_full <- bind_rows(imp_all, imp_QBs, imp_smaller)


# Plot variable importance side-by-side for all models
ggplot(imp_full, aes(x = reorder(variable, MeanDecreaseGini),
                     y = MeanDecreaseGini,
                     fill = model)) +
  geom_col(position = "dodge") +      # side-by-side bars
  coord_flip() +                      # flip axes to make it readable
  theme_bw() +                        # clean theme
  labs(
    title = "Variable Importance Comparison Across Forests",
    x = "Variable",
    y = "Mean Decrease in Gini"
  )
ggsave("output/forest_comparison.pdf")













>>>>>>> 8c9eb9ad8e5a73bcbaac54f67827fc4c7aa9512c






