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

#heatmap by region

state_summary <- nfl %>%
  group_by(homeState, region) %>%   # keep both!
  summarise(
    top3_count = sum(top_three_round_bin == 1),
    total_players = n(),
    proportion_top3 = top3_count / total_players
  ) %>%
  rename(state = homeState)

plot_usmap(data = state_summary, values = "proportion_top3", color = "white") +
  scale_fill_distiller(
    name = "Proportion of Top 3 Picks",
    palette =  'YlOrRd', 
    direction = 1,
    label = scales::percent
  ) +
  labs(title = "Proportion of Top 3 NFL Draft Picks by State") +
  theme(legend.position = "right")


# Box plots of weight by region
ggplot(nfl, aes(x = region, y = combineWeight, fill = factor(top_three_round_bin))) +
  geom_boxplot() + 
  facet_wrap(~region, scales = "free") +
  scale_fill_brewer(name = "Top 3 Pick", palette = "Paired")
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
  scale_color_viridis_d(option = "magma")

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
#agility and speed are essential for defense players

#age and weight of the defense positions
ggplot(nfl_defense, aes(x = ageAtDraft , y = combineWeight, color = factor(top_three_round_bin))) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "Defensive Players: weight vs Age") +
  scale_color_manual(values = c("0" = "#0072B2", "1" = "#D55E00"))
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
#most offensive players are between 21 and 24. And they are generally faster.

# 3-cone vs 40-yard dash (agility vs speed for offense)
ggplot(nfl_offense, aes(x = combine3cone, y = combine40yd, color = factor(top_three_round_bin))) +
  geom_point() + geom_smooth(method = "lm") +
  labs(title = "Defensive Players: Agility vs Speed Combination") + 
  scale_color_manual(values = c("0" = "#009E73", "1" = "#CC79A7"), name = "Top 3 Pick")
#it is positively correlated. 
