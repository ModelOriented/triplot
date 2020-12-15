# read data ---------------------------------------------------------------

library("DALEX")
data(fifa)
fifa$value_eur <- fifa$value_eur/10^6
fifa[, c("nationality", "overall", "potential", 
         "wage_eur")] <- NULL

# create model ------------------------------------------------------------

library("ranger")
set.seed(2020)
fifa_model_rf <- ranger(value_eur~., data = fifa)
fifa_expl_rf <- DALEX::explain(fifa_model_rf,
                                 data = fifa[,-1],
                                 y = fifa$value_eur,
                                 label = "Random Forest")

# select player -----------------------------------------------------------

top_player <- fifa[order(fifa$value_eur, decreasing = TRUE),][1,]


# local triplot -----------------------------------------------------------

fifa_triplot_local <- predict_triplot(fifa_explainer, top_player,
                                      N = 5000, 
                                      cor_method = "pearson")
plot(fifa_triplot_local)

# build second model ------------------------------------------------------

library("gbm")
fifa_model_gbm <- gbm(value_eur ~ ., data = fifa, 
                      n.trees = 250,
                      interaction.depth = 4, 
                      distribution = "gaussian")
fifa_expl_gbm <- DALEX::explain(fifa_model_gbm,
                                data = fifa[,-1],
                                y = fifa$value_eur,
                                label = "Gradient Boosting")

# analyze predictions with predict_aspects --------------------------------

top_player$value_eur
fifa_expl_rf$y_hat[order(fifa$value_eur, decreasing = TRUE)[1]]
fifa_expl_gbm$y_hat[order(fifa$value_eur, decreasing = TRUE)[1]]

fifa_aspects <- list(
  "age" = "age",
  "body" = c("height_cm", "weight_kg"),
  "attacking" = c("attacking_crossing",
                  "attacking_finishing", 
                  "attacking_heading_accuracy",
                  "attacking_short_passing", 
                  "attacking_volleys"),
  "skill" = c("skill_dribbling",
              "skill_curve", "skill_fk_accuracy", 
              "skill_long_passing",
              "skill_ball_control"),
  "movement" = c("movement_acceleration", "movement_sprint_speed",
                 "movement_agility", "movement_reactions", 
                 "movement_balance"),
  "power" = c("power_shot_power", "power_jumping", "power_stamina", 
              "power_strength", "power_long_shots"),
  "mentality" = c("mentality_aggression", "mentality_interceptions",
                  "mentality_positioning", "mentality_vision", 
                  "mentality_penalties", "mentality_composure"),
  "defending" = c("defending_marking", "defending_standing_tackle",
                  "defending_sliding_tackle"),
  "goalkeeping" = c("goalkeeping_diving",
                    "goalkeeping_handling", "goalkeeping_kicking",
                    "goalkeeping_positioning", 
                    "goalkeeping_reflexes"))

fifa_pa_rf <- predict_aspects(fifa_expl_rf,
                           new_observation = top_player,
                           variable_groups = fifa_aspects)

fifa_pa_gbm <- predict_aspects(fifa_expl_gbm,
                              new_observation = top_player,
                              variable_groups = fifa_aspects)

plot(fifa_pa_rf, fifa_pa_gbm)




