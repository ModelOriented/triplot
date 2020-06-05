library(DALEX)
library(triplot)


# prepare data ------------------------------------------------------------

#excluding non numeric features
apartments_num <- apartments[,unlist(lapply(apartments, is.numeric))] 
model <- lm(m2.price ~ ., data = apartments_num)
apartments_no_target <- apartments_num[,-1]

# predict aspects ---------------------------------------------------------

explain <- explain(model, data = apartments_num)

aspects <- list(
  living.area = c("surface", "no.rooms"), 
  construction.year = "construction.year",
  floor = "floor")

new_apartment <- data.frame(construction.year = 1983, 
                            surface = 25, 
                            floor = 3, 
                            no.rooms = 1)

pa <- predict_aspects(
  x = explain,
  new_observation = new_apartment,
  variable_groups = aspects)

print(pa, show_features = TRUE)
plot(pa)


# triplots ----------------------------------------------------------------

explain <- explain(model, 
                   data = apartments_no_target,
                   y = apartments_num$m2.price)

triplot <- model_triplot(explain)
plot(triplot) 

triplot <- predict_triplot(
  explain,
  new_observation = new_apartment)
plot(triplot)

# group variables ---------------------------------------------------------

group_variables(apartments_num, h = 0.5)


