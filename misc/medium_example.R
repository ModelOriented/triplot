library("DALEX")
data(fifa)
fifa$value_eur <- fifa$value_eur/10^6
fifa[, c("nationality", "overall", "potential", 
         "wage_eur")] <- NULL

library("dplyr")
fifa_subset <- fifa %>% 
  select(matches('goalkeeping|defending|mentality'))

library("corrplot")
corrplot(cor(fifa_subset), 
         method = "color", type = "upper", order = "hclust", 
         addCoef.col = "black",number.cex = .7, diag = FALSE)

library("ranger")
set.seed(2020)
fifa_model <- ranger(value_eur~., data = fifa)
fifa_explainer <- DALEX::explain(fifa_model,
                                 data = fifa[,-1],
                                 y = fifa$value_eur,
                                 label = "Random Forest", 
                                 verbose = FALSE)

library("triplot")
fifa_triplot_global <- model_triplot(fifa_explainer, 
                                     B = 1, 
                                     N = 5000, 
                                     cor_method = "pearson")
plot(fifa_triplot_global, margin_mid = 0)