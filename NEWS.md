# triplot 1.2

* last vertical line in hierarchical_importance() plot of model_triplot() shows 
baseline value
* left margin of middle panel in triplot can be set with "margin_mid" 
* colors in plot for predict_aspects() changed to match DALEX::colors_discrete_drwhy()
* changed order or n_var and sample_method parameters in aspect_importance() functions
* renamed triplot title to Global / Local triplot


# triplot 1.1.2

* modified triplot object and methods
* removed ingredients dependency

# triplot 1.1.1

* switched from GridExtra to Patchwork
* improved plotting of triplot object

# triplot 1.1.0

* renamed prediction_aspect() to predict_aspects()
* added aliases predict_triplot() and model_triplot()
* added bar_width param to triplot
* added warning if target is in data
* added parameter "type" to calculate_triplot()
* added print for predict_aspects() and triplot()

# triplot 1.0.4

* restructured group_variables() and triplot objects
* added hierarchical_importance object (instead of 
plot_aspects_importance_grouping)
* triplot renamed to calculate_triplot()
* triplot is plotted by calling plot on calculate_triplot() results

# triplot 1.0.3

* updated plot_aspects_importance_grouping so it can draw last clusters 
connection
* add_importance_labels parameter set to FALSE by default


# triplot 1.0.2

* new feature: triplot can present both: automatic aspect and feature importance 
grouping (by using ingredients::feature_importance())


# triplot 1.0.1

* aspectImportance is renamed to triplot 
* added second vignette


# aspectImportance 1.0

* aspect_importance is moved out of `DALEXtra` into standalone package
