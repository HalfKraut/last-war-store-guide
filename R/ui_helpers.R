
# Helper functions for UI components
# Intended to declutter UI definition in ui.R and provide reusable
# components for consistent styling across the app.

metric_box <- function( label, value, additional_class = NULL) {
  
  label_class <- paste(c("label", additional_class), collapse = " ")
  
  div(class = "metric",
      div(class = label_class, label),
      div(class = "value", value)
  )
}