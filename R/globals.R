# Suppress R CMD check notes about undefined global variables
utils::globalVariables(c(
  "Individual_Variance", 
  "Cumulative_Variance", 
  "Exclude", 
  "Timepoint"
))
