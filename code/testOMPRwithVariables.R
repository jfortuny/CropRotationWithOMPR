library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.lpsolve)
library(ompr)
library(ompr.roi)
library(magrittr)

v <- c('a', 'b')
n <- c(1,2,3)

results <- MILPModel() %>% 
  add_variable(crop[i], i = n , type = 'binary') %>%
#  variable_keys() %>%
  set_objective(sum_expr(crop[i], i = n)) %>%
  add_constraint(sum_expr(crop[i], i = n) <= 2) %>%
#  extract_constraints() %>%
  solve_model(with_ROI(solver = "lpsolve")) %>%
#  objective_value() %>%
  get_solution(crop[i]) 
# print results
print(results)
