# COVID-travel

## Structure
- All parameters are selected in 'driver.yaml'
- `run_model` in `helper_functions.R` takes in the location of the `driver.yaml`
file and expands the grid to create a data frame of all possible combinations
of parameters of interest.
- Each row (combination of parameters) is then given a unique_id which is
a simple hash key.
- For each row, the mobility network and the communities are created using
`init_model_objects` from `helper_functions.R`. This function takes in the parameters
and returns the list of parameters with the communities, new variables and
the mobility networks attached. This is now known as `packed_model_objects`.
- For each list of `packed_model_objects` we input these parameters into a model.
In this case we start with the function `model_a`, however this can be updated
and refer to other models.
- The helper function `run_model` then distributes the runs across ncores_available - 2
since each run given a set of parameters is independent.
- Results are collected through all runs and output into a csv named with the `unique_id`
for the given set of parameters.
- A log is generated capturing when the run was completed, the parameters involved
and the `unique_id`.
- This log can be used to pull and generate figures for analysis for a particular
set of parameters. 
