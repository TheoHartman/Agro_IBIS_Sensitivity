# Agro_IBIS_Sensitivity

## Run_IBIS_Sensitivity.r 
This code uses parameter flags within parameter files of the Agro-IBIS model to iteratively run simulations for user defined parameters and parameter values in a full factorial sensitivity analysis with user defined outputs saved after each iteration. Outputs can be used with sensitvity analysis statistics to determine model parameters most sensitive to outputs of interest. 

## optim_coefm.R 
This code uses parameter flags within parameter files of the Agro-IBIS model and user defined observations to optimize parameter values fitting simulated outputs to observations. This code uses the function optim to solve the optimization problem.
