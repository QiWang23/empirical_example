# empirical_example
Mplus code for empirical study

application_example.dat is the dataset.

cfa_ho1_uli.out and cfa_sa1.out include the dimensionality tests, and reliability estimates (omega coefficients).

The Mplus input files cfa_long_ho_sa_config.inp, cfa_long_ho_sa_metric.inp, and cfa_long_ho_sa_scalar.inp contain the code used to test longitudinal measurement invariance.
 
lclpm_without_random_intercept_v1.out provides the approach for fitting the LCLPM-F without random intercepts. 

lclpm_ci_without_random_intercept is the Mplus code for LCLPM-CI without random intercept.

causal_predominance_test.r provides a function for testing whether causal predominance is statistically significant using the delta method. To use this function, simply run the script in R and supply the required input parameters.
