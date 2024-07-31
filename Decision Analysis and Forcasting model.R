library(decisionSupport)
library(ggplot2)
library(pls)
# Load the data

input_estimates <- read.csv("input_estimates.csv")

make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

make_variables(as.estimate(input_estimates))


# Define the decision function
decision_function <- function(x, varnames){
  #the baseline is just a normal income without bio gas
  annual_household_income <- income_per_month * 12 
  
  # calculate the cost 
  
  # establishment cost, to be paid only for th first year
  biogas_establishment_cost <- installation_cost
  
  # annual cost
  # with manure as raw material
  biogas_manure_cost_precalc <- biogas_cost_per_year + 	
    labour_cost + equipement_cost + manure_raw_material_cost
  biogas_manure_cost <- vv(biogas_manure_cost_precalc,var_CV, n_years)
  # add the annual cost with establishment cost for the first year
  biogas_manure_cost[1] <- biogas_manure_cost[1] + biogas_establishment_cost
  
  # with raw material from industrial based (?)
  biogas_industry_cost_precalc <- biogas_cost_per_year + 	
    labour_cost + equipement_cost + industry_raw_material_cost
  biogas_industry_cost <- vv(biogas_industry_cost_precalc,var_CV, n_years)
  # add the annual cost with establishment cost for the first year
  biogas_industry_cost[1] <- biogas_industry_cost[1] + biogas_establishment_cost
  
  
  # Profit from biogas production system
  # how much bigas we can produced per year
  #we can assume that the machine will be running 15 per month
  biogas_product_per_year_precalc <- biogas_product * 15 * 12
  biogas_product_per_year <- vv(biogas_product_per_year_precalc,var_CV, n_years)
  
  # revenue of biogas
  # to get the net revenue subtract the annual operation cost from the price
   annual_revenue_biogas_from_manure <- biogas_product_per_year - biogas_price
  
manure_biogas_result <- annual_revenue_biogas_from_manure - biogas_manure_cost
household_income_with_biogas_manure <- manure_biogas_result + annual_household_income

industry_biogas_result <- annual_revenue_biogas_from_manure - biogas_industry_cost
household_income_with_biogas_industry <- industry_biogas_result + annual_household_income

# calculate NPV

NPV_household_income_with_biogas_manure <-
  discount(household_income_with_biogas_manure, discount_rate, calculate_NPV = TRUE)

NPV_household_income_with_biogas_industry <-
  discount(household_income_with_biogas_industry, discount_rate, calculate_NPV = TRUE)

NPV_household <- discount(annual_household_income, discount_rate, calculate_NPV = TRUE)

 
return(list(manure_based_biogas_NPV = NPV_household_income_with_biogas_manure,
            industry_based_biogas_NPV = NPV_household_income_with_biogas_industry,
            NO_biogas_NPV = NPV_household,
            NPV_decision_do = NPV_household_income_with_biogas_manure - NPV_household,
            Cashflow_decision_do = manure_biogas_result))
}

mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("input_estimates.csv"),
  model_function = decision_function,
  numberOfModelRuns = 1e3, #run 1,000 times
  functionSyntax = "plainNames"
)

#### Plot Net Present Value (NPV) distributions 
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_do", 
                                             "NO_biogas_NPV","industry_based_biogas_NPV", "manure_based_biogas_NPV"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_do",
                                             "NO_biogas_NPV","industry_based_biogas_NPV", "manure_based_biogas_NPV"),
                                    method = 'boxplot')

#### Cashflow analysis

plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_do")

#### Projection to Latent Structures (PLS) analysis

pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[3], ncomp = 1)

plot_pls(pls_result, threshold = 0)

#### Value of Information (VoI) analysis
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:4])
#
evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "manure_based_biogas_NPV") # first value on return result
plot_evpi(evpi, decision_vars = "NPV_decision_do")
# too high uncertain for the equipement.. so in order to get a PI you should
#spend not more than 50,000

compound_figure(mcSimulation_object = mcSimulation_results, input_table = input_estimates, 
                plsrResults = pls_result, EVPIresults = evpi, 
                decision_var_name = "NPV_decision_do", 
                cashflow_var_name = "Cashflow_decision_do", base_size = 7)


