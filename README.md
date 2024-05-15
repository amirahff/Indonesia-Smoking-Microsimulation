# Indonesia-Smoking-Microsimulation

In Indonesia, the prevalence of smoking poses a significant public health challenge, affecting 34.5% of the adult population (WHO, 2021). This alarming statistic is particularly concerning given the well-documented role of smoking as a major risk factor for mortality. The detrimental impact of smoking on public health necessitates the development of effective strategies and policies aimed at reducing smoking rates and mitigating its health consequences.

In the United States, the Simulation of Tobacco Policy Outcomes (STOP) model has been instrumental in simulating the potential effects of various tobacco control measures, such as taxation, on smoking prevalence (Reddy et al., 2020). Inspired by the success and utility of the STOP model, this project proposes the development of a similar model tailored to the Indonesian context. However, this endeavor will initially focus on creating a pilot version of the model.

The primary objective of this project is to develop, calibrate, and validate a novel, individual-level microsimulation model specifically designed for the Indonesian population. This model will intricately simulate the dynamics of smoking behavior, including initiation, cessation, and relapse. By accurately representing these complex processes, the model aims to provide valuable insights into the effectiveness of different tobacco control strategies in Indonesia, ultimately informing policy decisions and contributing to the reduction of smoking prevalence and its associated health burdens.  

To efficiently process and analyze the data, start by using the ReadStata.R script to import the export data from Stata. Follow this by preprocessing the data with the PrepareData.R script. Once preprocessing is complete, you are ready to run the microsimulation function available in the Microsimulation.R script. After running the microsimulation, proceed to calibrate the subgroups using the code files located in the Subgroup Calibration folder. Once the calibration code has been executed, use the ApplyCalibration.R script to calculate the RMSE and MAPE based on the runs from the Subgroup Calibration folder. This script also allows you to apply the final calibration adjustments. Finally, you can conduct sensitivity analysis using the SensitivityAnalysis.R script to further explore the impacts of various parameters on the model outcomes.
  
R version 4.2.2 (2022-10-31)  
kableExtra_1.3.4  
ggplot2_3.4.3  
tidyverse_2.0.0  
mcreplicate_0.1.2  
