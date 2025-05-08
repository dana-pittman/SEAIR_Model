These are the files for the SEAIR model presented in "Evaluating preventive measures for the zoonotic transmission of swine influenza A variant at agricultural fairs in the United States: A Mathematical Modeling Study" 
Code was generated Dana C. Pittman Ratterree & Dr. Martial L. Ndeffo-Mbah

SEAIR stands for : Susuceptible-Exposed-Asympotmatic-Infected-Recovered

This project used 3 programs and consists of 3 folders housing the code for each program.

MATLAB
All_POP_function.m is MATLAB function code for the ODE equations for the SEAIR model
All_POP_sim.m was used to visualize the results of the maxium liklihood estiamtion for the probability of transmission per miniute against the observed data.
ESS_fit_attendee_cumulative.m is model fitting for the attendee population using maximum liklihood estimation the probability of transmission per miniute.
ESS_fit_member_cumulative.m is model fitting for the member/exhibitor population using maximum liklihood estimation the probability of transmission per miniute.

Python
Tau_leap_model.py is the python code for the tau-leap stochastic model using tau-leaping.
Tau_leap_model_Quarentine.py is a varaition of tau-leap stochastic mdoel that adds a quarentine compartment.

R
Cumulative_Infection_Central_Tendency.R was used to analyze the outputs of the stochastic models and perform basic data managment procedures.
Figure_plotting.R was used to visualize the results and generate figures for th manuscript. 
