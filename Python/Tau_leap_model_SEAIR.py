# Tau leap code for SEAIR agricultural fair model
# Developed 4/29/25 Dana C. Pittman Ratterree
# Last modification 12/10/24 DCPR

import numpy as np
import pylab as pl
import pandas as pd

### Parameters ###
## Movement between compartments
# PIG
R0 = 9  # The reproductive number of influenza in pigs
gammas = 1 / 5  # recovery rate of swine
kappas = 1 / 2 # incubation period
beta = R0 * gammas  # transmission rate
delta = 0.83  # probability of becoming asymptomatic

# HUMAN
Cm = 60  # contact duration per minute members
Pm = 0.00356  # probability of transmission members
Ca = 5  # contact duration per minute attendees
Pa = 0.002  # probability of transmission attendees
kappah = 1 / 2  # rate exposed become infected
gammah = 1 / 5  # recovery rate

ND = MaxTime = 8 #8 means 9 days b/c python counts from 0
tau = 0.1  # setting the value of tau

## Defining initial conditions
# PIG
Ss0, Es0, As0, Is0, Rs0 = 203, 0, 5, 0, 0

N = Ss0 + Es0 + As0 + Is0 + Rs0

# MEMBER
Sm0, Em0, Im0, Rm0 = 90, 0, 0, 10

# ATTENDEE
Sa0, Ea0, Ia0, Ra0 = 10042, 0, 0, 4868

# Generating the array for the initial population values
INPUT = np.array((Ss0, Es0, As0, Is0, Rs0, Sm0, Em0, Im0, Rm0, Sa0, Ea0, Ia0, Ra0))

### DEFINING RATE EVENTS ###
def stoch_eqs(INP):
    V = INP
    Rate = np.zeros((11))
    Change = np.zeros((11, 13))
    EventCounts = np.zeros(11, dtype=int)

    Rate[0] = beta * V[0] * ((V[2] + V[3]) / N) 
    Change[0, :] = [-1, +1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

    Rate[1] = kappas * V[1] * delta
    Change[1, :] = [0, -1, +1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

    Rate[2] = kappas * V[1] * (1 - delta)
    Change[2, :] = [0, -1, 0, +1, 0, 0, 0, 0, 0, 0, 0, 0, 0]

    Rate[3] = gammas * V[2]
    Change[3, :] = [0, 0, -1, 0, +1, 0, 0, 0, 0, 0, 0, 0, 0]
    
    Rate[4] = gammas * V[3]
    Change[4, :] = [0, 0, 0, -1, +1, 0, 0, 0, 0, 0, 0, 0, 0]

    Rate[5] = Cm * Pm * V[5] * ((V[2] + V[3]) / N)
    Change[5, :] = [0, 0, 0, 0, 0, -1, +1, 0, 0, 0, 0, 0, 0]

    Rate[6] = kappah * V[6]
    Change[6, :] = [0, 0, 0, 0, 0, 0, -1, +1, 0, 0, 0, 0, 0]

    Rate[7] = gammah * V[7]
    Change[7, :] = [0, 0, 0, 0, 0, 0, 0, -1, +1, 0, 0, 0, 0]

    Rate[8] = Ca * Pa * V[9] * ((V[2] + V[3]) / N)
    Change[8, :] = [0, 0, 0, 0, 0, 0, 0, 0, 0, -1, +1, 0, 0]

    Rate[9] = kappah * V[10]
    Change[9, :] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, +1, 0]
    
    Rate[10] = gammah * V[11]
    Change[10, :] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, +1]

    for i in range(11):
        Num = np.random.poisson(Rate[i] * tau)
        Use = min([Num] + [V[j] for j in np.where(Change[i, :] < 0)[0]])
        V = V + Change[i, :] * Use
        EventCounts[i] = Use

    return V, EventCounts

### SETTING UP THE ITERATIONS ###
def Stoch_Iteration(INPUT):
    
    Ss, Es, As, Is, Rs = [INPUT[0]], [INPUT[1]], [INPUT[2]], [INPUT[3]], [INPUT[4]]
    Sm, Em, Im, Rm = [INPUT[5]], [INPUT[6]], [INPUT[7]], [INPUT[8]]
    Sa, Ea, Ia, Ra = [INPUT[9]], [INPUT[10]], [INPUT[11]], [INPUT[12]]

    swine_cumulative_infections = 0  # Initialize cumulative infection counter
    member_cumulative_infections = 0  # Initialize cumulative infection counter
    attendee_cumulative_infections = 0  # Initialize cumulative infection counter

    for _ in T:
        res, EventCounts = stoch_eqs(INPUT)
        
        # Count infection events
        swine_cumulative_infections += EventCounts[0]  # S → E
                
        member_cumulative_infections += (INPUT[5] - res[5])  # Count members leaving Sm
        attendee_cumulative_infections += (INPUT[9] - res[9])  # Count attendees leaving Sa
        
        # Update the compartment lists
        Ss.append(res[0])
        Es.append(res[1])
        As.append(res[2])
        Is.append(res[3])
        Rs.append(res[4])
        Sm.append(res[5])
        Em.append(res[6])
        Im.append(res[7])
        Rm.append(res[8])
        Sa.append(res[9])
        Ea.append(res[10])
        Ia.append(res[11])
        Ra.append(res[12])

        INPUT = res  # Update INPUT for the next iteration

    return [Ss, Es, As, Is, Rs, Sm, Em, Im, Rm, Sa, Ea, Ia, Ra,
        swine_cumulative_infections, member_cumulative_infections, attendee_cumulative_infections]



T = np.arange(0.0, ND, tau)

[Ss, Es, As, Is, Rs, Sm, Em, Im, Rm, Sa, Ea, Ia, Ra,
 swine_cumulative_infections, member_cumulative_infections, attendee_cumulative_infections] = Stoch_Iteration(INPUT)

# Making lists
tT = np.array(T)
tSs = np.array(Ss)[1:]
tEs = np.array(Es)[1:]
tAs = np.array(As)[1:]
tIs = np.array(Is)[1:]
tRs = np.array(Rs)[1:]
tSm = np.array(Sm)[1:]
tEm = np.array(Em)[1:]
tIm = np.array(Im)[1:]
tRm = np.array(Rm)[1:]
tSa = np.array(Sa)[1:]
tEa = np.array(Ea)[1:]
tIa = np.array(Ia)[1:]
tRa = np.array(Ra)[1:]

### MULTIPLE SIMULATIONS ###
num_iterations = 10000

all_results = []  # Store all results
swine_cumulative_infections_list = []  # Store swine results
member_cumulative_infections_list = []  # Store member results
attendee_cumulative_infections_list = []  # Store attendee results

for _ in range(num_iterations):
    INPUT = np.array((Ss0, Es0, As0, Is0, Rs0, Sm0, Em0, Im0, Rm0, Sa0, Ea0, Ia0, Ra0))

    results = Stoch_Iteration(INPUT)  # Run sim for this iteration
    
    swine_cumulative_infections = results[-3]  # Extract cumulative infections for swine
    member_cumulative_infections = results[-2]
    attendee_cumulative_infections = results[-1]

    swine_cumulative_infections_list.append(swine_cumulative_infections)
    member_cumulative_infections_list.append(member_cumulative_infections)
    attendee_cumulative_infections_list.append(attendee_cumulative_infections)

    all_results.append(results[:-3])  # Append the rest of the results
    
### SAVING RESULTS ###
import pandas as pd

# ## SIMULATIONS ##
# columns = ['Ss', 'Es', 'As', 'Is', 'Rs', 'Sm', 'Em', 'Im', 'Rm', 'Sa', 'Ea', 'Ia', 'Ra']
# sim_res = pd.DataFrame({col: np.concatenate([res[idx] for res in all_results]) for idx, col in enumerate(columns)})

# # Create a time column for the range of days (corresponding to T)
# time_column = np.tile(np.arange(0.0, 0.1 * (len(T) + 1), 0.1), num_iterations)  # Repeat for each iteration

# # Add the time column to the DataFrame
# sim_res['Time'] = time_column

# # Add a simulation iteration identifier
# simulation_ids = np.repeat(range(1, num_iterations + 1), len(T) + 1)
# sim_res['Simulation'] = simulation_ids

# # Save the DataFrame
# sim_res.to_csv(r'C:\Users\dcpit\OneDrive\Documents\GRADUATE SCHOOL\PhD-yr1\Project 4\SEAIR_Model\Results\Tau leap Sims\Model_results_R0_8_SEAIR.csv', index=False)

## CUMULATIVE ##
cum_res = pd.DataFrame({
    'Swine': swine_cumulative_infections_list,
    'Member': member_cumulative_infections_list,
    'Attendee': attendee_cumulative_infections_list
})

# Save the DataFrame
cum_res.to_csv(r'C:\Users\dcpit\OneDrive\Documents\GRADUATE SCHOOL\PhD-yr1\Project 4\SEAIR_Model\Results\Tau leap Sims\Cumulative_infection_R0_9.csv', index=False)
