from casadi import *
import numpy as np
from csv import reader
 

solutions = np.array([]).reshape(0,4)

with open('C:/Users/Nathan/Downloads/Compustat/asset_pcts_and_Z1_weights.csv', 'r') as f:
    csv_reader = reader(f)

    for row in csv_reader:
        row = [float(i) for i in row]

        shares = np.array(row[0:4]).reshape(1,4)
        ideals = row[4:8]
        allassetadjustment = row[8]

        inventories = SX.sym('inventories')
        realestate = SX.sym('realestate')
        equipment = SX.sym('equipment')
        intellectualproperty = SX.sym('intellectualproperty')
        
        weights_to_estimate = vertcat(inventories,realestate,equipment,intellectualproperty)

        constraint_from_non_financial_aggregates = shares @ weights_to_estimate + 1-sum2(shares) - allassetadjustment
        objective = sum1((weights_to_estimate - ideals)**2)
        
        nlp = {'x':weights_to_estimate, 'f':objective, 'g':constraint_from_non_financial_aggregates}
        solver = nlpsol('solver', 'ipopt', nlp,{'ipopt.print_level':0})
        solution = solver(x0=DM.ones(4),lbg=vertcat(DM.zeros(1)-1e-10),ubg=vertcat(DM.zeros(1)+1e-10))
        solutions = np.concatenate([solutions,np.array(solution['x']).reshape(1,4)])


weights_to_write = np.c_[np.array(range(solutions.shape[0]))+1950,solutions]
print(weights_to_write)
numpy.savetxt("OptimalZ1Weights.csv", weights_to_write, delimiter=",",comments='',
    header="calendaryear,reweightedInventoriesadjustment,reweightedRealEstateadjustment,reweightedEquipmentadjustment,reweightedIntellectualPropertyadjustment")