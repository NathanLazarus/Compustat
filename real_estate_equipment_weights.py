from casadi import *
import numpy as np
from csv import reader


solutions = np.array([]).reshape(0, 3)

allyears = np.genfromtxt(
    "C:/Users/Nathan/Downloads/Compustat/real_estate_equipment_to_weight.csv",
    delimiter=",",
    names=True,
)

for yr in np.unique(allyears["calendaryear"]):

    thisyear = allyears[allyears["calendaryear"] == yr]
    for i in thisyear.dtype.names:
        exec("%s = thisyear['%s']" % (i, i))

    n_industries = thisyear.shape[0]
    weights_to_estimate = SX.sym("weights_to_estimate", n_industries, 1)

    objective = sum1(
        (
            (weights_to_estimate * uncategorized + realestate_categorized) / total
            - sector_re_share
        )
        ** 2
    )

    ub_x = DM.ones(n_industries)
    lb_x = DM.zeros(n_industries)
    constraint_from_Z1_equipment_and_realestate = (
        sum1(weights_to_estimate * uncategorized + realestate_categorized) / sum1(total)
        - desired_real_estate_share[0]
    )

    nlp = {
        "x": weights_to_estimate,
        "f": objective,
        "g": constraint_from_Z1_equipment_and_realestate,
    }
    solver = nlpsol(
        "solver", "ipopt", nlp, {"ipopt.print_level": 0, "ipopt.tol": 1e-10}
    )
    solution = solver(
        x0=DM.ones(n_industries), lbx=lb_x, ubx=ub_x, lbg=-1e-10, ubg=1e-10
    )
    solutions = np.vstack(
        [
            solutions,
            np.hstack(
                [
                    np.array(solution["x"]),
                    calendaryear.reshape(-1, 1),
                    twodigitsic.reshape(-1, 1),
                ]
            ),
        ]
    )

numpy.savetxt(
    "Optimal_real_estate_equipment_Weights.csv",
    solutions,
    delimiter=",",
    comments="",
    header="uncategorized_real_estate_share,calendaryear,twodigitsic",
)
