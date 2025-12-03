import json
from collections import OrderedDict
import math


def load_json(path):
    """Load and return a JSON file as a dictionary."""
    with open(path, "r") as f:
        return json.load(f)

def main():
    # ----- CHANGE THESE TO YOUR FOUR FILES -----
    file0 = "critcoll_data.json"

    output_file = "critcoll_data_plot.json"

    # Load JSON dictionary
    dict = load_json(file0)
    

    # Merge into one dictionary (later entries overwrite earlier ones)
    
    for d, parameters in dict.items():
        delnt = math.floor(parameters["Ntau"] / 200 )   
        delnx = math.floor((parameters["NLeft"] + parameters["NRight"]) / 200 )   
        parameters.update({"SchemeIRK": 2, "PrecisionNewton": 0.1, "Converged": False, "Debug": True, "DebugNtau": delnt, "DebugNx": delnx})

    # Save the merged dictionary
    with open(output_file, "w") as f:
        json.dump(dict, f, indent=4)

    print(f"Rewritten JSON written to {output_file}")
    

if __name__ == "__main__":
    main()