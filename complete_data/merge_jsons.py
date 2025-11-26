import json
from collections import OrderedDict


def load_json(path):
    """Load and return a JSON file as a dictionary."""
    with open(path, "r") as f:
        return json.load(f)

def main():
    # ----- CHANGE THESE TO YOUR FOUR FILES -----
    file0 = "CppData/simulation_data_512.json"
    file1 = "CppData/data_lowD_1024_1.json"
    file2 = "CppData/data_lowD_1024_2.json"
    file3 = "CppData/data_lowD_1024_3.json"
    file4 = "CppData/data_lowD_1024_4.json"

    fortfile = "FortranData/data_Fortran.json"

    output_file = "critcoll_data.json"

    # Load all four JSON dictionaries
    dict0 = load_json(file0)
    dict1 = load_json(file1)
    dict2 = load_json(file2)
    dict3 = load_json(file3)
    dict4 = load_json(file4)
    dictfort = load_json(fortfile)

    # Merge into one dictionary (later entries overwrite earlier ones)
    merged = {}
    for d in [dict0,dict1, dict2, dict3, dict4, dictfort]:
        merged.update(d)

    
    ordered = OrderedDict(sorted(merged.items()))
    # Save the merged dictionary
    with open(output_file, "w") as f:
        json.dump(ordered, f, indent=4)

    print(f"Merged JSON written to {output_file}")
    print(f"Total keys: {len(ordered)}")

if __name__ == "__main__":
    main()