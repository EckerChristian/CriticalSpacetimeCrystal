import os
import json

"""
This script reads out all the .out files from the various folders in here and converts them to a single .json file that can be read in by the C++ code
"""

def read_list_from_out(filename):
    """
    Read a list of numbers from a .out file and return them as a Python list.
    The file can have whitespace-separated numbers on any number of lines.
    """
    values = []
    with open(filename, "r") as f:
        for line in f:
            line = line.strip()
            if not line:  # skip empty lines
                continue
            parts = line.split()
            for p in parts:
                values.append(float(p))
    return values

def convert_value(raw):
    """Convert a string value like '1.d-8', 'T', '0', '5.2690' into Python types."""
    raw = raw.strip()

    # Booleans
    if raw.upper() == "T":
        return True
    if raw.upper() == "F":
        return False

    # Fortran-style floats: 1.d-8 → 1.e-8
    raw = raw.replace("D", "E").replace("d", "e")

    # Try integer
    try:
        return int(raw)
    except ValueError:
        pass

    # Try float
    try:
        return float(raw)
    except ValueError:
        pass

    # Fallback: keep as string
    return raw

def main():
    dirs = [name for name in os.listdir(".") if os.path.isdir(os.path.join(".", name))]

    output = {}

    for dir in dirs[:]:

        if not os.listdir(os.path.join(dir,"512")):   # if empty directory
            continue   # go to next iteration
         
        dimtest = float(dir[3:])
       
        #Build dict for initial conditions
        init_conds = {}

        filename = os.path.join(dir,"512","Up.out")
        up = read_list_from_out(filename)
        init_conds["Up"] = up
        filename = os.path.join(dir,"512","psic.out")
        psic = read_list_from_out(filename)
        init_conds["psic"] = psic
        filename = os.path.join(dir,"512","fc.out")
        fc = read_list_from_out(filename)
        init_conds["fc"] = fc

        #Read in Delta
        filename = os.path.join(dir,"512","Delta.out")
        with open(filename, "r") as f:
            for line in f:
                line = line.strip()
                Delta = float(line)

        init_conds["Delta"] = Delta

        #Build dict for parameters
        #Template:
        params = {"IterNewton": 0,
            "MaxIterIRK": 100,
            "MaxIterNewton": 100,
            "NLeft": 38000,
            "NRight": 18000,
            "Ntau": 2048,
            "PrecisionIRK": 1e-15,
            "PrecisionNewton": 1e-1,
            "SlowError": 0.001,
            "Verbose": False,
            "XLeft": 0.0001,
            "XMid": 0.023,
            "XRight": 0.999,
            "DebugNx": 500,
            "DebugNtau": 512,
            "mismatchNorm": 1.0,
            "Converged": True,
            "Debug": False,
            "Dim": 3.06,
            "EpsNewton": 1e-06}

        filename = os.path.join(dir,"512","shoot_inner.par")

        paramsFort = {}

        #Generate a dict out of the shoot_inner.par file and fill the .json dict with values
        with open(filename, "r") as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue  # skip empty lines

                parts = line.split(None, 1)  # split into value + description
                if len(parts) != 2:
                    continue  # skip malformed lines
                
                value_raw, description = parts
                key = description.strip().rstrip(",")  # remove trailing commas
                value = convert_value(value_raw)

                paramsFort[key] = value

        params["NLeft"] = paramsFort["nleft"]
        params["NRight"] = paramsFort["nright"]
        params["Ntau"] = paramsFort["ny, doubled modes"]
        params["XLeft"] = paramsFort["xleft"]
        params["XMid"] = paramsFort["xmid"]
        params["XRight"] = paramsFort["xright"]
        params["Dim"] = round(paramsFort["dim"],8)
        params["EpsNewton"] = paramsFort["eps"]
        params["Initial_Conditions"] = init_conds

        if params["Dim"] != dimtest:
            print("Error in dim")

        output[str(dimtest)] = params
     
    with open("data_Fortran.json", "w") as f:
            json.dump(output, f, indent=4)

    print("Done.")

if __name__ == "__main__":
    main()