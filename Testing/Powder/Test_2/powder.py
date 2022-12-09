import json
import powder_mod

def load_data(fname : str) -> dict:

    with open(fname) as f:
        data = json.load(f)
    return data

def compute_pattern(data: dict):

    return powder_mod.simulation(data)
