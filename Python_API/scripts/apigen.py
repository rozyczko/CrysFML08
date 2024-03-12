"""
Python script for building CFML_Wraps.
Author: Nebil A. Katcho
February 2024

---------
Functions
---------
run() -> None
set_public_types() -> None
"""

import os
import reader
import wrapper_procs
import wrapper_types
try:
    import colorama
    colorama.init()
    is_colorama = True
except:
    is_colorama = False

modules = {}

def run() -> None:

    cwd = os.getcwd()
    if is_colorama:
        print(f"{' ' :>20}{colorama.Fore.GREEN}{'==========='}{colorama.Style.RESET_ALL}")
        print(f"{' ' :>20}{colorama.Back.GREEN}{'Build Wraps'}{colorama.Style.RESET_ALL}")
        print(f"{' ' :>20}{colorama.Fore.GREEN}{'==========='}{colorama.Style.RESET_ALL}")
    else:
        print(f"{' ' :>20}{'==========='}")
        print(f"{' ' :>20}{'Build Wraps'}")
        print(f"{' ' :>20}{'==========='}")
    if is_colorama:
        print(f"{colorama.Fore.GREEN}{'Reading modules'}{colorama.Style.RESET_ALL}")
    else:
        print(f"{'Reading modules'}")

    reader.read(modules, '../../Src/')
    reader.set_childs(modules)
    reader.set_lucy(modules)
    set_public_types()

    if is_colorama:
        print(f"{colorama.Fore.GREEN}{'Setting childs'}{colorama.Style.RESET_ALL}")
    else:
        print(f"{'Setting childs'}")
    if is_colorama:
        print(f"{colorama.Fore.GREEN}{'Building wraps / unwraps of CrysFML08 types'}{colorama.Style.RESET_ALL}")
    else:
        print(f"{'Building wraps / unwraps of CrysFML08 types'}")
    wrapper_types.wrap(modules)
    if is_colorama:
        print(f"{colorama.Fore.GREEN}{'Building wraps / unwraps of CrysFML08 procedures'}{colorama.Style.RESET_ALL}")
    else:
        print(f"{'Building wraps / unwraps of CrysFML08 procedures'}")
    wrapper_procs.wrap(modules)
    os.chdir(cwd)

def set_public_types():

    publics_types = {}
    for m in modules:
        t = modules[m].types
        for s in t:
            publics_types[s] = t[s]
    wrapper_types.publics_types = publics_types
    wrapper_procs.publics_types = publics_types

if __name__ == '__main__':

    run()
