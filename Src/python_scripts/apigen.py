"""
Python script for building CFML_Wraps.
Author: Nebil A. Katcho
February 2024

---------
Functions
---------
local_variables_unwrap(tipos : list[cfml_objects.FortranType]) -> list
local_variables_wrap(tipos : list[cfml_objects.FortranType]) -> list
run() -> None
wrap_types() -> None
wrap_cfml_module_types(file_name : str) -> None
write_cfml_wraps() -> None
write_unwrap_proc(f,t : dict,s : str) -> None
write_unwrap_proc_no_alloc(f,t : dict,s : str) -> None
write_unwrap_type(f,t : cfml_objects.FortranType,n : int,forvar : str)
write_wrap_proc(f,t : dict,s : str) -> None:
write_wrap_type(f,t : cfml_objects.FortranType,n : int,forvar : str)
"""

import cfml_objects
import os
import parser_utils
import reader
import wrapper_types
try:
    import colorama
    colorama.init()
    is_colorama = True
except:
    is_colorama = False

modules = {}
lucy = {} # Base class for every type

def run() -> None:

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
    reader.read(modules,lucy)
    if is_colorama:
        print(f"{colorama.Fore.GREEN}{'Setting childs'}{colorama.Style.RESET_ALL}")
    else:
        print(f"{'Setting childs'}")
    if is_colorama:
        print(f"{colorama.Fore.GREEN}{'Start building wraps'}{colorama.Style.RESET_ALL}")
    else:
        print(f"{'Building wraps / unwraps of CrysFML08 types'}")
    wrapper_types.wrap(modules,lucy)
    #wrap_procs()

if __name__ == '__main__':

    run()
