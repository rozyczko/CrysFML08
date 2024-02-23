"""
Python script for building CFML_Wraps.
Author: Nebil A. Katcho
February 2024

---------
Functions
---------
get_cfml_modules_filenames() -> list
read(modules : dict) -> None
read_cfml_module(file_name : str,modules : dict) -> None
set_childs(modules : dict)
"""
import cfml_objects
import glob
import os
import parser_utils
try:
    import colorama
    colorama.init()
    is_colorama = True
except:
    is_colorama = False

# In EXCLUDED we put the modules we do not want to wrap
EXCLUDED = ['database','enbvs','fft','global','keycodes','keyword','mag_superspace_database','maths','messages','optimization_lsq','python','random','strings','vtk','wraps']

def get_cfml_modules_filenames() -> list:

    my_modules = []
    cfml_modules_names = glob.glob('CFML*.f90')
    for m in cfml_modules_names:
        exclude = False
        for e in EXCLUDED:
            if m.lower().find(e) > -1:
                exclude = True
                break
        if exclude:
            continue
        my_modules.append(m)
    if len(my_modules) == 0:
        if is_colorama:
            print(f"{colorama.Fore.RED}{'Error: No Fortran modules found. There is nothing to do. Bye bye.'}{colorama.Style.RESET_ALL}")
        else:
            print(f"{'Error: No Fortran modules found. There is nothing to do. Bye bye.'}")
        raise IOError
    return my_modules

def read(modules : dict) -> None:

    os.chdir('../../Src')
    cfml_modules_fnames = get_cfml_modules_filenames()
    for file_name in cfml_modules_fnames:
        read_cfml_module(file_name,modules)
    return None

def read_cfml_module(file_name : str,modules : dict) -> None:

    with open(file_name,'r') as f:
        lines = f.readlines()

    # Get module name
    try:
        m_name = parser_utils.get_module_name(lines)
    except Exception as e:
        if is_colorama:
            print(f"{colorama.Fore.RED}{'Error: '}{e}{colorama.Style.RESET_ALL}")
        else:
            print(f"{'Error: '}{e}")
        raise IOError
    modules[m_name] = cfml_objects.Module(name=m_name)
    if is_colorama:
        print(f"{' ':>4}{colorama.Fore.GREEN}{'Module name: '}{colorama.Fore.CYAN}{m_name}{colorama.Style.RESET_ALL}")
    else:
        print(f"{' ':>4}{'Module name: '}{m_name}")

    # Get used modules
    n = parser_utils.get_uses(0,lines,modules[m_name].uses)

    # Get publics
    n = parser_utils.get_publics(0,lines,modules[m_name].publics)

    # Get types
    n = parser_utils.get_types(0,lines,modules[m_name].types)

    # Get overloads
    n = parser_utils.get_overloads(0,lines,modules[m_name].interface)

    # Get procedures
    n = parser_utils.get_procedures(0,lines,modules[m_name].publics,modules[m_name].interface,modules[m_name].procedures)

def set_childs(modules : dict):

    for m_name in modules:
        m = modules[m_name]
        for t_name in m.types:
            t = m.types[t_name]
            if t.parent:
                p = m.types[t.parent]
                p.childs.append(t_name)

def set_lucy(modules : dict):

    for m_name in modules:
        m = modules[m_name]
        for t_name in m.types:
            parent = t_name
            t = m.types[t_name]
            p = t.parent
            while p:
                parent = p 
                p = m.types[p].parent 
            m.types[t_name].lucy = parent



                