"""
Python script for building CFML_Wraps.
Author: Nebil A. Katcho
December 2023

---------
Functions
---------
get_cfml_modules_filenames() -> list
get_types(lines : list,n : int =0) -> tuple
get_uses(lines : list,n : int =0) -> tuple
read() -> None
read_cfml_module(file_name : str) -> None
run() -> None
wrap() -> None
wrap_cfml_module(file_name : str) -> None
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
EXCLUDED = ['database','fft','global','keycodes','keyword','maths','messages','python','random','rational','strings','tables','vtk']
modules = {}

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

def get_types(lines : list,n : int =0) -> tuple:

    types = {}
    while n < len(lines):
        line = lines[n].lower().strip()
        if line.startswith('interface') or line.startswith('contains'):
            return (types,n-1)
        if not line.startswith('type'):
            n += 1
            continue
        if line[4:].strip().startswith('('):
            n += 1
            continue
        if line.find('public') < 0:
            n += 1
            continue
        n,line = parser_utils.get_line(n,lines)
        t_name = parser_utils.get_type_name(line)
        p_name = parser_utils.get_type_parent(line)
        #if is_colorama:
        #    print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'type' : <11}{colorama.Fore.CYAN}{t_name}{colorama.Style.RESET_ALL}")
        #else:
        #    print(f"{' ':>4}{'Parsing '}{'type' : <11}{t_name}")
        types[t_name] = cfml_objects.init_type(name=t_name,parent=p_name)
        n = parser_utils.get_type_components(n+1,lines,types[t_name]['components'])
    return (types,n)

def get_uses(lines : list,n : int =0) -> tuple:

    uses = []
    while n < len(lines):
        line = lines[n].lower().strip()
        if line.startswith('interface') or line.startswith('contains'):
            return (uses,n-1)
        if not line.startswith('use'):
            n += 1
            continue
        n,line = parser_utils.get_line(n,lines)
        uses.append(line)
        n += 1
    return (uses,n)

def read() -> None:

    os.chdir('../')
    cfml_modules_fnames = get_cfml_modules_filenames()
    for file_name in cfml_modules_fnames:
        read_cfml_module(file_name)
    return None

def read_cfml_module(file_name : str) -> None:

    #print('')
    #if is_colorama:
    #    print(f"{colorama.Fore.GREEN}{'Reading file '}{colorama.Fore.CYAN}{file_name}{colorama.Style.RESET_ALL}")
    #else:
    #    print(f"{'Reading file '}{file_name}")
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
    modules[m_name] = cfml_objects.init_module(name=m_name)
    if is_colorama:
        print(f"{' ':>4}{colorama.Fore.GREEN}{'Module name: '}{colorama.Fore.CYAN}{m_name}{colorama.Style.RESET_ALL}")
    else:
        print(f"{' ':>4}{'Module name: '}{m_name}")

    # Get used modules
    modules[m_name]['uses'],n = get_uses(lines,0)

    # Get types
    modules[m_name]['types'],n = get_types(lines,0)

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
    read()
    if is_colorama:
        print(f"{colorama.Fore.GREEN}{'Setting childs'}{colorama.Style.RESET_ALL}")
    else:
        print(f"{'Setting childs'}")
    set_childs()
    if is_colorama:
        print(f"{colorama.Fore.GREEN}{'Start building wraps'}{colorama.Style.RESET_ALL}")
    else:
        print(f"{'Start building wraps'}")
    wrap()

def set_childs():

    for m in modules:
        for t in modules[m]['types']:
            if modules[m]['types'][t]['parent']:
                p = modules[m]['types'][t]['parent']
                modules[m]['types'][p]['childs'].append(t)

def wrap() -> None:

    if not os.path.isdir('CFML_Wraps'):
        os.mkdir('CFML_Wraps')
    for m in modules:
        wrap_cfml_module(m)
    return None

def wrap_cfml_module(m_name : str) -> None:

    w_name = 'Wraps_'+m_name[5:]
    w_file = os.path.join('CFML_Wraps',w_name+'.f90')
    with open(w_file,'w') as f:
        f.write(f"submodule (CFML_Wraps) {w_name}\n")
        f.write(f"{'':>4} implicit none\n")
        f.write(f"{'':>4} contains\n")

        f.write(f"end submodule")


if __name__ == '__main__':

    run()
