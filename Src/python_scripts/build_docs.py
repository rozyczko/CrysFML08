"""
Python script for building Sphinx Documentation.
Author: Oier Arcelus
February 2024

---------
Functions
---------
get_cfml_modules_filenames() -> list
read() -> None
read_cfml_module(file_name : str) -> None
run() -> None
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
EXCLUDED = ['database','fft','global','keycodes','keyword','maths','messages','python','random','strings','vtk','wraps']
PRIMITIVES = ['integer','real','logical','character','complex']
NUMERICALS = ['integer','real']
modules = {}
publics_unwrap = []
publics_unwrap_no_alloc = []
publics_wrap = []
lucy = {} # Base class for every type

def get_cfml_modules_filenames() -> list:

    my_modules = []
    cfml_modules_names = glob.glob('../Src/CFML*.f90')
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

def read() -> None:

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
    modules[m_name] = cfml_objects.Module(name=m_name)
    if is_colorama:
        print(f"{' ':>4}{colorama.Fore.GREEN}{'Module name: '}{colorama.Fore.CYAN}{m_name}{colorama.Style.RESET_ALL}")
    else:
        print(f"{' ':>4}{'Module name: '}{m_name}")

    # Get used modules
    n = parser_utils.get_uses(0,lines,modules[m_name].uses)

    # Get types
    n = parser_utils.get_types(0,lines,modules[m_name].types)

def run() -> None:

    if is_colorama:
        print(f"{' ' :>20}{colorama.Fore.GREEN}{'==========='}{colorama.Style.RESET_ALL}")
        print(f"{' ' :>20}{colorama.Back.GREEN}{'Build Docs'}{colorama.Style.RESET_ALL}")
        print(f"{' ' :>20}{colorama.Fore.GREEN}{'==========='}{colorama.Style.RESET_ALL}")
    else:
        print(f"{' ' :>20}{'==========='}")
        print(f"{' ' :>20}{'Build Docs'}")
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
        print(f"{colorama.Fore.GREEN}{'Start building docs'}{colorama.Style.RESET_ALL}")
    else:
        print(f"{'Start building docs'}")
    build_docs()

def set_childs():

    for m in modules:
        for t in modules[m].types:
            if modules[m].types[t].parent:
                parent = modules[m].types[t].parent
                p = modules[m].types[t].parent
                level = 0
                while modules[m].types[p].parent:
                    parent = modules[m].types[p].parent
                    p = modules[m].types[p].parent
                    level += 1
                modules[m].types[parent].childs.append([t,level])
                lucy[t] = parent
            else:
                lucy[t] = t

def build_docs() -> None:

    if not os.path.isdir('./pages'):
        os.mkdir('./pages')

    build_entrypage()
    return None

def build_entrypage() -> None:
    with open('./pages/index.rst', 'w') as f:
        f.write(".. include:: ./types.rst\n")
        if not os.path.isdir('./pages/types'):
            os.mkdir('./pages/types')

    with open('./pages/types.rst', 'w') as f:
        f.write("#####\n")
        f.write("Types\n")
        f.write("#####\n")
        f.write(".. toctree::\n")
        f.write("   :glob:\n")
        f.write("\n")
        f.write("   ./types/index\n")

        document_cfml_type_modules()

def document_cfml_type_modules() -> None:
    with open(f'./pages/types/index.rst', 'w') as fi:
        fi.write("#######\n")
        fi.write("Modules\n")
        fi.write("#######\n")
        fi.write(".. toctree::\n")
        fi.write("\n")
        for m_name in modules.keys():
            if not os.path.isdir(f"./pages/types/py_{m_name.lower()}"):
                os.mkdir(f"./pages/types/py_{m_name.lower()}")

            with open(f'./pages/types/py_{m_name.lower()}.rst', 'w') as f:
                f.write("#"*(len(m_name)+3)+"\n")
                f.write(f"py_{m_name.lower()}\n")
                f.write("#"*(len(m_name)+3)+"\n")
                f.write(".. toctree::\n")
                f.write("\n")
                f.write(f"   ./py_{m_name.lower()}/index\n")

            fi.write(f"   ./py_{m_name.lower()}\n")
            document_cfml_type_modules_dicts(m_name)

def document_cfml_type_modules_dicts(m_name: str) -> None:
    with open(f'./pages/types/py_{m_name.lower()}/index.rst', 'w') as fi:
        fi.write(".. toctree::\n")
        fi.write("\n")
        for t_name in modules[m_name].types.keys():
            with open(f"./pages/types/py_{m_name.lower()}/{t_name}.rst", "w") as f:
                f.write("#"*len(t_name)+"\n")
                f.write(f"{t_name}\n")
                f.write("#"*len(t_name)+"\n")
                f.write("\n")

            fi.write(f"   ./{t_name}\n")

if __name__ == '__main__':

    run()
