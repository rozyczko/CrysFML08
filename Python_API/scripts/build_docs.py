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
from typing import TextIO

try:
    import colorama
    colorama.init()
    is_colorama = True
except:
    is_colorama = False

# In EXCLUDED we put the modules we do not want to wrap
EXCLUDED = ['database','fft','global','keycodes','keyword','maths','messages','python','random','strings','vtk','wraps']
PRIMITIVES = {'integer': 'int', 'real': 'float','logical': 'bool','character': 'str'}
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
                modules[m].types[parent].childs.append(t)

def build_docs() -> None:

    if not os.path.isdir('./pages'):
        os.mkdir('./pages')

    build_entrypage()
    return None

def build_entrypage() -> None:
    if not os.path.isdir('./pages/types'):
        os.mkdir('./pages/types')

    with open('./pages/types.rst', 'w') as f:
        f.write("#####\n")
        f.write("Types\n")
        f.write("#####\n")
        f.write("\n")
        f.write("Description\n")
        f.write("-----------\n")
        f.write("\n")
        f.write("PyCrysFML08 dictionaries correspond to Fortran derived types and classes of CrysFML08. They can be created by calling the appropiate PyCrysFML08 function.\n")
        f.write("\n")
        f.write("Modules\n")
        f.write("-------\n")
        f.write("\n")
        f.write(".. toctree::\n")
        f.write("   :maxdepth: 1\n")
        f.write("\n")

        document_cfml_type_modules(f)

def document_cfml_type_modules(file: TextIO) -> None:
    for m_name in modules.keys():
        file.write(f"   ./types/py_{m_name.lower()}\n")
        if not os.path.isdir(f"./pages/types/py_{m_name.lower()}"):
            os.mkdir(f"./pages/types/py_{m_name.lower()}")

        with open(f'./pages/types/py_{m_name.lower()}.rst', 'w') as f:
            f.write("#"*(len(m_name)+3)+"\n")
            f.write(f"py_{m_name.lower()}\n")
            f.write("#"*(len(m_name)+3)+"\n")
            f.write(".. toctree::\n")
            f.write("   :titlesonly:\n")
            f.write("   :maxdepth: 1\n")
            f.write("\n")

            document_cfml_type_modules_dicts(m_name, f)

def document_cfml_type_modules_dicts(m_name: str, file: TextIO) -> None:
    for t_name in modules[m_name].types.keys():
        file.write(f"   ./py_{m_name.lower()}/{t_name}\n")
        with open(f"./pages/types/py_{m_name.lower()}/{t_name}.rst", "w") as f:
            f.write("#"*len(t_name)+"\n")
            f.write(f"{t_name}\n")
            f.write("#"*len(t_name)+"\n")
            f.write("\n")
            parent = modules[m_name].types[t_name].parent
            childs = modules[m_name].types[t_name].childs
            if parent:
                f.write(f"**Extends From:** :doc:`{modules[m_name].types[t_name].parent} <{modules[m_name].types[t_name].parent}>`\n")
            if childs:
                childlist = [f":doc:`{child} <{child}>`" for child in modules[m_name].types[t_name].childs]
                f.write(f"**Extended By:**  {', '.join(childlist)}\n")
            f.write("\n")
            f.write("Properties\n")
            f.write("----------\n")
            f.write(".. list-table::\n")
            f.write("   :header-rows: 1\n")
            f.write("\n")
            f.write(f"   * - Key\n")
            f.write(f"     - Value\n")
            f.write(f"     - Description\n")
            for c_name, c_val in modules[m_name].types[t_name].components.items():
                if c_val.ndim == 0:
                    if c_val.ftype.lower() not in PRIMITIVES:
                        pytype = 'dict'
                    else:
                        pytype = PRIMITIVES[c_val.ftype.lower()]
                else:
                    if c_val.ftype.lower() in NUMERICALS:
                        if c_val.ftype.lower() == 'integer':
                            dtype = 'int32'
                        elif c_val.ftype.lower() == 'real':
                            dtype = 'float32'
                        else:
                            dtype = 'complex?'
                        print(c_val.dim)
                        shape = ','.join(c_val.dim) 
                        pytype = f"ndarray: dtype={dtype}, shape=({shape})"
                    else:
                        pytype = ''
                        #my_lists.append(f"li_{c_val.name}")
                f.write(f"   * - {c_name}\n")
                f.write(f"     - {pytype}\n")
                f.write(f"     - Description\n")

            f.write("\n")
            f.write("Functions\n")
            f.write("---------\n")
            f.write(f"The following functions use **{t_name}** as an argument\n")


if __name__ == '__main__':

    run()
