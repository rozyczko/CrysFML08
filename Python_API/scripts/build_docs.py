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

import os
from typing import TextIO
try:
    import colorama
    colorama.init()
    is_colorama = True
except:
    is_colorama = False

modules = {}

PRIMITIVES = {'integer': 'int','real': 'float','logical': 'bool','character': 'str','complex': 'TODEFINE'}
NUMERICALS = ['integer','real']

def build_docs(mod: dict) -> None:

    if not os.path.isdir('./pages'):
        os.mkdir('./pages')

    global modules
    modules = mod

    build_entrypage()
    return None

def build_entrypage() -> None:
    if not os.path.isdir('./pages/types'):
        os.mkdir('./pages/types')
    
    if not os.path.isdir('./pages/functions'):
        os.mkdir('./pages/functions')

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
    
    with open('./pages/functions.rst', 'w') as f:
        f.write("#########\n")
        f.write("Functions\n")
        f.write("#########\n")
        f.write("\n")
        f.write("Description\n")
        f.write("-----------\n")
        f.write("\n")
        f.write("PyCyrsFML08 functions correspond to Fortran procedures (functions and subroutines). They return a tuple. The first element of the tuple is an integer, the error code. If it is different from zero, it means that an error ocurred during the execution of the Fortran procedure. The second argument is a string, that contains the error message (value of the err_cfml%msg variable of CrysFML08). This variable is used to provide information about errors during the runtime. If the error code is zero, the string will be empty. The total number of elements of the tuple and their types depends on the particular function, described in the link below.\n")
        f.write("\n")
        f.write("Modules\n")
        f.write("-------\n")
        f.write("\n")
        f.write(".. toctree::\n")
        f.write("   :maxdepth: 1\n")
        f.write("\n")

        document_cfml_function_modules(f)

def document_cfml_function_modules(file: TextIO) -> None:
    for m_name in modules.keys():
        file.write(f"   ./functions/py_{m_name.lower()}\n")
        if not os.path.isdir(f"./pages/functions/py_{m_name.lower()}"):
            os.mkdir(f"./pages/functions/py_{m_name.lower()}")

        with open(f'./pages/functions/py_{m_name.lower()}.rst', 'w') as f:
            f.write("#"*(len(m_name)+3)+"\n")
            f.write(f"py_{m_name.lower()}\n")
            f.write("#"*(len(m_name)+3)+"\n")
            f.write("Functions\n")
            f.write("---------\n")
            f.write(".. list-table::\n")
            f.write("   :header-rows: 0\n")
            f.write("   :class: functionlist\n")
            f.write("\n")

            document_cfml_function_modules_dicts(m_name, f)

            f.write("\n")
            f.write(".. toctree::\n")
            f.write("   :hidden:\n")
            f.write("\n")
            for t_name in modules[m_name].procedures.keys():
                f.write(f"   ./py_{m_name.lower()}/{t_name}\n")

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

def get_immediate_parents_and_childs(m_name: str, t_name: str) -> tuple:
    old_parent = modules[m_name].types[t_name].lucy
    childs = modules[m_name].types[old_parent].childs
    
    level = next((child[1] for child in childs if child[0] == t_name), -1)
    parent = modules[m_name].types[t_name].parent
    child = [child[0] for child in childs if child[1] == int(level) + 1]

    return parent, child

def find_module_of_type(ftype: str) -> str:
    for m_name, modval in modules.items():
        for typval in modval.types.values():
            if typval.name == ftype:
                return m_name
   
    return ''

def document_cfml_type_modules_dicts(m_name: str, file: TextIO) -> None:
    for t_name in modules[m_name].types.keys():
        file.write(f"   ./py_{m_name.lower()}/{t_name}\n")
        with open(f"./pages/types/py_{m_name.lower()}/{t_name}.rst", "w") as f:
            f.write("#"*len(t_name)+"\n")
            f.write(f"{t_name}\n")
            f.write("#"*len(t_name)+"\n")
            f.write("\n")
            parent, childs = get_immediate_parents_and_childs(m_name, t_name)
            if parent:
                f.write(f"**Extends From:** :doc:`{parent} <{parent}>`\n")
            if childs:
                childlist = [f":doc:`{child} <{child}>`" for child in childs]
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
                        mod = find_module_of_type(c_val.ftype)
                        if mod:
                            pytype = f":doc:`{c_val.ftype} <../py_{mod.lower()}/{c_val.ftype}>`"
                        else:
                            print("Could not find module for Fortran Type, something bad happened...")
                            exit()
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
                        shape = ','.join(c_val.dim) 
                        pytype = f"ndarray: dtype={dtype}, shape=({shape})"
                    else:
                        if c_val.ftype.lower() not in PRIMITIVES:
                            mod = find_module_of_type(c_val.ftype)
                            if mod:
                                pytype = f"list[:doc:`{c_val.ftype} <../py_{mod.lower()}/{c_val.ftype}>`]"
                            else:
                                print("Could not find module for Fortran Type, something bad happened...")
                                exit()
                        else:
                            pytype = f"list[{PRIMITIVES[c_val.ftype.lower()]}]"

                f.write(f"   * - {c_name}\n")
                f.write(f"     - {pytype}\n")
                f.write(f"     - {c_val.info}\n")

            f.write("\n")
            f.write("Functions\n")
            f.write("---------\n")
            f.write(f"The following functions use **{t_name}** as an argument\n")

def get_arguments_of_functions(m_name: str, t_name: str) -> tuple:
    func = modules[m_name].procedures[t_name]
    arguments = [value.name for value in func.arguments.values() if (value.intent == 'in' or value.intent == 'inout') and not value.optional]
    return tuple(arguments)

def document_cfml_function_modules_dicts(m_name: str, file: TextIO) -> None:
    for t_name in modules[m_name].procedures.keys():
        args = get_arguments_of_functions(m_name, t_name)
        argstr = ', '.join(args)
        file.write(f"   * - :doc:`py_{m_name.lower()}.{t_name} <./py_{m_name.lower()}/{t_name}>` **({argstr})**\n")
        with open(f"./pages/functions/py_{m_name.lower()}/{t_name}.rst", "w") as f:
            f.write("#"*len(t_name)+"\n")
            f.write(f"{t_name}\n")
            f.write("#"*len(t_name)+"\n")
            f.write("\n")
            f.write("Function Call\n")
            f.write("-------------\n")
            f.write(".. code-block:: python\n")
            f.write(f"\n")
            f.write(f"   from pycrysfml08.py_{m_name.lower()} import {t_name}\n")
            f.write(f"\n")
            f.write(f"   r = {t_name}({argstr})\n")
            f.write(f"\n")
            f.write("Arguments\n")
            f.write("---------\n")
            f.write(".. list-table::\n")
            f.write("   :header-rows: 1\n")
            f.write("\n")
            f.write(f"   * - Argument\n")
            f.write(f"     - Type\n")
            f.write(f"     - Description\n")
            for c_name, c_val in modules[m_name].procedures[t_name].arguments.items():
                print(m_name, t_name, c_val.ftype, c_val.ndim, c_name)
                if c_val.ndim == 0:
                    if c_val.ftype.lower() not in PRIMITIVES:
                        mod = find_module_of_type(c_val.ftype)
                        if mod:
                            pytype = f":doc:`{c_val.ftype} <../../types/py_{mod.lower()}/{c_val.ftype}>`"
                        else:
                            print("Could not find module for Fortran Type, something bad happened...")
                            exit()
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
                        shape = ','.join(c_val.dim) 
                        pytype = f"ndarray: dtype={dtype}, shape=({shape})"
                    else:
                        if c_val.ftype.lower() not in PRIMITIVES:
                            mod = find_module_of_type(c_val.ftype)
                            if mod:
                                pytype = f"list[:doc:`{c_val.ftype} <../../types/py_{mod.lower()}/{c_val.ftype}>`]"
                            else:
                                print("Could not find module for Fortran Type, something bad happened...")
                                exit()
                        else:
                            pytype = f"list[{PRIMITIVES[c_val.ftype.lower()]}]"

                f.write(f"   * - {c_name}\n")
                f.write(f"     - {pytype}\n")
                f.write(f"     - {c_val.info}\n")



