"""
Python script for generating an API of the CrysFML08 library.
Author: Nebil A. Katcho
November 2022

---------
Functions
---------
fortran_types_to_python_dicts() -> None
get_cfml_modules_filenames() -> list
get_overloads(m_name : str,lines : list,n : int =0) -> int
get_procedures(m_name : str,lines : list,n : int =0) -> int
get_publics(m_name : str,lines : list,n : int =0) -> int)
get_types(m_name : str,lines : list,n : int =0) -> int
move_to_install() -> None
move_to_source() -> None
read_cfml_module(file_name : str) -> None
read_crysfml08() -> None
run() -> None
"""
import colorama
import cfml_objects
import glob
import os
import parser_utils

DIR_CRYSFML08 = 'C:\\Users\\katcho\\git\\CrysFML2008\\'
colorama.init()

modules = {}
is_read = False

def fortran_types_to_python_dicts() -> None:

    if not is_read:
        print(f"{colorama.Fore.RED}{'Error: CrysFML08 library must be read before writing the API.'}{colorama.Style.RESET_ALL}")
        raise IOError
    print('')
    move_to_install()
    print(f"{colorama.Fore.GREEN}{'Converting Fortran types in Python dictionaries'}{colorama.Style.RESET_ALL}")
    for m_name in modules.keys():
        print(f"{' ':>4}{colorama.Fore.GREEN}{'Module name: '}{colorama.Fore.CYAN}{m_name}{colorama.Style.RESET_ALL}")
        if len(modules[m_name].types.keys()) == 0:
            continue
        with open('API_'+m_name+'.py','w') as f:
            for t_name in modules[m_name].types.keys():
                print(f"{' ':>4}{colorama.Fore.GREEN}{'Converting '}{colorama.Fore.YELLOW}{'type' : <11}{colorama.Fore.CYAN}{t_name}{colorama.Style.RESET_ALL}")
                f.write(f"{'def create_'}{t_name}{'():'}\n")
                f.write('\n')
                t = modules[m_name].types[t_name]
                if t.parent:
                    f.write(f"{' ':>4}{'d = create_'}{t.parent}{'()'}\n")
                else:
                    f.write(f"{' ':>4}{'d = '}{'{}'}\n")
                f.write('\n')
                for c in t.components.keys():
                    left = "d['"+c+"']"
                    right = '= '+t.components[c].value
                    f.write(f"{' ':>4}{left:<35}{right}\n")
                for c in t.components.keys():
                    left = "d['ftype']['"+c+"']"
                    right = "= '"+t.components[c].fortran_type+"'"
                    f.write(f"{' ':>4}{left:<35}{right}\n")
                f.write('\n')
                f.write(f"{' ':>4}{'return d'}\n")
                f.write('\n')

def get_cfml_modules_filenames() -> list:

    cfml_modules_names = glob.glob('CFML*.f90')
    if len(cfml_modules_names) == 0:
        print(f"{colorama.Fore.RED}{'Error: No Fortran modules found. There is nothing to do. Bye bye.'}{colorama.Style.RESET_ALL}")
        raise IOError
    return cfml_modules_names

def get_overloads(m_name : str,lines : list,n : int =0) -> int:

    while n < len(lines):

        line = lines[n].lower()
        if line.startswith('contains'):
            return n-1
        if not line.strip().startswith('interface'):
            n += 1
            continue
        n,line = parser_utils.get_line(n,lines)
        i_name = parser_utils.get_interface_name(line)
        if i_name == '': # Starting the interface zone
            return n-1
        print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'interface' : <11}{colorama.Fore.CYAN}{i_name}{colorama.Style.RESET_ALL}")
        modules[m_name].interface[i_name] = cfml_objects.Interface(name=i_name)
        n = parser_utils.get_overload_procedures(n+1,lines,modules[m_name].interface[i_name])
        n += 1
    return n

def get_procedures(m_name : str,lines : list,n : int =0) -> int:

    while n < len(lines):

        n,line = parser_utils.get_line(n,lines)
        if parser_utils.is_procedure('function',line):
            f_name = parser_utils.get_function_name(line)
            if f_name in modules[m_name].publics:
                print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'function' : <11}{colorama.Fore.CYAN}{f_name}{colorama.Style.RESET_ALL}")
                modules[m_name].procedures[f_name] = cfml_objects.Function(name=f_name)
                parser_utils.get_arguments(line,modules[m_name].procedures[f_name])
                parser_utils.get_function_result(line,modules[m_name].procedures[f_name])
                n = parser_utils.get_function_types(n+1,lines,modules[m_name].procedures[f_name])
            else:
                n += 1
        elif parser_utils.is_procedure('subroutine',line):
            s_name = parser_utils.get_subroutine_name(line)
            if s_name in modules[m_name].publics:
                print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'subroutine' : <11}{colorama.Fore.CYAN}{s_name}{colorama.Style.RESET_ALL}")
                modules[m_name].procedures[s_name] = cfml_objects.Subroutine(name=s_name)
                parser_utils.get_arguments(line,modules[m_name].procedures[s_name])
                n = parser_utils.get_subroutine_types(n+1,lines,modules[m_name].procedures[s_name])
            else:
                n += 1
        else:
            n += 1
    return n

def get_publics(m_name : str,lines : list,n : int =0) -> int:

    while n < len(lines):
        line = lines[n].lower().strip()
        if line.startswith('type') or line.startswith('interface') or \
            line.startswith('contains') or line.startswith('end'):
            break
        if line.strip().startswith('public'):
            n,line = parser_utils.get_line(n,lines)
            line = line.lower().strip()
            i = line.find('::')
            for p in line[i+2:].split(','):
                modules[m_name].publics.append(p.strip())
        n += 1
    return n-1

def get_types(m_name : str,lines : list,n : int =0) -> int:

    while n < len(lines):
        line = lines[n].lower().strip()
        if line.startswith('interface') or line.startswith('contains'):
            return n-1
        if not line.startswith('type'):
            n += 1
            continue
        if line[4:].strip().startswith('('):
            n += 1
            continue
        n,line = parser_utils.get_line(n,lines)
        t_name = parser_utils.get_type_name(line)
        p_name = parser_utils.get_type_parent(line)
        print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'type' : <11}{colorama.Fore.CYAN}{t_name}{colorama.Style.RESET_ALL}")
        modules[m_name].types[t_name] = cfml_objects.FortranType(name=t_name,parent=p_name)
        n = parser_utils.get_type_components(n+1,lines,modules[m_name].types[t_name])
    return n

def move_to_install() -> None:

    # Create install directory if it doesn't exist
    os.chdir(DIR_CRYSFML08)
    if not os.path.isdir('API'):
        os.makedirs('API')
    print(f"{colorama.Fore.GREEN}{'Entering in API directory: '}{colorama.Fore.YELLOW}{os.path.join(DIR_CRYSFML08,'API')}{colorama.Style.RESET_ALL}")
    os.chdir('API')
    return None

def move_to_source() -> None:

    # Move to Crysfml08
    if not DIR_CRYSFML08:
        print(f"{colorama.Fore.RED}{'Error: variable DIR_CRYSFML08 must be set at the beginning of this script.'}{colorama.Style.RESET_ALL}")
        raise IOError
    if not os.path.isdir(DIR_CRYSFML08):
        print(f"{colorama.Fore.RED}{'Error: '}{colorama.Fore.YELLOW}{DIR_CRYSFML08}{colorama.Fore.RED}{' does not exist'}{colorama.Style.RESET_ALL}")
        raise IOError
    print(f"{colorama.Fore.GREEN}{'Entering in CrysFML08 directory: '}{colorama.Fore.YELLOW}{DIR_CRYSFML08}{colorama.Style.RESET_ALL}")
    os.chdir(DIR_CRYSFML08)

    # Move to Src\
    if not os.path.isdir('Src'):
        print(f"{colorama.Fore.RED}{'Error: Src directory not found in '}{DIR_CRYSFML08}{colorama.Style.RESET_ALL}")
        raise IOError
    print(f"{colorama.Fore.GREEN}{'Entering '}{colorama.Fore.YELLOW}{'Src'}{colorama.Fore.GREEN}{' directory'}{colorama.Style.RESET_ALL}")
    os.chdir('Src')
    return None

def read_cfml_module(file_name : str) -> None:

    print('')
    print(f"{colorama.Fore.GREEN}{'Reading file '}{colorama.Fore.CYAN}{file_name}{colorama.Style.RESET_ALL}")
    with open(file_name,'r') as f:
        lines = f.readlines()

    # Get module name
    try:
        m_name = parser_utils.get_module_name(lines)
    except Exception as e:
        print(f"{colorama.Fore.RED}{'Error: '}{e}{colorama.Style.RESET_ALL}")
        raise IOError
    modules[m_name] = cfml_objects.Module(name=m_name)
    print(f"{' ':>4}{colorama.Fore.GREEN}{'Module name: '}{colorama.Fore.CYAN}{m_name}{colorama.Style.RESET_ALL}")

    # Get public objects
    n = get_publics(m_name,lines)

    # Get types
    n = get_types(m_name,lines,n)

    # Get overloads
    n = get_overloads(m_name,lines,n)

    # Get procedures
    n = get_procedures(m_name,lines,n)

def read_crysfml08() -> None:

    global is_read
    move_to_source()
    cfml_modules_fnames = get_cfml_modules_filenames()
    for file_name in cfml_modules_fnames:
        read_cfml_module(file_name)
    is_read = True
    return None

def run() -> None:

    print(f"{' ' :>20}{colorama.Fore.GREEN}{'==========================='}{colorama.Style.RESET_ALL}")
    print(f"{' ' :>20}{colorama.Back.GREEN}{'API Generator for CRYSFML08'}{colorama.Style.RESET_ALL}")
    print(f"{' ' :>20}{colorama.Fore.GREEN}{'==========================='}{colorama.Style.RESET_ALL}")

    read_crysfml08()
    fortran_types_to_python_dicts()
    return None

if __name__ == '__main__':

    run()
