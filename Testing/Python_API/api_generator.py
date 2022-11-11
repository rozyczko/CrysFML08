"""
Python script for generating an API of the CrysFML08 library.
Author: Nebil A. Katcho
November 2022

---------
Functions
---------
get_component(line)
get_procedure(line)
read()
run()
"""
import colorama
import cfml_objects
import glob
import os
import parser_utils

DIR_CRYSFML08 = 'C:\\Users\\katcho\\git\\CrysFML2008\\'
colorama.init()

modules = {}

def get_cfml_modules_filenames():

    cfml_modules_names = glob.glob('CFML*.f90')
    if len(cfml_modules_names) == 0:
        print(f"{colorama.Fore.RED}{'Error: No Fortran modules found. There is nothing to do. Bye bye.'}")
        raise SystemExit
    return cfml_modules_names

def get_types(m_name,lines):

    n = 0
    while n < len(lines):
        line = lines[n].lower().strip()
        if line.startswith('interface') or line.startswith('contains'):
            break
        if not line.startswith('type'):
            n += 1
            continue
        if line[4:].strip().startswith('('):
            n += 1
            continue
        n,line = parser_utils.get_line(n,lines)
        t_name = parser_utils.get_type_name(line)
        p_name = parser_utils.get_type_parent(line)
        modules[m_name].types[t_name] = cfml_objects.XType(name=t_name,parent=p_name)
        print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'type' : <11}{colorama.Fore.CYAN}{t_name}{colorama.Style.RESET_ALL}")
        n = parser_utils.get_type_components(n+1,lines,modules[m_name].types[t_name])

def move_to_source():

    # Move to Crysfml08
    if not DIR_CRYSFML08:
        print(f"{colorama.Fore.RED}{'Error: variable DIR_CRYSFML08 must be set at the beginning of this script.'}")
        raise SystemExit
    if not os.path.isdir(DIR_CRYSFML08):
        print(f"{colorama.Fore.RED}{'Error: '}{colorama.Fore.YELLOW}{DIR_CRYSFML08}{colorama.Fore.RED}{' does not exist'}{colorama.Style.RESET_ALL}")
        raise SystemExit
    print(f"{colorama.Fore.GREEN}{'Entering in CrysFML08 directory: '}{colorama.Fore.YELLOW}{DIR_CRYSFML08}{colorama.Style.RESET_ALL}")
    os.chdir(DIR_CRYSFML08)

    # Move to Src\
    if not os.path.isdir('Src'):
        print(f"{colorama.Fore.RED}{'Error: Src directory not found in '}{DIR_CRYSFML08}{colorama.Style.RESET_ALL}")
        raise SystemExit
    print(f"{colorama.Fore.GREEN}{'Entering '}{colorama.Fore.YELLOW}{'Src'}{colorama.Fore.GREEN}{' directory'}{colorama.Style.RESET_ALL}")
    os.chdir('Src')

def read_cfml_module(file_name):

    print('')
    print(f"{colorama.Fore.GREEN}{'Reading file '}{colorama.Fore.CYAN}{file_name}{colorama.Style.RESET_ALL}")
    with open(file_name,'r') as f:
        lines = f.readlines()

    # Get module name
    try:
        m_name = parser_utils.get_module_name(lines)
    except Exception as e:
        print(f"{colorama.Fore.RED}{'Error: '}{e}{colorama.Style.RESET_ALL}")
        raise SystemExit
    modules[m_name] = cfml_objects.Module(name=m_name)
    print(f"{colorama.Fore.GREEN}{'  Module name: '}{colorama.Fore.CYAN}{m_name}{colorama.Style.RESET_ALL}")

    # Get types
    get_types(m_name,lines)

    # Get interfaces
    #n = 0
    #for line in lines:
    #    line = line.lower()
    #    l = line.split()
    #    if len(l) == 1 and l[0] == 'interface':
    #        break
    #    if len(l) == 2:
    #        if l[0] == 'interface':
    #            i_name = l[1]
    #            print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'interface' : <11}{colorama.Fore.CYAN}{i_name}{colorama.#Style.RESET_ALL}")
    #            modules[m_name].inter[i_name] = cfml_objects.Interface(name=i_name)
    #            # Get the procedures
    #            in_interface = True
    #            i = n + 1
    #            while in_interface:
    #                line = lines[i].lower()
    #                l = line.split()
    #                if len(l) > 0:
    #                    if l[0] == 'end' or  l[0] == 'endinterface':
    #                        in_interface = False
    #                        break
    #                    else:
    #                        p = get_procedure(line)
    #                        if p is not None:
    #                            modules[m_name].inter[i_name].procs.append(p)
    #                i += 1
    #                line = lines[i].lower()
    #                l = line.split()
    #    n += 1

    # Get functions and subroutines in interfaces
    #n = 0
    #for line in lines:
    #    if parser_utils.is_procedure('function',line):
    #        pass
    #        #print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'function' : <11}{colorama.Fore.CYAN}{f_name}{colorama.#Style.#RESET_ALL}")
    #        #parser_utils.get_function(n,lines)
    #n = 0
    #for line in lines:
    #    line = line.lower()
    #    l = line.split()
    #    if len(l) == 1 and l[0] == 'interface':
    #        break
    #    n += 1
    #while n < len(lines):
    #    line = lines[n].lower()
    #    ii = line.find('module')
    #    jj = line.find('function')
    #    kk = line.find('subroutine')
    #    if ii > -1:
    #        if jj > -1 and line.strip()[0] != '!':
    #            in_function = True
    #            # Get function name
    #            ll = line[jj:].find('(')
    #            f_name = line[jj+8:jj+ll].strip()
    #            modules[m_name].procs[f_name] = cfml_objects.Function()
    #            print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'function' : <11}{colorama.Fore.CYAN}{f_name}{colorama.#Style.RESET_ALL}")
    #            # Get arguments
    #            mm = line[jj:].find(')')
    #            for a in line[jj+ll+1:jj+mm].split(','):
    #                modules[m_name].procs[f_name].arguments[a.strip()] = cfml_objects.Argument(name=a.strip())
    #            # Get result
#
    #            while in_function:
    #                in_interface = False
    #                line = lines[n].lower()
    #                ii = line.find('interface')
    #                if ii > 1:
    #                    in_interface = True
    #                    while (in_interface):
    #                        n += 1
    #                        l = lines[n].lower()
    #                        jj = l.find('end')
    #                        if jj > -1:
    #                            jj = l.find('interface')
    #                            if jj > -1:
    #                                in_interface = False
    #                else:
    #                    l = line.split()
    #                    if len(l) > 0:
    #                        if l[0] == 'end' or  l[0] == 'endfunction':
    #                            in_function = False
    #                        else:
    #                            c = get_component(line)
    #                            if c is not None:
    #                                for nam in c[0]:
    #                                    try:
    #                                        modules[m_name].procs[f_name].arguments[nam].xtype = c[1]
    #                                        modules[m_name].procs[f_name].arguments[nam].value = c[2]
    #                                        modules[m_name].procs[f_name].arguments[nam].info  = c[3]
    #                                        modules[m_name].procs[f_name].arguments[nam].dim   = c[4]
    #                                    except KeyError:
    #                                        pass
    #                n += 1
    #        elif kk > -1 and line.strip()[0] != '!':
    #            in_subroutine = True
    #            # Get subroutine name
    #            ll = line[kk:].find('(')
    #            s_name = line[kk+10:kk+ll].strip()
    #            modules[m_name].procs[s_name] = cfml_objects.Subroutine()
    #            print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'subroutine' : <11}{colorama.Fore.CYAN}{s_name}#{colorama.Style.RESET_ALL}")
    #            # Get arguments
    #            mm = line.find('&')
    #            while mm > -1:
    #                n += 1
    #                line = line + lines[n].lower()
    #                mm = lines[n].find('&')
    #            line = line.replace('\n',' ')
    #            line = line.replace('&',' ')
    #            mm = line[kk:].find(')')
    #            for a in line[kk+ll+1:kk+mm].split(','):
    #                modules[m_name].procs[s_name].arguments[a.strip()] = cfml_objects.Argument(name=a.strip())
    #            n += 1
    #            while in_subroutine:
    #                in_interface = False
    #                line = lines[n].lower()
    #                ii = line.find('interface')
    #                if ii > -1:
    #                    in_interface = True
    #                    while (in_interface):
    #                        n += 1
    #                        l = lines[n].lower()
    #                        jj = l.find('end')
    #                        if jj > -1:
    #                            jj = l.find('interface')
    #                            if jj > -1:
    #                                in_interface = False
    #                else:
    #                    l = line.split()
    #                    if len(l) > 0:
    #                        if l[0] == 'end' or  l[0] == 'endsubroutine':
    #                            in_subroutine = False
    #                        else:
    #                            c = get_component(line)
    #                            if c is not None:
    #                                for nam in c[0]:
    #                                    modules[m_name].procs[s_name].arguments[nam].xtype = c[1]
    #                                    modules[m_name].procs[s_name].arguments[nam].value = c[2]
    #                                    modules[m_name].procs[s_name].arguments[nam].info  = c[3]
    #                                    modules[m_name].procs[s_name].arguments[nam].dim   = c[4]
    #                n += 1
    #    n += 1


def read_crysfml08():

    move_to_source()
    cfml_modules_fnames = get_cfml_modules_filenames()
    for file_name in cfml_modules_fnames:
        read_cfml_module(file_name)

def run():

    print(f"{' ' :>20}{colorama.Fore.GREEN}{'==========================='}{colorama.Style.RESET_ALL}")
    print(f"{' ' :>20}{colorama.Back.GREEN}{'API Generator for CRYSFML08'}{colorama.Style.RESET_ALL}")
    print(f"{' ' :>20}{colorama.Fore.GREEN}{'==========================='}{colorama.Style.RESET_ALL}")

    read_crysfml08()

if __name__ == '__main__':

    run()
