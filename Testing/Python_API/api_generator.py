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

DIR_CRYSFML08 = 'C:\\Users\\katcho\\git\\CrysFML2008\\'
colorama.init()

modules = {}

def get_component(line):

    try:
        j = line.index('::')
        c_type = line[:j].replace(' ','')
        c_value = 'None'
        c_info = ''
        k = line[:].find('!')
        if k > -1 :
            c_info = line[k+1:].strip()
            l = line[j+2:k].find('=')
            if l > -1:
                c_name = line[j+2:l].strip()
                c_value = line[j+l+2:k]
            else:
                c_name = line[j+2:k].strip()
        else:
            l = line[j+2:].find('=')
            if l > -1:
                c_name = line[j+2:l].strip()
                c_value = line[j+l+2:]
            else:
                c_name = line[j+2:].strip()
        # Get the dimension
        try:
            m = c_type.index('dimension')
            j = c_type[m:].index('(')
            k = c_type[m:].index(')')
            c_dim = c_type[m+j:m+k+1]
        except:
            c_dim = '(0)'
        names = []
        for n in c_name.split(','):
            names.append(n.strip())
        return (names,c_type,c_value,c_info,c_dim)
    except ValueError:
        return None

def get_procedure(line):

    if not line:
        return None
    try:
        j = line.index('!')
        p = line[:j-1].split()[-1]
    except ValueError:
        p = line.split()[-1]
    return p

def read():

    # Check that the variable DIR_CRYSFML08 has been set and it exists
    if not DIR_CRYSFML08:
        print(f"{colorama.Fore.RED}{'Error: variable DIR_CRYSFML08 must be set at the beginning of this script.'}")
        raise SystemExit
    if not os.path.isdir(DIR_CRYSFML08):
        print(f"{colorama.Fore.RED}{'Error: '}{colorama.Fore.YELLOW}{DIR_CRYSFML08}{colorama.Fore.RED}{' does not exist'}{colorama.Style.RESET_ALL}")
        raise SystemExit
    print(f"{colorama.Fore.GREEN}{'Entering in CrysFML08 directory: '}{colorama.Fore.YELLOW}{DIR_CRYSFML08}{colorama.Style.RESET_ALL}")
    os.chdir(DIR_CRYSFML08)

    # Check that Src directory exists
    if not os.path.isdir('Src'):
        print(f"{colorama.Fore.RED}{'Error: Src directory not found in '}{DIR_CRYSFML08}{colorama.Style.RESET_ALL}")
        raise SystemExit
    print(f"{colorama.Fore.GREEN}{'Entering '}{colorama.Fore.YELLOW}{'Src'}{colorama.Fore.GREEN}{' directory'}{colorama.Style.RESET_ALL}")
    os.chdir('Src')

    # Get CFML modules
    cfml_modules_names = glob.glob('CFML*.f90')
    if len(cfml_modules_names) == 0:
        print(f"{colorama.Fore.RED}{'Error: No Fortran modules found. There is nothing to do. Bye bye.'}")
        raise SystemExit
    for nam in cfml_modules_names:
        print('')
        print(f"{colorama.Fore.GREEN}{'Reading module '}{colorama.Fore.CYAN}{nam}{colorama.Style.RESET_ALL}")
        with open(nam,'r') as f:
            lines = f.readlines()
        m_name = ''

        # Get module name
        for line in lines:
            l = line.split()
            if len(l) > 0:
                if l[0].lower() == 'module':
                    m_name = l[1].lower()
                    modules[m_name] = cfml_objects.Module(name=m_name)
                    break
        if not m_name:
            raise Exception('  Unable to process module. Keyword "Module" not found.')

        # Get types
        n = 0
        for line in lines:
            line = line.lower()
            l = line.split()
            if len(l) > 0:
                if l[0][0:4] == 'type':
                    # Check if the type is public
                    try:
                        i = line.index('public')
                    except ValueError:
                        i = 0
                    if i > 0:
                        # Get the type name
                        i = line.index('::')
                        t_name = line[i+2:].split()[0]
                        modules[m_name].types[t_name] = cfml_objects.XType(name=t_name)
                        print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'type' : <11}{colorama.Fore.CYAN}{t_name}{colorama.Style.RESET_ALL}")
                        # Get the components
                        in_type = True
                        i = n + 1
                        while in_type:
                            line = lines[i].lower()
                            l = line.split()
                            if len(l) > 0:
                                if l[0] == 'end' or  l[0] == 'endtype':
                                    in_type = False
                                    break
                                else:
                                    c = get_component(line)
                                    if c is not None:
                                        for nam in c[0]:
                                            modules[m_name].types[t_name].components[nam] = cfml_objects.Type_Component(name=nam,xtype=c[1],value=c[2],info=c[3],dim=c[4])
                            i += 1
                            line = lines[i].lower()
                            l = line.split()
            n += 1

        # Get interfaces
        n = 0
        for line in lines:
            line = line.lower()
            l = line.split()
            if len(l) == 1 and l[0] == 'interface':
                break
            if len(l) == 2:
                if l[0] == 'interface':
                    i_name = l[1]
                    print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'interface' : <11}{colorama.Fore.CYAN}{i_name}{colorama.Style.RESET_ALL}")
                    modules[m_name].inter[i_name] = cfml_objects.Interface(name=i_name)
                    # Get the procedures
                    in_interface = True
                    i = n + 1
                    while in_interface:
                        line = lines[i].lower()
                        l = line.split()
                        if len(l) > 0:
                            if l[0] == 'end' or  l[0] == 'endinterface':
                                in_interface = False
                                break
                            else:
                                p = get_procedure(line)
                                if p is not None:
                                    modules[m_name].inter[i_name].procs.append(p)
                        i += 1
                        line = lines[i].lower()
                        l = line.split()
            n += 1

        # Get functions and subroutines
        n = 0
        for line in lines:
            line = line.lower()
            l = line.split()
            if len(l) == 1 and l[0] == 'interface':
                break
            n += 1
        while n < len(lines):
            line = lines[n].lower()
            ii = line.find('module')
            jj = line.find('function')
            kk = line.find('subroutine')
            if ii > -1:
                if jj > -1 and line.strip()[0] != '!':
                    in_function = True
                    # Get function name
                    ll = line[jj:].find('(')
                    f_name = line[jj+8:jj+ll].strip()
                    modules[m_name].procs[f_name] = cfml_objects.Function()
                    print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'function' : <11}{colorama.Fore.CYAN}{f_name}{colorama.Style.RESET_ALL}")
                    # Get arguments
                    mm = line[jj:].find(')')
                    for a in line[jj+ll+1:jj+mm].split(','):
                        modules[m_name].procs[f_name].arguments[a.strip()] = cfml_objects.Argument(name=a.strip())

                    while in_function:
                        in_interface = False
                        line = lines[n].lower()
                        ii = line.find('interface')
                        if ii > 1:
                            in_interface = True
                            while (in_interface):
                                n += 1
                                l = lines[n].lower()
                                jj = l.find('end')
                                if jj > -1:
                                    jj = l.find('interface')
                                    if jj > -1:
                                        in_interface = False
                        else:
                            l = line.split()
                            if len(l) > 0:
                                if l[0] == 'end' or  l[0] == 'endfunction':
                                    in_function = False
                                else:
                                    c = get_component(line)
                                    if c is not None:
                                        for nam in c[0]:
                                            try:
                                                modules[m_name].procs[f_name].arguments[nam].xtype = c[1]
                                                modules[m_name].procs[f_name].arguments[nam].value = c[2]
                                                modules[m_name].procs[f_name].arguments[nam].info  = c[3]
                                                modules[m_name].procs[f_name].arguments[nam].dim   = c[4]
                                            except KeyError:
                                                pass
                        n += 1
                elif kk > -1 and line.strip()[0] != '!':
                    in_subroutine = True
                    # Get subroutine name
                    ll = line[kk:].find('(')
                    s_name = line[kk+10:kk+ll].strip()
                    modules[m_name].procs[s_name] = cfml_objects.Subroutine()
                    print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'subroutine' : <11}{colorama.Fore.CYAN}{s_name}{colorama.Style.RESET_ALL}")
                    # Get arguments
                    mm = line.find('&')
                    while mm > -1:
                        n += 1
                        line = line + lines[n].lower()
                        mm = lines[n].find('&')
                    line = line.replace('\n',' ')
                    line = line.replace('&',' ')
                    mm = line[kk:].find(')')
                    for a in line[kk+ll+1:kk+mm].split(','):
                        modules[m_name].procs[s_name].arguments[a.strip()] = cfml_objects.Argument(name=a.strip())
                    n += 1
                    while in_subroutine:
                        in_interface = False
                        line = lines[n].lower()
                        ii = line.find('interface')
                        if ii > -1:
                            in_interface = True
                            while (in_interface):
                                n += 1
                                l = lines[n].lower()
                                jj = l.find('end')
                                if jj > -1:
                                    jj = l.find('interface')
                                    if jj > -1:
                                        in_interface = False
                        else:
                            l = line.split()
                            if len(l) > 0:
                                if l[0] == 'end' or  l[0] == 'endsubroutine':
                                    in_subroutine = False
                                else:
                                    c = get_component(line)
                                    if c is not None:
                                        for nam in c[0]:
                                            modules[m_name].procs[s_name].arguments[nam].xtype = c[1]
                                            modules[m_name].procs[s_name].arguments[nam].value = c[2]
                                            modules[m_name].procs[s_name].arguments[nam].info  = c[3]
                                            modules[m_name].procs[s_name].arguments[nam].dim   = c[4]
                        n += 1
            n += 1

def run():

    print(f"{' ' :>20}{colorama.Fore.GREEN}{'==========================='}{colorama.Style.RESET_ALL}")
    print(f"{' ' :>20}{colorama.Back.GREEN}{'API Generator for CRYSFML08'}{colorama.Style.RESET_ALL}")
    print(f"{' ' :>20}{colorama.Fore.GREEN}{'==========================='}{colorama.Style.RESET_ALL}")

    read()

if __name__ == '__main__':

    run()
