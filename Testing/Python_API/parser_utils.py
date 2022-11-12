"""
Utilities for parsing CrysFML08

---------
Functions
---------
get_component(line : str) -> tuple
get_function_name(lines : list) -> str
get_interface_name(line : str) -> str
get_line(n : int, lines : list) -> tuple
get_module_name(lines : list) -> str
get_overload_procedures(n : int, lines: list, t : cfml_objects.Interface) -> int
get_procedure(line : str) -> str
get_subroutine_arguments(line: str, s : cfml_objects.Subroutine) -> None
get_subroutine_name(lines : list) -> str
get_type_components(n : int, lines: list, t : cfml_objects.XType) -> int
get_type_name(line : str) -> str
get_type_parent(line : str) -> str
is_empty(line : str) -> bool
is_procedure(procedure : str,line : str) -> bool

"""
import cfml_objects

def get_arguments(line: str, s : cfml_objects.Subroutine) -> None:

    line = line.lower()
    i = line.find('(')
    j = line.find(')')
    l = line[i+1:j].split(',')
    for arg in l:
        name = arg.strip()
        s.arguments[name] = cfml_objects.Argument(name=name)

    return None

def get_component(line : str) -> tuple:

    # Defaults
    c_value = None
    c_info = ''
    c_dim = '(0)'

    # Type
    i = line.index('::')
    c_type = line[:i].replace(' ','')

    # Name, value and info
    j = line[:].find('!')
    if j > -1: # Info is given
        c_info = line[j+1:].strip()
        k = line[i:j].find('=')
        if k > -1: # Value is given
            c_name = line[i+2:i+k].strip()
            c_value = line[i+k+1:j].strip()
        else: # No value
            c_name = line[i+2:j].strip()
    else: # No info
        k = line[i:].find('=')
        if k > -1: # Value is given
            c_name = line[i+2:i+k].strip()
            c_value = line[i+k+1:].strip()
        else: # No value
            c_name = line[i+2:].strip()

    # Get the dimension
    i = c_type.find('dimension')
    if i > -1:
        j = c_type[i:].index('(')
        k = c_type[i:].index(')')
        c_dim = c_type[i+j:i+k+1]
    else:
        c_dim = '(0)'

    # More than one variable can be given in the same line
    names = []
    for n in c_name.split(','):
        names.append(n.strip())
    return (names,c_type,c_value,c_info,c_dim)

def get_function_name(line : str) -> str:

    line = line.lower()
    i = line.find('function')
    if i < 0:
        return ''
    else:
        j = line.find('(')
        return line[i+8:j].strip()

def get_interface_name(line : str) -> str:

    line = line.lower()
    l = line.split()
    if len(l) < 2:
        return ''
    else:
        return l[1]

def get_line(n : int, lines : list) -> tuple:

    i = lines[n].find('&')
    if i < 0:
        return (n,lines[n])
    else:
        line = lines[n]
        while i > -1:
            if line.strip()[-1] == '&' and line.find('!') == -1:
                n += 1
                line = line + lines[n]
                i = line.find('&')
            else:
                i = -1
        line = line.replace('\n',' ')
        line = line.replace('&',' ')
        return (n,line)

def get_module_name(lines : list) -> str:

    for line in lines:
        if is_empty(line):
            continue
        line = line.lower()
        l = line.split()
        if l[0] == 'module':
            return l[1]
    raise Exception('Module name not found')

def get_overload_procedures(n : int, lines: list, i : cfml_objects.Interface) -> int:

    in_interface = True
    while in_interface:
        n,line = get_line(n,lines)
        if is_empty(line) or line.strip().startswith('!'):
            n += 1
            continue
        line = line.lower().strip()
        l = line.split()
        if l[0] == 'end' or l[0] == 'endinterface':
            in_interface = False
        else:
            p = get_procedure(line)
            i.procedures.append(p)
        n += 1
    return n

def get_procedure(line : str) -> str:

    line = line.lower()
    j = line.find('!')
    if j > -1:
        p = line[:j-1].split()[-1]
    else:
        p = line.split()[-1]
    return p

def get_subroutine_name(line : str) -> str:

    line = line.lower()
    i = line.find('subroutine')
    if i < 0:
        return ''
    else:
        j = line.find('(')
        return line[i+10:j].strip()

def get_type_components(n : int, lines: list, t : cfml_objects.XType) -> int:

    in_type = True
    while in_type:
        n,line = get_line(n,lines)
        if is_empty(line) or line.strip().startswith('!'):
            n += 1
            continue
        line = line.lower().strip()
        l = line.split()
        if l[0] == 'end' or l[0] == 'endtype':
            in_type = False
        else:
            c = get_component(line)
            for nam in c[0]:
                t.components[nam] = cfml_objects.Type_Component(name=nam,xtype=c[1],value=c[2],info=c[3],dim=c[4])
        n += 1
    return n

def get_type_name(line : str) -> str:

    line = line.lower()
    i = line.find('::')
    return line[i+2:].split()[0]

def get_type_parent(line : str) -> str:

    line = line.lower()
    i = line.find('extends')
    if i > -1:
        j = line[i:].find('(')
        k = line[i:].find(')')
        return(line[i+j+1:i+k].strip())
    else:
        return('')

def is_empty(line : str) -> bool:

    if len(line) == 0 or line.isspace():
        return True
    else:
        return False

def is_procedure(procedure : str,line : str) -> bool:

    line = line.lower()
    i = line.find(procedure)
    if i < 0:
        return False
    line = ' '.join(line.split())
    if line.startswith(procedure) or \
       line.startswith('elemental '+ procedure) or \
       line.startswith('module '+ procedure) or \
       line.startswith('pure '+ procedure) or \
       line.startswith('elemental module '+ procedure) or \
       line.startswith('pure module '+ procedure):
        return True
    else:
        return False