"""
Utilities for parsing CrysFML08

---------
Functions
---------
build_var_def(var : cfml_objects.FortranVar) -> str
cast_python(val : str) -> str
get_arguments(line : str, s : cfml_objects.Subroutine) -> None:
get_function_name(lines : list) -> str
get_function_result(line : str, s : cfml_objects.Function) -> None:
get_function_types(n : int, lines : list, f : cfml_objects.Function) -> int
get_intent(line : str) -> str
get_interface_name(line : str) -> str
get_len(ftype : str) -> str
get_line(n : int, lines : list) -> tuple
get_module_name(lines : list) -> str
get_overload_procedures(n : int, lines : list, t : cfml_objects.Interface) -> int
get_procedure(line : str) -> str
get_subroutine_arguments(line : str, s : cfml_objects.Subroutine) -> None
get_subroutine_name(lines : list) -> str
get_subroutine_types(n : int, lines : list, s : cfml_objects.Subroutine) -> int
get_type_components(n : int, lines : list, t : cfml_objects.FortranType) -> int
get_type_name(line : str) -> str
get_type_parent(line : str) -> str
is_array(dim : str) -> bool
is_empty(line : str) -> bool
is_optional(line : str) -> bool
is_primitive(line : str) -> bool
is_procedure(procedure : str,line : str) -> bool
parse_var(line : str) -> tuple
"""
import cfml_objects
import colorama

def build_var_def(var : cfml_objects.FortranVar) -> str:

    if var.ftype == 'integer' or var.ftype == 'real' or var.ftype == 'logical' or var.ftype == 'complex':
        var_def = var.ftype
        if var.kind:
            var_def = var_def + '(kind=' + var.kind + ')'
    elif var.ftype == 'character':
        var_def = var.ftype +'(len='+var.len+')'
    elif var.is_class:
        var_def = 'class('+var.ftype+')'
    else:
        var_def = 'type('+var.ftype+')'
    if var.ndim > 0:
        var_def = var_def + ',dimension(' + ','.join(var.dim) + ')'
    if var.allocatable:
        var_def = var_def + ',allocatable'
    return var_def

def cast_python(val : str) -> str:

    if val == '.false.':
        v = 'False'
    elif val == '.true.':
        v = 'True'
    elif val.find('_cp') > -1:
        v = val.replace('_cp','')
    else:
        v = val
    return v

def get_arguments(line: str, s : cfml_objects.Subroutine) -> None:

    line = line.lower()
    i = line.find('(')
    j = line.find(')')
    l = line[i+1:j].split(',')
    for arg in l:
        name = arg.strip()
        s.arguments[name] = cfml_objects.FortranVar(name,'')
    return None

def get_function_name(line : str) -> str:

    line = line.lower()
    i = line.find('function')
    if i < 0:
        return ''
    else:
        j = line.find('(')
        return line[i+8:j].strip()

def get_function_result(line: str, f : cfml_objects.Function) -> None:

    line = line.lower()
    i = line.find('result')
    j = line[i:].find('(')
    k = line[i:].find(')')
    f.xreturn = cfml_objects.FortranVar(line[i+j+1:i+k].strip(),'')

    return None

def get_function_types(n : int, lines : list, f : cfml_objects.Function) -> int:

    in_function = True
    while in_function:
        n,line = get_line(n,lines)
        if is_empty(line) or line.strip().startswith('!'):
            n += 1
            continue
        line = line.lower()
        i = line.find('::')
        if i > -1:
            v = parse_var(line)
            if 'intent' in v[2].keys():
                for var_name in v[0]:
                    f.arguments[var_name] = cfml_objects.FortranVar(var_name,v[1],**v[2])
            else:
                f.xreturn = cfml_objects.FortranVar(v[0][0],v[1],**v[2])
        else:
            in_function = False
        n += 1
    return n

def get_intent(line : str) -> str:

    line = line.lower()
    i = line.find('intent')
    if i < 0:
        intent = ''
    else:
        i = i+6
        j = line[i:].find('(')
        k = line[i:].find(')')
        m = line[i+j+1:i+k].find('in')
        n = line[i+j+1:i+k].find('out')
        if m > -1 and n > -1:
            intent = 'inout'
        elif m > -1:
            intent = 'in'
        else:
            intent = 'out'
    return intent

def get_interface_name(line : str) -> str:

    line = line.lower()
    l = line.split()
    if len(l) < 2:
        return ''
    else:
        return l[1]

def get_len(ftype : str) -> str:

    i = ftype.find('len=')
    if i > 0:
        j = ftype[i:].find(')')
        if j > 0:
            return ftype[i+4:i+j]
        else:
            return ''
    else:
        return ''

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

def get_overload_procedures(n : int, lines : list, i : cfml_objects.Interface) -> int:

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

def get_subroutine_types(n : int, lines : list, s : cfml_objects.Subroutine) -> int:

    in_subroutine = True
    while in_subroutine:
        n,line = get_line(n,lines)
        if is_empty(line) or line.strip().startswith('!'):
            n += 1
            continue
        line = line.lower()
        if line.strip().startswith('interface'):
            s.has_interface = True
            in_interface = True
            while in_interface:
                n,line = get_line(n,lines)
                if is_empty(line) or line.strip().startswith('!'):
                    n += 1
                    continue
                line = line.lower()
                line = ' '.join(line.split())
                if line.startswith('end interface'):
                    in_interface = False
                n += 1
        n,line = get_line(n,lines)
        line = line.lower()
        i = line.find('::')
        if i > -1:
            v = parse_var(line)
            for var_name in v[0]:
                s.arguments[var_name] = cfml_objects.FortranVar(var_name,v[1],**v[2])
            n += 1
        else:
            in_subroutine = False
    return n

def get_type_components(n : int, lines: list, t : cfml_objects.FortranType) -> int:

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
            v = parse_var(line)
            for var_name in v[0]:
                t.components[var_name] = cfml_objects.FortranVar(var_name,v[1],**v[2])
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

def is_array(dim : str) -> bool:

    if dim.strip() == '0':
        return False
    else:
        return True

def is_empty(line : str) -> bool:

    if len(line) == 0 or line.isspace():
        return True
    else:
        return False

def is_optional(line : str) -> bool:

    optional = False
    line = line.strip().lower()
    i = line.find('optional')
    if i > -1:
        optional = True
    return optional

def is_primitive(line : str) -> bool:

    line = line.strip().lower()
    if line.startswith('integer') or \
        line.startswith('real') or \
        line.startswith('character') or \
        line.startswith('logical'):
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

def parse_var(line : str) -> list:

    var_names = ''
    var_optionals = {}

    # Fortran Type
    i = line.index('::')
    var_type = line[:i].replace(' ','')
    if var_type.startswith('type'):
        m = var_type.find('(')
        n = var_type.find(')')
        var_type = var_type[m+1:n]
    elif var_type.startswith('class'):
        m = var_type.find('(')
        n = var_type.find(')')
        var_type = var_type[m+1:n]
        var_optionals['is_class'] = True
    elif var_type.startswith('integer'):
        var_type = 'integer'
    elif var_type.startswith('real'):
        var_type = 'real'
    elif var_type.startswith('complex'):
        var_type = 'complex'
    elif var_type.startswith('character'):
        var_type = 'character'
        ii = line[:i].find('len')
        if ii > -1:
            j = line[ii:i].index('=')
            k = line[ii:i].index(')')
            var_optionals['len'] = line[ii+j+1:ii+k].strip()
        else:
            print(f"{colorama.Fore.RED}Error: character variable without len attribute{colorama.Style.RESET_ALL}")
            raise IOError
    elif var_type.startswith('logical'):
        var_type = 'logical'
    else:
        print(line)
        print(f"{colorama.Fore.RED}Error: unable to parse type {var_type}{colorama.Style.RESET_ALL}")
        raise IOError

    # Name, value and info
    i = line.index('::')
    j = line[:].find('!')
    if j > -1: # Info is given
        var_optionals['info'] = line[j+1:].strip()
        k = line[i:j].find('=')
        if k > -1: # Value is given
            var_names = line[i+2:i+k].strip()
            var_optionals['value'] = cast_python(line[i+k+1:j].strip())
        else: # No value
            var_names = line[i+2:j].strip()
    else: # No info
        k = line[i:].find('=')
        if k > -1: # Value is given
            var_names = line[i+2:i+k].strip()
            var_optionals['value'] = cast_python(line[i+k+1:].strip())
        else: # No value
            var_names = line[i+2:].strip()
    if not var_names:
        print(f"{colorama.Fore.RED}Error: unable to parse variable. Variable name not found{colorama.Style.RESET_ALL}")
        raise IOError

    # Kind
    i = line.index('::')
    ii = line[:i].find('kind')
    if ii > -1:
        j = line[ii:i].index('=')
        k = line[ii:i].index(')')
        var_optionals['kind'] = line[ii+j+1:ii+k].strip()

    # Dimension
    ii = line[:i].find('dimension')
    if ii > -1:
        j = line[ii:i].index('(')
        k = line[ii:i].index(')')
        var_optionals['dim'] = line[ii+j+1:ii+k].split(',')
        var_optionals['ndim'] = len(var_optionals['dim'])

    # Intent
    ii = line[:i].find('intent')
    if ii > -1:
        j = line[ii:i].index('(')
        k = line[ii:i].index(')')
        var_optionals['intent'] = ''.join(line[ii+j+1:ii+k].split())

    # Allocatable
    ii = line[:i].find('allocatable')
    if ii > -1:
        var_optionals['allocatable'] = True

    for i in range(len(var_names)):
        if var_names[i].lower() == 'str':
            var_names[i] = 'mystr'
            break
        
    # More than one variable can be given in the same line
    return [var_names.split(','),var_type,var_optionals]
