"""
---------
Functions
---------
fortran_proc(p) -> None
get_fortran_type(line : str) -> str
init_module(m : cfml_objects.Module) -> None
init_proc(p_name : str) -> None
set_fortran_arguments(p) -> None
set_local_variables(p,rets : list) -> None
set_rets(p) -> list
wrap(p) -> None
"""
import cfml_objects

w_name = ''
w_file = ''

def fortran_proc(p) -> None:

    with open(w_file,'a') as f:
        f.write(f"\n{' ':>8}{'! Call CrysFML procedure'}\n")
        arguments = '('
        for arg in p.arguments.keys():
            arguments = arguments + arg + ','
        arguments = arguments[:-1] + ')'
        if type(p) == cfml_objects.Subroutine:
            f.write(f"{' ':>8}{'if (ierror == 0) call '}{p.name}{arguments}\n")
        else:
            f.write(f"{' ':>8}{'if (ierror == 0) '}{p.xreturn.name}{' = '}{p.name}{arguments}\n")

def get_fortran_type(line : str) -> str:

    line = line.lower()
    i = line.find('intent')
    if i < 0:
        ftype = line
    else:
        i = line[:i].rfind(',')
        ftype = line[:i].strip()
    return ftype

def init_module(m : cfml_objects.Module) -> None:

    global w_name
    global w_file
    w_name = 'py_' + m.name
    w_file = w_name + '.f90'

    with open(w_file,'w') as f:
        f.write(f"{'module '}{w_name}\n")
        f.write(f"\n{' ':>4}{'use forpy_mod '}\n")
        f.write(f"{' ':>4}{'use iso_c_binding '}\n")
        f.write(f"\n{' ':>4}{'use cfml_globaldeps '}\n")
        f.write(f"{' ':>4}{'use '}{m.name}\n")
        f.write(f"\n{' ':>4}{'implicit none'}\n")
        f.write(f"\n{' ':>4}{'contains'}\n")

def init_proc(p_name : str) -> None:

    with open(w_file,'a') as f:
        f.write(f"\n{' ':>4}{'function '}{'py_'}{p_name}{'(self_ptr,args_ptr) result(r) bind(c)'}\n")
        f.write(f"\n{' ':>8}{'! Arguments'}\n")
        f.write(f"{' ':>8}{'type(c_ptr), value :: self_ptr'}\n")
        f.write(f"{' ':>8}{'type(c_ptr), value :: args_ptr'}\n")
        f.write(f"{' ':>8}{'type(c_ptr)        :: r'}\n")

def return_tuple(p,rets : list) -> None:

    n = 2 + len(rets)
    with open(w_file,'a') as f:
        f.write(f"\n{' ':>8}{'! Return tuple'}\n")
        f.write(f"{' ':>8}{'ierror = tuple_create(ret,'}{n}{')'}\n")
        f.write(f"{' ':>8}{'ret'}%{'setitem(0,err_cfml'}%{'flag)'}\n")
        f.write(f"{' ':>8}{'ret'}%{'setitem(1,err_cfml'}%{'msg)'}\n")
        i = 2
        for r in rets:
            f.write(f"{' ':>8}{'ret'}%{'setitem('}{i}{','}{r}{')'}\n")
            i += 1
        f.write(f"{' ':>8}{'r = ret'}%{'get_c_ptr()'}\n")
        f.write(f"\n{' ':>4}{'end function '}{'py_'}{p.name}\n")

def set_fortran_arguments(p) -> None:

    with open(w_file,'a') as f:
        f.write(f"\n{' ':>8}{'! Fortran arguments'}\n")
        for arg in p.arguments.keys():
            ftype = get_fortran_type(p.arguments[arg].fortran_type)
            f.write(f"{' ':>8}{ftype}{' :: '}{p.arguments[arg].name}\n")
        if type(p) == cfml_objects.Function:
            ftype = get_fortran_type(p.xreturn.fortran_type)
            f.write(f"{' ':>8}{ftype}{' :: '}{p.xreturn.name}\n")

def set_local_variables(p,rets : list) -> None:

    with open(w_file,'a') as f:
        f.write(f"\n{' ':>8}{'! Local variables'}\n")
        f.write(f"{' ':>8}{'integer :: ierror'}\n")
        f.write(f"{' ':>8}{'type(object) :: item'}\n")
        f.write(f"{' ':>8}{'type(tuple) :: args,ret'}\n")

def set_rets(p) -> list:

    rets = []
    for arg in p.arguments.keys():
        if p.arguments[arg].intent != 'in':
            rets.append(arg)
    if type(p) == cfml_objects.Function:
        rets.append(p.xreturn.name)
    return rets

def unwrap(p):

    with open(w_file,'a') as f:
        f.write(f"\n{' ':>8}{'! Reset error variable'}\n")
        f.write(f"{' ':>8}{'call clear_error()'}\n")
        f.write(f"\n{' ':>8}{'! Unwrap arguments'}\n")
        f.write(f"{' ':>8}{'call unsafe_cast_from_c_ptr(args,args_ptr)'}\n")
        i = 0
        for arg in p.arguments.keys():
            f.write(f"{' ':>8}{'if (ierror == 0) getitem(item,'}{i}{')'}\n")
            f.write(f"{' ':>8}{'if (ierror == 0) cast('}{arg}{',item)'}\n")
            i += 1
        f.write(f"{' ':>8}{'if (ierror == 0) then'}\n")
        f.write(f"{' ':>12}{'err_cfml'}%{'flag = .true.'}\n")
        msg = "'py_{}: Error reading arguments'".format(p.name)
        f.write(f"{' ':>12}{'err_cfml'}%{'msg = '}{msg}\n")
        f.write(f"{' ':>8}{'end if'}\n")

def wrap(p) -> None:

    init_proc(p.name)
    set_fortran_arguments(p)
    rets = set_rets(p)
    set_local_variables(p,rets)
    unwrap(p)
    fortran_proc(p)
    return_tuple(p,rets)

