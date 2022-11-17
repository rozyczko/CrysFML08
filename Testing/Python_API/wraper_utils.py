"""
---------
Functions
---------
end_module(m : cfml_objects.Module) -> None
fortran_proc(f,p) -> None
get_fortran_type(line : str) -> str
init_module(m : cfml_objects.Module) -> None
init_proc(f,p_name : str) -> None
return_tuple(f,p,rets : list) -> None
set_fortran_arguments(f,p) -> None
set_local_variables(f,p,rets : list) -> None
set_rets(p) -> list
unwrap(f,p) -> None
wrap(p) -> None
write_api_init(procs : dict,nprocs : int) -> None
write_license(f) -> None
"""
import cfml_objects

w_name = ''
w_file = ''

def end_module(m : cfml_objects.Module) -> None:

    with open(w_file,'a') as f:
        f.write(f"\nend module {w_name}\n")

def fortran_proc(f,p) -> None:

    f.write(f"\n{' ':>8}! Call CrysFML procedure\n")
    f.write(f"{' ':>8}if (ierror == 0) then\n")
    f.write(f"{' ':>12}call clear_error()\n")
    arguments = '('
    for arg in p.arguments.keys():
        arguments = arguments + arg + ','
    arguments = arguments[:-1] + ')'
    if type(p) == cfml_objects.Subroutine:
        f.write(f"{' ':>12}call {p.name}{arguments}\n")
    else:
        f.write(f"{' ':>12}{p.xreturn.name} = {p.name}{arguments}\n")
    f.write(f"{' ':>12}if (err_cfml%flag) then\n")
    f.write(f"{' ':>16}ierror = EXCEPTION_ERROR\n")
    f.write(f"{' ':>16}call raise_exception(RuntimeError,\"{p.name}: \"//trim(err_cfml%msg))\n")
    f.write(f"{' ':>12}end if\n")
    f.write(f"{' ':>8}end if\n")

def get_fortran_type(line : str) -> str:

    line = line.lower()
    i = line.find('intent')
    if i < 0:
        ftype = line
    else:
        i = line[:i].rfind(',')
        ftype = line[:i].strip()
    if ftype == 'character(len=*)':
        ftype = 'character(len=:), allocatable'
    return ftype

def init_module(m : cfml_objects.Module) -> None:

    global w_name
    global w_file
    w_name = 'py_' + m.name
    w_file = w_name + '.f90'

    with open(w_file,'w') as f:
        write_license(f)
        f.write(f"module {w_name}\n")
        f.write(f"\n{' ':>4}use forpy_mod \n")
        f.write(f"{' ':>4}use iso_c_binding \n")
        f.write(f"\n{' ':>4}use cfml_globaldeps \n")
        f.write(f"{' ':>4}use {m.name}\n")
        f.write(f"\n{' ':>4}implicit none\n")
        f.write(f"\n{' ':>4}contains\n")

def init_proc(f,p_name : str) -> None:

    f.write(f"\n{' ':>4}function py_{p_name}(self_ptr,args_ptr) result(resul) bind(c)\n")
    f.write(f"\n{' ':>8}! Arguments\n")
    f.write(f"{' ':>8}type(c_ptr), value :: self_ptr\n")
    f.write(f"{' ':>8}type(c_ptr), value :: args_ptr\n")
    f.write(f"{' ':>8}type(c_ptr)        :: resul\n")

def return_tuple(f,p,rets : list) -> None:

    n = len(rets)
    f.write(f"\n{' ':>8}! Return tuple\n")
    f.write(f"{' ':>8}if (ierror == 0) ierror = tuple_create(ret,{n})\n")
    i = 0
    for r in rets:
        f.write(f"{' ':>8}if (ierror == 0) ierror = ret%setitem({i},{r})\n")
        i += 1
    f.write(f"{' ':>8}if (ierror == 0) then \n")
    f.write(f"{' ':>12}resul = ret%get_c_ptr()\n")
    f.write(f"{' ':>8}else\n")
    f.write(f"{' ':>12}ierror = nonetype_create(nret)\n")
    f.write(f"{' ':>12}resul = nret%get_c_ptr()\n")
    f.write(f"{' ':>8}end if\n")
    f.write(f"\n{' ':>4}end function py_{p.name}\n")

def set_fortran_arguments(f,p) -> None:

    f.write(f"\n{' ':>8}! Fortran arguments\n")
    for arg in p.arguments.keys():
        ftype = get_fortran_type(p.arguments[arg].fortran_type)
        var = ftype + ' :: ' + p.arguments[arg].name
        f.write(f"{' ':>8}{var :<60} !{p.arguments[arg].fortran_type}\n")
    if type(p) == cfml_objects.Function:
        ftype = get_fortran_type(p.xreturn.fortran_type)
        var = ftype + ' :: ' + p.xreturn.name
        f.write(f"{' ':>8}{var :<60} !{p.xreturn.fortran_type}\n")

def set_local_variables(f) -> None:

    f.write(f"\n{' ':>8}! Local variables\n")
    f.write(f"{' ':>8}integer :: ierror\n")
    f.write(f"{' ':>8}type(object) :: item\n")
    f.write(f"{' ':>8}type(tuple) :: args,ret\n")
    f.write(f"{' ':>8}type(nonetype) :: nret\n")

def set_rets(p) -> list:

    rets = []
    for arg in p.arguments.keys():
        if p.arguments[arg].intent != 'in':
            rets.append(arg)
    if type(p) == cfml_objects.Function:
        rets.append(p.xreturn.name)
    return rets

def unwrap(f,p) -> None:

    f.write(f"\n{' ':>8}! Reset error variable\n")
    f.write(f"{' ':>8}ierror = 0\n")
    f.write(f"\n{' ':>8}! Unwrap arguments\n")
    f.write(f"{' ':>8}call unsafe_cast_from_c_ptr(args,args_ptr)\n")
    i = 0
    for arg in p.arguments.keys():
        f.write(f"{' ':>8}if (ierror == 0) ierror = args%getitem(item,{i})\n")
        f.write(f"{' ':>8}if (ierror == 0) ierror = cast({arg},item)\n")
        i += 1

def wrap(p) -> None:

    with open(w_file,'a') as f:
        init_proc(f,p.name)
        set_fortran_arguments(f,p)
        rets = set_rets(p)
        set_local_variables(f)
        unwrap(f,p)
        fortran_proc(f,p)
        return_tuple(f,p,rets)

def write_api_init(procs : dict,nprocs : int) -> None:

    with open('api_init.f90','w') as f:
        write_license(f)
        f.write(f"\nmodule api_init\n")
        f.write(f"\n{' ':>4}use forpy_mod\n")
        f.write(f"{' ':>4}use iso_c_binding\n")
        f.write(f"\n")
        for m in procs.keys():
            f.write(f"{' ':>4}use py_{m}\n")
        f.write(f"\n{' ':>4}implicit none\n")
        f.write(f"\n{' ':>4}type(PythonModule), save :: mod_Def\n")
        f.write(f"{' ':>4}type(PythonMethodTable), save :: method_Table\n")
        f.write(f"\n{' ':>4}contains\n")
        f.write(f"\n{' ':>4}! Initialization function for Python 3\n")
        f.write(f"{' ':>4}! Called when importing module\n")
        f.write(f"{' ':>4}! Must use bind(c, name=\"PyInit_<module name>\")\n")
        f.write(f"{' ':>4}! Return value must be type(c_ptr),\n")
        f.write(f"{' ':>4}! use the return value of PythonModule%init\n")
        f.write(f"{' ':>4}function PyInit_crysfml08_api() bind(c,name=\"PyInit_crysfml08_api\") result(m)\n")
        f.write(f"{' ':>4}!DEC$ ATTRIBUTES DLLEXPORT :: PyInit_crysfml08_api\n")
        f.write(f"\n{' ':>8}! Local variables\n")
        f.write(f"{' ':>8}type(c_ptr) :: m\n")
        f.write(f"\n{' ':>8}m = Init()\n")
        f.write(f"\n{' ':>4}end function PyInit_crysfml08_api\n")
        f.write(f"\n{' ':>4}function Init() result(m)\n")
        f.write(f"\n{' ':>8}! Local variables\n")
        f.write(f"{' ':>8}type(c_ptr) :: m\n")
        f.write(f"{' ':>8}integer :: ierror\n")
        f.write(f"\n{' ':>8}ierror = Forpy_Initialize()\n")
        f.write(f"\n{' ':>8}! Build method table\n")
        f.write(f"{' ':>8}call method_Table%init({nprocs})\n")
        for m in procs.keys():
            for p in procs[m]:
                f.write(f"{' ':>8}call method_Table%add_method(\"{p}\",&\n")
                f.write(f"{' ':>12}\"{p}\",METH_VARARGS,&\n")
                f.write(f"{' ':>12}c_funloc(py_{p}))\n")

        f.write(f"\n{' ':>8}! Build mod_Def\n")
        f.write(f"{' ':>8}m = mod_Def%init(\"pycrysfml08\",&\n")
        f.write(f"{' ':>12}\"A Python API for CrysFML08\",method_Table)")
        f.write(f"\n{' ':>4}end function Init\n")
        f.write(f"\nend module api_init\n")

def write_license(f) -> None:

    f.write(f"! ------------------------------------------------------------\n")
    f.write(f"! CrysFML08 API\n")
    f.write(f"!\n")
    f.write(f"{'! @license':<14}GNU LGPL (see LICENSE)\n")
    f.write(f"{'! @copyright':<14}Institut Laue Langevin 2020-now\n")
    f.write(f"{'! @authors':<14}Scientific Computing Group at ILL (see AUTHORS),\n")
    f.write(f"!{' ':>13}based on Elias Rabel work for Forpy\n")
    f.write(f"! ------------------------------------------------------------\n")
    f.write(f"\n")