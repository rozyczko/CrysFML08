"""
---------
Functions
---------
end_module(m : cfml_objects.Module) -> None
fortran_procedure(f,p) -> None
get_fortran_type(line : str) -> str
init_module(m : cfml_objects.Module) -> None
init_procedure(f,p_name : str) -> None
return_tuple(f,p,rets : list) -> None
set_fortran_procedures_args(f,p) -> None
set_local_variables(f,p,rets : list) -> None
set_local_variables_charray(f,var : str,ftype : str, dim : str) -> None
set_local_variables_primitive(f,var : str,ftype_s : str,dimen : str) -> None
set_rets(p) -> list
unwrap_arguments(f,p) -> None
wrap_procedure(p) -> None
write_api_init(procs : dict,nprocs : int) -> None
write_charlist() -> None
write_license(f) -> None
"""
import cfml_objects
import parser_utils

w_name = ''
w_file = ''

def end_module(m : cfml_objects.Module) -> None:

    with open(w_file,'a') as f:
        f.write(f"\nend module {w_name}\n")

def fortran_procedure(f,p) -> None:

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
    if ftype == 'character(len=*)' or ftype.startswith('character(len=len'):
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

def init_procedure(f,p_name : str) -> None:

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
        ftype = get_fortran_type(r.fortran_type)
        ftype_s = r.fortran_type_short
        if ftype.find('dimension') > -1:
            if ftype_s == 'character':
                f.write(f"{' ':>8}if (ierror == 0) ierror = ret%setitem({i},li_{r.name})\n")
            else:
                f.write(f"{' ':>8}if (ierror == 0) ierror = ndarray_create(nd_{r.name},{r.name})\n")
                f.write(f"{' ':>8}if (ierror == 0) ierror = ret%setitem({i},nd_{r.name})\n")
        else:
            f.write(f"{' ':>8}if (ierror == 0) ierror = ret%setitem({i},{r.name})\n")
        i += 1
    f.write(f"{' ':>8}if (ierror == 0) then \n")
    f.write(f"{' ':>12}resul = ret%get_c_ptr()\n")
    f.write(f"{' ':>8}else\n")
    f.write(f"{' ':>12}ierror = nonetype_create(nret)\n")
    f.write(f"{' ':>12}resul = nret%get_c_ptr()\n")
    f.write(f"{' ':>8}end if\n")
    f.write(f"\n{' ':>4}end function py_{p.name}\n")

def set_fortran_procedures_args(f,p) -> None:

    f.write(f"\n{' ':>8}! Arguments for the Fortran procedure\n")
    for arg in p.arguments.keys():
        ftype = get_fortran_type(p.arguments[arg].fortran_type)
        f.write(f"{' ':>8}{ftype} :: {p.arguments[arg].name}\n")
    if type(p) == cfml_objects.Function:
        ftype = get_fortran_type(p.xreturn.fortran_type)
        f.write(f"{' ':>8}{ftype} :: {p.xreturn.name}\n")

def set_local_variables(f,p,rets) -> None:
    """
    Local variables are needed for fortran arrays and derived types
    """
    f.write(f"\n{' ':>8}! Local variables\n")
    f.write(f"{' ':>8}integer :: ierror\n")
    f.write(f"{' ':>8}type(object) :: item\n")
    f.write(f"{' ':>8}type(tuple) :: args,ret\n")
    f.write(f"{' ':>8}type(nonetype) :: nret\n")
    for arg in p.arguments.keys():
        if p.arguments[arg].intent == 'in':
            dim = p.arguments[arg].dim
            ftype = p.arguments[arg].fortran_type
            ftype_s = p.arguments[arg].fortran_type_short
            if parser_utils.is_primitive(ftype) and parser_utils.is_array(dim):
                if ftype_s == 'character':
                    set_local_variables_charray(f,arg,ftype,dim)
                else:
                    set_local_variables_primitive(f,arg,ftype_s,dim)
    for r in rets: # arguments with intent = 'inout' or 'out' and, if p is a function, the result
        ftype = get_fortran_type(r.fortran_type)
        ftype_s = r.fortran_type_short
        if parser_utils.is_primitive(ftype):
            if parser_utils.is_array(r.dim):
                if ftype_s == 'character':
                    set_local_variables_charray(f,r.name,ftype,r.dim)
                else:
                    set_local_variables_primitive(f,r.name,ftype_s,r.dim)

def set_local_variables_charray(f,var : str,ftype : str, dim : str) -> None:

    f.write(f"{' ':>8}type(list) :: li_{var}\n")
    l = parser_utils.get_len(ftype)
    if l == '*':
        f.write(f"{' ':>8}integer :: maxlen_{var}\n")
    else:
        f.write(f"{' ':>8}integer :: maxlen_{var} = {l}\n")
    if dim.find(':') > -1:
        f.write(f"{' ':>8}integer :: size_{var}\n")
    else:
        f.write(f"{' ':>8}integer :: size_{var} = {dim}\n")

def set_local_variables_primitive(f,var : str,ftype_s : str,dimen : str) -> None:
    """
    For integer, real and logical types
    """
    dim = len(dimen.split(','))-1
    d = '(:'
    for i in range(dim):
        d = d + ',:'
    d = d + ')'
    f.write(f"{' ':>8}type(ndarray) :: nd_{var}\n")
    f.write(f"{' ':>8}{ftype_s}, dimension{d}, pointer :: pointer_{var}\n")

def set_rets(p) -> list:
    """
    Variables that must be returned. Arguments with intent = 'inout' and 'out',
    and, if p is a function, the function result
    """
    rets = []
    for arg in p.arguments.keys():
        if p.arguments[arg].intent != 'in':
            rets.append(p.arguments[arg])
    if type(p) == cfml_objects.Function:
        rets.append(p.xreturn)
    return rets

def unwrap_arguments(f,p) -> None:
    """
    Only arguments with intent = 'in' or 'inout' must be unwrapped
    """
    f.write(f"\n{' ':>8}! Reset error variable\n")
    f.write(f"{' ':>8}ierror = 0\n")
    f.write(f"\n{' ':>8}! unwrap_arguments\n")
    f.write(f"{' ':>8}call unsafe_cast_from_c_ptr(args,args_ptr)\n")
    i = 0
    for arg in p.arguments.keys():
        if p.arguments[arg].intent.find('in') > -1:
            dim = p.arguments[arg].dim
            ftype = p.arguments[arg].fortran_type
            ftype_s = p.arguments[arg].fortran_type_short
            f.write(f"{' ':>8}if (ierror == 0) ierror = args%getitem(item,{i})\n")
            if parser_utils.is_primitive(ftype):
                if parser_utils.is_array(dim):
                    if ftype_s == 'character':
                        unwrap_charray(f,p,arg,ftype,dim)
                    else:
                        pass
                        #unwrap_primarray(f,arg,ftype)
                else:
                    f.write(f"{' ':>8}if (ierror == 0) ierror = cast({arg},item)\n")
        i += 1

def unwrap_charray(f,p,var : str,ftype : str,dim : str) -> None:

    f.write(f"{' ':>8}if (ierror == 0) ierror = cast(li_{var},item)!{dim} {ftype}\n")
    l = parser_utils.get_len(ftype)
    if l == '*': # Get len from the maximum length of the list
        f.write(f"{' ':>8}if (ierror == 0) call maxlen_from_li_charr(li_{var},maxlen_{var},ierror)\n")
        f.write(f"{' ':>8}if (ierror == 0) then\n")
        f.write(f"{' ':>12}ierror = EXCEPTION_ERROR\n")
        f.write(f"{' ':>12}call raise_exception(RuntimeError,\"{p.name}: Error unwrapping python list\")\n")
        f.write(f"{' ':>8}end if\n")
    if dim.find(':') > -1: # Set size = number of elements of the list
        f.write(f"{' ':>8}if (ierror == 0) ierror = li_{var}%len(size_{var})\n")
    else:
        f.write(f"{' ':>8}if (ierror == 0) ierror = li_{var}%len(size_aux)\n")
        f.write(f"{' ':>8}if (size_aux /= size_{var}) then\n")
        f.write(f"{' ':>12}ierror = EXCEPTION_ERROR\n")
        f.write(f"{' ':>12}call raise_exception(RuntimeError,\"{p.name}: Error in list size of argument {var}\")\n")
        f.write(f"{' ':>8}end if\n")
    f.write(f"{' ':>8}if (ierror == 0) allocate(character(len=maxlen_{var})::{var}(size_{var}))\n")
    f.write(f"{' ':>8}if (ierror == 0) call list_to_charray(li_{var},{var},ierror)\n")

def unwrap_primarray(f,var : str) -> None:

    f.write(f"{' ':>8}if (ierror == 0) ierror = cast(nd_{var},item)\n")
    f.write(f"{' ':>8}if (ierror == 0) ierror = nd_{var}%get_data(pointer_{var},order='F')\n")
    # Here we must check that dimensions of var and pointer_var are consistent
    f.write(f"{' ':>8}if (ierror == 0) {var} = pointer_{var}\n")

def wrap_procedure(p) -> None:

    with open(w_file,'a') as f:
        init_procedure(f,p.name)
        set_fortran_procedures_args(f,p)
        rets = set_rets(p)
        set_local_variables(f,p,rets)
        unwrap_arguments(f,p)
        fortran_procedure(f,p)
        #return_tuple(f,p,rets)

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

def write_interconversion() -> None:

    with open('interconversion.f90','w') as f:
        write_license(f)
        f.write(f"\nmodule interconversion\n")
        f.write(f"\n{' ':>4}use forpy_mod\n")
        f.write(f"\n{' ':>4}implicit none\n")
        f.write(f"\n{' ':>4}contains\n")
        # Subroutine chararray_to_list
        f.write(f"\n{' ':>4}subroutine chararray_to_list(charr,li_charr,ierror)\n")
        f.write(f"\n{' ':>8}! Arguments\n")
        f.write(f"{' ':>8}character(len=*), dimension(:), intent(in)  :: charr\n")
        f.write(f"{' ':>8}type(list),                     intent(out) :: li_charr\n")
        f.write(f"{' ':>8}integer,                        intent(out) :: ierror\n")
        f.write(f"\n{' ':>8}! Local variables\n")
        f.write(f"{' ':>8}integer :: i\n")
        f.write(f"\n{' ':>8}ierror = 0\n")
        f.write(f"{' ':>8}do i = 1 , size(charr)\n")
        f.write(f"{' ':>12}if (ierror == 0) ierror = li_charr%append(charr(i))\n")
        f.write(f"{' ':>8}end do\n")
        f.write(f"\n{' ':>4}end subroutine chararray_to_list\n")
        # Subroutine list_to_chararray
        f.write(f"\n{' ':>4}subroutine list_to_chararray(li_charr,l,charr,ierror)\n")
        f.write(f"\n{' ':>8}! Arguments\n")
        f.write(f"{' ':>8}type(list),                     intent(in)  :: li_charr\n")
        f.write(f"{' ':>8}integer,                        intent(in)  :: l\n")
        f.write(f"{' ':>8}character(len=l), dimension(:), intent(out) :: charr\n")
        f.write(f"{' ':>8}integer,                        intent(out) :: ierror\n")
        f.write(f"\n{' ':>8}! Local variables\n")
        f.write(f"{' ':>8}integer :: i,n\n")
        f.write(f"{' ':>8}type(object) :: item\n")
        f.write(f"\n{' ':>8}ierror = li_charr%len(n)\n")
        f.write(f"{' ':>8}do i = 1 , n\n")
        f.write(f"{' ':>12}if (ierror == 0) ierror = li_charr%getitem(item,i-1)\n")
        f.write(f"{' ':>12}if (ierror == 0) ierror = cast(charr(i),item)\n")
        f.write(f"{' ':>8}end do\n")
        f.write(f"\n{' ':>4}end subroutine list_to_chararray\n")
        # Subroutine maxlen_from_li_charr
        f.write(f"\n{' ':>4}subroutine maxlen_from_li_charr(li_charr,l,ierror)\n")
        f.write(f"\n{' ':>8}! Arguments\n")
        f.write(f"{' ':>8}type(list),                     intent(in)  :: li_charr\n")
        f.write(f"{' ':>8}integer,                        intent(out) :: l\n")
        f.write(f"{' ':>8}integer,                        intent(out) :: ierror\n")
        f.write(f"\n{' ':>8}! Local variables\n")
        f.write(f"{' ':>8}integer :: i,n\n")
        f.write(f"{' ':>8}character(len=1024) :: str\n")
        f.write(f"{' ':>8}type(object) :: item\n")
        f.write(f"\n{' ':>8}l = 0\n")
        f.write(f"{' ':>8}ierror = li_charr%len(n)\n")
        f.write(f"{' ':>8}do i = 1 , n\n")
        f.write(f"{' ':>12}if (ierror == 0) ierror = li_charr%getitem(item,i-1)\n")
        f.write(f"{' ':>12}if (ierror == 0) ierror = cast(str,item)\n")
        f.write(f"{' ':>12}if (ierror == 0) then \n")
        f.write(f"{' ':>16}if (len(trim(str)) > l) l = len(trim(str)) \n")
        f.write(f"{' ':>12}end if\n")
        f.write(f"{' ':>8}end do\n")
        f.write(f"\n{' ':>4}end subroutine maxlen_from_li_charr\n")
        f.write(f"\nend module interconversion\n")

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