"""
Python script for building CFML_Wraps.
Author: Nebil A. Katcho
December 2023

---------
Functions
---------
get_cfml_modules_filenames() -> list
local_variables_unwrap(t : cfml_objects.FortranType) -> list
read() -> None
read_cfml_module(file_name : str) -> None
run() -> None
wrap() -> None
wrap_cfml_module(file_name : str) -> None
write_unwrap_proc(f,t : dict,s : str) -> None
write_unwrap_type(f,t : cfml_objects.FortranType,n : int,forvar : str)
write_wrap_proc(f,t : dict,s : str) -> None:
"""

import cfml_objects
import glob
import os
import parser_utils

try:
    import colorama
    colorama.init()
    is_colorama = True
except:
    is_colorama = False

# In EXCLUDED we put the modules we do not want to wrap
EXCLUDED = ['database','fft','global','keycodes','keyword','maths','messages','python','random','rational','strings','tables','vtk']
PRIMITIVES = ['integer','real','logical','character','complex']
NUMERICALS = ['integer','real']
modules = {}
lucy = {} # Base class for every type

def get_cfml_modules_filenames() -> list:

    my_modules = []
    cfml_modules_names = glob.glob('CFML*.f90')
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

def local_variables_unwrap(t : cfml_objects.FortranType) -> list:

    p_int_1D = False
    p_int_2D = False
    p_int_3D = False
    p_real_1D = False
    p_real_2D = False
    p_real_3D = False
    my_list = False
    my_dicts = []
    for c in t.components:
        var = t.components[c]
        if var.ndim == 0:
            if var.ftype.lower() not in PRIMITIVES:
                my_dicts.append(f"dict_{var.name}")
        else:
            if var.ftype.lower() == 'integer':
                if var.ndim == 1:
                    p_int_1D = True
                elif var.ndim == 2:
                    p_int_2D = True
                elif var.ndim == 3:
                    p_int_3D = True
            elif var.ftype.lower() == 'real':
                if var.ndim == 1:
                    p_real_1D = True
                elif var.ndim == 2:
                    p_real_2D = True
                elif var.ndim == 3:
                    p_real_3D = True
            else:
                my_list = True
    lv = []
    order = False
    if p_int_1D:
        lv.append('integer, dimension(:), pointer :: p_int_1d')
    if p_int_2D:
        lv.append('integer, dimension(:,:), pointer :: p_int_2d')
        order = True
    if p_int_3D:
        lv.append('integer, dimension(:,:,:), pointer :: p_int_3d')
        order = True
    if p_real_1D:
        lv.append('real, dimension(:), pointer :: p_real_1d')
    if p_real_2D:
        lv.append('real, dimension(:,:), pointer :: p_real_2d')
        order = True
    if p_real_3D:
        lv.append('real, dimension(:,:,:), pointer :: p_real_3d')
        order = True
    if order:
        lv.append('character(len=1) :: order')
    if my_list:
        lv.append('type(list) :: my_list')
    if my_dicts:
        lv.append('type(dict) :: '+','.join(my_dicts))
    return lv

def read() -> None:

    os.chdir('../')
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
        print(f"{' ' :>20}{colorama.Back.GREEN}{'Build Wraps'}{colorama.Style.RESET_ALL}")
        print(f"{' ' :>20}{colorama.Fore.GREEN}{'==========='}{colorama.Style.RESET_ALL}")
    else:
        print(f"{' ' :>20}{'==========='}")
        print(f"{' ' :>20}{'Build Wraps'}")
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
        print(f"{colorama.Fore.GREEN}{'Start building wraps'}{colorama.Style.RESET_ALL}")
    else:
        print(f"{'Start building wraps'}")
    wrap()

def set_childs():

    for m in modules:
        for t in modules[m].types:
            if modules[m].types[t].parent:
                parent = modules[m].types[t].parent
                p = modules[m].types[t].parent
                level = 0
                while modules[m].types[p].parent:
                    parent = modules[m].types[p].parent
                    p = modules[m].types[p].parent
                    level += 1
                modules[m].types[parent].childs.append([t,level])
                lucy[t] = parent
            else:
                lucy[t] = t

def wrap() -> None:

    if not os.path.isdir('CFML_Wraps'):
        os.mkdir('CFML_Wraps')
    for m in modules:
        wrap_cfml_module(m)
        #break
    return None

def wrap_cfml_module(m_name : str) -> None:

    w_name = 'Wraps_'+m_name[5:]
    w_file = os.path.join('CFML_Wraps',w_name+'.f90')
    with open(w_file,'w') as f:
        f.write(f"submodule (CFML_Wraps) {w_name}\n")
        f.write(f"\n{'':>4}implicit none\n")
        f.write(f"{'':>4}contains\n")
        t = modules[m_name].types
        for s in t:
            if not t[s].parent:
                write_unwrap_proc(f,t,s)
                write_wrap_proc(f,t,s)
            #break
        f.write(f"\nend submodule")

def write_unwrap_proc(f,t : dict,s : str) -> None:

    f.write(f"\n{'':>4}Module Subroutine Unwrap_{s}(py_var,for_var,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    f.write(f"{'':>8}type(dict), intent(inout) :: py_var\n")
    if t[s].childs:
        f.write(f"{'':>8}class({s}), allocatable, intent(out) :: for_var\n")
    else:
        f.write(f"{'':>8}type({s}), intent(out) :: for_var\n")
    f.write(f"{'':>8}integer, intent(out) :: ierror\n")
    # Local variables
    local_var = local_variables_unwrap(t[s])
    f.write(f"\n{'':>8}! Local variables\n")
    f.write(f"{'':>8}character(len=:), allocatable :: fortran_type\n")
    for lv in local_var:
        f.write(f"{'':>8}{lv}\n")
    # Initialization
    f.write(f"\n{'':>8}ierror = 0\n")
    # Procedure
    f.write(f"{'':>8}ierror = py_var%getitem(fortran_type,'fortran_type')\n")
    f.write(f"{'':>8}if (ierror /= 0) then\n")
    f.write(f"{'':>12}err_cfml%flag = .true.\n")
    f.write(f"{'':>12}err_cfml%ierr = ierror\n")
    f.write(f"{'':>12}err_cfml%msg  = 'Unwrap_{s}: Cannot determine fortran type'\n")
    f.write(f"{'':>8}else\n")
    f.write(f"{'':>12}if (fortran_type == '{s}') then\n")
    f.write(f"{'':>16}allocate({s} :: for_var)\n")
    childs = t[s].childs
    if childs:
        level = 0
        n = 1
        while n > 0:
            n = 0
            for ch in childs:
                if ch[1] == level:
                    f.write(f"{'':>12}else if (fortran_type == '{ch[0]}') then\n")
                    f.write(f"{'':>16}allocate({ch[0]} :: for_var)\n")
                    n += 1
            level += 1
    f.write(f"{'':>12}else\n")
    f.write(f"{'':>16}ierror = -1\n")
    f.write(f"{'':>16}err_cfml%flag = .true.\n")
    f.write(f"{'':>16}err_cfml%ierr = ierror\n")
    f.write(f"{'':>16}err_cfml%msg  = 'Unwrap_{s}: Wrong fortran type:'//adjustl(trim(fortran_type))\n")
    f.write(f"{'':>12}end if\n")
    f.write(f"{'':>8}end if\n")
    write_unwrap_type(f,t[s],8,'for_var')
    if childs:
        f.write(f"{'':>8}if (ierror == 0) then\n")
        level = 0
        n = 1
        while n > 0:
            n = 0
            for ch in childs:
                if ch[1] == level:
                    if (n == 0):
                        f.write(f"{'':>12}select type (A => for_var)\n")
                    f.write(f"{'':>16}class is ({ch[0]})\n")
                    write_unwrap_type(f,t[ch[0]],20,'A')
                    n += 1
            if n > 0:
                f.write(f"{'':>12}end select\n")
            level += 1
        f.write(f"{'':>8}end if\n")
    f.write(f"\n{'':>4}End Subroutine Unwrap_{s}\n")

def write_unwrap_type(f,t : cfml_objects.FortranType,n : int,forvar : str):

    tab = ''
    for i in range(n):
        tab = tab + ' '
    for c in t.components:
        var = t.components[c]
        if var.ndim == 0:
            if var.ftype.lower() in PRIMITIVES:
                f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,{forvar}%{var.name},ierror)\n")
            else:
                f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,dict_{var.name},ierror)\n")
                f.write(f"{tab}if (ierror == 0) call unwrap_{lucy[var.ftype]}('Unwrap_{t.name}','{var.name}',dict_{var.name},{forvar}%{var.name},ierror)\n")
        else:
            if var.ftype.lower() in NUMERICALS:
                if var.ftype.lower() == 'integer':
                    pointer = 'p_int_'+str(var.ndim)+'d'
                else:
                    pointer = 'p_real_'+str(var.ndim)+'d'
                if var.allocatable:
                    func = 'pointer_to_array_alloc'
                else:
                    func = 'pointer_to_array'
                if var.ndim == 1:
                    f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,{pointer},ierror)\n")
                    f.write(f"{tab}if (ierror == 0) call {func}('Unwrap_{t.name}','{var.name}',{pointer},{forvar}%{var.name},ierror)\n")
                else:
                    f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,{pointer},ierror,order)\n")
                    f.write(f"{tab}if (ierror == 0) call {func}('Unwrap_{t.name}','{var.name}',{pointer},{forvar}%{var.name},ierror,order)\n")
            else:
                f.write(f"{tab}if (ierror == 0) ierror = list_create(my_list)\n")
                f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t.name}','{var.name}',py_var,my_list,ierror)\n")
                f.write(f"{tab}if (ierror == 0) call list_to_array('Unwrap_{t.name}','{var.name}',my_list,{forvar}%{var.name},ierror)\n")
                f.write(f"{tab}if (ierror == 0) call my_list%destroy\n")

def write_wrap_proc(f,t : dict,s : str) -> None:

    f.write(f"\n{'':>4}Module Subroutine Wrap_{s}(py_var,for_var,ierror)\n")
    f.write(f"\n{'':>8}! Arguments\n")
    if t[s].childs:
        f.write(f"{'':>8}class({s}), intent(in) :: for_var\n")
    else:
        f.write(f"{'':>8}type({s}), intent(in) :: for_var\n")
    f.write(f"{'':>8}type(dict), intent(inout) :: py_var\n")
    f.write(f"{'':>8}integer, intent(out) :: ierror\n")
    f.write(f"\n{'':>4}End Subroutine Wrap_{s}\n")


if __name__ == '__main__':

    run()
