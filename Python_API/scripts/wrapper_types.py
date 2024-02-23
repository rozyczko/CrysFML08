"""
Python script for building CFML_Wraps.
Author: Nebil A. Katcho
February 2024

---------
Functions
---------
get_levels(t : dict,s : str)
local_variables_unwrap(tipos : list) -> list
local_variables_wrap(tipos : list) -> list
wrap(modules : dict) -> None
wrap_cfml_module_types(m : cfml_objects.Module,m_name : str) -> None
write_cfml_wraps(modules : dict) -> None
write_list_to_array1d(f,t : dict,s : str,alloc : bool= True) -> None
write_list_to_array1d_alloc_class(f,t : dict,s : str) -> None
write_list_to_array2d(f,t : dict,s : str,alloc : bool= True) -> None
write_list_to_array2d_alloc_class(f,t : dict,s : str) -> None
write_unwrap_proc(f,t : dict,s : str) -> None
write_unwrap_proc_alloc_class(f,t : dict,s : str) -> None
write_unwrap_proc_no_alloc_class(f,t : dict,s : str) -> None
write_unwrap_type(f,t : dict,s : str,n : int,forvar : str)
write_wrap_proc(f,t : dict,s : str) -> None
write_wrap_type(f,t : cfml_objects.FortranType,n : int,forvar : str)
"""
import cfml_objects
import os

publics_types = {}
publics_unwrap = []
publics_unwrap_alloc_class = []
publics_unwrap_no_alloc_class = []
publics_wrap = []

PRIMITIVES = ['integer','real','logical','character','complex']
NUMERICALS = ['integer','real','complex']

def get_levels(t : dict,s : str):

    levels = [[t[s]]]
    if t[s].childs: # Get inheritance starting from the base class
        n = 0        
        new_level = True
        while new_level:
            new_level = False
            lv = []
            for ti in levels[n]:
                if ti.childs:
                    new_level = True 
                    for ch in ti.childs:
                        lv.append(t[ch])
            if new_level:
                levels.append(lv)
                n += 1
    return levels

def local_variables_unwrap(tipos : list) -> list:

    p_int_1D = False
    p_int_2D = False
    p_int_3D = False
    p_real_1D = False
    p_real_2D = False
    p_real_3D = False
    p_complex_1D = False
    p_complex_2D = False
    p_complex_3D = False
    p_complex_4D = False
    my_list = False
    my_dicts = []
    for t in tipos:
        for c in t.components:
            var = t.components[c]
            if var.ndim == 0:
                if var.ftype.lower() not in PRIMITIVES:
                    my_dicts.append(f"di_{var.name}")
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
                elif var.ftype.lower() == 'complex':
                    if var.ndim == 1:
                        p_complex_1D = True
                    elif var.ndim == 2:
                        p_complex_2D = True
                    elif var.ndim == 3:
                        p_complex_3D = True
                    elif var.ndim == 4:
                        p_complex_4D = True
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
    if p_complex_1D:
        lv.append('complex, dimension(:), pointer :: p_complex_1d')
    if p_complex_2D:
        lv.append('complex, dimension(:,:), pointer :: p_complex_2d')
        order = True
    if p_complex_3D:
        lv.append('complex, dimension(:,:,:), pointer :: p_complex_3d')
        order = True
    if p_complex_4D:
        lv.append('complex, dimension(:,:,:,:), pointer :: p_complex_4d')
        order = True
    if order:
        lv.append('character(len=1) :: order')
    if my_list:
        lv.append('type(list) :: my_list')
    if my_dicts:
        lv.append('type(dict) :: '+','.join(my_dicts))
    return lv

def local_variables_wrap(tipos : list) -> list:

    my_dicts = []
    my_dicts_alloc = []
    my_dicts2d_alloc = []
    my_lists = []
    my_ndarrays = []
    is_list_list = False
    for t in tipos:
        for c in t.components:
            var = t.components[c]
            if var.ndim == 0:
                if var.ftype.lower() not in PRIMITIVES:
                    di = f"di_{var.name}"
                    if di not in my_dicts:
                        my_dicts.append(di)
            else:
                if var.ftype.lower() in NUMERICALS:
                    nd = f"nd_{var.name}"
                    if nd not in my_ndarrays:
                        my_ndarrays.append(nd)
                else:
                    li = f"li_{var.name}"
                    if var.ndim > 1:
                        is_list_list = True
                    if li not in my_lists:
                        my_lists.append(li)
                        if var.ftype.lower() not in PRIMITIVES:
                            if var.ndim == 1:
                                my_dicts_alloc.append(f"di_{var.name}")
                            elif var.ndim == 2:
                                my_dicts2d_alloc.append(f"di_{var.name}")
    lv = []
    if is_list_list:
        my_lists.append("li")
    if my_lists:
        if is_list_list:
            lv.append('integer :: i,j')
        else:
            lv.append('integer :: i')
        lv.append('type(list) :: '+','.join(my_lists))
    if my_dicts:
        lv.append('type(dict) :: '+','.join(my_dicts))
    if my_dicts_alloc:
        lv.append('type(dict), dimension(:), allocatable :: '+','.join(my_dicts_alloc))
    if my_dicts2d_alloc:
        lv.append('type(dict), dimension(:,:), allocatable :: '+','.join(my_dicts2d_alloc))
    if my_ndarrays:
        lv.append('type(ndarray) :: '+','.join(my_ndarrays))
    return lv

def wrap(modules : dict) -> None:

    if not os.path.isdir('CFML_Wraps'):
        os.mkdir('CFML_Wraps')

    # Wrap every module
    for m in modules:
        wrap_cfml_module_types(modules[m],m)
    write_cfml_wraps(modules)
    return None

def wrap_cfml_module_types(m : cfml_objects.Module,m_name : str) -> None:

    w_name = 'Wraps_'+m_name[5:]
    w_file = os.path.join('CFML_Wraps',w_name+'.f90')
    with open(w_file,'w') as f:
        f.write(f"submodule (CFML_Wraps) {w_name}\n")
        f.write(f"\n{'':>4}implicit none\n")
        f.write(f"{'':>4}contains\n")
        t = m.types
        # Wrap and unwrap procedures
        for s in t:
            if not t[s].parent:
                write_wrap_proc(f,t,s)
            if t[s].childs:
                write_unwrap_proc_alloc_class(f,t,s)
                write_unwrap_proc_no_alloc_class(f,t,s)
            elif not t[s].parent:
                write_unwrap_proc(f,t,s)
        # List to array procedures
        for s in t:                           
            write_list_to_array1d(f,t,s)
            write_list_to_array1d(f,t,s,alloc=False)
            write_list_to_array2d(f,t,s)
            write_list_to_array2d(f,t,s,alloc=False)
            if t[s].childs:
                write_list_to_array1d_alloc_class(f,t,s)
                write_list_to_array2d_alloc_class(f,t,s)
        f.write(f"\nend submodule")

def write_cfml_wraps(modules : dict) -> None:

    w_file = os.path.join('CFML_Wraps.f90')
    with open(w_file,'w') as f:
        f.write(f"\nModule CFML_Wraps\n")
        f.write(f"\n{'':>4}use cfml_globaldeps\n")
        f.write(f"\n{'':>4}use forpy_mod, str_forpy => str\n")
        f.write(f"\n{'':>4}use cfml_wraps_utils\n")
        for m in modules:
            if modules[m].types:
                f.write(f"{'':>4}use {m}, only: ")
                n = 0
                for s in modules[m].types:
                    if n == 0:
                        f.write(f"{s}")
                    else:
                        f.write(f",{s}")
                    n += 1
                f.write(f"\n")
        f.write(f"\n{'':>4}implicit none\n")
        f.write(f"\n{'':>4}private\n")
        # -----------------
        # Public procedures
        # -----------------
        # - wraps
        for p in publics_wrap:
            f.write(f"{'':>4}public :: wrap_{p[0]}\n")
        # - unwraps       
        for p in publics_unwrap:
            f.write(f"{'':>4}public :: unwrap_{p}\n")
        for p in publics_unwrap_alloc_class:
            f.write(f"{'':>4}public :: unwrap_{p}_alloc_class\n")
        for p in publics_unwrap_no_alloc_class:
            f.write(f"{'':>4}public :: unwrap_{p}_no_alloc_class\n")
        # - list_to_array
        for m in modules:
            t = modules[m].types
            for s in t:
                f.write(f"{'':>4}public :: list_to_alloc_array1d_{s}\n")
                f.write(f"{'':>4}public :: list_to_no_alloc_array1d_{s}\n")
                f.write(f"{'':>4}public :: list_to_alloc_array2d_{s}\n")
                f.write(f"{'':>4}public :: list_to_no_alloc_array2d_{s}\n")
                if t[s].childs:
                    f.write(f"{'':>4}public :: list_to_alloc_array1d_{s}_alloc_class\n")
                    f.write(f"{'':>4}public :: list_to_alloc_array2d_{s}_alloc_class\n")
        # ---------
        # Overloads
        # ---------
        # - list to array
        for m in modules:
            t = modules[m].types
            for s in t:
                f.write(f"\n{'':>4}interface list_to_alloc_array_{s}\n")
                f.write(f"{'':>8}module procedure list_to_alloc_array1d_{s}\n")
                f.write(f"{'':>8}module procedure list_to_alloc_array2d_{s}\n")
                f.write(f"{'':>4}end interface\n")
                f.write(f"\n{'':>4}interface list_to_no_alloc_array_{s}\n")
                f.write(f"{'':>8}module procedure list_to_no_alloc_array1d_{s}\n")
                f.write(f"{'':>8}module procedure list_to_no_alloc_array2d_{s}\n")
                f.write(f"{'':>4}end interface\n")
                if t[s].childs:
                    f.write(f"\n{'':>4}interface list_to_alloc_array_{s}_alloc_class\n")
                    f.write(f"{'':>8}module procedure list_to_alloc_array1d_{s}_alloc_class\n")
                    f.write(f"{'':>8}module procedure list_to_alloc_array2d_{s}_alloc_class\n")
                    f.write(f"{'':>4}end interface\n")
        # ----------
        # Interfaces
        # ----------
        f.write(f"\n{'':>4}interface\n")  
        # - Wraps
        for p in publics_wrap:
            f.write(f"\n{'':>8}module subroutine wrap_{p[0]}(for_var,py_var,ierror)\n")
            if p[1] == 'class':
                f.write(f"{'':>12}class({p[0]}), intent(in) :: for_var\n")
            else:
                f.write(f"{'':>12}type({p[0]}), intent(in) :: for_var\n")
            f.write(f"{'':>12}type(dict), intent(inout) :: py_var\n")
            f.write(f"{'':>12}integer, intent(out) :: ierror\n")
            f.write(f"{'':>8}end subroutine wrap_{p[0]}\n")  
        # - Unwraps    
        for p in publics_unwrap:
            f.write(f"\n{'':>8}module subroutine unwrap_{p}(py_var,for_var,ierror)\n")
            f.write(f"{'':>12}type(dict), intent(inout) :: py_var\n")
            f.write(f"{'':>12}type({p}), intent(out) :: for_var\n")
            f.write(f"{'':>12}integer, intent(out) :: ierror\n")
            f.write(f"{'':>8}end subroutine unwrap_{p}\n")
        for p in publics_unwrap_alloc_class:
            f.write(f"\n{'':>8}module subroutine unwrap_{p}_alloc_class(py_var,for_var,ierror)\n")
            f.write(f"{'':>12}type(dict), intent(inout) :: py_var\n")
            f.write(f"{'':>12}class({p}), allocatable, intent(out) :: for_var\n")
            f.write(f"{'':>12}integer, intent(out) :: ierror\n")
            f.write(f"{'':>8}end subroutine unwrap_{p}_alloc_class\n")
        for p in publics_unwrap_no_alloc_class:
            f.write(f"\n{'':>8}module subroutine unwrap_{p}_no_alloc_class(py_var,for_var,ierror)\n")
            f.write(f"{'':>12}type(dict), intent(inout) :: py_var\n")
            f.write(f"{'':>12}class({p}), intent(out) :: for_var\n")
            f.write(f"{'':>12}integer, intent(out) :: ierror\n")
            f.write(f"{'':>8}end subroutine unwrap_{p}_no_alloc_class\n") 
        # - List to array
        for m in modules:
            t = modules[m].types
            for s in t:
                # list_to_alloc_array1d
                f.write(f"\n{'':>8}module subroutine list_to_alloc_array1d_{s}(procedure_name,var_name,my_list,arr,ierror)\n")
                f.write(f"{'':>12}character(len=*), intent(in) :: procedure_name\n")
                f.write(f"{'':>12}character(len=*), intent(in) :: var_name\n")
                f.write(f"{'':>12}type(list), intent(inout) :: my_list\n")
                f.write(f"{'':>12}type({s}), dimension(:), allocatable, intent(out) :: arr\n")
                f.write(f"{'':>12}integer, intent(inout) :: ierror\n")
                f.write(f"{'':>8}end subroutine list_to_alloc_array1d_{s}\n")
                # list_to_no_alloc_array1d
                f.write(f"\n{'':>8}module subroutine list_to_no_alloc_array1d_{s}(procedure_name,var_name,my_list,arr,ierror)\n")
                f.write(f"{'':>12}character(len=*), intent(in) :: procedure_name\n")
                f.write(f"{'':>12}character(len=*), intent(in) :: var_name\n")
                f.write(f"{'':>12}type(list), intent(inout) :: my_list\n")
                f.write(f"{'':>12}type({s}), dimension(:), intent(out) :: arr\n")
                f.write(f"{'':>12}integer, intent(inout) :: ierror\n")
                f.write(f"{'':>8}end subroutine list_to_no_alloc_array1d_{s}\n")
                # list_to_alloc_array2d
                f.write(f"\n{'':>8}module subroutine list_to_alloc_array2d_{s}(procedure_name,var_name,my_list,arr,ierror)\n")
                f.write(f"{'':>12}character(len=*), intent(in) :: procedure_name\n")
                f.write(f"{'':>12}character(len=*), intent(in) :: var_name\n")
                f.write(f"{'':>12}type(list), intent(inout) :: my_list\n")
                f.write(f"{'':>12}type({s}), dimension(:,:), allocatable, intent(out) :: arr\n")
                f.write(f"{'':>12}integer, intent(inout) :: ierror\n")
                f.write(f"{'':>8}end subroutine list_to_alloc_array2d_{s}\n")
                # list_to_no_alloc_array2d
                f.write(f"\n{'':>8}module subroutine list_to_no_alloc_array2d_{s}(procedure_name,var_name,my_list,arr,ierror)\n")
                f.write(f"{'':>12}character(len=*), intent(in) :: procedure_name\n")
                f.write(f"{'':>12}character(len=*), intent(in) :: var_name\n")
                f.write(f"{'':>12}type(list), intent(inout) :: my_list\n")
                f.write(f"{'':>12}type({s}), dimension(:,:), intent(out) :: arr\n")
                f.write(f"{'':>12}integer, intent(inout) :: ierror\n")
                f.write(f"{'':>8}end subroutine list_to_no_alloc_array2d_{s}\n")   
                if t[s].childs:
                    # list_to_alloc_array1d_alloc_class
                    f.write(f"\n{'':>8}module subroutine list_to_alloc_array1d_{s}_alloc_class(procedure_name,var_name,my_list,arr,ierror)\n")
                    f.write(f"{'':>12}character(len=*), intent(in) :: procedure_name\n")
                    f.write(f"{'':>12}character(len=*), intent(in) :: var_name\n")
                    f.write(f"{'':>12}type(list), intent(inout) :: my_list\n")
                    f.write(f"{'':>12}class({s}), dimension(:), allocatable, intent(out) :: arr\n")
                    f.write(f"{'':>12}integer, intent(inout) :: ierror\n")
                    f.write(f"{'':>8}end subroutine list_to_alloc_array1d_{s}_alloc_class\n")
                    # list_to_alloc_array1d_alloc_class
                    f.write(f"\n{'':>8}module subroutine list_to_alloc_array2d_{s}_alloc_class(procedure_name,var_name,my_list,arr,ierror)\n")
                    f.write(f"{'':>12}character(len=*), intent(in) :: procedure_name\n")
                    f.write(f"{'':>12}character(len=*), intent(in) :: var_name\n")
                    f.write(f"{'':>12}type(list), intent(inout) :: my_list\n")
                    f.write(f"{'':>12}class({s}), dimension(:,:), allocatable, intent(out) :: arr\n")
                    f.write(f"{'':>12}integer, intent(inout) :: ierror\n")
                    f.write(f"{'':>8}end subroutine list_to_alloc_array2d_{s}_alloc_class\n")

        f.write(f"\n{'':>4}end interface\n")        
        f.write(f"\nEnd Module CFML_Wraps\n")

def write_list_to_array1d(f,t : dict,s : str,alloc : bool= True) -> None:

    if alloc:
        f.write(f"\n{'':>4}Module Subroutine list_to_alloc_array1d_{s}(procedure_name,var_name,my_list,arr,ierror)\n")
    else:
        f.write(f"\n{'':>4}Module Subroutine list_to_no_alloc_array1d_{s}(procedure_name,var_name,my_list,arr,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    f.write(f"{'':>8}character(len=*), intent(in) :: procedure_name\n")
    f.write(f"{'':>8}character(len=*), intent(in) :: var_name\n")
    f.write(f"{'':>8}type(list), intent(inout) :: my_list\n")
    if alloc:
        f.write(f"{'':>8}type({s}), dimension(:), allocatable, intent(out) :: arr\n")
    else:
        f.write(f"{'':>8}type({s}), dimension(:), intent(out) :: arr\n")
    f.write(f"{'':>8}integer, intent(inout) :: ierror\n")
    # Local variables
    f.write(f"\n{'':>8}! Local variables\n")
    f.write(f"{'':>8}integer :: i,n\n")
    f.write(f"{'':>8}type(object) :: item\n")
    f.write(f"{'':>8}type(dict) :: my_dict\n")
    f.write(f"\n{'':>8}ierror = my_list%len(n)\n")
    f.write(f"{'':>8}if (ierror /= 0) then\n")
    f.write(f"{'':>12}ierror = -1\n")
    f.write(f"{'':>12}err_cfml%flag = .true.\n")
    f.write(f"{'':>12}err_cfml%ierr = -1\n")
    f.write(f"{'':>12}err_cfml%msg  = 'list_to_no_alloc_array1d_{s}: Error getting array dimension'\n")
    f.write(f"{'':>8}end if\n")
    if not alloc:
        f.write(f"{'':>8}if (n /= size(arr)) then\n")
        f.write(f"{'':>12}ierror = -1\n")
        f.write(f"{'':>12}err_cfml%flag = .true.\n")
        f.write(f"{'':>12}err_cfml%ierr = -1\n")
        f.write(f"{'':>12}err_cfml%msg  = 'list_to_no_alloc_array_{s}: Dimension of list and arr inconsistent'\n")
        f.write(f"{'':>8}end if\n")
    f.write(f"{'':>8}if (ierror == 0 .and. n > 0) then\n")
    if alloc:
        f.write(f"{'':>12}allocate(arr(n))\n")
    f.write(f"{'':>12}do i = 0 , n-1\n")
    f.write(f"{'':>16}if (ierror == 0) ierror = my_list%getitem(item,i)\n")
    f.write(f"{'':>16}if (ierror == 0) ierror = cast(my_dict,item)\n")
    if not t[s].parent:
        if not t[s].childs:   
            f.write(f"{'':>16}if (ierror == 0) call unwrap_{s}(my_dict,arr(i+1),ierror)\n")
        else:
            f.write(f"{'':>16}if (ierror == 0) call unwrap_{s}_no_alloc_class(my_dict,arr(i+1),ierror)\n")
    else:
        f.write(f"{'':>16}if (ierror == 0) call unwrap_{t[s].lucy}_no_alloc_class(my_dict,arr(i+1),ierror)\n")
    f.write(f"{'':>16}if (ierror == 0) ierror = err_cfml%ierr\n")
    f.write(f"{'':>12}end do\n")
    f.write(f"{'':>8}end if\n")
    if alloc:
        f.write(f"\n{'':>4}End Subroutine list_to_alloc_array1d_{s}\n")
    else:
        f.write(f"\n{'':>4}End Subroutine list_to_no_alloc_array1d_{s}\n")

def write_list_to_array1d_alloc_class(f,t : dict,s : str) -> None:

    f.write(f"\n{'':>4}Module Subroutine list_to_alloc_array1d_{s}_alloc_class(procedure_name,var_name,my_list,arr,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    f.write(f"{'':>8}character(len=*), intent(in) :: procedure_name\n")
    f.write(f"{'':>8}character(len=*), intent(in) :: var_name\n")
    f.write(f"{'':>8}type(list), intent(inout) :: my_list\n")
    f.write(f"{'':>8}class({s}), dimension(:), allocatable, intent(out) :: arr\n")
    f.write(f"{'':>8}integer, intent(inout) :: ierror\n")
    # Levels
    levels = get_levels(t,s)
    nlevels = len(levels)
    # Local variables
    tipos = []
    for l in levels:
        for ti in l:
            tipos.append(ti)
    # Local variables
    f.write(f"\n{'':>8}! Local variables\n")
    f.write(f"{'':>8}integer :: i,n\n")
    f.write(f"{'':>8}character(len=:), allocatable :: fortran_type\n")
    f.write(f"{'':>8}type(object) :: item\n")
    f.write(f"{'':>8}type(dict) :: my_dict\n")
    f.write(f"{'':>8}type({s}) :: src1\n")
    n = 2
    for ti in tipos:
        f.write(f"{'':>8}type({ti.name}) :: src{n}\n")
        n += 1
    f.write(f"\n{'':>8}ierror = my_list%len(n)\n")
    f.write(f"{'':>8}if (ierror /= 0 .or. n < 1) then\n")
    f.write(f"{'':>12}ierror = -1\n")
    f.write(f"{'':>12}err_cfml%flag = .true.\n")
    f.write(f"{'':>12}err_cfml%ierr = -1\n")
    f.write(f"{'':>12}err_cfml%msg  = 'list_to_alloc_array1d_{s}_alloc_class: Error getting array dimension'\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"{'':>8}if (ierror == 0) then\n")
    f.write(f"{'':>12}if (ierror == 0) ierror = my_list%getitem(item,0)\n")
    f.write(f"{'':>12}if (ierror == 0) ierror = cast(my_dict,item)\n")
    f.write(f"{'':>12}if (ierror == 0) then\n")
    f.write(f"{'':>16}ierror = my_dict%getitem(fortran_type,'fortran_type')\n")
    f.write(f"{'':>16}if (ierror /= 0) then\n")
    f.write(f"{'':>20}err_cfml%flag = .true.\n")
    f.write(f"{'':>20}err_cfml%ierr = -1\n")
    f.write(f"{'':>20}err_cfml%msg  = 'list_to_array1d_{s}_alloc_class: Cannot determine fortran type'\n")
    f.write(f"{'':>16}else if (fortran_type == '{s}') then\n")
    f.write(f"{'':>20}allocate(arr(n),source=src1)\n")
    n = 2
    for ti in tipos:
        f.write(f"{'':>16}else if (fortran_type == '{ti.name}') then\n")
        f.write(f"{'':>24}allocate(arr(n),source=src{n})\n")
        n += 1
    f.write(f"{'':>16}else\n")
    f.write(f"{'':>20}ierror = -1\n")
    f.write(f"{'':>20}err_cfml%flag = .true.\n")
    f.write(f"{'':>20}err_cfml%ierr = -1\n")
    f.write(f"{'':>20}err_cfml%msg  = 'list_to_array1d_{s}_alloc_class: Wrong fortran type'\n")
    f.write(f"{'':>16}end if\n")
    f.write(f"{'':>12}end if\n")
    f.write(f"{'':>12}do i = 0 , n-1\n")
    f.write(f"{'':>16}if (ierror == 0) ierror = my_list%getitem(item,i)\n")
    f.write(f"{'':>16}if (ierror == 0) ierror = cast(my_dict,item)\n")
    f.write(f"{'':>16}if (ierror == 0) call unwrap_{t[s].lucy}_no_alloc_class(my_dict,arr(i+1),ierror)\n")
    f.write(f"{'':>12}end do\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"\n{'':>4}End Subroutine list_to_alloc_array1d_{s}_alloc_class\n")

def write_list_to_array2d(f,t : dict,s : str,alloc : bool= True) -> None:

    if alloc:
        f.write(f"\n{'':>4}Module Subroutine list_to_alloc_array2d_{s}(procedure_name,var_name,my_list,arr,ierror)\n")
    else:
        f.write(f"\n{'':>4}Module Subroutine list_to_no_alloc_array2d_{s}(procedure_name,var_name,my_list,arr,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    f.write(f"{'':>8}character(len=*), intent(in) :: procedure_name\n")
    f.write(f"{'':>8}character(len=*), intent(in) :: var_name\n")
    f.write(f"{'':>8}type(list), intent(inout) :: my_list\n")
    if alloc:
        f.write(f"{'':>8}type({s}), dimension(:,:), allocatable, intent(out) :: arr\n")
    else:
        f.write(f"{'':>8}type({s}), dimension(:,:), intent(out) :: arr\n")
    f.write(f"{'':>8}integer, intent(inout) :: ierror\n")
    # Local variables
    f.write(f"\n{'':>8}! Local variables\n")
    f.write(f"{'':>8}integer :: i,j,m,n\n")
    f.write(f"{'':>8}type(object) :: item\n")
    f.write(f"{'':>8}type(dict) :: my_dict\n")
    f.write(f"{'':>8}type(list) :: li\n")
    f.write(f"\n{'':>8}ierror = my_list%len(m)\n")
    f.write(f"{'':>8}if (ierror /= 0) then\n")
    f.write(f"{'':>12}ierror = -1\n")
    f.write(f"{'':>12}err_cfml%flag = .true.\n")
    f.write(f"{'':>12}err_cfml%ierr = -1\n")
    f.write(f"{'':>12}err_cfml%msg  = 'list_to_no_alloc_array2d_{s}: Error getting array dimension'\n")
    f.write(f"{'':>8}end if\n")    
    if not alloc:
        f.write(f"{'':>8}if (m /= size(arr,1)) then\n")
        f.write(f"{'':>12}ierror = -1\n")
        f.write(f"{'':>12}err_cfml%flag = .true.\n")
        f.write(f"{'':>12}err_cfml%ierr = -1\n")
        f.write(f"{'':>12}err_cfml%msg  = 'list_to_no_alloc_array2d_{s}: Dimension of list and arr inconsistent'\n")
        f.write(f"{'':>8}end if\n")
    f.write(f"{'':>8}if (ierror == 0 .and. m > 0) then\n")
    f.write(f"{'':>12}if (ierror == 0) ierror = my_list%getitem(item,0)\n")
    f.write(f"{'':>12}if (ierror == 0) ierror = cast(li,item)\n")
    f.write(f"{'':>12}if (ierror == 0) ierror = li%len(n)\n")
    f.write(f"{'':>12}if (ierror == 0) then\n")
    f.write(f"{'':>16}do i = 0 , m-1\n")
    f.write(f"{'':>20}if (ierror == 0) ierror = my_list%getitem(item,i)\n")
    f.write(f"{'':>20}if (ierror == 0) ierror = cast(li,item)\n")
    f.write(f"{'':>20}if (ierror == 0) ierror = li%len(n)\n")    
    if not alloc:
        f.write(f"{'':>20}if (n /= size(arr,2)) then\n")
        f.write(f"{'':>24}ierror = -1\n")
        f.write(f"{'':>24}err_cfml%flag = .true.\n")
        f.write(f"{'':>24}err_cfml%ierr = -1\n")
        f.write(f"{'':>24}err_cfml%msg  = 'list_to_no_alloc_array2d_{s}: Dimension of list and arr inconsistent'\n")
        f.write(f"{'':>20}end if\n")
    else:
        f.write(f"{'':>20}if (ierror == 0 .and. .not. allocated(arr)) allocate(arr(m,n))\n")
    f.write(f"{'':>20}do j = 0 , n-1\n")
    f.write(f"{'':>24}if (ierror == 0) ierror = li%getitem(item,j)\n")
    f.write(f"{'':>24}if (ierror == 0) ierror = cast(my_dict,item)\n") 
    if not t[s].parent: 
        if not t[s].childs:   
            f.write(f"{'':>24}if (ierror == 0) call unwrap_{s}(my_dict,arr(i+1,j+1),ierror)\n")
        else:
            f.write(f"{'':>24}if (ierror == 0) call unwrap_{s}_no_alloc_class(my_dict,arr(i+1,j+1),ierror)\n")
    else:
        f.write(f"{'':>24}if (ierror == 0) call unwrap_{t[s].lucy}_no_alloc_class(my_dict,arr(i+1,j+1),ierror)\n")
    f.write(f"{'':>24}if (ierror == 0) ierror = err_cfml%ierr\n")
    f.write(f"{'':>20}end do\n")
    f.write(f"{'':>16}end do\n")
    f.write(f"{'':>12}end if\n")
    f.write(f"{'':>8}end if\n")
    if alloc:
        f.write(f"\n{'':>4}End Subroutine list_to_alloc_array2d_{s}\n")
    else:
        f.write(f"\n{'':>4}End Subroutine list_to_no_alloc_array2d_{s}\n")

def write_list_to_array2d_alloc_class(f,t : dict,s : str) -> None:

    f.write(f"\n{'':>4}Module Subroutine list_to_alloc_array2d_{s}_alloc_class(procedure_name,var_name,my_list,arr,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    f.write(f"{'':>8}character(len=*), intent(in) :: procedure_name\n")
    f.write(f"{'':>8}character(len=*), intent(in) :: var_name\n")
    f.write(f"{'':>8}type(list), intent(inout) :: my_list\n")
    f.write(f"{'':>8}class({s}), dimension(:,:), allocatable, intent(out) :: arr\n")
    f.write(f"{'':>8}integer, intent(inout) :: ierror\n")
    # Levels
    levels = get_levels(t,s)
    nlevels = len(levels)
    # Local variables
    tipos = []
    for l in levels:
        for ti in l:
            tipos.append(ti)
    # Local variables
    f.write(f"\n{'':>8}! Local variables\n")
    f.write(f"{'':>8}integer :: i,j,m,n\n")
    f.write(f"{'':>8}character(len=:), allocatable :: fortran_type\n")
    f.write(f"{'':>8}type(object) :: item\n")
    f.write(f"{'':>8}type(dict) :: my_dict\n")
    f.write(f"{'':>8}type(list) :: li\n")
    f.write(f"{'':>8}type({s}) :: src1\n")
    n = 2
    for ti in tipos:
        f.write(f"{'':>8}type({ti.name}) :: src{n}\n")
        n += 1
    f.write(f"\n{'':>8}ierror = my_list%len(m)\n")   
    f.write(f"{'':>8}if (ierror /= 0 .or. m < 1) then\n")
    f.write(f"{'':>12}ierror = -1\n")
    f.write(f"{'':>12}err_cfml%flag = .true.\n")
    f.write(f"{'':>12}err_cfml%ierr = -1\n")
    f.write(f"{'':>12}err_cfml%msg  = 'list_to_alloc_array2d_{s}_alloc_class: Error getting array dimension'\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"{'':>8}if (ierror == 0) then\n")
    f.write(f"{'':>12}if (ierror == 0) ierror = my_list%getitem(item,0)\n")
    f.write(f"{'':>12}if (ierror == 0) ierror = cast(li,item)\n")
    f.write(f"{'':>12}if (ierror == 0) ierror = li%len(n)\n")
    f.write(f"{'':>12}if (ierror == 0) then\n")
    f.write(f"{'':>16}if (n < 1) then\n")
    f.write(f"{'':>20}ierror = -1\n")
    f.write(f"{'':>20}err_cfml%flag = .true.\n")
    f.write(f"{'':>20}err_cfml%ierr = -1\n")
    f.write(f"{'':>20}err_cfml%msg  = 'list_to_alloc_array2d_{s}_alloc_class: Error getting array dimension'\n")
    f.write(f"{'':>16}end if\n")
    f.write(f"{'':>16}if (ierror == 0) then\n")
    f.write(f"{'':>20}if (ierror == 0) ierror = li%getitem(item,0)\n")
    f.write(f"{'':>20}if (ierror == 0) ierror = cast(my_dict,item)\n")
    f.write(f"{'':>20}if (ierror == 0) then\n")
    f.write(f"{'':>24}ierror = my_dict%getitem(fortran_type,'fortran_type')\n")
    f.write(f"{'':>24}if (ierror /= 0) then\n")
    f.write(f"{'':>28}err_cfml%flag = .true.\n")
    f.write(f"{'':>28}err_cfml%ierr = -1\n")
    f.write(f"{'':>28}err_cfml%msg  = 'list_to_array2d_{s}_alloc_class: Cannot determine fortran type'\n")
    f.write(f"{'':>24}else if (fortran_type == '{s}') then\n")
    f.write(f"{'':>28}allocate(arr(m,n),source=src1)\n")
    n = 2
    for ti in tipos:
        f.write(f"{'':>24}else if (fortran_type == '{ti.name}') then\n")
        f.write(f"{'':>28}allocate(arr(m,n),source=src{n})\n")
        n += 1
    f.write(f"{'':>24}else\n")
    f.write(f"{'':>28}ierror = -1\n")
    f.write(f"{'':>28}err_cfml%flag = .true.\n")
    f.write(f"{'':>28}err_cfml%ierr = -1\n")
    f.write(f"{'':>28}err_cfml%msg  = 'list_to_array2d_{s}_alloc_class: Wrong fortran type'\n")
    f.write(f"{'':>24}end if\n")
    f.write(f"{'':>24}do i = 0 , m-1\n")
    f.write(f"{'':>28}do j = 0 , n-1\n")
    f.write(f"{'':>32}if (ierror == 0) ierror = li%getitem(item,j)\n")
    f.write(f"{'':>32}if (ierror == 0) ierror = cast(my_dict,item)\n") 
    f.write(f"{'':>32}if (ierror == 0) call unwrap_{t[s].lucy}_no_alloc_class(my_dict,arr(i+1,j+1),ierror)\n")
    f.write(f"{'':>32}if (ierror == 0) ierror = err_cfml%ierr\n")
    f.write(f"{'':>28}end do\n")
    f.write(f"{'':>24}end do\n")
    f.write(f"{'':>20}end if\n")
    f.write(f"{'':>16}end if\n")
    f.write(f"{'':>12}end if\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"\n{'':>4}End Subroutine list_to_alloc_array2d_{s}_alloc_class\n")

def write_unwrap_proc(f,t : dict,s : str) -> None:

    publics_unwrap.append(s)
    f.write(f"\n{'':>4}Module Subroutine Unwrap_{s}(py_var,for_var,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    f.write(f"{'':>8}type(dict), intent(inout) :: py_var\n")
    f.write(f"{'':>8}type({s}), intent(out) :: for_var\n")
    f.write(f"{'':>8}integer, intent(out) :: ierror\n")
    # Local variables
    local_var = local_variables_unwrap([t[s]])
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
    f.write(f"{'':>12}if (fortran_type /= '{s}') then\n")
    f.write(f"{'':>16}ierror = -1\n")
    f.write(f"{'':>16}err_cfml%flag = .true.\n")
    f.write(f"{'':>16}err_cfml%ierr = ierror\n")
    f.write(f"{'':>16}err_cfml%msg  = 'Unwrap_{s}: Wrong fortran type:'//adjustl(trim(fortran_type))\n")
    f.write(f"{'':>12}end if\n")
    f.write(f"{'':>8}end if\n")
    write_unwrap_type(f,t,s,8,'for_var')
    f.write(f"{'':>8}if (ierror /= 0) then\n")
    f.write(f"{'':>12}err_cfml%flag = .true.\n")
    f.write(f"{'':>12}err_cfml%ierr = -1\n")
    f.write(f"{'':>12}err_cfml%msg  = 'Unwrap_{s}: Unwrapping failed'\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"\n{'':>4}End Subroutine Unwrap_{s}\n")

def write_unwrap_proc_alloc_class(f,t : dict,s : str) -> None:

    publics_unwrap_alloc_class.append(s)
    f.write(f"\n{'':>4}Module Subroutine Unwrap_{s}_alloc_class(py_var,for_var,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    f.write(f"{'':>8}type(dict), intent(inout) :: py_var\n")
    f.write(f"{'':>8}class({s}), allocatable, intent(out) :: for_var\n")
    f.write(f"{'':>8}integer, intent(out) :: ierror\n")
    # Levels
    levels = get_levels(t,s)
    nlevels = len(levels)
    # Local variables
    tipos = []
    for l in levels:
        for ti in l:
            tipos.append(ti)
    local_var = local_variables_unwrap(tipos)
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
    f.write(f"{'':>12}err_cfml%msg  = 'Unwrap_{s}_alloc_class: Cannot determine fortran type'\n")
    f.write(f"{'':>8}else\n")
    f.write(f"{'':>12}if (fortran_type == '{s}') then\n")
    f.write(f"{'':>16}allocate({s} :: for_var)\n")
    for n in range(1,nlevels):
        for ti in levels[n]:
            f.write(f"{'':>12}else if (fortran_type == '{ti.name}') then\n")
            f.write(f"{'':>16}allocate({ti.name} :: for_var)\n")
    f.write(f"{'':>12}else\n")
    f.write(f"{'':>16}ierror = -1\n")
    f.write(f"{'':>16}err_cfml%flag = .true.\n")
    f.write(f"{'':>16}err_cfml%ierr = ierror\n")
    f.write(f"{'':>16}err_cfml%msg  = 'Unwrap_{s}_alloc_class: Wrong fortran type:'//adjustl(trim(fortran_type))\n")
    f.write(f"{'':>12}end if\n")
    f.write(f"{'':>8}end if\n")
    write_unwrap_type(f,t,s,8,'for_var')
    f.write(f"{'':>8}if (ierror == 0) then\n")
    for n in range(1,nlevels):
        f.write(f"{'':>12}select type (A => for_var)\n")
        for ti in levels[n]:
            f.write(f"{'':>16}class is ({ti.name})\n")
            write_unwrap_type(f,t,ti.name,20,'A')
        f.write(f"{'':>12}end select\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"{'':>8}if (ierror /= 0) then\n")
    f.write(f"{'':>12}err_cfml%flag = .true.\n")
    f.write(f"{'':>12}err_cfml%ierr = -1\n")
    f.write(f"{'':>12}err_cfml%msg  = 'Unwrap_{s}_alloc_class: Unwrapping failed'\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"\n{'':>4}End Subroutine Unwrap_{s}_alloc_class\n")

def write_unwrap_proc_no_alloc_class(f,t : dict,s : str) -> None:

    publics_unwrap_no_alloc_class.append(s)
    f.write(f"\n{'':>4}Module Subroutine Unwrap_{s}_no_alloc_class(py_var,for_var,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    f.write(f"{'':>8}type(dict), intent(inout) :: py_var\n")
    f.write(f"{'':>8}class({s}), intent(out) :: for_var\n")
    f.write(f"{'':>8}integer, intent(out) :: ierror\n")
    # Levels
    levels = get_levels(t,s)
    nlevels = len(levels)
    # Local variables
    tipos = []
    for l in levels:
        for ti in l:
            tipos.append(ti)
    local_var = local_variables_unwrap(tipos)
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
    f.write(f"{'':>12}err_cfml%msg  = 'Unwrap_{s}_no_alloc_class: Cannot determine fortran type'\n")
    f.write(f"{'':>8}else\n")
    f.write(f"{'':>12}if (fortran_type /= '{s}' &\n")
    for n in range(1,nlevels):
        for ti in levels[n]:
            f.write(f"{'':>16}.and. fortran_type /= '{ti.name}' &\n")
    f.write(f"{'':>16}) then\n")
    f.write(f"{'':>16}ierror = -1\n")
    f.write(f"{'':>16}err_cfml%flag = .true.\n")
    f.write(f"{'':>16}err_cfml%ierr = ierror\n")
    f.write(f"{'':>16}err_cfml%msg  = 'Unwrap_{s}_no_alloc_class: Wrong fortran type:'//adjustl(trim(fortran_type))\n")
    f.write(f"{'':>12}end if\n")
    f.write(f"{'':>8}end if\n")
    write_unwrap_type(f,t,s,8,'for_var')
    f.write(f"{'':>8}if (ierror == 0) then\n")
    for n in range(1,nlevels):
        f.write(f"{'':>12}select type (A => for_var)\n")
        for ti in levels[n]:
            f.write(f"{'':>16}class is ({ti.name})\n")
            write_unwrap_type(f,t,ti.name,20,'A')
        f.write(f"{'':>12}end select\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"{'':>8}if (ierror /= 0) then\n")
    f.write(f"{'':>12}err_cfml%flag = .true.\n")
    f.write(f"{'':>12}err_cfml%ierr = -1\n")
    f.write(f"{'':>12}err_cfml%msg  = 'Unwrap_{s}_no_alloc_class: Unwrapping failed'\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"\n{'':>4}End Subroutine Unwrap_{s}_no_alloc_class\n")

def write_unwrap_type(f,t : dict,s : str,n : int,forvar : str):

    tab = ''
    for i in range(n):
        tab = tab + ' '
    for c in t[s].components:
        var = t[s].components[c]
        if var.ndim == 0: # No arrays
            if var.ftype.lower() in PRIMITIVES:
                f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t[s].name}','{var.name}',py_var,{forvar}%{var.name},ierror)\n")
            else:
                f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t[s].name}','{var.name}',py_var,di_{var.name},ierror)\n")
                if var.is_class:
                    if var.allocatable:
                        f.write(f"{tab}if (ierror == 0) call unwrap_{var.ftype}_alloc_class(di_{var.name},{forvar}%{var.name},ierror)\n")
                    else:
                        f.write(f"{tab}if (ierror == 0) call unwrap_{var.ftype}_no_alloc_class(di_{var.name},{forvar}%{var.name},ierror)\n")
                else:
                    if publics_types[var.ftype].childs or publics_types[var.ftype].childs:
                        f.write(f"{tab}if (ierror == 0) call unwrap_{publics_types[var.ftype].lucy}_no_alloc_class(di_{var.name},{forvar}%{var.name},ierror)\n")
        else: # arrays
            if var.ftype.lower() in NUMERICALS:
                if var.ftype.lower() == 'integer':
                    pointer = 'p_int_'+str(var.ndim)+'d'
                elif var.ftype.lower() == 'real':
                    pointer = 'p_real_'+str(var.ndim)+'d'
                else:
                    pointer = 'p_complex_'+str(var.ndim)+'d'
                if var.allocatable:
                    func = 'pointer_to_alloc_array'
                else:
                    func = 'pointer_to_array'
                if var.ndim == 1:
                    f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t[s].name}','{var.name}',py_var,{pointer},ierror)\n")
                    f.write(f"{tab}if (ierror == 0) call {func}('Unwrap_{t[s].name}','{var.name}',{pointer},{forvar}%{var.name},ierror)\n")
                else:
                    f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t[s].name}','{var.name}',py_var,{pointer},ierror,order)\n")
                    f.write(f"{tab}if (ierror == 0) call {func}('Unwrap_{t[s].name}','{var.name}',{pointer},{forvar}%{var.name},ierror,order)\n")
            else:
                f.write(f"{tab}if (ierror == 0) ierror = list_create(my_list)\n")
                f.write(f"{tab}if (ierror == 0) call unwrap_dict_item('Unwrap_{t[s].name}','{var.name}',py_var,my_list,ierror)\n")
                if var.is_class:
                    f.write(f"{tab}if (ierror == 0) call list_to_alloc_array_alloc_class('Unwrap_{t[s].name}','{var.name}',my_list,{forvar}%{var.name},ierror)\n")
                else:
                    if var.allocatable:
                        f.write(f"{tab}if (ierror == 0) call list_to_alloc_array('Unwrap_{t[s].name}','{var.name}',my_list,{forvar}%{var.name},ierror)\n")
                    else:
                        f.write(f"{tab}if (ierror == 0) call list_to_no_alloc_array('Unwrap_{t[s].name}','{var.name}',my_list,{forvar}%{var.name},ierror)\n")
                f.write(f"{tab}if (ierror == 0) call my_list%destroy\n")

def write_wrap_proc(f,t : dict,s : str) -> None:

    f.write(f"\n{'':>4}Module Subroutine Wrap_{s}(for_var,py_var,ierror)\n")
    # Arguments
    f.write(f"\n{'':>8}! Arguments\n")
    if t[s].childs:
        publics_wrap.append([s,'class'])
        f.write(f"{'':>8}class({s}), intent(in) :: for_var\n")
    else:
        publics_wrap.append([s,'type'])
        f.write(f"{'':>8}type({s}), intent(in) :: for_var\n")
    f.write(f"{'':>8}type(dict), intent(inout) :: py_var\n")
    f.write(f"{'':>8}integer, intent(out) :: ierror\n")
    # Levels
    levels = get_levels(t,s)
    # Local variables
    tipos = []
    for l in levels:
        for ti in l:
            tipos.append(ti)
    local_var = local_variables_wrap(tipos)
    f.write(f"\n{'':>8}! Local variables\n")
    for lv in local_var:
        f.write(f"{'':>8}{lv}\n")
    # Initialization
    f.write(f"\n{'':>8}ierror = 0\n")
    # Procedure
    write_wrap_type(f,t[s],8,'for_var')
    nlevels = len(levels)
    if nlevels > 1:        
        f.write(f"{'':>8}if (ierror == 0) then\n")
    for n in range(1,nlevels):
        f.write(f"{'':>12}select type (A => for_var)\n")
        for ti in levels[n]:
            f.write(f"{'':>16}class is ({ti.name})\n")
            write_wrap_type(f,ti,20,'A')
        f.write(f"{'':>12}end select\n")
    if nlevels > 1:
        f.write(f"{'':>8}end if\n")
    f.write(f"{'':>8}if (ierror /= 0) then\n")
    f.write(f"{'':>12}err_cfml%flag = .true.\n")
    f.write(f"{'':>12}err_cfml%ierr = -1\n")
    f.write(f"{'':>12}err_cfml%msg  = 'Wrap_{s}: Wrapping failed'\n")
    f.write(f"{'':>8}end if\n")
    f.write(f"\n{'':>4}End Subroutine Wrap_{s}\n")

def write_wrap_type(f,t : cfml_objects.FortranType,n : int,forvar : str):

    tab = ''
    for i in range(n):
        tab = tab + ' '
    for c in t.components:
        var = t.components[c]
        if var.ndim == 0:
            if var.ftype.lower() in PRIMITIVES:
                f.write(f"{tab}if (ierror == 0) ierror = py_var%setitem('{var.name}',{forvar}%{var.name})\n")
            else:
                f.write(f"{tab}if (ierror == 0) call wrap_{publics_types[var.ftype].lucy}({forvar}%{var.name},di_{var.name},ierror)\n")
                f.write(f"{tab}if (ierror == 0) ierror = py_var%setitem('{var.name}',di_{var.name})\n")
        else:
            if var.ftype.lower() in NUMERICALS:
                f.write(f"{tab}if (ierror == 0) ierror = ndarray_create(nd_{var.name},{forvar}%{var.name})\n")
                f.write(f"{tab}if (ierror == 0) ierror = py_var%setitem('{var.name}',nd_{var.name})\n")
            elif var.ftype.lower() in PRIMITIVES:
                if var.ndim == 1:
                    f.write(f"{tab}if (ierror == 0) ierror = list_create(li_{var.name})\n")
                    f.write(f"{tab}if (ierror == 0) then\n")
                    f.write(f"{tab}    do i = 1 , size({forvar}%{var.name})\n")
                    f.write(f"{tab}        if (ierror == 0) ierror = li_{var.name}%append({forvar}%{var.name}(i))\n")
                    f.write(f"{tab}    end do\n")
                    f.write(f"{tab}end if\n")
                    f.write(f"{tab}if (ierror == 0) ierror = py_var%setitem('{var.name}',li_{var.name})\n")
                elif var.ndim == 2: 
                    f.write(f"{tab}if (ierror == 0) ierror = list_create(li_{var.name})\n")
                    f.write(f"{tab}if (ierror == 0) then\n")
                    f.write(f"{tab}    do i = 1 , size({forvar}%{var.name},1)\n")
                    f.write(f"{tab}        if (ierror == 0) ierror = list_create(li)\n")
                    f.write(f"{tab}        do j = 1 , size({forvar}%{var.name},2)\n")
                    f.write(f"{tab}            if (ierror == 0) ierror = li%append({forvar}%{var.name}(i,j))\n")
                    f.write(f"{tab}        end do\n")
                    f.write(f"{tab}        if (ierror == 0) ierror = li_{var.name}%append(li)\n")
                    f.write(f"{tab}        if (ierror == 0) call li%destroy\n")
                    f.write(f"{tab}    end do\n")
                    f.write(f"{tab}end if\n")
                    f.write(f"{tab}if (ierror == 0) ierror = py_var%setitem('{var.name}',li_{var.name})\n")
            else:
                if var.ndim == 1:
                    f.write(f"{tab}if (ierror == 0) ierror = list_create(li_{var.name})\n")
                    f.write(f"{tab}if (ierror == 0) allocate(di_{var.name}(size({forvar}%{var.name})))\n")
                    f.write(f"{tab}if (ierror == 0) then\n")
                    f.write(f"{tab}    do i = 1 , size({forvar}%{var.name})\n")
                    f.write(f"{tab}        ierror = dict_create(di_{var.name}(i))\n")
                    f.write(f"{tab}        if (ierror == 0) call wrap_{publics_types[var.ftype].lucy}({forvar}%{var.name}(i),di_{var.name}(i),ierror)\n")
                    f.write(f"{tab}        if (ierror == 0) ierror = li_{var.name}%append(di_{var.name}(i))\n")
                    f.write(f"{tab}    end do\n")
                    f.write(f"{tab}end if\n")
                    f.write(f"{tab}if (ierror == 0) ierror = py_var%setitem('{var.name}',li_{var.name})\n")
                elif var.ndim == 2:
                    f.write(f"{tab}if (ierror == 0) ierror = list_create(li_{var.name})\n")
                    f.write(f"{tab}if (ierror == 0) allocate(di_{var.name}(size({forvar}%{var.name},1),size({forvar}%{var.name},2)))\n")
                    f.write(f"{tab}if (ierror == 0) then\n")
                    f.write(f"{tab}    do i = 1 , size({forvar}%{var.name},1)\n")
                    f.write(f"{tab}        if (ierror == 0) ierror = list_create(li)\n")                    
                    f.write(f"{tab}        do j = 1 , size({forvar}%{var.name},2)\n")
                    f.write(f"{tab}            ierror = dict_create(di_{var.name}(i,j))\n")
                    f.write(f"{tab}            if (ierror == 0) call wrap_{publics_types[var.ftype].lucy}({forvar}%{var.name}(i,j),di_{var.name}(i,j),ierror)\n")
                    f.write(f"{tab}            if (ierror == 0) ierror = li%append(di_{var.name}(i,j))\n")
                    f.write(f"{tab}        end do\n")
                    f.write(f"{tab}        if (ierror == 0) ierror = li_{var.name}%append(li)\n")
                    f.write(f"{tab}        if (ierror == 0) call li%destroy\n")
                    f.write(f"{tab}    end do\n")
                    f.write(f"{tab}end if\n")
                    f.write(f"{tab}if (ierror == 0) ierror = py_var%setitem('{var.name}',li_{var.name})\n")