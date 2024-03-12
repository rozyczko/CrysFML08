"""
Python script for building CFML_Wraps.
Author: Nebil A. Katcho
February 2024

---------
Functions
---------
get_mandatory(p) -> int
wrap(modules : dict) -> None
wrap_cfml_module_procs(m : cfml_objects.Module,m_name : str) -> None
write_init(f,nproc : int,m_name : str,m : cfml_objects.Module)
write_module_header(f,m_name : str,w_name : str,m : cfml_objects.Module) -> None:
write_pyinit(f,w_name : str) -> None:
"""
import cfml_objects
import os

publics_types = {}

def get_mandatory(p) -> int:
	n = 0
	for a in p.arguments:
		if not p.arguments[a].optional:
			n += 1
	return n

def get_methods(m : dict) -> list:

	pass

def wrap(modules : dict) -> None:

    if not os.path.isdir('../Python_API/src'):
        os.mkdir('../Python_API/src')
    for m in modules:
        wrap_cfml_module_procs(modules[m],m)

def wrap_cfml_module_procs(m : cfml_objects.Module,m_name : str) -> None:

	nproc = len(m.procedures)
	if nproc == 0:
		return
	w_name = 'py_'+m_name.lower()
	w_file = os.path.join('../Python_API/src',w_name+'.f90')
	with open(w_file,'w') as f:
		write_module_header(f,m_name.lower(),w_name.lower(),m)
		write_pyinit(f,w_name.lower())
		write_init(f,nproc,m_name,m)
		for p in m.procedures:
			write_wrap_procedure(f,m.procedures[p])
		f.write(f"\nend module {w_name}")

def write_init(f,nproc : int,m_name : str,m : cfml_objects.Module):

	f.write(f"\n{'':>4}function Init() result(m)\n")
	f.write(f"\n{'':>8}! Local variables\n")
	f.write(f"{'':>8}type(c_ptr) :: m\n")
	f.write(f"{'':>8}integer :: ierror\n")
	f.write(f"\n{'':>8}ierror = Forpy_Initialize()\n")
	f.write(f"\n{'':>8}! Build method table\n")
	f.write(f"{'':>8}call table_{m_name[5:].lower()}%init({nproc})\n")
	for p in m.procedures:
		f.write(f"{'':>8}call table_{m_name[5:].lower()}%add_method('{p}','py_{p}',&\n")
		f.write(f"{'':>24}METH_VARARGS,c_funloc(py_{p.lower()}))\n")
	f.write(f"\n{'':>8}! Build mod_m_name\n")
	f.write(f"{'':>8}m = mod_{m_name[5:].lower()}%init('py_{m_name.lower()}','Python wrapper for module {m_name} of CrysFML08',table_{m_name[5:].lower()})\n")
	f.write(f"\n{'':>4}end function Init()\n")

def write_module_header(f,m_name : str,w_name : str,m : cfml_objects.Module):

	f.write(f"module {w_name}\n")
	f.write(f"\n{'':>4}use forpy\n")
	f.write(f"{'':>4}use iso_c_binding\n")
	for u in m.uses:
		f.write(f"{'':>4}{u.lstrip()}")
	f.write(f"{'':>4}use CFML_Wraps\n")
	f.write(f"{'':>4}use CFML_Wraps_Utils\n")
	f.write(f"\n{'':>4}implicit none\n")
	f.write(f"\n{'':>4}type(PythonModule), save :: mod_{m_name[5:]}\n")
	f.write(f"{'':>4}type(PythonMethodTable), save :: table_{m_name[5:]}\n")		
	f.write(f"\n{'':>4}contains\n")

def write_pyinit(f,w_name : str):

	f.write(f"\n{'':>4}function PyInit_{w_name}() bind(c,name='PyInit_py_{w_name}') result(m)\n")
	f.write(f"{'':>4}!DEC$ ATTRIBUTES DLLEXPORT :: PyInit_{w_name}\n")
	f.write(f"\n{'':>8}! Local variables\n")
	f.write(f"{'':>8}type(c_ptr) :: m\n")
	f.write(f"\n{'':>8}m = Init()\n")
	f.write(f"\n{'':>4}end function PyInit_{w_name}\n")

def write_wrap_procedure(f,p):

	proc_name = p.name.lower()
	f.write(f"\n{'':>4}function py_{proc_name}(self_ptr,args_ptr) result(resul) bind(c)\n")
	# Arguments
	f.write(f"\n{'':>8}! Arguments\n")
	f.write(f"{'':>8}type(c_ptr), value :: self_ptr\n")
	f.write(f"{'':>8}type(c_ptr), value :: args_ptr\n")
	f.write(f"{'':>8}type(c_ptr) :: resul\n")
	# CrysFML08 arguments
	f.write(f"\n{'':>8}! CrysFML08 arguments\n")
	for arg in p.arguments:
		a = p.arguments[arg]
		if not a.primitive:
			if a.ndim == 0:
				a.ptype = 'dict'
				f.write(f"{'':>8}type(dict) :: di_{a.name} ! CrysFML08 : {a.fortran_def}\n")
			else:
				a.ptype = 'list'
				f.write(f"{'':>8}type(list) :: li_{a.name} ! CrysFML08 : {a.fortran_def}\n")
		else:
			if a.ndim > 0:
				if a.ftype == 'integer' or a.ftype == 'real' or a.ftype == 'complex':
					f.write(f"{'':>8}type(ndarray) :: nd_{a.name} ! CrysFML08 type : {a.fortran_def}\n")
					a.ptype = 'ndarray'
				else:
					f.write(f"{'':>8}type(list) :: li_{a.name} ! CrysFML08 type : {a.fortran_def}\n")
					a.ptype = 'list'
			else:
				a.ptype = ''
	# Local variables	
	f.write(f"\n{'':>8}! Local variables\n")
	n = get_mandatory(p)
	f.write(f"{'':>8}integer, parameter :: NMANDATORY = {n}\n")
	f.write(f"{'':>8}integer :: ierror,narg\n")
	f.write(f"{'':>8}type(object) :: item\n")
	f.write(f"{'':>8}type(tuple) :: args,ret\n")
	# Procedure
	f.write(f"\n{'':>8}ierror = 0\n")
	f.write(f"{'':>8}call clear_error()\n")
	f.write(f"\n{'':>8}! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict\n")
	f.write(f"{'':>8}call unsafe_cast_from_c_ptr(args,args_ptr)\n")
	f.write(f"\n{'':>8}! Get arguments\n")
	f.write(f"{'':>8}call check_number_of_arguments('py_{proc_name}',args,NMANDATORY,narg,ierror)\n")
	f.write(f"\n{'':>4}end function py_{proc_name}\n")

