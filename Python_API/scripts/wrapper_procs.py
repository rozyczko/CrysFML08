"""
Python script for building CFML_Wraps.
Author: Nebil A. Katcho
February 2024

---------
Functions
---------
wrap(modules : dict) -> None
wrap_cfml_module_procs(m : cfml_objects.Module,m_name : str) -> None
write_init(f,nproc : int,m_name : str,m : cfml_objects.Module)
write_module_header(f,m_name : str,w_name : str,m : cfml_objects.Module) -> None:
write_pyinit(f,w_name : str) -> None:
"""
import cfml_objects
import os

publics_types = {}

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

	f.write(f"\n{'':>4}function py_{p.name.lower()}(self_ptr,args_ptr) result(resul) bind(c)\n")
	f.write(f"\n{'':>8}! Arguments\n")
	f.write(f"{'':>8}type(c_ptr), value :: self_ptr\n")
	f.write(f"{'':>8}type(c_ptr), value :: args_ptr\n")
	f.write(f"{'':>8}type(c_ptr) :: resul\n")
	f.write(f"\n{'':>4}end function py_{p.name.lower()}\n")

