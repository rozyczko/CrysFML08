"""
Python script for building CFML_Wraps.
Author: Nebil A. Katcho
February 2024

---------
Functions
---------
wrap(modules : dict,lucy : dict) -> None
wrap_cfml_module_procs(m : cfml_objects.Module,m_name : str,lucy : dict) -> None
"""
import cfml_objects
import os

def get_methods(m : dict) -> list:

	pass

def wrap(modules : dict,lucy : dict) -> None:

    if not os.path.isdir('../Python_API'):
        os.mkdir('../Python_API')
    for m in modules:
        wrap_cfml_module_procs(modules[m],m,lucy)

def wrap_cfml_module_procs(m : cfml_objects.Module,m_name : str,lucy : dict) -> None:

	methods = get_methods(m)
	w_name = 'py_'+m_name.lower()
	w_file = os.path.join('../Python_API',w_name+'.f90')
	with open(w_file,'w') as f:
		f.write(f"module {w_name}\n")
		f.write(f"\n{'':>4}use forpy\n")
		f.write(f"{'':>4}use iso_c_binding\n")
		for u in m.uses:
			f.write(f"{'':>4}{u.lstrip()}")
		f.write(f"\n{'':>4}implicit none\n")
		f.write(f"\n{'':>4}type(PythonModule), save :: mod_{m_name[5:].lower()}\n")
		f.write(f"{'':>4}type(PythonMethodTable), save :: table_{m_name[5:].lower()}\n")		
		f.write(f"\n{'':>4}contains\n")
		f.write(f"\n{'':>4}function PyInit_{w_name.lower()}() bind(c,name='PyInit_py_{w_name.lower()}') result(m)\n")
		f.write(f"{'':>4}!DEC$ ATTRIBUTES DLLEXPORT :: PyInit_{w_name.lower()}\n")
		f.write(f"\n{'':>4}type(c_ptr) :: m\n")
		f.write(f"\n{'':>4}end function PyInit_{w_name.lower()}\n")
		f.write(f"\nend module {w_name}")