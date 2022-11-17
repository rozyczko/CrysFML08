"""
---------
Functions
---------
create_scripts(directory : str) -> None
create_script_windows() -> None
"""

import colorama
import os

def create_scripts(directory : str) -> None:

    print(f"{colorama.Fore.GREEN}{'Writing compilation scripts'}{colorama.Style.RESET_ALL}")
    os.chdir(directory)
    if not os.path.isdir('scripts'):
        os.makedirs('scripts')
    if not os.path.isdir('scripts/windows'):
        os.makedirs('scripts/windows')
    create_script_windows()

def create_script_windows() -> None:

    opt="/I%CRYSFML08_INSTALL%\\include /fpp /libs:dll /threads /c"
    with open('scripts/windows/make_ifort_crysfml08api.bat','w') as f:
        f.write(f"{'@echo off'}\n")
        f.write(f"\nifort %FORPY%\\forpy_mod.f90 /fpp /libs:dll /threads /c\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_atoms.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_eos.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_extincorr.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_geom.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_gspacegroups.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_keycodes.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_maps.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_maths.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_profiles.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_random.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_scattering_tables.f90 {opt}\n")
        #f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_strings.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_sxtal_geom.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\py_cfml_symmetry_tables.f90 {opt}\n")
        f.write(f"ifort ..\\..\\src\\fortran\\API_init.f90 {opt}\n")
        f.write(f"\nlink *.obj /out:\"crysfml08_api.dll\" /libpath:%CRYSFML08_INSTALL%\\lib /dll libCrysFML08.a %LIBPYTHON% /NODEFAULTLIB:libcmt.lib\n")
        f.write(f"\nmove crysfml08_api.dll ..\\..\\src\\python\\crysfml08_api.pyd\n")
        f.write(f"del *.obj *.mod *.exp *.lib")