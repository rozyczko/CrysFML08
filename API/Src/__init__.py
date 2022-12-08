# **************************************************************************
#
# CrysFML API
#
# @file      Src/__init__.py
# @brief     __init__ for API module
#
# @homepage  https://code.ill.fr/scientific-software/crysfml
# @license   GNU LGPL (see LICENSE)
# @copyright Institut Laue Langevin 2020-now
# @authors   Scientific Computing Group at ILL (see AUTHORS)
#
# **************************************************************************

# Try to import fortran binding
try:
    import CFML_api.crysfml_api
    import CFML_api.powder_mod
except ImportError as e:
    raise ImportError(str(e) + "\n\n=> Fortran binding could not be found. It may not be properly compiled, or it may be linked with another Python interpreter")
