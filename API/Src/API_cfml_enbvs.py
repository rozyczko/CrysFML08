def create_atoms_conf_list_type():

    d = {}

    d['natoms']                        = None
    d['n_spec']                        = None
    d['n_anions']                      = None
    d['n_cations']                     = None
    d['tol']                           = None
    d['totatoms']                      = None
    d['species']                       = None
    d['radius']                        = None
    d['atom']                          = None
    d['ftype']['natoms']               = 'integer'
    d['ftype']['n_spec']               = 'integer'
    d['ftype']['n_anions']             = 'integer'
    d['ftype']['n_cations']            = 'integer'
    d['ftype']['tol']                  = 'real(kind=cp)'
    d['ftype']['totatoms']             = 'real(kind=cp)'
    d['ftype']['species']              = 'character(len=4),dimension(:),allocatable'
    d['ftype']['radius']               = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['atom']                 = 'type(atm_type),dimension(:),allocatable'

    return d

