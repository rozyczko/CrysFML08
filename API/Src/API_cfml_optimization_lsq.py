def create_lsq_conditions_type():

    d = {}

    d['constr']                        = False
    d['reached']                       = False
    d['failed']                        = False
    d['corrmax']                       = 50
    d['nfev']                          = 0
    d['njev']                          = 0
    d['icyc']                          = 0
    d['npvar']                         = 0
    d['iw']                            = 0
    d['nprint']                        = 0
    d['tol']                           = 0.0
    d['percent']                       = 0.0
    d['ftype']['constr']               = 'logical'
    d['ftype']['reached']              = 'logical'
    d['ftype']['failed']               = 'logical'
    d['ftype']['corrmax']              = 'integer'
    d['ftype']['nfev']                 = 'integer'
    d['ftype']['njev']                 = 'integer'
    d['ftype']['icyc']                 = 'integer'
    d['ftype']['npvar']                = 'integer'
    d['ftype']['iw']                   = 'integer'
    d['ftype']['nprint']               = 'integer'
    d['ftype']['tol']                  = 'real(kind=cp)'
    d['ftype']['percent']              = 'real(kind=cp)'

    return d

def create_lsq_data_type():

    d = {}

    d['nobs']                          = None
    d['iw']                            = None
    d['x']                             = None
    d['y']                             = None
    d['sw']                            = None
    d['yc']                            = None
    d['ftype']['nobs']                 = 'integer'
    d['ftype']['iw']                   = 'integer'
    d['ftype']['x']                    = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['y']                    = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['sw']                   = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['yc']                   = 'real(kind=cp),dimension(:),allocatable'

    return d

def create_lsq_state_vector_type():

    d = {}

    d['np']                            = None
    d['code_comp']                     = None
    d['code_max']                      = None
    d['mul']                           = None
    d['pv']                            = None
    d['spv']                           = None
    d['dpv']                           = None
    d['code']                          = None
    d['nampar']                        = None
    d['ftype']['np']                   = 'integer'
    d['ftype']['code_comp']            = 'logical'
    d['ftype']['code_max']             = 'integer(kind=2)'
    d['ftype']['mul']                  = 'real(kind=cp),dimension(max_free_par)'
    d['ftype']['pv']                   = 'real(kind=cp),dimension(max_free_par)'
    d['ftype']['spv']                  = 'real(kind=cp),dimension(max_free_par)'
    d['ftype']['dpv']                  = 'real(kind=cp),dimension(max_free_par)'
    d['ftype']['code']                 = 'integer(kind=4),dimension(max_free_par)'
    d['ftype']['nampar']               = 'character(len=40),dimension(max_free_par)'

    return d

