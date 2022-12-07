def create_polar_calc_type():

    d = {}

    d['h']                             = 0.0
    d['spv']                           = 0.0
    d['cell']                          = None
    d['p']                             = 0.0
    d['miv']                           = 0.0
    d['nsf']                           = 0.0
    d['nc']                            = 0.0
    d['my']                            = 0.0
    d['mz']                            = 0.0
    d['ry']                            = 0.0
    d['rz']                            = 0.0
    d['iy']                            = 0.0
    d['iz']                            = 0.0
    d['tc']                            = 0.0
    d['mm']                            = 0.0
    d['cs']                            = 0.0
    d['pij']                           = 0.0
    d['ftype']['h']                    = 'real(kind=cp),dimension(3)'
    d['ftype']['spv']                  = 'real(kind=cp),dimension(3)'
    d['ftype']['cell']                 = 'type(cell_g_type)'
    d['ftype']['p']                    = 'real(kind=cp)'
    d['ftype']['miv']                  = 'complex(kind=cp),dimension(3,2,24)'
    d['ftype']['nsf']                  = 'complex(kind=cp)'
    d['ftype']['nc']                   = 'real(kind=cp)'
    d['ftype']['my']                   = 'real(kind=cp),dimension(2,24)'
    d['ftype']['mz']                   = 'real(kind=cp),dimension(2,24)'
    d['ftype']['ry']                   = 'real(kind=cp),dimension(2,24)'
    d['ftype']['rz']                   = 'real(kind=cp),dimension(2,24)'
    d['ftype']['iy']                   = 'real(kind=cp),dimension(2,24)'
    d['ftype']['iz']                   = 'real(kind=cp),dimension(2,24)'
    d['ftype']['tc']                   = 'real(kind=cp),dimension(2,24)'
    d['ftype']['mm']                   = 'real(kind=cp),dimension(2,24)'
    d['ftype']['cs']                   = 'real(kind=cp),dimension(3,2,24)'
    d['ftype']['pij']                  = 'real(kind=cp),dimension(3,3)'

    return d

def create_polar_calc_list_type():

    d = {}

    d['nref']                          = None
    d['polari']                        = None
    d['ftype']['nref']                 = 'integer'
    d['ftype']['polari']               = 'type(polar_calc_type),dimension(:),allocatable'

    return d

def create_polar_calcmulti_list_type():

    d = {}

    d['nset']                          = None
    d['polarilist']                    = None
    d['ftype']['nset']                 = 'integer'
    d['ftype']['polarilist']           = 'type(polar_calc_list_type),dimension(:),allocatable'

    return d

def create_polar_info_type():

    d = {}

    d['h']                             = None
    d['spv']                           = None
    d['cell']                          = None
    d['p']                             = None
    d['miv']                           = None
    d['nsf']                           = None
    d['nc']                            = None
    d['my']                            = None
    d['mz']                            = None
    d['ry']                            = None
    d['rz']                            = None
    d['iy']                            = None
    d['iz']                            = None
    d['tc']                            = None
    d['mm']                            = None
    d['cs']                            = None
    d['pij']                           = None
    d['ftype']['h']                    = 'real(kind=cp),dimension(3)'
    d['ftype']['spv']                  = 'real(kind=cp),dimension(3)'
    d['ftype']['cell']                 = 'type(cell_g_type)'
    d['ftype']['p']                    = 'real(kind=cp)'
    d['ftype']['miv']                  = 'complex(kind=cp),dimension(3)'
    d['ftype']['nsf']                  = 'complex(kind=cp)'
    d['ftype']['nc']                   = 'real(kind=cp)'
    d['ftype']['my']                   = 'real(kind=cp)'
    d['ftype']['mz']                   = 'real(kind=cp)'
    d['ftype']['ry']                   = 'real(kind=cp)'
    d['ftype']['rz']                   = 'real(kind=cp)'
    d['ftype']['iy']                   = 'real(kind=cp)'
    d['ftype']['iz']                   = 'real(kind=cp)'
    d['ftype']['tc']                   = 'real(kind=cp)'
    d['ftype']['mm']                   = 'real(kind=cp)'
    d['ftype']['cs']                   = 'real(kind=cp),dimension(3)'
    d['ftype']['pij']                  = 'real(kind=cp),dimension(3,3)'

    return d

def create_polar_obs_type():

    d = {}

    d['h']                             = None
    d['spv']                           = None
    d['p']                             = None
    d['opij']                          = None
    d['sopij']                         = None
    d['wopij']                         = None
    d['ftype']['h']                    = 'real(kind=cp),dimension(3)'
    d['ftype']['spv']                  = 'real(kind=cp),dimension(3)'
    d['ftype']['p']                    = 'real(kind=cp)'
    d['ftype']['opij']                 = 'real(kind=cp),dimension(3,3)'
    d['ftype']['sopij']                = 'real(kind=cp),dimension(3,3)'
    d['ftype']['wopij']                = 'real(kind=cp),dimension(3,3)'

    return d

def create_polar_obs_list_type():

    d = {}

    d['nref']                          = None
    d['polaro']                        = None
    d['ftype']['nref']                 = 'integer'
    d['ftype']['polaro']               = 'type(polar_obs_type),dimension(:),allocatable'

    return d

def create_polar_obsmulti_list_type():

    d = {}

    d['nset']                          = None
    d['polarolist']                    = None
    d['ftype']['nset']                 = 'integer'
    d['ftype']['polarolist']           = 'type(polar_obs_list_type),dimension(:),allocatable'

    return d

def create_polar_calc_svs_type():

    d = {}

    d['h']                             = None
    d['spv']                           = None
    d['cell']                          = None
    d['p']                             = None
    d['pij']                           = None
    d['ftype']['h']                    = 'real(kind=cp),dimension(3)'
    d['ftype']['spv']                  = 'real(kind=cp),dimension(3)'
    d['ftype']['cell']                 = 'type(cell_g_type)'
    d['ftype']['p']                    = 'real(kind=cp)'
    d['ftype']['pij']                  = 'real(kind=cp),dimension(3,3)'

    return d

def create_polar_calc_svs_list_type():

    d = {}

    d['nref']                          = None
    d['polarisvs']                     = None
    d['ftype']['nref']                 = 'integer'
    d['ftype']['polarisvs']            = 'type(polar_calc_svs_type),dimension(:),allocatable'

    return d

def create_polar_calcmulti_svs_list_type():

    d = {}

    d['nset']                          = None
    d['polarisvslist']                 = None
    d['ftype']['nset']                 = 'integer'
    d['ftype']['polarisvslist']        = 'type(polar_calc_svs_list_type),dimension(:),allocatable'

    return d

