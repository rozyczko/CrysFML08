def create_multistate_vector_type():

    d = {}

    d['npar']                          = None
    d['nconf']                         = None
    d['code']                          = None
    d['bound']                         = None
    d['state']                         = None
    d['stp']                           = None
    d['cost']                          = None
    d['low']                           = None
    d['high']                          = None
    d['config']                        = None
    d['sigma']                         = None
    d['nampar']                        = None
    d['best_cost']                     = None
    d['ftype']['npar']                 = 'integer'
    d['ftype']['nconf']                = 'integer'
    d['ftype']['code']                 = 'integer,dimension(np_san)'
    d['ftype']['bound']                = 'integer,dimension(np_san)'
    d['ftype']['state']                = 'real(kind=cp),dimension(np_san,np_conf)'
    d['ftype']['stp']                  = 'real(kind=cp),dimension(np_san,np_conf)'
    d['ftype']['cost']                 = 'real(kind=cp),dimension(np_conf)'
    d['ftype']['low']                  = 'real(kind=cp),dimension(np_san)'
    d['ftype']['high']                 = 'real(kind=cp),dimension(np_san)'
    d['ftype']['config']               = 'real(kind=cp),dimension(np_san)'
    d['ftype']['sigma']                = 'real(kind=cp),dimension(np_san)'
    d['ftype']['nampar']               = 'character(len=15),dimension(np_san)'
    d['ftype']['best_cost']            = 'real(kind=cp)'

    return d

def create_simann_conditions_type():

    d = {}

    d['t_ini']                         = None
    d['anneal']                        = None
    d['accept']                        = None
    d['threshold']                     = None
    d['initconfig']                    = None
    d['nalgor']                        = None
    d['nm_cycl']                       = None
    d['num_temps']                     = None
    d['num_therm']                     = None
    d['num_conf']                      = None
    d['cost_function_name']            = None
    d['seed']                          = None
    d['ftype']['t_ini']                = 'real(kind=cp)'
    d['ftype']['anneal']               = 'real(kind=cp)'
    d['ftype']['accept']               = 'real(kind=cp)'
    d['ftype']['threshold']            = 'real(kind=cp)'
    d['ftype']['initconfig']           = 'integer'
    d['ftype']['nalgor']               = 'integer'
    d['ftype']['nm_cycl']              = 'integer'
    d['ftype']['num_temps']            = 'integer'
    d['ftype']['num_therm']            = 'integer'
    d['ftype']['num_conf']             = 'integer'
    d['ftype']['cost_function_name']   = 'character(len=60)'
    d['ftype']['seed']                 = 'integer'

    return d

def create_state_vector_type():

    d = {}

    d['npar']                          = None
    d['code']                          = None
    d['bound']                         = None
    d['state']                         = None
    d['stp']                           = None
    d['low']                           = None
    d['high']                          = None
    d['config']                        = None
    d['cost']                          = None
    d['nampar']                        = None
    d['ftype']['npar']                 = 'integer'
    d['ftype']['code']                 = 'integer,dimension(np_san)'
    d['ftype']['bound']                = 'integer,dimension(np_san)'
    d['ftype']['state']                = 'real(kind=cp),dimension(np_san)'
    d['ftype']['stp']                  = 'real(kind=cp),dimension(np_san)'
    d['ftype']['low']                  = 'real(kind=cp),dimension(np_san)'
    d['ftype']['high']                 = 'real(kind=cp),dimension(np_san)'
    d['ftype']['config']               = 'real(kind=cp),dimension(np_san)'
    d['ftype']['cost']                 = 'real(kind=cp)'
    d['ftype']['nampar']               = 'character(len=15),dimension(np_san)'

    return d

