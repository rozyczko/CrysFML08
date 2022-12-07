def create_atomic_properties_type():

    d = {}

    d['z']                             = 0
    d['symb']                          = " "
    d['oxs']                           = 0
    d['dox']                           = 0
    d['mass']                          = 0.0
    d['n']                             = 0
    d['g']                             = 0
    d['b']                             = 0
    d['rc']                            = 0.0
    d['sigma']                         = 0.0
    d['ftype']['z']                    = 'integer'
    d['ftype']['symb']                 = 'character(len=4)'
    d['ftype']['oxs']                  = 'integer'
    d['ftype']['dox']                  = 'integer'
    d['ftype']['mass']                 = 'real(kind=cp)'
    d['ftype']['n']                    = 'integer'
    d['ftype']['g']                    = 'integer'
    d['ftype']['b']                    = 'integer'
    d['ftype']['rc']                   = 'real(kind=cp)'
    d['ftype']['sigma']                = 'real(kind=cp)'

    return d

def create_bvel_par_type():

    d = {}

    d['symb']                          = " "
    d['avcoor']                        = 0.0
    d['rzero']                         = 0.0
    d['rcutoff']                       = 0.0
    d['dzero']                         = 0.0
    d['rmin']                          = 0.0
    d['alpha']                         = 0.0
    d['refnum']                        = 0
    d['ftype']['symb']                 = 'character(len=5)'
    d['ftype']['avcoor']               = 'real(kind=cp),dimension(bvel_anions_n)'
    d['ftype']['rzero']                = 'real(kind=cp),dimension(bvel_anions_n)'
    d['ftype']['rcutoff']              = 'real(kind=cp),dimension(bvel_anions_n)'
    d['ftype']['dzero']                = 'real(kind=cp),dimension(bvel_anions_n)'
    d['ftype']['rmin']                 = 'real(kind=cp),dimension(bvel_anions_n)'
    d['ftype']['alpha']                = 'real(kind=cp),dimension(bvel_anions_n)'
    d['ftype']['refnum']               = 'integer,dimension(bvel_anions_n)'

    return d

def create_bvs_par_type():

    d = {}

    d['symb']                          = " "
    d['d0']                            = 0.0
    d['b_par']                         = 0.0
    d['refnum']                        = 0
    d['ftype']['symb']                 = 'character(len=4)'
    d['ftype']['d0']                   = 'real(kind=cp),dimension(bvs_anions_n)'
    d['ftype']['b_par']                = 'real(kind=cp),dimension(bvs_anions_n)'
    d['ftype']['refnum']               = 'integer,dimension(bvs_anions_n)'

    return d

def create_sbvs_par_type():

    d = {}

    d['symb']                          = " "
    d['d0']                            = 0.0
    d['b_par']                         = 0.0
    d['cn']                            = 0.0
    d['ctoff']                         = 0.0
    d['refnum']                        = 0
    d['ftype']['symb']                 = 'character(len=4)'
    d['ftype']['d0']                   = 'real(kind=cp),dimension(bvs_anions_n)'
    d['ftype']['b_par']                = 'real(kind=cp),dimension(bvs_anions_n)'
    d['ftype']['cn']                   = 'real(kind=cp),dimension(bvs_anions_n)'
    d['ftype']['ctoff']                = 'real(kind=cp),dimension(bvs_anions_n)'
    d['ftype']['refnum']               = 'integer,dimension(bvs_anions_n)'

    return d

