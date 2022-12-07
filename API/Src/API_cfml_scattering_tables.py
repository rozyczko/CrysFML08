def create_anomalous_sc_type():

    d = {}

    d['symb']                          = " "
    d['fp']                            = 0.0
    d['fpp']                           = 0.0
    d['ftype']['symb']                 = 'character(len=2)'
    d['ftype']['fp']                   = 'real(kind=cp),dimension(5)'
    d['ftype']['fpp']                  = 'real(kind=cp),dimension(5)'

    return d

def create_chem_info_type():

    d = {}

    d['symb']                          = " "
    d['name']                          = " "
    d['z']                             = 0
    d['atwe']                          = 0.0
    d['rcov']                          = 0.0
    d['rwaals']                        = 0.0
    d['vatm']                          = 0.0
    d['oxid']                          = 0
    d['rion']                          = 0.0
    d['sctf']                          = 0.0
    d['sedinc']                        = 0.0
    d['sea']                           = 0.0
    d['ftype']['symb']                 = 'character(len=2)'
    d['ftype']['name']                 = 'character(len=12)'
    d['ftype']['z']                    = 'integer'
    d['ftype']['atwe']                 = 'real(kind=cp)'
    d['ftype']['rcov']                 = 'real(kind=cp)'
    d['ftype']['rwaals']               = 'real(kind=cp)'
    d['ftype']['vatm']                 = 'real(kind=cp)'
    d['ftype']['oxid']                 = 'integer,dimension(5)'
    d['ftype']['rion']                 = 'real(kind=cp),dimension(5)'
    d['ftype']['sctf']                 = 'real(kind=cp)'
    d['ftype']['sedinc']               = 'real(kind=cp)'
    d['ftype']['sea']                  = 'real(kind=cp)'

    return d

def create_magnetic_form_type():

    d = {}

    d['symb']                          = " "
    d['sctm']                          = 0.0
    d['ftype']['symb']                 = 'character(len=4)'
    d['ftype']['sctm']                 = 'real(kind=cp),dimension(7)'

    return d

def create_xray_form_type():

    d = {}

    d['symb']                          = " "
    d['z']                             = 0
    d['a']                             = 0.0
    d['b']                             = 0.0
    d['c']                             = 0.0
    d['ftype']['symb']                 = 'character(len=4)'
    d['ftype']['z']                    = 'integer'
    d['ftype']['a']                    = 'real(kind=cp),dimension(4)'
    d['ftype']['b']                    = 'real(kind=cp),dimension(4)'
    d['ftype']['c']                    = 'real(kind=cp)'

    return d

def create_xray_wavelength_type():

    d = {}

    d['symb']                          = " "
    d['kalfa']                         = 0.0
    d['kbeta']                         = 0.0
    d['ftype']['symb']                 = 'character(len=2)'
    d['ftype']['kalfa']                = 'real(kind=cp),dimension(2)'
    d['ftype']['kbeta']                = 'real(kind=cp)'

    return d

