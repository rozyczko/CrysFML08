def create_scattering_species_type():

    d = {}

    d['num_species']                   = 0
    d['num_magspc']                    = 0
    d['symb']                          = None
    d['symb_mag']                      = None
    d['br']                            = None
    d['bi']                            = None
    d['delta_fp']                      = None
    d['delta_fpp']                     = None
    d['xcoef']                         = None
    d['mcoef']                         = None
    d['ftype']['num_species']          = 'integer'
    d['ftype']['num_magspc']           = 'integer'
    d['ftype']['symb']                 = 'character(len=6),dimension(:),allocatable'
    d['ftype']['symb_mag']             = 'character(len=6),dimension(:),allocatable'
    d['ftype']['br']                   = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['bi']                   = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['delta_fp']             = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['delta_fpp']            = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['xcoef']                = 'type(xray_form_type),dimension(:),allocatable'
    d['ftype']['mcoef']                = 'type(magnetic_form_type),dimension(:),allocatable'

    return d

def create_strf_type():

    d = {}

    d['sqnuc']                         = 0.0
    d['sqmiv']                         = 0.0
    d['nsf']                           = 0.0
    d['msf']                           = 0.0
    d['miv']                           = 0.0
    d['mivc']                          = 0.0
    d['ftype']['sqnuc']                = 'real(kind=cp)'
    d['ftype']['sqmiv']                = 'real(kind=cp)'
    d['ftype']['nsf']                  = 'complex(kind=cp)'
    d['ftype']['msf']                  = 'complex(kind=cp),dimension(3)'
    d['ftype']['miv']                  = 'complex(kind=cp),dimension(3)'
    d['ftype']['mivc']                 = 'complex(kind=cp),dimension(3)'

    return d

def create_strflist_type():

    d = {}

    d['nref']                          = 0
    d['strf']                          = None
    d['ftype']['nref']                 = 'integer'
    d['ftype']['strf']                 = 'type(strf_type),dimension(:),allocatable'

    return d

