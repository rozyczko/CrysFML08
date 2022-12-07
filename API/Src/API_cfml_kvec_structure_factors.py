def create_magh_type():

    d = {}

    d['keqv_minus']                    = None
    d['mult']                          = None
    d['num_k']                         = None
    d['signp']                         = None
    d['s']                             = None
    d['sqmiv']                         = None
    d['h']                             = None
    d['msf']                           = None
    d['tmsf']                          = None
    d['miv']                           = None
    d['mivc']                          = None
    d['ftype']['keqv_minus']           = 'logical'
    d['ftype']['mult']                 = 'integer'
    d['ftype']['num_k']                = 'integer'
    d['ftype']['signp']                = 'real(kind=cp)'
    d['ftype']['s']                    = 'real(kind=cp)'
    d['ftype']['sqmiv']                = 'real(kind=cp)'
    d['ftype']['h']                    = 'real(kind=cp),dimension(3)'
    d['ftype']['msf']                  = 'complex(kind=cp),dimension(3)'
    d['ftype']['tmsf']                 = 'complex(kind=cp),dimension(3,3)'
    d['ftype']['miv']                  = 'complex(kind=cp),dimension(3)'
    d['ftype']['mivc']                 = 'complex(kind=cp),dimension(3)'

    return d

def create_magh_list_type():

    d = {}

    d['nref']                          = None
    d['mh']                            = None
    d['ftype']['nref']                 = 'integer'
    d['ftype']['mh']                   = 'type(magh_type),dimension(:),allocatable'

    return d

def create_maghd_type():

    d = {}

    d['keqv_minus']                    = None
    d['num_k']                         = None
    d['signp']                         = None
    d['s']                             = None
    d['sqamiv']                        = None
    d['sqmiv']                         = None
    d['h']                             = None
    d['msf']                           = None
    d['miv']                           = None
    d['mivc']                          = None
    d['amiv']                          = None
    d['ftype']['keqv_minus']           = 'logical'
    d['ftype']['num_k']                = 'integer'
    d['ftype']['signp']                = 'real(kind=cp)'
    d['ftype']['s']                    = 'real(kind=cp)'
    d['ftype']['sqamiv']               = 'real(kind=cp)'
    d['ftype']['sqmiv']                = 'real(kind=cp)'
    d['ftype']['h']                    = 'real(kind=cp),dimension(3)'
    d['ftype']['msf']                  = 'complex(kind=cp),dimension(3,2,24)'
    d['ftype']['miv']                  = 'complex(kind=cp),dimension(3,2,24)'
    d['ftype']['mivc']                 = 'complex(kind=cp),dimension(3,2,24)'
    d['ftype']['amiv']                 = 'complex(kind=cp),dimension(3)'

    return d

def create_maghd_list_type():

    d = {}

    d['nref']                          = None
    d['mh']                            = None
    d['ftype']['nref']                 = 'integer'
    d['ftype']['mh']                   = 'type(maghd_type),dimension(:),allocatable'

    return d

