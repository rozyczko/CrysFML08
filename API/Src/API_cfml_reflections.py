def create_refl_type():

    d = {}

    d['h']                             = None
    d['mult']                          = 0
    d['s']                             = 0.0
    d['imag']                          = 0
    d['pcoeff']                        = 0
    d['ftype']['h']                    = 'integer,dimension(:),allocatable'
    d['ftype']['mult']                 = 'integer'
    d['ftype']['s']                    = 'real(kind=cp)'
    d['ftype']['imag']                 = 'integer'
    d['ftype']['pcoeff']               = 'integer'

    return d

def create_srefl_type():

    d = create_refl_type()

    d['fo']                            = 0.0
    d['fc']                            = 0.0
    d['sfo']                           = 0.0
    d['phase']                         = 0.0
    d['a']                             = 0.0
    d['b']                             = 0.0
    d['w']                             = 1.0
    d['ftype']['fo']                   = 'real(kind=cp)'
    d['ftype']['fc']                   = 'real(kind=cp)'
    d['ftype']['sfo']                  = 'real(kind=cp)'
    d['ftype']['phase']                = 'real(kind=cp)'
    d['ftype']['a']                    = 'real(kind=cp)'
    d['ftype']['b']                    = 'real(kind=cp)'
    d['ftype']['w']                    = 'real(kind=cp)'

    return d

def create_mrefl_type():

    d = create_srefl_type()

    d['mivo']                          = 0.0
    d['smivo']                         = 0.0
    d['msf']                           = cmplx(0.0,0.0)
    d['miv']                           = cmplx(0.0,0.0)
    d['ftype']['mivo']                 = 'real(kind=cp)'
    d['ftype']['smivo']                = 'real(kind=cp)'
    d['ftype']['msf']                  = 'complex(kind=cp),dimension(3)'
    d['ftype']['miv']                  = 'complex(kind=cp),dimension(3)'

    return d

def create_reflist_type():

    d = {}

    d['nref']                          = 0
    d['ref']                           = None
    d['ftype']['nref']                 = 'integer'
    d['ftype']['ref']                  = 'class(refl_type),dimension(:),allocatable'

    return d

