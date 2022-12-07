def create_deriv_tof_type():

    d = {}

    d['alfa']                          = 0.0
    d['beta']                          = 0.0
    d['dt']                            = 0.0
    d['sigma']                         = 0.0
    d['gamma']                         = 0.0
    d['eta']                           = 0.0
    d['kappa']                         = 0.0
    d['ftype']['alfa']                 = 'real(kind=cp)'
    d['ftype']['beta']                 = 'real(kind=cp)'
    d['ftype']['dt']                   = 'real(kind=cp)'
    d['ftype']['sigma']                = 'real(kind=cp)'
    d['ftype']['gamma']                = 'real(kind=cp)'
    d['ftype']['eta']                  = 'real(kind=cp)'
    d['ftype']['kappa']                = 'real(kind=cp)'

    return d

