def create_opt_conditions_type():

    d = {}

    d['method']                        = None
    d['nmeth']                         = None
    d['npar']                          = None
    d['mxfun']                         = None
    d['loops']                         = None
    d['iquad']                         = None
    d['iout']                          = None
    d['nflag']                         = None
    d['ifun']                          = None
    d['iter']                          = None
    d['eps']                           = None
    d['acc']                           = None
    d['ftype']['method']               = 'character(len=20)'
    d['ftype']['nmeth']                = 'integer'
    d['ftype']['npar']                 = 'integer'
    d['ftype']['mxfun']                = 'integer'
    d['ftype']['loops']                = 'integer'
    d['ftype']['iquad']                = 'integer'
    d['ftype']['iout']                 = 'integer'
    d['ftype']['nflag']                = 'integer'
    d['ftype']['ifun']                 = 'integer'
    d['ftype']['iter']                 = 'integer'
    d['ftype']['eps']                  = 'real(kind=cp)'
    d['ftype']['acc']                  = 'real(kind=cp)'

    return d

