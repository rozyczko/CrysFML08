def create_group_k_type():

    d = {}

    d['g0']                            = None
    d['ngk']                           = 0
    d['k_equiv_minusk']                = False
    d['minusk']                        = False
    d['extended']                      = False
    d['p']                             = 0
    d['co']                            = 0
    d['nk']                            = 0
    d['stark']                         = 0.0
    d['eqv_k']                         = 0.0
    d['ftype']['g0']                   = 'type(spg_type)'
    d['ftype']['ngk']                  = 'integer'
    d['ftype']['k_equiv_minusk']       = 'logical'
    d['ftype']['minusk']               = 'logical'
    d['ftype']['extended']             = 'logical'
    d['ftype']['p']                    = 'integer,dimension(192)'
    d['ftype']['co']                   = 'integer,dimension(48,48)'
    d['ftype']['nk']                   = 'integer'
    d['ftype']['stark']                = 'real(kind=cp),dimension(3,48)'
    d['ftype']['eqv_k']                = 'real(kind=cp),dimension(3,48)'

    return d

