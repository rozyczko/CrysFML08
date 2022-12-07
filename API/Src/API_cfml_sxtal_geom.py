def create_psd_val_type():

    d = {}

    d['name_inst']                     = None
    d['xoff']                          = None
    d['zoff']                          = None
    d['radius']                        = None
    d['yoff']                          = None
    d['cgap']                          = None
    d['agap']                          = None
    d['ncat']                          = None
    d['nano']                          = None
    d['ipsd']                          = None
    d['ftype']['name_inst']            = 'character(len=12)'
    d['ftype']['xoff']                 = 'real(kind=cp)'
    d['ftype']['zoff']                 = 'real(kind=cp)'
    d['ftype']['radius']               = 'real(kind=cp)'
    d['ftype']['yoff']                 = 'real(kind=cp)'
    d['ftype']['cgap']                 = 'real(kind=cp)'
    d['ftype']['agap']                 = 'real(kind=cp)'
    d['ftype']['ncat']                 = 'integer'
    d['ftype']['nano']                 = 'integer'
    d['ftype']['ipsd']                 = 'integer'

    return d

def create_sxd_val_type():

    d = {}

    d['distms']                        = None
    d['distsd']                        = None
    d['dimx']                          = None
    d['dimz']                          = None
    d['xoff']                          = None
    d['yoff']                          = None
    d['zoff']                          = None
    d['toff']                          = None
    d['velcon']                        = None
    d['nxcel']                         = None
    d['nzcel']                         = None
    d['ftype']['distms']               = 'real(kind=cp)'
    d['ftype']['distsd']               = 'real(kind=cp)'
    d['ftype']['dimx']                 = 'real(kind=cp)'
    d['ftype']['dimz']                 = 'real(kind=cp)'
    d['ftype']['xoff']                 = 'real(kind=cp)'
    d['ftype']['yoff']                 = 'real(kind=cp)'
    d['ftype']['zoff']                 = 'real(kind=cp)'
    d['ftype']['toff']                 = 'real(kind=cp)'
    d['ftype']['velcon']               = 'real(kind=cp)'
    d['ftype']['nxcel']                = 'integer'
    d['ftype']['nzcel']                = 'integer'

    return d

def create_twin_type():

    d = {}

    d['twin_name']                     = None
    d['ityp']                          = None
    d['n_twins']                       = None
    d['twin_mat']                      = None
    d['twin_matinv']                   = None
    d['twin_axis']                     = None
    d['twin_ang']                      = None
    d['ftype']['twin_name']            = 'character(len=80)'
    d['ftype']['ityp']                 = 'integer'
    d['ftype']['n_twins']              = 'integer'
    d['ftype']['twin_mat']             = 'real(kind=cp),dimension(3,3,48)'
    d['ftype']['twin_matinv']          = 'real(kind=cp),dimension(3,3,48)'
    d['ftype']['twin_axis']            = 'real(kind=cp),dimension(3,48)'
    d['ftype']['twin_ang']             = 'real(kind=cp),dimension(48)'

    return d

