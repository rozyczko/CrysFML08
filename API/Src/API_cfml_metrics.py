def create_cell_type():

    d = {}

    d['cell']                          = 0.0
    d['scell']                         = 0.0
    d['ang']                           = 0.0
    d['sang']                          = 0.0
    d['vol']                           = 0.0
    d['svol']                          = 0.0
    d['ftype']['cell']                 = 'real(kind=cp),dimension(3)'
    d['ftype']['scell']                = 'real(kind=cp),dimension(3)'
    d['ftype']['ang']                  = 'real(kind=cp),dimension(3)'
    d['ftype']['sang']                 = 'real(kind=cp),dimension(3)'
    d['ftype']['vol']                  = 'real(kind=cp)'
    d['ftype']['svol']                 = 'real(kind=cp)'

    return d

def create_cell_g_type():

    d = create_cell_type()

    d['rcell']                         = 0.0
    d['rang']                          = 0.0
    d['rvol']                          = 0.0
    d['gd']                            = 0.0
    d['gr']                            = 0.0
    d['cr_orth_cel']                   = 0.0
    d['orth_cr_cel']                   = 0.0
    d['bl_m']                          = 0.0
    d['inv_bl_m']                      = 0.0
    d['carttype']                      = "ca"
    d['ftype']['rcell']                = 'real(kind=cp),dimension(3)'
    d['ftype']['rang']                 = 'real(kind=cp),dimension(3)'
    d['ftype']['rvol']                 = 'real(kind=cp)'
    d['ftype']['gd']                   = 'real(kind=cp),dimension(3,3)'
    d['ftype']['gr']                   = 'real(kind=cp),dimension(3,3)'
    d['ftype']['cr_orth_cel']          = 'real(kind=cp),dimension(3,3)'
    d['ftype']['orth_cr_cel']          = 'real(kind=cp),dimension(3,3)'
    d['ftype']['bl_m']                 = 'real(kind=cp),dimension(3,3)'
    d['ftype']['inv_bl_m']             = 'real(kind=cp),dimension(3,3)'
    d['ftype']['carttype']             = 'character(len=2)'

    return d

def create_cell_ls_type():

    d = create_cell_type()

    d['lcell']                         = 0
    d['lang']                          = 0
    d['ftype']['lcell']                = 'integer,dimension(3)'
    d['ftype']['lang']                 = 'integer,dimension(3)'

    return d

def create_cell_gls_type():

    d = create_cell_g_type()

    d['lcell']                         = 0
    d['lang']                          = 0
    d['ftype']['lcell']                = 'integer,dimension(3)'
    d['ftype']['lang']                 = 'integer,dimension(3)'

    return d

def create_twofold_axes_type():

    d = {}

    d['ntwo']                          = 0
    d['tol']                           = 3.0
    d['caxes']                         = 0.0
    d['dtwofold']                      = 0
    d['rtwofold']                      = 0
    d['dot']                           = 0
    d['cross']                         = 0.0
    d['maxes']                         = 0.0
    d['a']                             = 0.0
    d['b']                             = 0.0
    d['c']                             = 0.0
    d['ftype']['ntwo']                 = 'integer'
    d['ftype']['tol']                  = 'real(kind=cp)'
    d['ftype']['caxes']                = 'real(kind=cp),dimension(3,12)'
    d['ftype']['dtwofold']             = 'integer,dimension(3,12)'
    d['ftype']['rtwofold']             = 'integer,dimension(3,12)'
    d['ftype']['dot']                  = 'integer,dimension(12)'
    d['ftype']['cross']                = 'real(kind=cp),dimension(12)'
    d['ftype']['maxes']                = 'real(kind=cp),dimension(12)'
    d['ftype']['a']                    = 'real(kind=cp),dimension(3)'
    d['ftype']['b']                    = 'real(kind=cp),dimension(3)'
    d['ftype']['c']                    = 'real(kind=cp),dimension(3)'

    return d

def create_zone_axis_type():

    d = {}

    d['nlayer']                        = 0
    d['uvw']                           = 0
    d['rx']                            = 0
    d['ry']                            = 0
    d['ftype']['nlayer']               = 'integer'
    d['ftype']['uvw']                  = 'integer,dimension(3)'
    d['ftype']['rx']                   = 'integer,dimension(3)'
    d['ftype']['ry']                   = 'integer,dimension(3)'

    return d

def create_strain_tensor_type():

    d = {}

    d['iref']                          = 0
    d['icell']                         = 0
    d['istype']                        = 0
    d['cell0']                         = None
    d['cell1']                         = None
    d['cartype']                       = None
    d['system']                        = " "
    d['pt']                            = None
    d['e']                             = 0.0
    d['esd']                           = 0.0
    d['eval']                          = 0.0
    d['evalesd']                       = 0.0
    d['evec']                          = 0.0
    d['cart_ang']                      = 0.0
    d['cell_ang']                      = 0.0
    d['dir_close']                     = None
    d['ep']                            = 0.0
    d['esdp']                          = 0.0
    d['evalp']                         = 0.0
    d['evalpesd']                      = 0.0
    d['property']                      = ''
    d['ftype']['iref']                 = 'integer'
    d['ftype']['icell']                = 'integer'
    d['ftype']['istype']               = 'integer'
    d['ftype']['cell0']                = 'type(cell_g_type)'
    d['ftype']['cell1']                = 'type(cell_g_type)'
    d['ftype']['cartype']              = 'character(len=2)'
    d['ftype']['system']               = 'character(len=40)'
    d['ftype']['pt']                   = 'real(kind=cp),dimension(0:1,1:2,1:2)'
    d['ftype']['e']                    = 'real(kind=cp),dimension(3,3)'
    d['ftype']['esd']                  = 'real(kind=cp),dimension(3,3)'
    d['ftype']['eval']                 = 'real(kind=cp),dimension(3)'
    d['ftype']['evalesd']              = 'real(kind=cp),dimension(3)'
    d['ftype']['evec']                 = 'real(kind=cp),dimension(3,3)'
    d['ftype']['cart_ang']             = 'real(kind=cp),dimension(3,3,2)'
    d['ftype']['cell_ang']             = 'real(kind=cp),dimension(3,3,4)'
    d['ftype']['dir_close']            = 'real(kind=cp),dimension(3,2,4)'
    d['ftype']['ep']                   = 'real(kind=cp),dimension(3,3)'
    d['ftype']['esdp']                 = 'real(kind=cp),dimension(3,3)'
    d['ftype']['evalp']                = 'real(kind=cp),dimension(3)'
    d['ftype']['evalpesd']             = 'real(kind=cp),dimension(3)'
    d['ftype']['property']             = 'character(len=60)'

    return d

