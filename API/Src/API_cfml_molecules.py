def create_molecule_type():

    d = {}

    d['name_mol']                      = ' '
    d['natoms']                        = 0
    d['in_xtal']                       = False
    d['is_eulermat']                   = False
    d['is_connect']                    = False
    d['rot_type']                      = 'e'
    d['coor_type']                     = 'f'
    d['therm_type']                    = 'iso'
    d['xcentre']                       = 0.0
    d['mxcentre']                      = 0.0
    d['lxcentre']                      = 0
    d['orient']                        = 0.0
    d['morient']                       = 0.0
    d['lorient']                       = 0
    d['t_tls']                         = 0.0
    d['mt_tls']                        = 0.0
    d['lt_tls']                        = 0
    d['l_tls']                         = 0.0
    d['ml_tls']                        = 0.0
    d['ll_tls']                        = 0
    d['s_tls']                         = 0.0
    d['ms_tls']                        = 0.0
    d['ls_tls']                        = 0
    d['euler']                         = 0.0
    d['atname']                        = None
    d['atsymb']                        = None
    d['atz']                           = None
    d['ptr']                           = None
    d['i_coor']                        = None
    d['mi_coor']                       = None
    d['li_coor']                       = None
    d['u_iso']                         = None
    d['mu_iso']                        = None
    d['lu_iso']                        = None
    d['occ']                           = None
    d['mocc']                          = None
    d['locc']                          = None
    d['nb']                            = None
    d['inb']                           = None
    d['tb']                            = None
    d['conn']                          = None
    d['ftype']['name_mol']             = 'character(len=80)'
    d['ftype']['natoms']               = 'integer'
    d['ftype']['in_xtal']              = 'logical'
    d['ftype']['is_eulermat']          = 'logical'
    d['ftype']['is_connect']           = 'logical'
    d['ftype']['rot_type']             = 'character(len=1)'
    d['ftype']['coor_type']            = 'character(len=1)'
    d['ftype']['therm_type']           = 'character(len=3)'
    d['ftype']['xcentre']              = 'real(kind=cp),dimension(3)'
    d['ftype']['mxcentre']             = 'real(kind=cp),dimension(3)'
    d['ftype']['lxcentre']             = 'integer,dimension(3)'
    d['ftype']['orient']               = 'real(kind=cp),dimension(3)'
    d['ftype']['morient']              = 'real(kind=cp),dimension(3)'
    d['ftype']['lorient']              = 'integer,dimension(3)'
    d['ftype']['t_tls']                = 'real(kind=cp),dimension(6)'
    d['ftype']['mt_tls']               = 'real(kind=cp),dimension(6)'
    d['ftype']['lt_tls']               = 'integer,dimension(6)'
    d['ftype']['l_tls']                = 'real(kind=cp),dimension(6)'
    d['ftype']['ml_tls']               = 'real(kind=cp),dimension(6)'
    d['ftype']['ll_tls']               = 'integer,dimension(6)'
    d['ftype']['s_tls']                = 'real(kind=cp),dimension(3,3)'
    d['ftype']['ms_tls']               = 'real(kind=cp),dimension(3,3)'
    d['ftype']['ls_tls']               = 'integer,dimension(3,3)'
    d['ftype']['euler']                = 'real(kind=cp),dimension(3,3)'
    d['ftype']['atname']               = 'character(len=20),dimension(  :),allocatable'
    d['ftype']['atsymb']               = 'character(len=4),dimension(  :),allocatable'
    d['ftype']['atz']                  = 'integer,dimension(  :),allocatable'
    d['ftype']['ptr']                  = 'integer,dimension(:,:),allocatable'
    d['ftype']['i_coor']               = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['mi_coor']              = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['li_coor']              = 'integer,dimension(:,:),allocatable'
    d['ftype']['u_iso']                = 'real(kind=cp),dimension(  :),allocatable'
    d['ftype']['mu_iso']               = 'real(kind=cp),dimension(  :),allocatable'
    d['ftype']['lu_iso']               = 'integer,dimension(  :),allocatable'
    d['ftype']['occ']                  = 'real(kind=cp),dimension(  :),allocatable'
    d['ftype']['mocc']                 = 'real(kind=cp),dimension(  :),allocatable'
    d['ftype']['locc']                 = 'integer,dimension(  :),allocatable'
    d['ftype']['nb']                   = 'integer,dimension(  :),allocatable'
    d['ftype']['inb']                  = 'integer,dimension(:,:),allocatable'
    d['ftype']['tb']                   = 'integer,dimension(:,:),allocatable'
    d['ftype']['conn']                 = 'integer,dimension(:,:),allocatable'

    return d

def create_molcrystal_type():

    d = {}

    d['n_free']                        = 0
    d['n_mol']                         = 0
    d['n_species']                     = 0
    d['npat']                          = 0
    d['cell']                          = None
    d['spg']                           = None
    d['atm']                           = None
    d['mol']                           = None
    d['ftype']['n_free']               = 'integer'
    d['ftype']['n_mol']                = 'integer'
    d['ftype']['n_species']            = 'integer'
    d['ftype']['npat']                 = 'integer'
    d['ftype']['cell']                 = 'type(cell_g_type)'
    d['ftype']['spg']                  = 'type(spg_type)'
    d['ftype']['atm']                  = 'class(atm_std_type),dimension(  :),allocatable'
    d['ftype']['mol']                  = 'type(molecule_type),dimension(  :),allocatable'

    return d

