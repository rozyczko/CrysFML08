def create_diffpat_type():

    d = {}

    d['title']                         = " "
    d['kindrad']                       = " "
    d['scatvar']                       = " "
    d['xmin']                          = 0.0
    d['xmax']                          = 0.0
    d['ymin']                          = 0.0
    d['ymax']                          = 0.0
    d['step']                          = 0.0
    d['npts']                          = 0
    d['sigvar']                        = True
    d['wave']                          = 0.0
    d['x']                             = None
    d['y']                             = None
    d['sigma']                         = None
    d['ftype']['title']                = 'character(len=180)'
    d['ftype']['kindrad']              = 'character(len=20)'
    d['ftype']['scatvar']              = 'character(len=20)'
    d['ftype']['xmin']                 = 'real(kind=cp)'
    d['ftype']['xmax']                 = 'real(kind=cp)'
    d['ftype']['ymin']                 = 'real(kind=cp)'
    d['ftype']['ymax']                 = 'real(kind=cp)'
    d['ftype']['step']                 = 'real(kind=cp)'
    d['ftype']['npts']                 = 'integer'
    d['ftype']['sigvar']               = 'logical'
    d['ftype']['wave']                 = 'real(kind=cp),dimension(5)'
    d['ftype']['x']                    = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['y']                    = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['sigma']                = 'real(kind=cp),dimension(:),allocatable'

    return d

def create_diffpat_e_type():

    d = create_diffpat_type()

    d['instr']                         = " "
    d['filename']                      = " "
    d['filepath']                      = " "
    d['monitor']                       = 0.0
    d['norm_mon']                      = 0.0
    d['col_time']                      = 0.0
    d['tsample']                       = 298.0
    d['tset']                          = 0.0
    d['ct_step']                       = False
    d['al_x']                          = False
    d['al_y']                          = False
    d['al_sigma']                      = False
    d['al_ycalc']                      = False
    d['al_bgr']                        = False
    d['al_istat']                      = False
    d['ycalc']                         = None
    d['bgr']                           = None
    d['istat']                         = None
    d['nd']                            = None
    d['ftype']['instr']                = 'character(len=30)'
    d['ftype']['filename']             = 'character(len=80)'
    d['ftype']['filepath']             = 'character(len=512)'
    d['ftype']['monitor']              = 'real(kind=cp)'
    d['ftype']['norm_mon']             = 'real(kind=cp)'
    d['ftype']['col_time']             = 'real(kind=cp)'
    d['ftype']['tsample']              = 'real(kind=cp)'
    d['ftype']['tset']                 = 'real(kind=cp)'
    d['ftype']['ct_step']              = 'logical'
    d['ftype']['al_x']                 = 'logical'
    d['ftype']['al_y']                 = 'logical'
    d['ftype']['al_sigma']             = 'logical'
    d['ftype']['al_ycalc']             = 'logical'
    d['ftype']['al_bgr']               = 'logical'
    d['ftype']['al_istat']             = 'logical'
    d['ftype']['ycalc']                = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['bgr']                  = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['istat']                = 'integer,dimension(:),allocatable'
    d['ftype']['nd']                   = 'integer,dimension(:),allocatable'

    return d

def create_diffpat_g_type():

    d = create_diffpat_e_type()

    d['legend_x']                      = " "
    d['legend_y']                      = " "
    d['gy']                            = False
    d['gycalc']                        = False
    d['gsigma']                        = False
    d['gbgr']                          = False
    d['ftype']['legend_x']             = 'character(len=40)'
    d['ftype']['legend_y']             = 'character(len=40)'
    d['ftype']['gy']                   = 'logical'
    d['ftype']['gycalc']               = 'logical'
    d['ftype']['gsigma']               = 'logical'
    d['ftype']['gbgr']                 = 'logical'

    return d

