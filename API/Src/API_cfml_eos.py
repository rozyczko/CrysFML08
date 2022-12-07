def create_pvt_table():

    d = {}

    d['np']                            = 0
    d['nt']                            = 0
    d['pmin']                          = 0.0
    d['pmax']                          = 1.0e6
    d['tmin']                          = 0.0
    d['tmax']                          = 1.0e6
    d['ptv']                           = None
    d['ftype']['np']                   = 'integer'
    d['ftype']['nt']                   = 'integer'
    d['ftype']['pmin']                 = 'real(kind=cp)'
    d['ftype']['pmax']                 = 'real(kind=cp)'
    d['ftype']['tmin']                 = 'real(kind=cp)'
    d['ftype']['tmax']                 = 'real(kind=cp)'
    d['ftype']['ptv']                  = 'real(kind=cp),dimension(:,:,:),allocatable'

    return d

def create_eos_type():

    d = {}

    d['title']                         = " "
    d['system']                        = " "
    d['model']                         = " "
    d['tmodel']                        = " "
    d['tranmodel']                     = " "
    d['smodel']                        = " "
    d['cmodel']                        = " "
    d['oscmodel']                      = " "
    d['parname']                       = " "
    d['comment']                       = " "
    d['pscale_name']                   = " "
    d['vscale_name']                   = " "
    d['doc']                           = " "
    d['savedate']                      = " "
    d['lineardir']                     = " "
    d['imodel']                        = 0
    d['iorder']                        = 0
    d['linear']                        = False
    d['itherm']                        = 0
    d['itran']                         = 0
    d['ishear']                        = 0
    d['icross']                        = 0
    d['iangle']                        = 0
    d['iosc']                          = 0
    d['iuse']                          = 0
    d['pref']                          = 0.0
    d['tref']                          = 298.0
    d['stoich']                        = 0.0
    d['density0']                      = 0.0
    d['tref_fixed']                    = False
    d['pthermaleos']                   = False
    d['osc_allowed']                   = False
    d['allowed_orders']                = None
    d['params']                        = 0.0
    d['esd']                           = 0.0
    d['x']                             = 0.0
    d['wchi2']                         = 0.0
    d['delpmax']                       = 0.0
    d['iwt']                           = 0
    d['iref']                          = 0
    d['factor']                        = 1.0
    d['alphafactor']                   = 1.0e5
    d['lastshift']                     = 0.0
    d['vcv']                           = 0.0
    d['angpoly']                       = 0.0
    d['table']                         = None
    d['cv_table']                      = None
    d['cv_external']                   = False
    d['ftype']['title']                = 'character(len=80)'
    d['ftype']['system']               = 'character(len=20)'
    d['ftype']['model']                = 'character(len=15)'
    d['ftype']['tmodel']               = 'character(len=20)'
    d['ftype']['tranmodel']            = 'character(len=15)'
    d['ftype']['smodel']               = 'character(len=15)'
    d['ftype']['cmodel']               = 'character(len=15)'
    d['ftype']['oscmodel']             = 'character(len=25),dimension(2)'
    d['ftype']['parname']              = 'character(len=5),dimension(n_eospar)'
    d['ftype']['comment']              = 'character(len=50),dimension(n_eospar)'
    d['ftype']['pscale_name']          = 'character(len=15)'
    d['ftype']['vscale_name']          = 'character(len=15)'
    d['ftype']['doc']                  = 'character(len=120),dimension(20)'
    d['ftype']['savedate']             = 'character(len=120)'
    d['ftype']['lineardir']            = 'character(len=32)'
    d['ftype']['imodel']               = 'integer'
    d['ftype']['iorder']               = 'integer'
    d['ftype']['linear']               = 'logical'
    d['ftype']['itherm']               = 'integer'
    d['ftype']['itran']                = 'integer'
    d['ftype']['ishear']               = 'integer'
    d['ftype']['icross']               = 'integer'
    d['ftype']['iangle']               = 'integer'
    d['ftype']['iosc']                 = 'integer,dimension(2)'
    d['ftype']['iuse']                 = 'integer,dimension(n_eospar)'
    d['ftype']['pref']                 = 'real(kind=cp)'
    d['ftype']['tref']                 = 'real(kind=cp)'
    d['ftype']['stoich']               = 'real(kind=cp)'
    d['ftype']['density0']             = 'real(kind=cp)'
    d['ftype']['tref_fixed']           = 'logical'
    d['ftype']['pthermaleos']          = 'logical'
    d['ftype']['osc_allowed']          = 'logical'
    d['ftype']['allowed_orders']       = 'logical,dimension(2:4)'
    d['ftype']['params']               = 'real(kind=cp),dimension(n_eospar)'
    d['ftype']['esd']                  = 'real(kind=cp),dimension(n_eospar)'
    d['ftype']['x']                    = 'real(kind=cp)'
    d['ftype']['wchi2']                = 'real(kind=cp)'
    d['ftype']['delpmax']              = 'real(kind=cp)'
    d['ftype']['iwt']                  = 'integer,dimension(4)'
    d['ftype']['iref']                 = 'integer,dimension(n_eospar)'
    d['ftype']['factor']               = 'real(kind=cp),dimension(n_eospar)'
    d['ftype']['alphafactor']          = 'real(kind=cp)'
    d['ftype']['lastshift']            = 'real(kind=cp),dimension(n_eospar)'
    d['ftype']['vcv']                  = 'real(kind=cp),dimension(n_eospar,n_eospar)'
    d['ftype']['angpoly']              = 'real(kind=cp),dimension(3,0:3,n_angpoly)'
    d['ftype']['table']                = 'type(pvt_table)'
    d['ftype']['cv_table']             = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['cv_external']          = 'logical'

    return d

def create_eos_list_type():

    d = {}

    d['n']                             = 0
    d['system']                        = " "
    d['eos']                           = None
    d['ftype']['n']                    = 'integer'
    d['ftype']['system']               = 'character(len=30)'
    d['ftype']['eos']                  = 'type(eos_type),dimension(:),allocatable'

    return d

def create_eos_cell_type():

    d = {}

    d['n']                             = 0
    d['system']                        = " "
    d['eos']                           = None
    d['unique_label']                  = " "
    d['unique']                        = 0
    d['obtuse']                        = False
    d['eosc']                          = None
    d['eosang']                        = None
    d['loaded']                        = 0
    d['cout']                          = " "
    d['inputlist']                     = " "
    d['ftype']['n']                    = 'integer'
    d['ftype']['system']               = 'character(len=30)'
    d['ftype']['eos']                  = 'type(eos_type),dimension(0:6)'
    d['ftype']['unique_label']         = 'character(len=1)'
    d['ftype']['unique']               = 'integer'
    d['ftype']['obtuse']               = 'logical,dimension(3)'
    d['ftype']['eosc']                 = 'type(eos_type)'
    d['ftype']['eosang']               = 'type(eos_type)'
    d['ftype']['loaded']               = 'integer,dimension(0:6)'
    d['ftype']['cout']                 = 'character(len=1),dimension(0:6,3)'
    d['ftype']['inputlist']            = 'character(len=30)'

    return d

def create_axis_type():

    d = {}

    d['v']                             = 0.0
    d['atype']                         = ' '
    d['ieos']                          = 0
    d['ftype']['v']                    = 'real(kind=cp),dimension(3)'
    d['ftype']['atype']                = 'character(len=1)'
    d['ftype']['ieos']                 = 'integer'

    return d

def create_eos_data_type():

    d = {}

    d['iuse']                          = 0
    d['igrp']                          = 0
    d['xtype']                         = 0
    d['t']                             = 298.0
    d['p']                             = 0.0
    d['v']                             = 0.0
    d['cell']                          = 0.0
    d['ang']                           = 0.0
    d['sigt']                          = 0.0
    d['sigp']                          = 0.0
    d['sigv']                          = 0.0
    d['sigc']                          = 0.0
    d['siga']                          = 0.0
    d['ftype']['iuse']                 = 'integer'
    d['ftype']['igrp']                 = 'integer,dimension(5)'
    d['ftype']['xtype']                = 'integer'
    d['ftype']['t']                    = 'real(kind=cp)'
    d['ftype']['p']                    = 'real(kind=cp)'
    d['ftype']['v']                    = 'real(kind=cp)'
    d['ftype']['cell']                 = 'real(kind=cp),dimension(3)'
    d['ftype']['ang']                  = 'real(kind=cp),dimension(3)'
    d['ftype']['sigt']                 = 'real(kind=cp)'
    d['ftype']['sigp']                 = 'real(kind=cp)'
    d['ftype']['sigv']                 = 'real(kind=cp)'
    d['ftype']['sigc']                 = 'real(kind=cp),dimension(3)'
    d['ftype']['siga']                 = 'real(kind=cp),dimension(3)'

    return d

def create_eos_data_list_type():

    d = {}

    d['title']                         = " "
    d['system']                        = " "
    d['n']                             = 0
    d['ic_dat']                        = 0
    d['pscale_name']                   = " "
    d['vscale_name']                   = " "
    d['lscale_name']                   = " "
    d['eosd']                          = None
    d['ftype']['title']                = 'character(len=80)'
    d['ftype']['system']               = 'character(len=40)'
    d['ftype']['n']                    = 'integer'
    d['ftype']['ic_dat']               = 'integer,dimension(ncol_data_max)'
    d['ftype']['pscale_name']          = 'character(len=15)'
    d['ftype']['vscale_name']          = 'character(len=15)'
    d['ftype']['lscale_name']          = 'character(len=15)'
    d['ftype']['eosd']                 = 'type(eos_data_type),dimension(:),allocatable'

    return d

