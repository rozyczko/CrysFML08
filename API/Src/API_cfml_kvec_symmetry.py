def create_sym_oper_type():

    d = {}

    d['rot']                           = None
    d['tr']                            = None
    d['ftype']['rot']                  = 'integer,dimension(3,3)'
    d['ftype']['tr']                   = 'real(kind=cp),dimension(3)'

    return d

def create_msym_oper_type():

    d = {}

    d['rot']                           = None
    d['phas']                          = None
    d['ftype']['rot']                  = 'integer,dimension(3,3)'
    d['ftype']['phas']                 = 'real(kind=cp)'

    return d

def create_magnetic_domain_type():

    d = {}

    d['nd']                            = 0
    d['chir']                          = False
    d['trans']                         = False
    d['twin']                          = False
    d['dmat']                          = 0
    d['dt']                            = 0.0
    d['pop']                           = 0.0
    d['lpop']                          = 0
    d['mpop']                          = 0.0
    d['pop_std']                       = 0.0
    d['lab']                           = None
    d['ftype']['nd']                   = 'integer'
    d['ftype']['chir']                 = 'logical'
    d['ftype']['trans']                = 'logical'
    d['ftype']['twin']                 = 'logical'
    d['ftype']['dmat']                 = 'integer,dimension(3,3,24)'
    d['ftype']['dt']                   = 'real(kind=cp),dimension(3,24)'
    d['ftype']['pop']                  = 'real(kind=cp),dimension(2,24)'
    d['ftype']['lpop']                 = 'integer,dimension(2,24)'
    d['ftype']['mpop']                 = 'real(kind=cp),dimension(2,24)'
    d['ftype']['pop_std']              = 'real(kind=cp),dimension(2,24)'
    d['ftype']['lab']                  = 'character(len=10),dimension(2,24)'

    return d

def create_magsymm_k_type():

    d = {}

    d['magmodel']                      = None
    d['sk_type']                       = None
    d['bns_number']                    = None
    d['og_number']                     = None
    d['bns_symbol']                    = None
    d['og_symbol']                     = None
    d['magtype']                       = None
    d['parent_num']                    = None
    d['parent_spg']                    = None
    d['latt']                          = None
    d['nirreps']                       = None
    d['irrep_dim']                     = None
    d['small_irrep_dim']               = None
    d['irrep_modes_number']            = None
    d['irrep_id']                      = None
    d['irrep_direction']               = None
    d['irrep_action']                  = None
    d['nmsym']                         = None
    d['centred']                       = None
    d['mcentred']                      = None
    d['nkv']                           = None
    d['kvec']                          = None
    d['num_lat']                       = None
    d['ltr']                           = None
    d['numops']                        = None
    d['multip']                        = None
    d['nbas']                          = None
    d['icomp']                         = None
    d['basf']                          = None
    d['symopsymb']                     = None
    d['symop']                         = None
    d['msymopsymb']                    = None
    d['msymop']                        = None
    d['ftype']['magmodel']             = 'character(len=31)'
    d['ftype']['sk_type']              = 'character(len=15)'
    d['ftype']['bns_number']           = 'character(len=15)'
    d['ftype']['og_number']            = 'character(len=15)'
    d['ftype']['bns_symbol']           = 'character(len=34)'
    d['ftype']['og_symbol']            = 'character(len=34)'
    d['ftype']['magtype']              = 'integer'
    d['ftype']['parent_num']           = 'integer'
    d['ftype']['parent_spg']           = 'character(len=20)'
    d['ftype']['latt']                 = 'character(len=1)'
    d['ftype']['nirreps']              = 'integer'
    d['ftype']['irrep_dim']            = 'integer,dimension(4)'
    d['ftype']['small_irrep_dim']      = 'integer,dimension(4)'
    d['ftype']['irrep_modes_number']   = 'integer,dimension(4)'
    d['ftype']['irrep_id']             = 'character(len=15),dimension(4)'
    d['ftype']['irrep_direction']      = 'character(len=20),dimension(4)'
    d['ftype']['irrep_action']         = 'character(len=20),dimension(4)'
    d['ftype']['nmsym']                = 'integer'
    d['ftype']['centred']              = 'integer'
    d['ftype']['mcentred']             = 'integer'
    d['ftype']['nkv']                  = 'integer'
    d['ftype']['kvec']                 = 'real(kind=cp),dimension(3,12)'
    d['ftype']['num_lat']              = 'integer'
    d['ftype']['ltr']                  = 'real(kind=cp),dimension(3,4)'
    d['ftype']['numops']               = 'integer'
    d['ftype']['multip']               = 'integer'
    d['ftype']['nbas']                 = 'integer,dimension(4)'
    d['ftype']['icomp']                = 'integer,dimension(12,4)'
    d['ftype']['basf']                 = 'complex(kind=cp),dimension(3,12,48,4)'
    d['ftype']['symopsymb']            = 'character(len=40),dimension(:),allocatable'
    d['ftype']['symop']                = 'type(sym_oper_type),dimension(:),allocatable'
    d['ftype']['msymopsymb']           = 'character(len=40),dimension(:,:),allocatable'
    d['ftype']['msymop']               = 'type(msym_oper_type),dimension(:,:),allocatable'

    return d

