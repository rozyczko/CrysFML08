def create_symm_oper_type():

    d = {}

    d['time_inv']                      = 1
    d['dt']                            = 1
    d['mat']                           = None
    d['ftype']['time_inv']             = 'integer'
    d['ftype']['dt']                   = 'integer'
    d['ftype']['mat']                  = 'type(rational),dimension(:,:),allocatable'

    return d

def create_group_type():

    d = {}

    d['multip']                        = 0
    d['d']                             = 0
    d['inv']                           = None
    d['op']                            = None
    d['symb_op']                       = None
    d['ftype']['multip']               = 'integer'
    d['ftype']['d']                    = 'integer'
    d['ftype']['inv']                  = 'integer,dimension(:),allocatable'
    d['ftype']['op']                   = 'type(symm_oper_type),dimension(:),allocatable'
    d['ftype']['symb_op']              = 'character(len=80),dimension(:),allocatable'

    return d

def create_spg_type():

    d = create_group_type()

    d['magnetic']                      = True
    d['standard_setting']              = True
    d['numspg']                        = 0
    d['numshu']                        = 0
    d['numops']                        = 0
    d['centred']                       = 0
    d['anticentred']                   = 0
    d['mag_type']                      = 0
    d['num_lat']                       = 0
    d['num_alat']                      = 0
    d['parent_num']                    = 0
    d['bravais_num']                   = 0
    d['spg_lat']                       = " "
    d['shu_lat']                       = " "
    d['init_label']                    = None
    d['parent_spg']                    = None
    d['tfrom_parent']                  = None
    d['centre']                        = None
    d['spg_symb']                      = None
    d['bns_num']                       = None
    d['og_num']                        = None
    d['bns_symb']                      = None
    d['og_symb']                       = None
    d['hall']                          = None
    d['uni']                           = None
    d['uni_num']                       = None
    d['crystalsys']                    = None
    d['pg']                            = None
    d['mag_pg']                        = None
    d['laue']                          = None
    d['setting']                       = None
    d['mat2std']                       = None
    d['mat2std_shu']                   = None
    d['generators_list']               = None
    d['ssg_symb']                      = None
    d['ssg_bravais']                   = None
    d['ssg_nlabel']                    = None
    d['centre_coord']                  = None
    d['anticentre_coord']              = None
    d['lat_tr']                        = None
    d['alat_tr']                       = None
    d['ftype']['magnetic']             = 'logical'
    d['ftype']['standard_setting']     = 'logical'
    d['ftype']['numspg']               = 'integer'
    d['ftype']['numshu']               = 'integer'
    d['ftype']['numops']               = 'integer'
    d['ftype']['centred']              = 'integer'
    d['ftype']['anticentred']          = 'integer'
    d['ftype']['mag_type']             = 'integer'
    d['ftype']['num_lat']              = 'integer'
    d['ftype']['num_alat']             = 'integer'
    d['ftype']['parent_num']           = 'integer'
    d['ftype']['bravais_num']          = 'integer'
    d['ftype']['spg_lat']              = 'character(len=1)'
    d['ftype']['shu_lat']              = 'character(len=1),dimension(2)'
    d['ftype']['init_label']           = 'character(len=:),allocatable'
    d['ftype']['parent_spg']           = 'character(len=:),allocatable'
    d['ftype']['tfrom_parent']         = 'character(len=:),allocatable'
    d['ftype']['centre']               = 'character(len=:),allocatable'
    d['ftype']['spg_symb']             = 'character(len=:),allocatable'
    d['ftype']['bns_num']              = 'character(len=:),allocatable'
    d['ftype']['og_num']               = 'character(len=:),allocatable'
    d['ftype']['bns_symb']             = 'character(len=:),allocatable'
    d['ftype']['og_symb']              = 'character(len=:),allocatable'
    d['ftype']['hall']                 = 'character(len=:),allocatable'
    d['ftype']['uni']                  = 'character(len=:),allocatable'
    d['ftype']['uni_num']              = 'character(len=:),allocatable'
    d['ftype']['crystalsys']           = 'character(len=:),allocatable'
    d['ftype']['pg']                   = 'character(len=:),allocatable'
    d['ftype']['mag_pg']               = 'character(len=:),allocatable'
    d['ftype']['laue']                 = 'character(len=:),allocatable'
    d['ftype']['setting']              = 'character(len=:),allocatable'
    d['ftype']['mat2std']              = 'character(len=:),allocatable'
    d['ftype']['mat2std_shu']          = 'character(len=:),allocatable'
    d['ftype']['generators_list']      = 'character(len=:),allocatable'
    d['ftype']['ssg_symb']             = 'character(len=:),allocatable'
    d['ftype']['ssg_bravais']          = 'character(len=:),allocatable'
    d['ftype']['ssg_nlabel']           = 'character(len=:),allocatable'
    d['ftype']['centre_coord']         = 'type(rational),dimension(:),allocatable'
    d['ftype']['anticentre_coord']     = 'type(rational),dimension(:),allocatable'
    d['ftype']['lat_tr']               = 'type(rational),dimension(:,:),allocatable'
    d['ftype']['alat_tr']              = 'type(rational),dimension(:,:),allocatable'

    return d

def create_superspacegroup_type():

    d = create_spg_type()

    d['nk']                            = 0
    d['nq']                            = 0
    d['kv']                            = None
    d['kv_std']                        = None
    d['sintlim']                       = None
    d['nharm']                         = None
    d['q_coeff']                       = None
    d['rot']                           = None
    d['m']                             = None
    d['ep']                            = None
    d['t']                             = None
    d['ti']                            = None
    d['ftype']['nk']                   = 'integer'
    d['ftype']['nq']                   = 'integer'
    d['ftype']['kv']                   = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['kv_std']               = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['sintlim']              = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['nharm']                = 'integer,dimension(:),allocatable'
    d['ftype']['q_coeff']              = 'integer,dimension(:,:),allocatable'
    d['ftype']['rot']                  = 'integer,dimension(:,:,:),allocatable'
    d['ftype']['m']                    = 'integer,dimension(:,:,:),allocatable'
    d['ftype']['ep']                   = 'integer,dimension(:,:,:),allocatable'
    d['ftype']['t']                    = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['ti']                   = 'real(kind=cp),dimension(:,:),allocatable'

    return d

def create_kvect_info_type():

    d = {}

    d['nk']                            = 0
    d['kv']                            = None
    d['kv_std']                        = None
    d['sintlim']                       = None
    d['nharm']                         = None
    d['nq']                            = 0
    d['q_coeff']                       = None
    d['ftype']['nk']                   = 'integer'
    d['ftype']['kv']                   = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['kv_std']               = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['sintlim']              = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['nharm']                = 'integer,dimension(:),allocatable'
    d['ftype']['nq']                   = 'integer'
    d['ftype']['q_coeff']              = 'integer,dimension(:,:),allocatable'

    return d

def create_point_orbit():

    d = {}

    d['mult']                          = 0
    d['pos']                           = None
    d['mom']                           = None
    d['pts']                           = None
    d['lat']                           = None
    d['ftype']['mult']                 = 'integer'
    d['ftype']['pos']                  = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['mom']                  = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['pts']                  = 'integer,dimension(:),allocatable'
    d['ftype']['lat']                  = 'integer,dimension(:,:),allocatable'

    return d

