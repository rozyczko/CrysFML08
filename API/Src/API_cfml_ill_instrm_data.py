def create_basic_numc_type():

    d = {}

    d['n']                             = None
    d['namevar']                       = None
    d['cvalues']                       = None
    d['ftype']['n']                    = 'integer'
    d['ftype']['namevar']              = 'character(len=40),dimension(:),allocatable'
    d['ftype']['cvalues']              = 'character(len=80),dimension(:),allocatable'

    return d

def create_basic_numi_type():

    d = {}

    d['n']                             = None
    d['namevar']                       = None
    d['ivalues']                       = None
    d['ftype']['n']                    = 'integer'
    d['ftype']['namevar']              = 'character(len=40),dimension(:),allocatable'
    d['ftype']['ivalues']              = 'integer,dimension(:),allocatable'

    return d

def create_basic_numr_type():

    d = {}

    d['n']                             = None
    d['namevar']                       = None
    d['rvalues']                       = None
    d['ftype']['n']                    = 'integer'
    d['ftype']['namevar']              = 'character(len=40),dimension(:),allocatable'
    d['ftype']['rvalues']              = 'real(kind=cp),dimension(:),allocatable'

    return d

def create_calibration_detector_type():

    d = {}

    d['name_instrm']                   = None
    d['ndet']                          = None
    d['npointsdet']                    = None
    d['posx']                          = None
    d['effic']                         = None
    d['active']                        = None
    d['ftype']['name_instrm']          = 'character(len=12)'
    d['ftype']['ndet']                 = 'integer'
    d['ftype']['npointsdet']           = 'integer'
    d['ftype']['posx']                 = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['effic']                = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['active']               = 'logical,dimension(:,:),allocatable'

    return d

def create_diffractometer_type():

    d = {}

    d['info']                          = None
    d['name_inst']                     = None
    d['geom']                          = None
    d['bl_frame']                      = None
    d['dist_units']                    = None
    d['r_ord']                         = None
    d['angl_units']                    = None
    d['detector_type']                 = None
    d['dist_samp_detector']            = None
    d['wave']                          = None
    d['wave_min']                      = None
    d['wave_max']                      = None
    d['vert']                          = None
    d['horiz']                         = None
    d['agap']                          = None
    d['cgap']                          = None
    d['np_vert']                       = None
    d['np_horiz']                      = None
    d['tilted']                        = None
    d['rd']                            = None
    d['ga_d']                          = None
    d['nu_d']                          = None
    d['tiltx_d']                       = 0.0
    d['tilty_d']                       = 0.0
    d['tiltz_d']                       = 0.0
    d['igeom']                         = None
    d['ipsd']                          = None
    d['e1']                            = None
    d['e2']                            = None
    d['e3']                            = None
    d['gnxz_limited']                  = None
    d['gap_min']                       = None
    d['gap_max']                       = None
    d['gan_min']                       = None
    d['gan_max']                       = None
    d['nu_min']                        = None
    d['nu_max']                        = None
    d['d_min']                         = None
    d['x_min']                         = None
    d['x_max']                         = None
    d['z_min']                         = None
    d['z_max']                         = None
    d['num_ang']                       = None
    d['ang_names']                     = None
    d['ang_limits']                    = None
    d['ang_offsets']                   = None
    d['ang_velocity']                  = None
    d['num_disp']                      = None
    d['disp_names']                    = None
    d['disp_limits']                   = None
    d['disp_offsets']                  = None
    d['displaced']                     = None
    d['det_offsets']                   = None
    d['rangtim']                       = None
    d['range_time']                    = None
    d['alphas']                        = None
    d['alpha_correct']                 = None
    d['alpha_file']                    = None
    d['resol_given']                   = None
    d['nga']                           = None
    d['nnu']                           = None
    d['resurf']                        = None
    d['ftype']['info']                 = 'character(len=80)'
    d['ftype']['name_inst']            = 'character(len=12)'
    d['ftype']['geom']                 = 'character(len=15)'
    d['ftype']['bl_frame']             = 'character(len=6)'
    d['ftype']['dist_units']           = 'character(len=4)'
    d['ftype']['r_ord']                = 'character(len=3)'
    d['ftype']['angl_units']           = 'character(len=4)'
    d['ftype']['detector_type']        = 'character(len=30)'
    d['ftype']['dist_samp_detector']   = 'real(kind=cp)'
    d['ftype']['wave']                 = 'real(kind=cp)'
    d['ftype']['wave_min']             = 'real(kind=cp)'
    d['ftype']['wave_max']             = 'real(kind=cp)'
    d['ftype']['vert']                 = 'real(kind=cp)'
    d['ftype']['horiz']                = 'real(kind=cp)'
    d['ftype']['agap']                 = 'real(kind=cp)'
    d['ftype']['cgap']                 = 'real(kind=cp)'
    d['ftype']['np_vert']              = 'integer'
    d['ftype']['np_horiz']             = 'integer'
    d['ftype']['tilted']               = 'logical'
    d['ftype']['rd']                   = 'real(kind=cp),dimension(3,3)'
    d['ftype']['ga_d']                 = 'real(kind=cp)'
    d['ftype']['nu_d']                 = 'real(kind=cp)'
    d['ftype']['tiltx_d']              = 'real(kind=cp)'
    d['ftype']['tilty_d']              = 'real(kind=cp)'
    d['ftype']['tiltz_d']              = 'real(kind=cp)'
    d['ftype']['igeom']                = 'integer'
    d['ftype']['ipsd']                 = 'integer'
    d['ftype']['e1']                   = 'real(kind=cp),dimension(3)'
    d['ftype']['e2']                   = 'real(kind=cp),dimension(3)'
    d['ftype']['e3']                   = 'real(kind=cp),dimension(3)'
    d['ftype']['gnxz_limited']         = 'logical'
    d['ftype']['gap_min']              = 'real(kind=cp)'
    d['ftype']['gap_max']              = 'real(kind=cp)'
    d['ftype']['gan_min']              = 'real(kind=cp)'
    d['ftype']['gan_max']              = 'real(kind=cp)'
    d['ftype']['nu_min']               = 'real(kind=cp)'
    d['ftype']['nu_max']               = 'real(kind=cp)'
    d['ftype']['d_min']                = 'real(kind=cp)'
    d['ftype']['x_min']                = 'real(kind=cp)'
    d['ftype']['x_max']                = 'real(kind=cp)'
    d['ftype']['z_min']                = 'real(kind=cp)'
    d['ftype']['z_max']                = 'real(kind=cp)'
    d['ftype']['num_ang']              = 'integer'
    d['ftype']['ang_names']            = 'character(len=12),dimension(15)'
    d['ftype']['ang_limits']           = 'real(kind=cp),dimension(15,2)'
    d['ftype']['ang_offsets']          = 'real(kind=cp),dimension(15)'
    d['ftype']['ang_velocity']         = 'real(kind=cp),dimension(15)'
    d['ftype']['num_disp']             = 'integer'
    d['ftype']['disp_names']           = 'character(len=12),dimension(10)'
    d['ftype']['disp_limits']          = 'real(kind=cp),dimension(10,2)'
    d['ftype']['disp_offsets']         = 'real(kind=cp),dimension(10)'
    d['ftype']['displaced']            = 'logical'
    d['ftype']['det_offsets']          = 'real(kind=cp),dimension(3 )'
    d['ftype']['rangtim']              = 'logical'
    d['ftype']['range_time']           = 'real(kind=cp),dimension(:,:,:),allocatable'
    d['ftype']['alphas']               = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['alpha_correct']        = 'logical'
    d['ftype']['alpha_file']           = 'character(len=512)'
    d['ftype']['resol_given']          = 'logical'
    d['ftype']['nga']                  = 'integer'
    d['ftype']['nnu']                  = 'integer'
    d['ftype']['resurf']               = 'real(kind=cp),dimension(:,:),allocatable'

    return d

def create_generic_numor_type():

    d = {}

    d['numor']                         = None
    d['instr']                         = None
    d['expname']                       = None
    d['date']                          = None
    d['title']                         = None
    d['sampleid']                      = None
    d['diffopt']                       = None
    d['monmpar']                       = None
    d['diffmpar']                      = None
    d['detpar']                        = None
    d['dacflags']                      = None
    d['dacparam']                      = None
    d['samplest']                      = None
    d['icounts']                       = None
    d['rcounts']                       = None
    d['ftype']['numor']                = 'integer'
    d['ftype']['instr']                = 'character(len=4)'
    d['ftype']['expname']              = 'character(len=10)'
    d['ftype']['date']                 = 'character(len=20)'
    d['ftype']['title']                = 'character(len=80)'
    d['ftype']['sampleid']             = 'type(basic_numc_type)'
    d['ftype']['diffopt']              = 'type(basic_numr_type)'
    d['ftype']['monmpar']              = 'type(basic_numr_type)'
    d['ftype']['diffmpar']             = 'type(basic_numr_type)'
    d['ftype']['detpar']               = 'type(basic_numr_type)'
    d['ftype']['dacflags']             = 'type(basic_numi_type)'
    d['ftype']['dacparam']             = 'type(basic_numr_type)'
    d['ftype']['samplest']             = 'type(basic_numr_type)'
    d['ftype']['icounts']              = 'type(basic_numi_type)'
    d['ftype']['rcounts']              = 'type(basic_numr_type)'

    return d

def create_ill_data_record_type():

    d = {}

    d['numor']                         = None
    d['nset_prime']                    = None
    d['ntran']                         = None
    d['inst_ch']                       = None
    d['date_ch']                       = None
    d['fill_ch']                       = None
    d['user_ch']                       = None
    d['lc_ch']                         = None
    d['text_ch']                       = None
    d['scan_motor']                    = None
    d['nvers']                         = None
    d['ntype']                         = None
    d['kctrl']                         = None
    d['manip']                         = None
    d['nbang']                         = None
    d['nkmes']                         = None
    d['npdone']                        = None
    d['jcode']                         = None
    d['icalc']                         = None
    d['ianal']                         = None
    d['imode']                         = None
    d['itgv']                          = None
    d['iregul']                        = None
    d['ivolt']                         = None
    d['naxe']                          = None
    d['npstart']                       = None
    d['ilasti']                        = None
    d['isa']                           = None
    d['flgkif']                        = None
    d['ih_sqs']                        = None
    d['ik_sqs']                        = None
    d['nbsqs']                         = None
    d['nb_cells']                      = None
    d['nfree1']                        = None
    d['icdesc']                        = None
    d['valco']                         = None
    d['valdef']                        = None
    d['valenv']                        = None
    d['ftype']['numor']                = 'integer'
    d['ftype']['nset_prime']           = 'integer'
    d['ftype']['ntran']                = 'integer'
    d['ftype']['inst_ch']              = 'character(len=4)'
    d['ftype']['date_ch']              = 'character(len=22)'
    d['ftype']['fill_ch']              = 'character(len=2)'
    d['ftype']['user_ch']              = 'character(len=6)'
    d['ftype']['lc_ch']                = 'character(len=6)'
    d['ftype']['text_ch']              = 'character(len=72)'
    d['ftype']['scan_motor']           = 'character(len=8)'
    d['ftype']['nvers']                = 'integer'
    d['ftype']['ntype']                = 'integer'
    d['ftype']['kctrl']                = 'integer'
    d['ftype']['manip']                = 'integer'
    d['ftype']['nbang']                = 'integer'
    d['ftype']['nkmes']                = 'integer'
    d['ftype']['npdone']               = 'integer'
    d['ftype']['jcode']                = 'integer'
    d['ftype']['icalc']                = 'integer'
    d['ftype']['ianal']                = 'integer'
    d['ftype']['imode']                = 'integer'
    d['ftype']['itgv']                 = 'integer'
    d['ftype']['iregul']               = 'integer'
    d['ftype']['ivolt']                = 'integer'
    d['ftype']['naxe']                 = 'integer'
    d['ftype']['npstart']              = 'integer'
    d['ftype']['ilasti']               = 'integer'
    d['ftype']['isa']                  = 'integer'
    d['ftype']['flgkif']               = 'integer'
    d['ftype']['ih_sqs']               = 'integer'
    d['ftype']['ik_sqs']               = 'integer'
    d['ftype']['nbsqs']                = 'integer'
    d['ftype']['nb_cells']             = 'integer'
    d['ftype']['nfree1']               = 'integer'
    d['ftype']['icdesc']               = 'integer,dimension(11)'
    d['ftype']['valco']                = 'real(kind=cp),dimension(35)'
    d['ftype']['valdef']               = 'real(kind=cp),dimension(10)'
    d['ftype']['valenv']               = 'real(kind=cp),dimension(5)'

    return d

def create_powder_numor_type():

    d = {}

    d['numor']                         = None
    d['manip']                         = None
    d['icalc']                         = None
    d['header']                        = None
    d['instrm']                        = None
    d['title']                         = None
    d['scantype']                      = None
    d['angles']                        = None
    d['scans']                         = None
    d['monitor']                       = None
    d['time']                          = None
    d['wave']                          = None
    d['conditions']                    = None
    d['nbdata']                        = None
    d['nframes']                       = None
    d['nbang']                         = None
    d['icdesc']                        = None
    d['tmc_ang']                       = None
    d['counts']                        = None
    d['ftype']['numor']                = 'integer'
    d['ftype']['manip']                = 'integer'
    d['ftype']['icalc']                = 'integer'
    d['ftype']['header']               = 'character(len=32)'
    d['ftype']['instrm']               = 'character(len=12)'
    d['ftype']['title']                = 'character(len=32)'
    d['ftype']['scantype']             = 'character(len=8)'
    d['ftype']['angles']               = 'real(kind=cp),dimension(5)'
    d['ftype']['scans']                = 'real(kind=cp),dimension(3)'
    d['ftype']['monitor']              = 'real(kind=cp)'
    d['ftype']['time']                 = 'real(kind=cp)'
    d['ftype']['wave']                 = 'real(kind=cp)'
    d['ftype']['conditions']           = 'real(kind=cp),dimension(5)'
    d['ftype']['nbdata']               = 'integer'
    d['ftype']['nframes']              = 'integer'
    d['ftype']['nbang']                = 'integer'
    d['ftype']['icdesc']               = 'integer,dimension(11)'
    d['ftype']['tmc_ang']              = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['counts']               = 'real(kind=cp),dimension(:,:),allocatable'

    return d

def create_sxtal_numor_type():

    d = {}

    d['filename']                      = None
    d['numor']                         = None
    d['manip']                         = None
    d['icalc']                         = None
    d['header']                        = None
    d['instrm']                        = None
    d['title']                         = None
    d['scantype']                      = None
    d['hmin']                          = None
    d['hmax']                          = None
    d['angles']                        = None
    d['ub']                            = None
    d['dh']                            = None
    d['scans']                         = None
    d['preset']                        = None
    d['wave']                          = None
    d['dist']                          = None
    d['cpl_fact']                      = None
    d['conditions']                    = None
    d['nbdata']                        = None
    d['nframes']                       = None
    d['nbang']                         = None
    d['icdesc']                        = None
    d['header_size']                   = None
    d['frame_size']                    = None
    d['selected_frames']               = None
    d['tmc_ang']                       = None
    d['counts']                        = None
    d['ftype']['filename']             = 'character(len=512)'
    d['ftype']['numor']                = 'integer'
    d['ftype']['manip']                = 'integer'
    d['ftype']['icalc']                = 'integer'
    d['ftype']['header']               = 'character(len=32)'
    d['ftype']['instrm']               = 'character(len=12)'
    d['ftype']['title']                = 'character(len=60)'
    d['ftype']['scantype']             = 'character(len=8)'
    d['ftype']['hmin']                 = 'real(kind=cp),dimension(3)'
    d['ftype']['hmax']                 = 'real(kind=cp),dimension(3)'
    d['ftype']['angles']               = 'real(kind=cp),dimension(5)'
    d['ftype']['ub']                   = 'real(kind=cp),dimension(3,3)'
    d['ftype']['dh']                   = 'real(kind=cp),dimension(3)'
    d['ftype']['scans']                = 'real(kind=cp),dimension(3)'
    d['ftype']['preset']               = 'real(kind=cp)'
    d['ftype']['wave']                 = 'real(kind=cp)'
    d['ftype']['dist']                 = 'real(kind=cp)'
    d['ftype']['cpl_fact']             = 'real(kind=cp)'
    d['ftype']['conditions']           = 'real(kind=cp),dimension(5)'
    d['ftype']['nbdata']               = 'integer'
    d['ftype']['nframes']              = 'integer'
    d['ftype']['nbang']                = 'integer'
    d['ftype']['icdesc']               = 'integer,dimension(11)'
    d['ftype']['header_size']          = 'integer'
    d['ftype']['frame_size']           = 'integer'
    d['ftype']['selected_frames']      = 'integer,dimension(:),allocatable'
    d['ftype']['tmc_ang']              = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['counts']               = 'real(kind=cp),dimension(:,:),allocatable'

    return d

def create_sxtal_orient_type():

    d = {}

    d['orient_set']                    = False
    d['wave']                          = None
    d['ub']                            = None
    d['ubinv']                         = None
    d['conv']                          = None
    d['ftype']['orient_set']           = 'logical'
    d['ftype']['wave']                 = 'real(kind=cp)'
    d['ftype']['ub']                   = 'real(kind=cp),dimension(3,3)'
    d['ftype']['ubinv']                = 'real(kind=cp),dimension(3,3)'
    d['ftype']['conv']                 = 'real(kind=cp),dimension(3,3)'

    return d

