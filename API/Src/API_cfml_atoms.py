def create_atm_type():

    d = {}

    d['lab']                           = " "
    d['chemsymb']                      = " "
    d['sfacsymb']                      = " "
    d['z']                             = 0
    d['mult']                          = 0
    d['charge']                        = 0
    d['x']                             = 0.0
    d['u_iso']                         = 0.0
    d['occ']                           = 1.0
    d['utype']                         = "b_ij"
    d['thtype']                        = "iso"
    d['u']                             = 0.0
    d['magnetic']                      = False
    d['mom']                           = 0.0
    d['moment']                        = 0.0
    d['ind_ff']                        = 0
    d['atminfo']                       = " "
    d['wyck']                          = " "
    d['varf']                          = 0.0
    d['active']                        = True
    d['ftype']['lab']                  = 'character(len=20)'
    d['ftype']['chemsymb']             = 'character(len=2)'
    d['ftype']['sfacsymb']             = 'character(len=4)'
    d['ftype']['z']                    = 'integer'
    d['ftype']['mult']                 = 'integer'
    d['ftype']['charge']               = 'integer'
    d['ftype']['x']                    = 'real(kind=cp),dimension(3)'
    d['ftype']['u_iso']                = 'real(kind=cp)'
    d['ftype']['occ']                  = 'real(kind=cp)'
    d['ftype']['utype']                = 'character(len=4)'
    d['ftype']['thtype']               = 'character(len=3)'
    d['ftype']['u']                    = 'real(kind=cp),dimension(6)'
    d['ftype']['magnetic']             = 'logical'
    d['ftype']['mom']                  = 'real(kind=cp)'
    d['ftype']['moment']               = 'real(kind=cp),dimension(3)'
    d['ftype']['ind_ff']               = 'integer,dimension(3)'
    d['ftype']['atminfo']              = 'character(len=40)'
    d['ftype']['wyck']                 = 'character(len=5)'
    d['ftype']['varf']                 = 'real(kind=cp),dimension(5)'
    d['ftype']['active']               = 'logical'

    return d

def create_atm_std_type():

    d = create_atm_type()

    d['x_std']                         = 0.0
    d['occ_std']                       = 0.0
    d['u_iso_std']                     = 0.0
    d['u_std']                         = 0.0
    d['moment_std']                    = 0.0
    d['ftype']['x_std']                = 'real(kind=cp),dimension(3)'
    d['ftype']['occ_std']              = 'real(kind=cp)'
    d['ftype']['u_iso_std']            = 'real(kind=cp)'
    d['ftype']['u_std']                = 'real(kind=cp),dimension(6)'
    d['ftype']['moment_std']           = 'real(kind=cp),dimension(3)'

    return d

def create_modatm_std_type():

    d = create_atm_std_type()

    d['n_oc']                          = 0
    d['n_bc']                          = 0
    d['n_mc']                          = 0
    d['n_dc']                          = 0
    d['n_uc']                          = 0
    d['poc_q']                         = 0
    d['pbc_q']                         = 0
    d['pmc_q']                         = 0
    d['pdc_q']                         = 0
    d['puc_q']                         = 0
    d['ocs']                           = 0.0
    d['ocs_std']                       = 0.0
    d['bcs']                           = 0.0
    d['bcs_std']                       = 0.0
    d['mcs']                           = 0.0
    d['mcs_std']                       = 0.0
    d['dcs']                           = 0.0
    d['dcs_std']                       = 0.0
    d['ucs']                           = 0.0
    d['ucs_std']                       = 0.0
    d['xs']                            = None
    d['moms']                          = None
    d['us']                            = None
    d['ftype']['n_oc']                 = 'integer'
    d['ftype']['n_bc']                 = 'integer'
    d['ftype']['n_mc']                 = 'integer'
    d['ftype']['n_dc']                 = 'integer'
    d['ftype']['n_uc']                 = 'integer'
    d['ftype']['poc_q']                = 'integer,dimension(max_mod)'
    d['ftype']['pbc_q']                = 'integer,dimension(max_mod)'
    d['ftype']['pmc_q']                = 'integer,dimension(max_mod)'
    d['ftype']['pdc_q']                = 'integer,dimension(max_mod)'
    d['ftype']['puc_q']                = 'integer,dimension(max_mod)'
    d['ftype']['ocs']                  = 'real(kind=cp),dimension(2, max_mod)'
    d['ftype']['ocs_std']              = 'real(kind=cp),dimension(2, max_mod)'
    d['ftype']['bcs']                  = 'real(kind=cp),dimension(2, max_mod)'
    d['ftype']['bcs_std']              = 'real(kind=cp),dimension(2, max_mod)'
    d['ftype']['mcs']                  = 'real(kind=cp),dimension(6, max_mod)'
    d['ftype']['mcs_std']              = 'real(kind=cp),dimension(6, max_mod)'
    d['ftype']['dcs']                  = 'real(kind=cp),dimension(6, max_mod)'
    d['ftype']['dcs_std']              = 'real(kind=cp),dimension(6, max_mod)'
    d['ftype']['ucs']                  = 'real(kind=cp),dimension(12,max_mod)'
    d['ftype']['ucs_std']              = 'real(kind=cp),dimension(12,max_mod)'
    d['ftype']['xs']                   = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['moms']                 = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['us']                   = 'real(kind=cp),dimension(:,:),allocatable'

    return d

def create_atm_ref_type():

    d = create_atm_std_type()

    d['l_x']                           = 0
    d['l_occ']                         = 0
    d['l_u_iso']                       = 0
    d['l_moment']                      = 0
    d['l_u']                           = 0
    d['m_x']                           = 0.0
    d['m_occ']                         = 0.0
    d['m_u_iso']                       = 0.0
    d['m_moment']                      = 0.0
    d['m_u']                           = 0.0
    d['ftype']['l_x']                  = 'integer,dimension(3)'
    d['ftype']['l_occ']                = 'integer'
    d['ftype']['l_u_iso']              = 'integer'
    d['ftype']['l_moment']             = 'integer,dimension(3)'
    d['ftype']['l_u']                  = 'integer,dimension(6)'
    d['ftype']['m_x']                  = 'real(kind=cp),dimension(3)'
    d['ftype']['m_occ']                = 'real(kind=cp)'
    d['ftype']['m_u_iso']              = 'real(kind=cp)'
    d['ftype']['m_moment']             = 'real(kind=cp),dimension(3)'
    d['ftype']['m_u']                  = 'real(kind=cp),dimension(6)'

    return d

def create_modatm_ref_type():

    d = create_modatm_std_type()

    d['l_x']                           = 0
    d['l_occ']                         = 0
    d['l_u_iso']                       = 0
    d['l_u']                           = 0
    d['m_x']                           = 0.0
    d['m_occ']                         = 0.0
    d['m_u_iso']                       = 0.0
    d['m_u']                           = 0.0
    d['l_ocs']                         = 0
    d['l_bcs']                         = 0
    d['l_mcs']                         = 0
    d['l_dcs']                         = 0
    d['l_ucs']                         = 0
    d['m_ocs']                         = 0.0
    d['m_bcs']                         = 0.0
    d['m_mcs']                         = 0.0
    d['m_dcs']                         = 0.0
    d['m_ucs']                         = 0.0
    d['ftype']['l_x']                  = 'integer,dimension(3)'
    d['ftype']['l_occ']                = 'integer'
    d['ftype']['l_u_iso']              = 'integer'
    d['ftype']['l_u']                  = 'integer,dimension(6)'
    d['ftype']['m_x']                  = 'real(kind=cp),dimension(3)'
    d['ftype']['m_occ']                = 'real(kind=cp)'
    d['ftype']['m_u_iso']              = 'real(kind=cp)'
    d['ftype']['m_u']                  = 'real(kind=cp),dimension(6)'
    d['ftype']['l_ocs']                = 'integer,dimension(2, max_mod)'
    d['ftype']['l_bcs']                = 'integer,dimension(2, max_mod)'
    d['ftype']['l_mcs']                = 'integer,dimension(6, max_mod)'
    d['ftype']['l_dcs']                = 'integer,dimension(6, max_mod)'
    d['ftype']['l_ucs']                = 'integer,dimension(12,max_mod)'
    d['ftype']['m_ocs']                = 'real(kind=cp),dimension(2, max_mod)'
    d['ftype']['m_bcs']                = 'real(kind=cp),dimension(2, max_mod)'
    d['ftype']['m_mcs']                = 'real(kind=cp),dimension(6, max_mod)'
    d['ftype']['m_dcs']                = 'real(kind=cp),dimension(6, max_mod)'
    d['ftype']['m_ucs']                = 'real(kind=cp),dimension(12,max_mod)'

    return d

def create_atm_cell_type():

    d = {}

    d['nat']                           = None
    d['lab']                           = None
    d['xyz']                           = None
    d['charge']                        = None
    d['moment']                        = None
    d['var_free']                      = None
    d['neighb']                        = None
    d['neighb_atom']                   = None
    d['distance']                      = None
    d['trans']                         = None
    d['ndist']                         = None
    d['ddist']                         = None
    d['ddlab']                         = None
    d['ftype']['nat']                  = 'integer'
    d['ftype']['lab']                  = 'character(len=20),dimension(:),allocatable'
    d['ftype']['xyz']                  = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['charge']               = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['moment']               = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['var_free']             = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['neighb']               = 'integer,dimension(:),allocatable'
    d['ftype']['neighb_atom']          = 'integer,dimension(:,:),allocatable'
    d['ftype']['distance']             = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['trans']                = 'real(kind=cp),dimension(:,:,:),allocatable'
    d['ftype']['ndist']                = 'integer'
    d['ftype']['ddist']                = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['ddlab']                = 'character(len=20),dimension(:),allocatable'

    return d

def create_atom_equiv_type():

    d = {}

    d['mult']                          = 0
    d['chemsymb']                      = " "
    d['lab']                           = None
    d['x']                             = None
    d['ftype']['mult']                 = 'integer'
    d['ftype']['chemsymb']             = 'character(len=2)'
    d['ftype']['lab']                  = 'character(len=20),dimension(:),allocatable'
    d['ftype']['x']                    = 'real(kind=cp),dimension(:,:),allocatable'

    return d

def create_atom_equiv_list_type():

    d = {}

    d['nauas']                         = 0
    d['atm']                           = None
    d['ftype']['nauas']                = 'integer'
    d['ftype']['atm']                  = 'type(atom_equiv_type),dimension(:),allocatable'

    return d

def create_atlist_type():

    d = {}

    d['natoms']                        = 0
    d['mcomp']                         = "crystal"
    d['symm_checked']                  = False
    d['active']                        = None
    d['iph']                           = None
    d['atom']                          = None
    d['ftype']['natoms']               = 'integer'
    d['ftype']['mcomp']                = 'character(len=9)'
    d['ftype']['symm_checked']         = 'logical'
    d['ftype']['active']               = 'logical,dimension(:),allocatable'
    d['ftype']['iph']                  = 'integer,dimension(:),allocatable'
    d['ftype']['atom']                 = 'class(atm_type),dimension(:),allocatable'

    return d

def create_matom_type():

    d = {}

    d['lab']                           = None
    d['chemsymb']                      = None
    d['sfacsymb']                      = None
    d['wyck']                          = None
    d['active']                        = None
    d['z']                             = None
    d['mult']                          = None
    d['x']                             = None
    d['x_std']                         = None
    d['mx']                            = None
    d['lx']                            = None
    d['occ']                           = None
    d['occ_std']                       = None
    d['mocc']                          = None
    d['locc']                          = None
    d['biso']                          = None
    d['biso_std']                      = None
    d['mbiso']                         = None
    d['lbiso']                         = None
    d['utype']                         = None
    d['thtype']                        = None
    d['u']                             = None
    d['u_std']                         = None
    d['ueq']                           = None
    d['mu']                            = None
    d['lu']                            = None
    d['charge']                        = None
    d['moment']                        = None
    d['ind']                           = None
    d['nvar']                          = None
    d['varf']                          = None
    d['mvarf']                         = None
    d['lvarf']                         = None
    d['atminfo']                       = None
    d['nvk']                           = None
    d['imat']                          = None
    d['skr']                           = None
    d['skr_std']                       = None
    d['spher_skr']                     = None
    d['spher_skr_std']                 = None
    d['mskr']                          = None
    d['lskr']                          = None
    d['ski']                           = None
    d['ski_std']                       = None
    d['spher_ski']                     = None
    d['spher_ski_std']                 = None
    d['mski']                          = None
    d['lski']                          = None
    d['mphas']                         = None
    d['mphas_std']                     = None
    d['mmphas']                        = None
    d['lmphas']                        = None
    d['cbas']                          = None
    d['cbas_std']                      = None
    d['mbas']                          = None
    d['lbas']                          = None
    d['chitype']                       = None
    d['chi']                           = None
    d['chi_std']                       = None
    d['chieq']                         = None
    d['mchi']                          = None
    d['lchi']                          = None
    d['ftype']['lab']                  = 'character(len=10)'
    d['ftype']['chemsymb']             = 'character(len=2)'
    d['ftype']['sfacsymb']             = 'character(len=4)'
    d['ftype']['wyck']                 = 'character(len=1)'
    d['ftype']['active']               = 'logical'
    d['ftype']['z']                    = 'integer'
    d['ftype']['mult']                 = 'integer'
    d['ftype']['x']                    = 'real(kind=cp),dimension(3)'
    d['ftype']['x_std']                = 'real(kind=cp),dimension(3)'
    d['ftype']['mx']                   = 'real(kind=cp),dimension(3)'
    d['ftype']['lx']                   = 'integer,dimension(3)'
    d['ftype']['occ']                  = 'real(kind=cp)'
    d['ftype']['occ_std']              = 'real(kind=cp)'
    d['ftype']['mocc']                 = 'real(kind=cp)'
    d['ftype']['locc']                 = 'integer'
    d['ftype']['biso']                 = 'real(kind=cp)'
    d['ftype']['biso_std']             = 'real(kind=cp)'
    d['ftype']['mbiso']                = 'real(kind=cp)'
    d['ftype']['lbiso']                = 'integer'
    d['ftype']['utype']                = 'character(len=4)'
    d['ftype']['thtype']               = 'character(len=5)'
    d['ftype']['u']                    = 'real(kind=cp),dimension(6)'
    d['ftype']['u_std']                = 'real(kind=cp),dimension(6)'
    d['ftype']['ueq']                  = 'real(kind=cp)'
    d['ftype']['mu']                   = 'real(kind=cp),dimension(6)'
    d['ftype']['lu']                   = 'integer,dimension(6)'
    d['ftype']['charge']               = 'real(kind=cp)'
    d['ftype']['moment']               = 'real(kind=cp)'
    d['ftype']['ind']                  = 'integer,dimension(5)'
    d['ftype']['nvar']                 = 'integer'
    d['ftype']['varf']                 = 'real(kind=cp),dimension(25)'
    d['ftype']['mvarf']                = 'real(kind=cp),dimension(25)'
    d['ftype']['lvarf']                = 'integer,dimension(25)'
    d['ftype']['atminfo']              = 'character(len=40)'
    d['ftype']['nvk']                  = 'integer'
    d['ftype']['imat']                 = 'integer,dimension(12)'
    d['ftype']['skr']                  = 'real(kind=cp),dimension(3,12)'
    d['ftype']['skr_std']              = 'real(kind=cp),dimension(3,12)'
    d['ftype']['spher_skr']            = 'real(kind=cp),dimension(3,12)'
    d['ftype']['spher_skr_std']        = 'real(kind=cp),dimension(3,12)'
    d['ftype']['mskr']                 = 'real(kind=cp),dimension(3,12)'
    d['ftype']['lskr']                 = 'integer,dimension(3,12)'
    d['ftype']['ski']                  = 'real(kind=cp),dimension(3,12)'
    d['ftype']['ski_std']              = 'real(kind=cp),dimension(3,12)'
    d['ftype']['spher_ski']            = 'real(kind=cp),dimension(3,12)'
    d['ftype']['spher_ski_std']        = 'real(kind=cp),dimension(3,12)'
    d['ftype']['mski']                 = 'real(kind=cp),dimension(3,12)'
    d['ftype']['lski']                 = 'integer,dimension(3,12)'
    d['ftype']['mphas']                = 'real(kind=cp),dimension(12)'
    d['ftype']['mphas_std']            = 'real(kind=cp),dimension(12)'
    d['ftype']['mmphas']               = 'real(kind=cp),dimension(12)'
    d['ftype']['lmphas']               = 'integer,dimension(12)'
    d['ftype']['cbas']                 = 'real(kind=cp),dimension(12,12)'
    d['ftype']['cbas_std']             = 'real(kind=cp),dimension(12,12)'
    d['ftype']['mbas']                 = 'real(kind=cp),dimension(12,12)'
    d['ftype']['lbas']                 = 'integer,dimension(12,12)'
    d['ftype']['chitype']              = 'character(len=5)'
    d['ftype']['chi']                  = 'real(kind=cp),dimension(6)'
    d['ftype']['chi_std']              = 'real(kind=cp),dimension(6)'
    d['ftype']['chieq']                = 'real(kind=cp)'
    d['ftype']['mchi']                 = 'real(kind=cp),dimension(6)'
    d['ftype']['lchi']                 = 'real(kind=cp),dimension(6)'

    return d

def create_matom_list_type():

    d = {}

    d['natoms']                        = None
    d['suscept']                       = None
    d['magfield']                      = None
    d['dir_mfield']                    = None
    d['atom']                          = None
    d['ftype']['natoms']               = 'integer'
    d['ftype']['suscept']              = 'logical'
    d['ftype']['magfield']             = 'real(kind=cp)'
    d['ftype']['dir_mfield']           = 'real(kind=cp),dimension(3)'
    d['ftype']['atom']                 = 'type(matom_type),dimension(:),allocatable'

    return d

