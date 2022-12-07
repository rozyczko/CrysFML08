def create_coordination_type():

    d = {}

    d['natoms']                        = None
    d['max_coor']                      = None
    d['coord_num']                     = None
    d['n_cooatm']                      = None
    d['n_sym']                         = None
    d['dist']                          = None
    d['s_dist']                        = None
    d['tr_coo']                        = None
    d['ftype']['natoms']               = 'integer'
    d['ftype']['max_coor']             = 'integer'
    d['ftype']['coord_num']            = 'integer,dimension(:),allocatable'
    d['ftype']['n_cooatm']             = 'integer,dimension(:,:),allocatable'
    d['ftype']['n_sym']                = 'integer,dimension(:,:),allocatable'
    d['ftype']['dist']                 = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['s_dist']               = 'real(kind=cp),dimension(:,:),allocatable'
    d['ftype']['tr_coo']               = 'real(kind=cp),dimension(:,:,:),allocatable'

    return d

def create_point_list_type():

    d = {}

    d['np']                            = None
    d['nam']                           = None
    d['p']                             = None
    d['x']                             = None
    d['ftype']['np']                   = 'integer'
    d['ftype']['nam']                  = 'character(len=20),dimension(:),allocatable'
    d['ftype']['p']                    = 'integer,dimension(:),allocatable'
    d['ftype']['x']                    = 'real(kind=cp),dimension(:,:),allocatable'

    return d

