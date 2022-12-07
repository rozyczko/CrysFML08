def create_interval_type():

    d = {}

    d['mina']                          = 0.0
    d['maxb']                          = 0.0
    d['ftype']['mina']                 = 'real(kind=cp)'
    d['ftype']['maxb']                 = 'real(kind=cp)'

    return d

def create_job_info_type():

    d = {}

    d['title']                         = " "
    d['num_phases']                    = 0
    d['num_patterns']                  = 0
    d['num_cmd']                       = 0
    d['patt_typ']                      = None
    d['phas_nam']                      = None
    d['cmd']                           = None
    d['range_stl']                     = None
    d['range_q']                       = None
    d['range_d']                       = None
    d['range_2theta']                  = None
    d['range_energy']                  = None
    d['range_tof']                     = None
    d['lambda']                        = None
    d['ratio']                         = None
    d['dtt1']                          = None
    d['dtt2']                          = None
    d['ftype']['title']                = 'character(len=120)'
    d['ftype']['num_phases']           = 'integer'
    d['ftype']['num_patterns']         = 'integer'
    d['ftype']['num_cmd']              = 'integer'
    d['ftype']['patt_typ']             = 'character(len=16),dimension(:),allocatable'
    d['ftype']['phas_nam']             = 'character(len=128),dimension(:),allocatable'
    d['ftype']['cmd']                  = 'character(len=128),dimension(:),allocatable'
    d['ftype']['range_stl']            = 'type(interval_type),dimension(:),allocatable'
    d['ftype']['range_q']              = 'type(interval_type),dimension(:),allocatable'
    d['ftype']['range_d']              = 'type(interval_type),dimension(:),allocatable'
    d['ftype']['range_2theta']         = 'type(interval_type),dimension(:),allocatable'
    d['ftype']['range_energy']         = 'type(interval_type),dimension(:),allocatable'
    d['ftype']['range_tof']            = 'type(interval_type),dimension(:),allocatable'
    d['ftype']['lambda']               = 'type(interval_type),dimension(:),allocatable'
    d['ftype']['ratio']                = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['dtt1']                 = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['dtt2']                 = 'real(kind=cp),dimension(:),allocatable'

    return d

