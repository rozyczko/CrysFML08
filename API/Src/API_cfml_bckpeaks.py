def create_pkb_type():

    d = {}

    d['np']                            = None
    d['x']                             = None
    d['y']                             = None
    d['bkg']                           = None
    d['ftype']['np']                   = 'integer'
    d['ftype']['x']                    = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['y']                    = 'real(kind=cp),dimension(:),allocatable'
    d['ftype']['bkg']                  = 'real(kind=cp),dimension(:),allocatable'

    return d

def create_peak_search_cond_type():

    d = {}

    d['peak_threshold']                = 0.02
    d['shoulder_threshold']            = 2.00
    d['bkg_threshold']                 = 0.05
    d['kindofpeaks']                   = 1
    d['iterations']                    = 3
    d['ftype']['peak_threshold']       = 'real(kind=cp)'
    d['ftype']['shoulder_threshold']   = 'real(kind=cp)'
    d['ftype']['bkg_threshold']        = 'real(kind=cp)'
    d['ftype']['kindofpeaks']          = 'integer'
    d['ftype']['iterations']           = 'integer'

    return d

