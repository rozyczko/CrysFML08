def create_shub_spgr_info_type():

    d = {}

    d['id_bns']                        = " "
    d['bns']                           = " "
    d['id_og']                         = " "
    d['og']                            = " "
    d['std']                           = " "
    d['mhall']                         = " "
    d['generators']                    = " "
    d['ftype']['id_bns']               = 'character(len=7)'
    d['ftype']['bns']                  = 'character(len=12)'
    d['ftype']['id_og']                = 'character(len=11)'
    d['ftype']['og']                   = 'character(len=14)'
    d['ftype']['std']                  = 'character(len=27)'
    d['ftype']['mhall']                = 'character(len=20)'
    d['ftype']['generators']           = 'character(len=104)'

    return d

def create_spgr_info_type():

    d = {}

    d['n']                             = 0
    d['hm']                            = " "
    d['hall']                          = " "
    d['laue']                          = 0
    d['pg']                            = 0
    d['asu']                           = 0
    d['inf_extra']                     = " "
    d['ftype']['n']                    = 'integer'
    d['ftype']['hm']                   = 'character(len=12)'
    d['ftype']['hall']                 = 'character(len=16)'
    d['ftype']['laue']                 = 'integer'
    d['ftype']['pg']                   = 'integer'
    d['ftype']['asu']                  = 'integer,dimension(6)'
    d['ftype']['inf_extra']            = 'character(len=5)'

    return d

def create_table_equiv_type():

    d = {}

    d['sc']                            = " "
    d['ml']                            = " "
    d['ko']                            = " "
    d['bc']                            = " "
    d['za']                            = " "
    d['ftype']['sc']                   = 'character(len=6)'
    d['ftype']['ml']                   = 'character(len=17)'
    d['ftype']['ko']                   = 'character(len=18)'
    d['ftype']['bc']                   = 'character(len=32)'
    d['ftype']['za']                   = 'character(len=18)'

    return d

def create_wyck_info_type():

    d = {}

    d['hm']                            = " "
    d['norbit']                        = 0
    d['corbit']                        = " "
    d['ftype']['hm']                   = 'character(len=12)'
    d['ftype']['norbit']               = 'integer'
    d['ftype']['corbit']               = 'character(len=15),dimension(26)'

    return d

