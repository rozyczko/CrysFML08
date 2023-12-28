class FortranVar():

    def __init__(self,name : str,ftype : str,kind : str='',ndim : int=0,dim : list=[],intent : str='',info : str='',len : str='',allocatable : bool=False, is_class : bool=False,value = None,parent : str='',childs : list=[]):

        self.name        = name
        self.ftype       = ftype
        self.kind        = kind
        self.ndim        = ndim
        self.dim         = dim
        self.intent      = intent
        self.info        = info
        self.len         = len
        self.allocatable = allocatable
        self.parent      = parent
        self.childs      = childs
        self.is_class    = is_class
        self.value       = value

class Subroutine():

    def __init__(self,name : str,module : str ='',arguments : dict ={},is_overload : bool = False,overload : str = '',has_interface : bool =False):

        self.name          = name
        self.module        = module
        self.arguments     = arguments.copy()
        self.is_overload   = is_overload
        self.overload      = overload
        self.has_interface = has_interface

class Function(Subroutine):

    def __init__(self,name : str,module : str ='',arguments : dict ={},xreturn : FortranVar = FortranVar('','')):

        super().__init__(name,module=module,arguments=arguments)
        self.xreturn = xreturn

class Interface():

    def __init__(self,name : str,procedures : list =[]):

        self.name       = name
        self.procedures = procedures.copy()

def init_fortran_var(name : str,ftype : str,kind : str='',ndim : int=0,dim : list=[],intent : str='',info : str='',len : str='',allocatable : bool=False, is_class : bool=False,value = None,parent : str='') -> dict:

    v = {}
    v['name']        = name
    v['ftype']       = ftype
    v['kind']        = kind
    v['ndim']        = ndim
    v['dim']         = dim
    v['intent']      = intent
    v['info']        = info
    v['len']         = len
    v['allocatable'] = allocatable
    v['parent']      = parent
    v['is_class']    = is_class
    v['value']       = value
    return v

def init_type(name : str ='',parent : str ='',childs : list=[],components : dict ={}) -> dict:

    t = {}
    t['name']       = name
    t['parent']     = parent
    t['childs']     = childs
    t['components'] = components
    return t

def init_module(name : str ='',uses : list=[],types : dict ={},procedures : dict ={},publics : list =[],interface : dict ={}) -> dict:

    m = {}
    m['name']       = name
    m['uses']       = uses
    m['types']      = types
    m['procedures'] = procedures
    m['publics']    = publics
    m['interface']  = interface
    return m
