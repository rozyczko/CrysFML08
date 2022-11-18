class Type_Component():

    def __init__(self,name : str ='',fortran_type : str ='',fortran_type_short : str ='',dim : str ='',value : str ='None',info : str =''):

        self.name               = name
        self.fortran_type       = fortran_type
        self.fortran_type_short = fortran_type_short
        self.dim                = dim
        self.value              = value
        self.info               = info

class Argument(Type_Component):

    def __init__(self,name : str ='',fortran_type : str ='',fortran_type_short : str ='',dim : str ='',value : str ='None',info : str ='',intent : str =''):

        super().__init__(name=name,fortran_type=fortran_type,fortran_type_short=fortran_type_short,dim=dim,value=value,info=info)
        self.intent = intent

class Subroutine():

    def __init__(self,name : str ='',module : str ='',arguments : dict ={},is_overload : bool = False,overload : str = '',has_interface : bool =False):

        self.name          = name
        self.module        = module
        self.arguments     = arguments.copy()
        self.is_overload   = is_overload
        self.overload      = overload
        self.has_interface = has_interface

class Function(Subroutine):

    def __init__(self,name : str ='',module : str ='',arguments : dict ={},xreturn : Type_Component =Type_Component()):

        super().__init__(name=name,module=module,arguments=arguments)
        self.xreturn = xreturn

class Interface():

    def __init__(self,name : str ='',procedures : list =[]):

        self.name       = name
        self.procedures = procedures.copy()

class FortranType():

    def __init__(self,name : str ='',parent : str ='',components : dict ={}):

        self.name       = name
        self.parent     = parent
        self.components = components.copy()

class Module():

    def __init__(self,name : str ='',types : dict ={},procedures : dict ={},publics : list =[],interface : dict ={}):

        self.name       = name
        self.types      = types.copy()
        self.procedures = procedures.copy()
        self.publics    = publics.copy()
        self.interface  = interface.copy()
