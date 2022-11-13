class Type_Component():

    def __init__(self,name : str ='',xtype : str ='',dim : str ='',value=None,info : str =''):

        self.name   = name
        self.xtype  = xtype
        self.dim    = dim
        self.value  = value
        self.info   = info

class Argument(Type_Component):

    def __init__(self,name : str ='',xtype : str ='',dim : str ='',value=None,info : str ='',intent : str =''):

        super().__init__(name=name,xtype=xtype,dim=dim,value=value,info=info)
        self.intent = intent

class Subroutine():

    def __init__(self,name : str ='',module : str ='',arguments : dict ={},has_interface : bool =False):

        self.name          = name
        self.module        = module
        self.arguments     = arguments.copy()
        self.has_interface = has_interface

class Function(Subroutine):

    def __init__(self,name : str ='',module : str ='',arguments : dict ={},xreturn : Type_Component =Type_Component()):

        super().__init__(name='',module='',arguments={})
        self.xreturn = xreturn

class Interface():

    def __init__(self,name : str ='',procedures : list =[]):

        self.name       = name
        self.procedures = procedures.copy()

class XType():

    def __init__(self,name : str ='',parent : str ='',components : dict ={}):

        self.name       = name
        self.parent     = parent
        self.components = components.copy()

class Module():

    def __init__(self,name : str ='',types : dict ={},procedures : dict ={},interface : dict ={}):

        self.name       = name
        self.types      = types.copy()
        self.procedures = procedures.copy()
        self.interface  = interface.copy()
