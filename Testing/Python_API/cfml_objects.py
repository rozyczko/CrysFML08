class Type_Component():

    def __init__(self,name='',xtype='',dim='',value=None,info=''):

        self.name   = name
        self.xtype  = xtype
        self.dim    = dim
        self.value  = value
        self.info   = info

class Argument(Type_Component):

    def __init__(self,name='',xtype='',dim='',value=None,info='',intent=None):

        super().__init__(name=name,xtype=xtype,dim=dim,value=value,info=info)
        self.intent = intent

class Subroutine():

    def __init__(self,name='',module='',arguments={}):

        self.name      = name
        self.module    = module
        self.arguments = arguments.copy()

class Function(Subroutine):

    def __init__(self,name='',module='',arguments={},xreturn=Type_Component()):

        super().__init__(name='',module='',arguments={})
        self.xreturn = xreturn

class Interface():

    def __init__(self,name='',procs=[]):

        self.name  = name
        self.procs = procs.copy()

class XType():

    def __init__(self,name='',parent='',components={}):

        self.name       = name
        self.parent     = parent
        self.components = components.copy()

class Module():

    def __init__(self,name='',types={},procs={},inter={}):

        self.name  = name
        self.types = types.copy()
        self.procs = procs.copy()
        self.inter = inter.copy()
