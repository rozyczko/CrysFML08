# <center>***Keywords on CFML***</center>

## ***Block definitions***
A general block into CFL file have the next structure: 
~~~
BlockName_StringIdentification    N  
...  
...  
END_BlockName_StringIdentification
~~~

#### **Examples:**
~~~
  PHASE_PbSO4    1  
  ...  
  ...  
  END_PHASE_PbSO4
~~~

~~~
  PATTERN_XRayMo  1  
  ...  
  ...  
  END_PATTERN_XRayMo
~~~

#### **Exceptions:**
~~~  
  COMMANDS    
  ...    
  ...    
  END_COMMANDS  
~~~

## ***Pattern Block***
| Directive |Options                   | Observations                        |
|:--------- |:------------------------ |:----------------------------------- |
| Patt_Type | Neutrons / X-rays  | |    
|           | Powder / Integrated_Intensities| |
|           | CW / TOF           | |
| Zero      | |
| Sycos     | |
| Sysin     | |
