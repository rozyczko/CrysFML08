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

Subblocks allowed within this one

~~~
  BACKGD  
  option1 N1
  ...
  ...  
  option2 N2   
  ...  
  ...  
  END_BACKGD
~~~
~~~
  EXCLUDE_REGIONS   
  ...  
  ...  
  END_EXCLUDE_REGIONS
~~~



| Directive |Options                   | Observations                        |
|:--------- |:------------------------ |:----------------------------------- |
| Backgd...End_Backgd | |
| Regions...End_Regions | |
| Patt_Type | Neutrons / X-rays  | |    
|           | Powder / Integrated_Intensities| |
|           | CW / TOF           | |
| Zero      | |
| Sycos     | |
| Sysin     | |
| Pow_File     | |
| Hkl_File     | |
| Irf_File     | |
| Profile_Function     | |
| Lambda     | |
| Range | |