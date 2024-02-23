######################
multistate_vector_type
######################


Properties
----------
.. list-table::
   :header-rows: 1

   * - Key
     - Value
     - Description
   * - npar
     - int
     - Description
   * - nconf
     - int
     - Description
   * - code
     - ndarray: dtype=int32, shape=(np_san)
     - Description
   * - bound
     - ndarray: dtype=int32, shape=(np_san)
     - Description
   * - state
     - ndarray: dtype=float32, shape=(np_san,np_conf)
     - Description
   * - stp
     - ndarray: dtype=float32, shape=(np_san,np_conf)
     - Description
   * - cost
     - ndarray: dtype=float32, shape=(np_conf)
     - Description
   * - low
     - ndarray: dtype=float32, shape=(np_san)
     - Description
   * - high
     - ndarray: dtype=float32, shape=(np_san)
     - Description
   * - config
     - ndarray: dtype=float32, shape=(np_san)
     - Description
   * - sigma
     - ndarray: dtype=float32, shape=(np_san)
     - Description
   * - nampar
     - 
     - Description
   * - best_cost
     - float
     - Description

Functions
---------
The following functions use **multistate_vector_type** as an argument
