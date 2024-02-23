#############
molecule_type
#############


Properties
----------
.. list-table::
   :header-rows: 1

   * - Key
     - Value
     - Description
   * - name_mol
     - str
     - Description
   * - natoms
     - int
     - Description
   * - in_xtal
     - bool
     - Description
   * - is_eulermat
     - bool
     - Description
   * - is_connect
     - bool
     - Description
   * - rot_type
     - str
     - Description
   * - coor_type
     - str
     - Description
   * - therm_type
     - str
     - Description
   * - xcentre
     - ndarray: dtype=float32, shape=(3)
     - Description
   * - mxcentre
     - ndarray: dtype=float32, shape=(3)
     - Description
   * - lxcentre
     - ndarray: dtype=int32, shape=(3)
     - Description
   * - orient
     - ndarray: dtype=float32, shape=(3)
     - Description
   * - morient
     - ndarray: dtype=float32, shape=(3)
     - Description
   * - lorient
     - ndarray: dtype=int32, shape=(3)
     - Description
   * - t_tls
     - ndarray: dtype=float32, shape=(6)
     - Description
   * - mt_tls
     - ndarray: dtype=float32, shape=(6)
     - Description
   * - lt_tls
     - ndarray: dtype=int32, shape=(6)
     - Description
   * - l_tls
     - ndarray: dtype=float32, shape=(6)
     - Description
   * - ml_tls
     - ndarray: dtype=float32, shape=(6)
     - Description
   * - ll_tls
     - ndarray: dtype=int32, shape=(6)
     - Description
   * - s_tls
     - ndarray: dtype=float32, shape=(3,3)
     - Description
   * - ms_tls
     - ndarray: dtype=float32, shape=(3,3)
     - Description
   * - ls_tls
     - ndarray: dtype=int32, shape=(3,3)
     - Description
   * - euler
     - ndarray: dtype=float32, shape=(3,3)
     - Description
   * - atname
     - 
     - Description
   * - atsymb
     - 
     - Description
   * - atz
     - ndarray: dtype=int32, shape=(  :)
     - Description
   * - ptr
     - ndarray: dtype=int32, shape=(:,:)
     - Description
   * - i_coor
     - ndarray: dtype=float32, shape=(:,:)
     - Description
   * - mi_coor
     - ndarray: dtype=float32, shape=(:,:)
     - Description
   * - li_coor
     - ndarray: dtype=int32, shape=(:,:)
     - Description
   * - u_iso
     - ndarray: dtype=float32, shape=(  :)
     - Description
   * - mu_iso
     - ndarray: dtype=float32, shape=(  :)
     - Description
   * - lu_iso
     - ndarray: dtype=int32, shape=(  :)
     - Description
   * - occ
     - ndarray: dtype=float32, shape=(  :)
     - Description
   * - mocc
     - ndarray: dtype=float32, shape=(  :)
     - Description
   * - locc
     - ndarray: dtype=int32, shape=(  :)
     - Description
   * - nb
     - ndarray: dtype=int32, shape=(  :)
     - Description
   * - inb
     - ndarray: dtype=int32, shape=(:,:)
     - Description
   * - tb
     - ndarray: dtype=int32, shape=(:,:)
     - Description
   * - conn
     - ndarray: dtype=int32, shape=(:,:)
     - Description

Functions
---------
The following functions use **molecule_type** as an argument
