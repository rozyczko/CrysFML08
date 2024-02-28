!!----
!!----
!!----
SubModule (CFML_Python)  DiffPatt_Python_Wraps
   implicit none
   Contains

   Module Subroutine Wrap_DiffPat_Type(for_var,py_var,ierror)
      !---- Arguments ----!
      class(diffpat_type), intent(in)    :: for_var
      type(dict),          intent(inout) :: py_var
      integer,             intent(out)   :: ierror

      !---- Local Variables ----!
      type(ndarray) :: nd_sigma,nd_wave,nd_x,nd_y
      type(ndarray) :: nd_bgr,nd_istat,nd_nd,nd_ycalc

      ierror = 0
      if (ierror == 0) ierror = py_var%setitem('fortran_type','diffpat_type')
      if (ierror == 0) ierror = ndarray_create(nd_sigma,for_var%sigma)
      if (ierror == 0) ierror = ndarray_create(nd_wave,for_var%wave)
      if (ierror == 0) ierror = ndarray_create(nd_x,for_var%x)
      if (ierror == 0) ierror = ndarray_create(nd_y,for_var%y)
      if (ierror == 0) ierror = py_var%setitem('kindrad',for_var%kindrad)
      if (ierror == 0) ierror = py_var%setitem('npts',for_var%npts)
      if (ierror == 0) ierror = py_var%setitem('scatvar',for_var%scatvar)
      if (ierror == 0) ierror = py_var%setitem('step',for_var%step)
      if (ierror == 0) ierror = py_var%setitem('sigma',nd_sigma)
      if (ierror == 0) ierror = py_var%setitem('sigvar',for_var%sigvar)
      if (ierror == 0) ierror = py_var%setitem('title',for_var%title)
      if (ierror == 0) ierror = py_var%setitem('wave',nd_wave)
      if (ierror == 0) ierror = py_var%setitem('x',nd_x)
      if (ierror == 0) ierror = py_var%setitem('xmin',for_var%xmin)
      if (ierror == 0) ierror = py_var%setitem('xmax',for_var%xmax)
      if (ierror == 0) ierror = py_var%setitem('y',nd_y)
      if (ierror == 0) ierror = py_var%setitem('ymin',for_var%ymin)
      if (ierror == 0) ierror = py_var%setitem('ymax',for_var%ymax)
      select type (A => for_var)
         class is (diffpat_e_type)
            if (ierror == 0) ierror = py_var%setitem('fortran_type','diffpat_e_type')
            if (ierror == 0) ierror = ndarray_create(nd_bgr,A%bgr)
            if (ierror == 0) ierror = ndarray_create(nd_istat,A%istat)
            if (ierror == 0) ierror = ndarray_create(nd_nd,A%nd)
            if (ierror == 0) ierror = ndarray_create(nd_ycalc,A%ycalc)
            if (ierror == 0) ierror = py_var%setitem('al_bgr',A%al_bgr)
            if (ierror == 0) ierror = py_var%setitem('al_istat',A%al_istat)
            if (ierror == 0) ierror = py_var%setitem('al_sigma',A%al_sigma)
            if (ierror == 0) ierror = py_var%setitem('al_x',A%al_x)
            if (ierror == 0) ierror = py_var%setitem('al_y',A%al_y)
            if (ierror == 0) ierror = py_var%setitem('bgr',nd_bgr)
            if (ierror == 0) ierror = py_var%setitem('col_time',A%col_time)
            if (ierror == 0) ierror = py_var%setitem('ct_step',A%ct_step)
            if (ierror == 0) ierror = py_var%setitem('filename',A%filename)
            if (ierror == 0) ierror = py_var%setitem('filepath',A%filepath)
            if (ierror == 0) ierror = py_var%setitem('instr',A%instr)
            if (ierror == 0) ierror = py_var%setitem('istat',nd_istat)
            if (ierror == 0) ierror = py_var%setitem('monitor',A%monitor)
            if (ierror == 0) ierror = py_var%setitem('nd',nd_nd)
            if (ierror == 0) ierror = py_var%setitem('norm_mon',A%norm_mon)
            if (ierror == 0) ierror = py_var%setitem('tsample',A%tsample)
            if (ierror == 0) ierror = py_var%setitem('tset',A%tset)
            if (ierror == 0) ierror = py_var%setitem('ycalc',nd_ycalc)
      end select
      select type (A => for_var)
         class is (diffpat_g_type)
            if (ierror == 0) ierror = py_var%setitem('fortran_type','diffpat_g_type')
            if (ierror == 0) ierror = py_var%setitem('legend_x',A%legend_x)
            if (ierror == 0) ierror = py_var%setitem('legend_y',A%legend_y)
            if (ierror == 0) ierror = py_var%setitem('gbgr',A%gbgr)
            if (ierror == 0) ierror = py_var%setitem('gsigma',A%gsigma)
            if (ierror == 0) ierror = py_var%setitem('gy',A%gy)
            if (ierror == 0) ierror = py_var%setitem('gycalc',A%gycalc)
      end select
      if (ierror /= 0) then
         err_cfml%flag = .true.
         err_cfml%ierr = -1
         err_cfml%msg  = 'Wrap_DiffPat_Type: Wrapping failed'
      end if

   End Subroutine Wrap_DiffPat_Type

End SubModule DiffPatt_Python_Wraps