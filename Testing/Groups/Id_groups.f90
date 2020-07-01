 Program Test_Identify_Groups
    !---- Use Modules ----!
    use CFML_Globaldeps, only: cp, err_cfml,Set_CFML_DEBUG
    use CFML_Symmetry_Tables
    use CFML_gSpaceGroups

    character(len=256)                  :: generatorList
    character(len=25)                   :: forma="(i5,tr2,a,   i4,a,i8)"
    type(Spg_Type)                      :: Grp
    integer :: i, j, ier
    real(kind=cp) :: start, fin

    !> Init

    call set_CFML_DEBUG(.true.)
    do
       write(*,'(a)') "x,-y,z;x,y,z+1/2;-x,-y,-z;x+1/2,y,z    <-- The generators of a Crystallographic 3D group"
       write(*,'(a)') "x,-y,z,t,1;x,y,z,t+1/2,-1              <-- The generators of a Magnetic SuperSpace group"
       write(*,'(a)') "x1,-x2,x3,x4,1;x1,x2,x3,x4+1/2,-1      <-- The generators of a Magnetic SuperSpace group"
       write(*,'(/,a)',advance='no') " => Introduce generators the group as indicated above: "
       read(*,'(a)') generatorList
       if (len_trim(generatorList) == 0) exit

       call CPU_TIME(start)
       call Set_SpaceGroup(generatorList,Grp)
       if (Err_CFML%Ierr /= 0) then
          write(*,'(/,4x,a)') trim(Err_CFML%Msg)
          cycle
       else
          call Write_SpaceGroup_Info(Grp)
       end if

       call CPU_TIME(fin)
       write(*,"(a,f12.3,a)") "CPU_TIME for this calculation: ",fin-start," seconds"
    end do

End Program Test_Identify_Groups