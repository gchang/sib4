!---------------------------------------------
subroutine spmerge_parse( &
    start_month, start_year, &
    end_month, end_year, &
    merge_restart, merge_requib, &
    merge_diagnostic, &
    nprocs, regridfile, in_path, out_path, &
    nselvar, varnames)
!---------------------------------------------

use sibinfo_module
implicit none

! input variables
integer, intent(in) :: start_month, start_year
integer, intent(in) :: end_month, end_year
logical, intent(in) :: merge_restart, merge_requib
logical, dimension(ndiag), intent(in) :: merge_diagnostic
integer, intent(in) :: nprocs
character(len=256), intent(in) :: regridfile
character(len=180), intent(in) :: in_path, out_path
integer, intent(in) :: nselvar
character(len=*), dimension(nselvar), intent(in) :: varnames

! internal variables
integer :: numyrs  ! number of years to process
integer :: numeqs  ! number of spinup simulations
integer :: nspinmax ! maximum number of spinup files to process (per process)
character(len=256) :: outfile   ! name of merged output file
character(len=256), dimension(:), allocatable :: &
      filenames ! file names for input files to be merged
character(len=256), dimension(:,:), allocatable :: &
      filesineq   ! file names for equilibrium input files 
character(len=256), dimension(:), allocatable :: &
      filesouteq  ! file names for equilibrium output files

! misc variables
integer :: dref, mref1, mref2
integer :: day, mydays
integer :: nsib, countgood
integer :: neq, m, p, y    ! index variables

!---------------
! check inputs
IF (nprocs < 2) THEN
   print(fa),''
   print(fa),'Error: Number of processors must be > 1'
   print(fa),''
   RETURN
ENDIF

! allocate input filename array
allocate( filenames(nprocs) )

!-------------------------------------------------------
! Merge Restart files
!-------------------------------------------------------
    if (merge_restart) then
      countgood = 0
      do y = start_year, end_year
         do m = start_month, end_month
           ! create input filenames
            do p = 1, nprocs
               call create_sib_namemonp(filenames(p), &
                    in_path, 'sib_r', '.nc', &
                     y, m, p, .true.)
            enddo

           IF (trim(filenames(1)) .NE. '') THEN
               countgood = countgood + 1
               IF (countgood .EQ. 1) THEN
                   print('(a)'),'Processing restart files.'
               ENDIF

               ! create output filename
               call create_sib_namemon(outfile, &
                    out_path, 'sib_r', '.nc', &
                    y, m, .false.)

               ! merge files
               call sprestartmerge( nprocs, regridfile, filenames, outfile )
            ENDIF
        enddo
      enddo

      IF (countgood .GE. 1) THEN
          call get_sib_dimsize(outfile, nsibnamer, nsib)
          print(fai),'  Combined landpoints: ',nsib
          print(fa),''
      ELSE
          print(fa),'No Restart Files Found.'
      ENDIF
    endif

!-------------------------------------------------------
! Merge Requib files
!-------------------------------------------------------
   if (merge_requib) then
      numyrs=end_year-start_year+1
      nspinmax=spinup_max+numyrs+2
      allocate(filesineq(nprocs,nspinmax))
      filesineq(:,:) = ''
      allocate(filesouteq(nspinmax))
      filesouteq(:) = ''

     ! create input filenames
      call create_sib_eqnames(nprocs, nspinmax, &
              in_path, out_path, &
              'sib_requib', '.nc',  &
              start_year, end_year, &
              filesineq, filesouteq, numeqs)

     IF (numeqs .ge. 1) THEN
        print(fa), 'Processing requib files.'
     ELSE
        call create_sib_eqname(nprocs, nspinmax, &
             in_path, out_path, &
             'sib_requib', '.nc', &
             start_year, end_year, &
             filesineq, filesouteq, numeqs)
     ENDIF

      do neq=1, numeqs 
         ! merge files
         print(f2a),'   Creating File: ',trim(filesouteq(neq))
         call sprestartmerge( nprocs, regridfile, filesineq(:,neq), filesouteq(neq) )

         IF (neq .eq. numeqs) THEN
             call get_sib_dimsize(filesouteq(numeqs), nsibnamer, nsib)
             print(fai),'   Combined landpoints: ',nsib
             print(fa),''
         ENDIF
      enddo
   endif !merge_requib

!-------------------------------------------------------
! Merge Diagnostic Output
!-------------------------------------------------------

   do dref = 1, ndiag
      if (merge_diagnostic(dref)) then
         countgood = 0
         do y = start_year, end_year
            mref1 = 1
            mref2 = 12
            if (y .eq. start_year) mref1=start_month
            if (y .eq. end_year)  mref2=end_month

            do m = mref1, mref2
               ! create input filenames
               do p = 1, nprocs
                  call create_sib_namemonp(filenames(p), &
                      in_path, sibprefix(dref), sibsuffix(dref), &
                      y, m, p, .true.) 
               enddo

               IF (trim(filenames(1)) .EQ. '') THEN
                 mydays = dayspermon(m)
                  IF ((mod(y,4) .eq. 0) .and. (m .eq. 2)) mydays=mydays+1
                  do day = 1, mydays
                     do p = 1, nprocs
                        print*,'Calling',day,p
                        call create_sib_namedayp(filenames(p), &
                             in_path, sibprefix(dref), sibsuffix(dref), &
                             y, m, day, p, .true.)
                     enddo
                  enddo
               ENDIF
               
               IF (trim(filenames(1)) .NE. '') THEN
                  countgood = countgood + 1
                  IF (countgood .EQ. 1) THEN
                     print(f3a),'Processing ', &
                            trim(siblabel(dref)),' files.'
                  ENDIF

                  ! create output filename
                  call create_sib_namemon(outfile, &
                       out_path, sibprefix(dref), sibsuffix(dref), &
                       y, m, .false.)

                  ! merge files
                  call spoutputmerge( nprocs, filenames, outfile, &
                       nselvar, varnames)
               ELSE  !TRY OUTPUT OF DAILY FILES
                  print*,''
                  print*,'!!No Output Files Found!!'
               ENDIF
           enddo  !m=start_month, end_month
         enddo !y=start_year, end_year

         IF (countgood .GE. 1) THEN
            call get_sib_dimsize(outfile, nsibnamed, nsib)
            print(fai),'   Combined landpoints: ',nsib
         ENDIF
       endif !merge_diagnostic
   enddo

end subroutine spmerge_parse

!
!========================================================
subroutine spoutputmerge( num, filenames, outfile, &
                        nselvar, varnames )
!========================================================
! Created for timeseries global output (03/14)
!--------------------------------------------------------

use netcdf
use sibinfo_module
implicit none

! parameters
integer, intent(in) :: num                                   ! # processes
character(len=256), dimension(num), intent(in) :: filenames  ! input file names
character(len=256), intent(in) :: outfile                    ! output file name
integer, intent(in) :: nselvar
character(len=*), dimension(nselvar), intent(in) :: varnames

! netcdf variables
integer :: status                              ! return value of netcdf function
integer, dimension(num) :: ncid                ! input file id#s
integer :: outid                               ! output file id#
integer :: numvars                             ! # variables in input files
integer :: numatts                             ! # attributes in input files
character(len=20) :: name                      ! variable name
integer, dimension(:), allocatable :: dimids   ! dimension id#s
integer, dimension(:), allocatable :: dimlens  ! dimension values
integer, dimension(:), allocatable :: vardims  ! variable dimensions
integer :: dimlen                              ! dimension length
integer :: xtype                               ! variable data type
integer :: numdims                             ! # dimensions in input files
integer :: varid                               ! variable id#

! local variables
integer :: nvarso
character(len=20), dimension(:), allocatable :: varnameso

integer :: landpoints
integer*1, dimension(:), allocatable :: bvalues1
character(len=3), dimension(:), allocatable :: cvalues3
character(len=6), dimension(:), allocatable :: cvalues6
character(len=7),  dimension(:), allocatable :: cvalues7 
character(len=10), dimension(:), allocatable :: cvalues10
double precision, dimension(:), allocatable :: dvalues
real, dimension(:), allocatable :: fvalues, fvaluesb
real, dimension(:,:), allocatable :: fvalues2
real, dimension(:,:,:), allocatable :: fvalues3
integer, dimension(:), allocatable :: ivalues1
integer, dimension(:,:), allocatable :: ivalues2
integer :: f, a, d, v, vid   ! index variables

print*,'HAS NOT BEEN CHANGED FROM MERGE_OUTPUT!!'
STOP
!
! open all input files
    do f = 1, num
        !print*,'Opening file: '
        !print*,trim(filenames(f))
        status = nf90_open( trim(filenames(f)), nf90_nowrite, ncid(f) )
    enddo
!
! create output file
    !print*,'Creating output file: '
    !print*,'  ',trim(outfile)
    if (use_deflate) then
       status = nf90_create( trim(outfile), nf90_hdf5, outid )
    else
       status = nf90_create( trim(outfile), nf90_64bit_offset, outid )
    endif
!
! get dimensions and attributes
    status = nf90_inquire( ncid(1), nDimensions=numdims, nVariables=numvars,  &
        nAttributes=numatts )
!
! copy global attributes over to output file
    do a = 1, numatts
        status = nf90_inq_attname( ncid(1), nf90_global, a, name )
        status = nf90_copy_att( ncid(1), nf90_global, trim(name),  &
            outid, nf90_global )
    enddo
!
! copy dimensions over to output file
    allocate( dimids(numdims) )
    allocate( dimlens(numdims) )

    landpoints = 0
    do f = 1, num
        status = nf90_inquire_dimension( ncid(f), 1, name=name, len=dimlen )
        landpoints = landpoints + dimlen
    enddo
    status = nf90_def_dim( outid, trim(name), landpoints, dimids(1) )
    dimlens(1) = landpoints

    do d = 2, numdims
       status = nf90_inquire_dimension( ncid(1), d, name=name, len=dimlens(d) )
       status = nf90_def_dim( outid, trim(name), dimlens(d), dimids(d) )
    enddo

    ! define variables and attributes in merged output file    
    allocate( vardims(numdims) )
    IF ((nselvar .EQ. 1) .AND. (trim(varnames(1)) .EQ. '')) THEN
       nvarso = numvars
       allocate(varnameso(nvarso))
       do v=1, numvars
           status = nf90_inquire_variable( ncid(1), v, name=name, &
                xtype=xtype, ndims=numdims, dimids=vardims, natts=numatts )
           varnameso(v) = name
           status = nf90_def_var( outid, trim(name), xtype,  &
                vardims(1:numdims), varid )
           if (use_deflate) THEN
               status = nf90_def_var_deflate(outid, varid, 1, 1, deflev)
           endif
               
           do a = 1, numatts
               status = nf90_inq_attname( ncid(1), v, a, name=name )
               status = nf90_copy_att( ncid(1), v, trim(name), outid, varid )
           enddo !a=1,numatts
        enddo !v=1,numvars
     ELSE
        nvarso = 0
        do v=1,nvarsreq
           status = nf90_inq_varid(ncid(1),trim(varsreq(v)),varid)
           IF (status .eq. nf90_noerr) nvarso=nvarso+1
        enddo

        do v=1,nselvar
           status = nf90_inq_varid(ncid(1),trim(varnames(v)),varid)
           IF (status .eq. nf90_noerr) nvarso=nvarso+1
        enddo

        allocate(varnameso(nvarso))
        varnameso(:)=''
        nvarso=0
        do v=1,nvarsreq
           status = nf90_inq_varid(ncid(1),trim(varsreq(v)),varid)
           IF (status .eq. nf90_noerr) THEN
              nvarso=nvarso+1
              varnameso(nvarso) = varsreq(v)
           ENDIF
        enddo

        do v=1, nselvar
           status=nf90_inq_varid(ncid(1),trim(varnames(v)),varid)
           IF (status .eq. nf90_noerr) THEN
              nvarso=nvarso+1
              varnameso(nvarso) = varnames(v)
           ENDIF
        enddo

        do v=1, nvarso
           status = nf90_inq_varid(ncid(1),trim(varnameso(v)), vid)
           status = nf90_inquire_variable( ncid(1), vid, name=name, &
                xtype=xtype, ndims=numdims, dimids=vardims, natts=numatts )
           status = nf90_def_var( outid, trim(name), xtype,  &
                vardims(1:numdims), varid )
           IF (use_deflate) THEN
               status = nf90_def_var_deflate(outid, varid, 1, 1, deflev)
           ENDIF
               
           do a = 1, numatts
               status = nf90_inq_attname( ncid(1), vid, a, name=name )
               status = nf90_copy_att( ncid(1), vid, trim(name), outid, varid )
           enddo !a=1,numatts
        enddo !v=1,numvars
        
     ENDIF
        
! stop defining variables
    status = nf90_enddef( outid )

! copy variables to output file
    do v=1, nvarso
       status = nf90_inq_varid(ncid(1), trim(varnameso(v)), vid)
       status = nf90_inquire_variable( ncid(1), vid, &
             name=name, xtype=xtype,  &
             ndims=numdims, dimids=vardims )

       if ((xtype .eq. 6) .and. (numdims .eq. 1)) then
          allocate(dvalues(dimlens(vardims(1))))
          status = nf90_get_var(ncid(1),vid,dvalues)
          status = nf90_put_var( outid, v, dvalues )
          deallocate(dvalues)

       elseif ((xtype .eq. 5) .and. (numdims .eq. 1)) then
          allocate(fvalues(dimlens(vardims(1))))
          if (vardims(1) .eq. 1) then
             d = 1
             do f=1,num
                status = nf90_inquire_dimension( ncid(f), 1, len=dimlen)
                status = nf90_get_var( ncid(f), vid, fvalues(d:d+dimlen-1))
                d = d + dimlen
             enddo
           else
              status = nf90_get_var( ncid(1), vid, fvalues)
           endif

          status = nf90_put_var( outid, v, fvalues )
          deallocate(fvalues)

       elseif ((xtype .eq. 5) .and. (numdims .eq. 2)) then
          allocate(fvalues2(dimlens(vardims(1)), &
                            dimlens(vardims(2))))
          d = 1
          do f=1,num
             status = nf90_inquire_dimension( ncid(f), 1, len=dimlen )
             status = nf90_get_var( ncid(f), vid, fvalues2(d:d+dimlen-1, :))
             d = d + dimlen
          enddo
          status = nf90_put_var( outid, v, fvalues2 )
          deallocate(fvalues2)

       elseif ((xtype .eq. 5) .and. (numdims .eq. 3)) then
          allocate(fvalues3(dimlens(vardims(1)), &
                            dimlens(vardims(2)), &
                            dimlens(vardims(3))))
          d = 1
          do f=1,num
             status = nf90_inquire_dimension( ncid(f), 1, len=dimlen )
             status = nf90_get_var( ncid(f), vid, fvalues3(d:d+dimlen-1,:,:))
             d = d + dimlen
          enddo
          status = nf90_put_var( outid, v, fvalues3 )
          deallocate(fvalues3)

       elseif ((xtype .eq. 4) .and. (numdims .eq. 1)) then
          allocate(ivalues1(dimlens(vardims(1))))
          status = nf90_get_var(ncid(1), vid, ivalues1)
          status = nf90_put_var(outid,v,ivalues1)
          deallocate(ivalues1)
          
       elseif ((xtype .eq. 4) .and. (numdims .eq. 2)) then
          allocate(ivalues2(dimlens(vardims(1)), &
                            dimlens(vardims(2))))
          d = 1
          do f=1,num
             status = nf90_inquire_dimension( ncid(f), 1, len=dimlen )
             status = nf90_get_var( ncid(f), vid, ivalues2(d:d+dimlen-1, :))
             d = d + dimlen
          enddo
          status = nf90_put_var( outid, v, ivalues2 )
          deallocate(ivalues2)

       elseif ((xtype .eq. 2) .and. (numdims .eq. 2)) then
            if (dimlens(vardims(1)) .eq. 3) then
              allocate(cvalues3(dimlens(vardims(2))))
              status = nf90_get_var( ncid(1), vid, cvalues3 )
              status = nf90_put_var( outid, v, cvalues3 )
              deallocate(cvalues3)
              
            elseif (dimlens(vardims(1)) .eq. 6) then
              allocate(cvalues6(dimlens(vardims(2))))

              d = 1
              do f=1,num
                 status = nf90_inquire_dimension( ncid(f), 1, len=dimlen)
                 status = nf90_get_var( ncid(f), vid, cvalues6(d:d+dimlen-1) )
                 d = d + dimlen
              enddo
              status = nf90_put_var( outid, v, cvalues6 )
              deallocate(cvalues6)
          
            elseif (dimlens(vardims(1)) .eq. 7) then
              allocate(cvalues7(dimlens(vardims(2))))

              d = 1
              do f=1,num
                 status = nf90_inquire_dimension( ncid(f), 1, len=dimlen)
                 status = nf90_get_var( ncid(f), vid, cvalues7(d:d+dimlen-1) )
                 d = d + dimlen
              enddo
              status = nf90_put_var( outid, v, cvalues7 )
              deallocate(cvalues7)

           elseif (dimlens(vardims(1)) .eq. 10) then
              allocate(cvalues10(dimlens(vardims(2))))
              if (vardims(2) .eq. 1) then 
                  d = 1
                  do f=1,num
                     status = nf90_inquire_dimension( ncid(f), 1, len=dimlen)
                     status = nf90_get_var( ncid(f), vid, cvalues10(d:d+dimlen-1) )
                     d = d + dimlen
                  enddo
              else
                  status = nf90_get_var(ncid(1), vid, cvalues10)
              endif

              status = nf90_put_var( outid, v, cvalues10 )
              deallocate(cvalues10)

           else
               print*,'SPOutputMerge: Unexpected Character Array.'
               print*,'  Var name and type: ',name,xtype
               print*, '  Number of dimensions: ',numdims
               print*, '  Dimension IDs: ',vardims
               stop
           endif

       elseif ((xtype .eq. 1) .and. (numdims .eq. 1)) then
          allocate(bvalues1(dimlens(vardims(1))))
          status = nf90_get_var(ncid(1), vid, bvalues1)
          status = nf90_put_var(outid,v,bvalues1)
          deallocate(bvalues1)
           
       else
           print*,'SPOutputMerge: Unexpected Output Type and/or Dimension.'
           print*,'  Var name and type: ',name,xtype
           print*, '  Number of dimensions: ',numdims
           print*, '  Dimension IDs: ',vardims
           stop
        endif
    enddo !v=1,numvars

! close all files
do f = 1, num
   status = nf90_close(ncid(f))
enddo
status = nf90_close(outid)


end subroutine spoutputmerge

!
!========================================================
subroutine sprestartmerge( num, regridfile, filenames, outfile )
!========================================================

use netcdf
use sibinfo_module, only: &
     use_deflate, deflev
implicit none

! parameters
integer, intent(in) :: num                     ! # processes
character(len=256), intent(in) :: regridfile
character(len=256), dimension(num), intent(in) :: &
     filenames  ! input file names
character(len=256), intent(in) :: outfile      ! output file

real, parameter :: badval=-999.

! inputfile variables
integer :: ncid2
integer :: nsib2
integer, dimension(:), allocatable :: numsib
real*4, dimension(:), allocatable :: lonsib, latsib
real*8 :: diff, mindist

! netcdf variables
integer :: status                              ! return value of netcdf function
integer, dimension(num) :: ncid                ! input file id#s
integer :: outid                               ! output file id#
integer :: varid                               ! variable id#
integer, dimension(:), allocatable :: &
    dimids, &    ! dimension id#s
    dimlens      ! dimension values
integer :: dimlen                              ! dimension length
integer :: dsib, nsib
integer :: numdims                             ! # dimensions in input files
integer :: numvars                             ! # variables in input files
integer, dimension(:), allocatable :: vardims  ! variable dimensions
character(len=20) :: name                      ! variable name
integer :: xtype                               ! variable data type

! local variables
integer :: d,v,f,n                                  ! index variables
integer*1, dimension(:), allocatable :: bvalues1
integer*1, dimension(:,:), allocatable :: bvalues2, bvalues2b  ! input data values
character(len=3), dimension(:), allocatable :: cvalues3
character(len=6), dimension(:), allocatable :: cvalues6, cvalues6b
character(len=7),  dimension(:), allocatable :: cvalues7, cvalues7b
character(len=10), dimension(:), allocatable :: cvalues10, cvalues10b
integer*4, dimension(:), allocatable :: ivalues1
integer*4, dimension(:,:), allocatable :: ivalues2, ivalues2b  
real :: fvalue, fvaluetemp
real, dimension(:), allocatable :: fvalues, fvaluesb
real, dimension(:,:), allocatable :: fvalues2, fvalues2b     ! input data values
real, dimension(:,:,:), allocatable :: fvalues3, fvalues3b
real, dimension(:,:,:,:), allocatable :: fvalues4, fvalues4b

    ! open regrid file and get number of land points/lat/lon arrays
    status = nf90_open( trim(regridfile), nf90_nowrite, ncid2 )
    status = nf90_inquire_dimension( ncid2, 1, name=name, len=nsib2 )

    print*,'    Total landpoints output: ',nsib2
    allocate(lonsib(nsib2),latsib(nsib2))
    status = nf90_inq_varid(ncid2, 'lonsib', varid)
    IF (status .eq. nf90_noerr) THEN
       status = nf90_get_var(ncid2, varid, lonsib)
    ELSE
       print*,'Info File Missing Longitude.  Stopping.'
       stop
    ENDIF

    status = nf90_inq_varid(ncid2,'latsib', varid)
    IF (status .eq. nf90_noerr) THEN
       status = nf90_get_var(ncid2, varid, latsib)
    ELSE
       print*,'Info File Missing Latitude.  Stopping.'
       stop
    ENDIF
    status = nf90_close(ncid2)
    
    ! create output file
    !print(f2a),'Writing merged file: ',trim(outfile)
    IF (use_deflate) THEN
        status = nf90_create( trim(outfile), nf90_hdf5, outid )
    ELSE
        status = nf90_create( trim(outfile), nf90_64bit_offset, outid )
    ENDIF

    ! open all restart files to merge and calculate total number of land points
    nsib = 0
    dsib = 0
    allocate(numsib(nsib2))
    do f = 1, num
        !print(fa),'Opening file: ',trim(filenames(f))
        status = nf90_open( trim(filenames(f)), nf90_nowrite, ncid(f) )
        status = nf90_inquire_dimension( ncid(f), 1, name=name, len=dimlen )

        allocate(fvalues(dimlen))
        status = nf90_inq_varid(ncid(f), 'lonsib', varid)
        IF (status .eq. nf90_noerr) THEN
           status = nf90_get_var(ncid(f), varid, fvalues)
        ELSE
           print*,'Error getting file lonsib'
           stop
        ENDIF

        allocate(fvaluesb(dimlen))
        status = nf90_inq_varid(ncid(f), 'latsib', varid)
        IF (status .eq. nf90_noerr) THEN
           status = nf90_get_var(ncid(f), varid, fvaluesb)
        ELSE
           print*,'Error getting file latsib'
           stop
        ENDIF

        do d=1,dimlen
           mindist=999.
           do n=1,nsib2
              diff = abs(lonsib(n) - fvalues(d)) + abs(latsib(n) - fvaluesb(d))
              if (diff .lt. mindist) then
                 numsib(nsib+d) = n
                 mindist = diff
              endif
           enddo   
        enddo

        nsib = nsib + dimlen
        deallocate(fvalues,fvaluesb)
    enddo
    !print*,'    Combined landpoints: ',nsib
    !print*,''

! find out how many dimensions and variables from first file
    status = nf90_inquire( ncid(1), nDimensions=numdims, nVariables=numvars )
!
! copy dimensions over to output file
    allocate( dimids(numdims) )
    allocate( dimlens(numdims) )

    status = nf90_def_dim( outid, 'nsib',  nsib2, dimids(1) )
    dimlens(1) = nsib

    do d = 2, numdims
        status = nf90_inquire_dimension( ncid(1), d, name=name, len=dimlens(d) )
        status = nf90_def_dim( outid, trim(name), dimlens(d), dimids(d) )
    enddo
!
! define variables in merged output file 
    allocate( vardims(numdims) )
    do n = 1, numvars
        status = nf90_inquire_variable( ncid(1), n, name=name, &
            xtype=xtype,  &
            ndims=numdims, dimids=vardims )
        status = nf90_def_var( outid, trim(name), xtype,  &
            vardims(1:numdims), varid )

        if (use_deflate) THEN
               status = nf90_def_var_deflate(outid, varid, 1, 1, deflev)
        endif

     enddo

! stop defining variables
    status = nf90_enddef( outid )

! copy variables to output file
    do v = 1, numvars
       status = nf90_inquire_variable( &
          ncid(1), v, name=name, xtype=xtype, &
          ndims=numdims, dimids=vardims )
           
       if ((xtype .eq. 5) .and. (numdims .eq. 1)) then
          if (name .eq. 'lonsib') then
             status = nf90_put_var( outid, v, lonsib )
          elseif (name .eq. 'latsib') then
             status = nf90_put_var( outid, v, latsib )
          else
             allocate(fvalues(nsib2))
             fvalues(:) = badval
             d = 1
             do f=1,num
                status = nf90_inquire_dimension( ncid(f), 1, len=dimlen )
                allocate(fvaluesb(dimlen))
                status = nf90_get_var( ncid(f), v, fvaluesb )
                fvalues(numsib(d:d+dimlen-1)) = fvaluesb(:)
                deallocate(fvaluesb)
                d = d + dimlen
            enddo
             status = nf90_put_var( outid, v, fvalues )
             deallocate(fvalues)
          endif
          
        elseif ((xtype .eq. 5) .and. (numdims .eq. 2)) then
           allocate(fvalues2(nsib2,dimlens(vardims(2))))
           fvalues2(:,:) = badval
            d = 1
            do f = 1, num
               status = nf90_inquire_dimension( ncid(f), 1, len=dimlen )
               allocate(fvalues2b(dimlen,dimlens(vardims(2))))
               status = nf90_get_var( ncid(f), v, fvalues2b)
               fvalues2(numsib(d:d+dimlen-1),:) = fvalues2b(:,:)
               deallocate(fvalues2b)
               d = d + dimlen
            enddo
           status = nf90_put_var( outid, v, fvalues2 )
           deallocate( fvalues2 )

       elseif ((xtype .eq. 5) .and. (numdims .eq. 3)) then
           allocate(fvalues3(nsib2, dimlens(vardims(2)), &
                dimlens(vardims(3))))
           fvalues3(:,:,:) = badval
           d = 1
           do f = 1, num
              status = nf90_inquire_dimension( ncid(f), 1, len=dimlen )
              allocate(fvalues3b(dimlen,dimlens(vardims(2)),dimlens(vardims(3))))
              status = nf90_get_var( ncid(f), v, fvalues3b)
              fvalues3(numsib(d:d+dimlen-1),:,:) = fvalues3b(:,:,:)
              deallocate(fvalues3b)
              d = d + dimlen
          enddo
          status = nf90_put_var( outid, v, fvalues3 )
          deallocate( fvalues3 )

       elseif ((xtype .eq. 5) .and. (numdims .eq. 4)) then
          allocate(fvalues4(nsib2, dimlens(vardims(2)), &
               dimlens(vardims(3)), dimlens(vardims(4))))
          fvalues4(:,:,:,:) = badval
          d = 1
          do f = 1, num
             status = nf90_inquire_dimension( ncid(f), 1, len=dimlen )
             allocate(fvalues4b(dimlen,dimlens(vardims(2)),dimlens(vardims(3)),dimlens(vardims(4))))
             status = nf90_get_var( ncid(f), v, fvalues4b)
             fvalues4(numsib(d:d+dimlen-1),:,:,:) = fvalues4b(:,:,:,:)
             deallocate(fvalues4b)
             d = d + dimlen
          enddo
          status = nf90_put_var( outid, v, fvalues4 )
          deallocate( fvalues4 )

       elseif ((xtype .eq. 4) .and. (numdims .eq. 0)) then
           fvaluetemp = 0
           fvalue = 0
           do f = 1, num
              status = nf90_get_var( ncid(f), v, fvalue)
              if (fvalue .ne. fvaluetemp) then
                 fvalue = fvalue + fvaluetemp
                 fvaluetemp = fvalue
              endif
           enddo
           status = nf90_put_var( outid, v, fvalue )

        elseif ((xtype .eq. 4) .and. (numdims .eq. 1)) then
           allocate(ivalues1(dimlens(vardims(1))))
           status = nf90_get_var(ncid(1), v, ivalues1)
           status = nf90_put_var(outid,v,ivalues1)
           deallocate(ivalues1)

        elseif ((xtype .eq. 4) .and. (numdims .eq. 2)) then
           allocate(ivalues2(nsib2,dimlens(vardims(2))))
           ivalues2(:,:) = int(badval)
            d = 1
            do f = 1, num
               status = nf90_inquire_dimension( ncid(f), 1, len=dimlen )
               allocate(ivalues2b(dimlen,dimlens(vardims(2))))
               status = nf90_get_var( ncid(f), v, ivalues2b)
               ivalues2(numsib(d:d+dimlen-1),:) = ivalues2b(:,:)
               deallocate(ivalues2b)
               d = d + dimlen
            enddo
           status = nf90_put_var( outid, v, ivalues2 )
           deallocate( ivalues2 )

       elseif ((xtype .eq. 2) .and. (numdims .eq. 2)) then
          if (dimlens(vardims(1)) .eq. 3) then
             allocate(cvalues3(dimlens(vardims(2))))
             status = nf90_get_var(ncid(1), v, cvalues3 )
             status = nf90_put_var(outid, v, cvalues3 )
             deallocate(cvalues3)

          elseif (dimlens(vardims(1)) .eq. 6) then
             allocate(cvalues6(nsib2))
             cvalues6(:) = ''
             d = 1
             do f=1,num
                status = nf90_inquire_dimension( ncid(f), 1, len=dimlen)
                allocate(cvalues6b(dimlen))
                status = nf90_get_var( ncid(f), v, cvalues6b)
                cvalues6(numsib(d:d+dimlen-1)) = cvalues6b(:)
                deallocate(cvalues6b)
                d = d + dimlen
             enddo
             status = nf90_put_var(outid, v, cvalues6)
             deallocate(cvalues6)
            
          elseif (dimlens(vardims(1)) .eq. 7) then
             allocate(cvalues7(nsib2))
             cvalues7(:) = ''

              d = 1
              do f=1,num
                 status = nf90_inquire_dimension( ncid(f), 1, len=dimlen)
                 allocate(cvalues7b(dimlen))
                 status = nf90_get_var( ncid(f), v, cvalues7b)
                 cvalues7(numsib(d:d+dimlen-1)) = cvalues7b(:)
                 deallocate(cvalues7b)
                 d = d + dimlen
              enddo
              status = nf90_put_var( outid, v, cvalues7 )
              deallocate(cvalues7)

           else
              allocate(cvalues10(nsib2))
              cvalues10(:)=''
              if (vardims(2) .eq. 1) then 
                  d = 1
                  do f=1,num
                     status = nf90_inquire_dimension( ncid(f), 1, len=dimlen)
                     allocate(cvalues10b(dimlen))
                     status = nf90_get_var( ncid(f), v, cvalues10b)
                     cvalues10(numsib(d:d+dimlen-1)) = cvalues10b(:)
                     deallocate(cvalues10b)
                     d = d + dimlen
                  enddo
              else
                  status = nf90_get_var(ncid(1), v, cvalues10)
              endif

              status = nf90_put_var( outid, v, cvalues10 )
              deallocate(cvalues10)

           endif
           
        elseif ((xtype .eq. 1) .and. (numdims .eq. 2)) then
           allocate(bvalues2(nsib2,dimlens(vardims(2))))
           bvalues2b(:,:) = 0
            d = 1
            do f = 1, num
               status = nf90_inquire_dimension( ncid(f), 1, len=dimlen )
               allocate(bvalues2b(dimlen,dimlens(vardims(2))))
               status = nf90_get_var( ncid(f), v, bvalues2b)
               bvalues2(numsib(d:d+dimlen-1),:) = bvalues2b(:,:)
               deallocate(bvalues2b)
               d = d + dimlen
            enddo
           status = nf90_put_var( outid, v, bvalues2 )
           deallocate( bvalues2 )

         elseif ((xtype .eq. 1) .and. (numdims .eq. 1)) then
            allocate(bvalues1(dimlens(vardims(1))))
            status = nf90_get_var(ncid(1), v, bvalues1)
            status = nf90_put_var(outid,v,bvalues1)
            deallocate(bvalues1)
           
         else
             print*,'SPRestartMerge: Unexpected Output Type and/or Dimensions.'
             print*,'  Var name and type: ',name,xtype
             print*,'  Number of dimensions: ',numdims
             print*,'  Dimension IDs: ',vardims
             stop
          endif

    enddo !v=1,numvars

    ! close all files
    do f=1,num
       status = nf90_close(ncid(f))
    enddo
    status = nf90_close(outid)

    deallocate( dimids )
    deallocate( dimlens )
    deallocate( vardims )

end subroutine sprestartmerge











