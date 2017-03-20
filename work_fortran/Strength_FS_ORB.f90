      program void
        IMPLICIT NONE
        
        
	!Q(kx,ky)
	type k_Q 
        	real(kind(0d0)) :: kx,ky
        	real(kind(0d0)) :: Q
        end type k_Q
        
        integer,parameter :: N = 64
        real(kind(0d0)),parameter :: pre = 0.001d0

        real(kind(0d0)) :: dum_z(2),dum_s(73)
        real(kind(0d0)) :: PI,k_c,mu,a
        real(kind(0d0)) :: k(2) ,E_min,E_max
        integer :: i,j,nout,nout2,l,m,c_dum
        real(kind(0d0)) :: ksi,kr,sav
        integer :: sita

        character(1) :: JOB,UPLO
        integer :: LDZ,LWORK,LIWORK,INFO
        integer,Allocatable :: iwork(:)
        real(kind(0d0)) ,Allocatable  :: ap(:),w(:),work(:),z(:,:)
        real(kind(0d0)), Allocatable :: b(:,:,:)
        ! ap is hamiltonian,E is E(nx,ny,num)
        integer :: status,status2
        
        type(k_Q) ,Allocatable :: E(:,:,:) !E(nx,ny,quantum_num) 
        type(k_Q) ,Allocatable :: E_sita(:)

!==================================================================!
!       HOPPING PARAMETER && filling                               !
!==================================================================!

        double precision, PARAMETER :: en=0.6d0                                                       
        double precision, PARAMETER :: t1=-1.0d0
        double precision, PARAMETER :: t2=1.3d0
        double precision, PARAMETER :: t3=-0.85d0
        double precision, PARAMETER :: t4=-0.85d0

!==================================================================!

!==================================================================!
!       setting of FS_orb                                          !
!          f(1)->(0,0)                                             !
!          f(2)->(PI.0)                                            !
!          f(3)->(PI,PI)                                           !
!==================================================================!
        character(50) :: cmd
        character(50) :: dir,mas
        character(20) :: FN(3),FM(3)
        integer,PARAMETER ::f(3)=(/1,0,1/),f_En(3)=(/1,2,1/)

!==================================================================!

        m=2        
        
        LDZ = m
        LIWORK = 5*m + 3
        LWORK = m*m + 6*m + 1
        Allocate (ap(m*(m+1)/2),w(m),work(lwork),z(ldz,m),iwork(liwork))
        Allocate (E(0:N,0:N,m),E_sita(N),b(-N:N,-N:N,m))
        JOB = 'V'
        UPLO = 'L'
        PI = acos(-1.0d0)

!====================================================================!
!       Filename and dir serect                                      !
!====================================================================!

        dir   = 'Fe4'
        cmd = 'sh mkdir_with_argument.sh '//trim(dir)
 !       write(*,*) cmd
        call system(cmd)
        dir   = './'//trim(dir)//'/'
        FN(1) = 'orb_0_0.dat'
        FN(2) = 'orb_PI_0.dat'
        FN(3) = 'orb_PI_PI.dat'

        FM(1) = 'FS1.dat'
        FM(2) = 'FS2.dat'
        FM(3) = 'FS3.dat'
        write(mas,'(f6.1)') en
        mas = Trim(dir)//'FS_rfill_'//Trim(adjustl(mas))//'.dat'
        do i=1,3
           FN(i) = Trim(dir)//FN(i)
           FM(i) = Trim(dir)//FM(i)
        end do

!=====================================================================!

        nout=16
        nout2=26
        OPEN(UNIT=nout+1,FILE = FN(1),STATUS="REPLACE",IOSTAT=status)
        OPEN(UNIT=nout+2,FILE = FN(2),STATUS="REPLACE",IOSTAT=status2)
        OPEN(UNIT=nout+3,FILE = FN(3),STATUS="REPLACE",IOSTAT=status2)
        OPEN(UNIT=nout2+1,FILE =FM(1),STATUS="REPLACE",IOSTAT=status2)
        OPEN(UNIT=nout2+2,FILE =FM(2),STATUS="REPLACE",IOSTAT=status2)
        OPEN(UNIT=nout2+3,FILE =FM(3),STATUS="REPLACE",IOSTAT=status2)
        OPEN(UNIT=114,FILE = mas,STATUS="REPLACE",IOSTAT=status2)

!====================================================================!
!       Defining of mu from filling                                  !
!====================================================================!
        E_min=0d0
        E_max=0d0
 
        do i=-N,N
           k(1)=1d0*i/N*PI
           do j=-N,N
              k(2)=1d0*j/N*PI              
              CALL Hamiltonian_2(k,ap)              
        call dspevd(JOB,UPLO,m,ap,w,z,LDZ,work,LWORK,iwork,LIWORK,INFO)
              b(i,j,1:2)=w(1:2)
              do l=1,2
                 if(E_min>w(l)) E_min=w(l)
                 if(E_max<w(l)) E_max=w(l)
              end do
            end do
         end do
        write(*,*)  E_min,E_max
        call E_two_law(mu,E_min, E_max, b, N, en)
        write(*,*)  mu

!=====================================================================!

!=====================================================================!
!      main                                                           !
!=====================================================================!

      do i=1,3
      if(f(i)==1) then

      do sita=0,360,5
         ksi=sita*PI/180
         kr=0d0
         k(1)=0d0
         k(2)=0d0
         if(i>1) k(1)=PI+k(1)
         if(i>2) k(2)=PI+k(2)
        CALL Hamiltonian_2(k,ap)   
        call dspevd(JOB,UPLO,m,ap,w,z,LDZ,work,LWORK,iwork,LIWORK,INFO)
         sav=w(f_En(i))-mu
         do
            k(1)=kr*cos(ksi)
            k(2)=kr*sin(ksi)
            if(i>1) k(1)=PI+k(1)
            if(i>2) k(2)=PI+k(2)
        CALL Hamiltonian_2(k,ap)   
        call dspevd(JOB,UPLO,m,ap,w,z,LDZ,work,LWORK,iwork,LIWORK,INFO)
            if(((w(f_En(i))-mu)*sav)<0d0) exit
            
            if(kr>PI*1.41421356) then
            write (*,*) 'error',i,sita
            write (nout+i,*) 'error',i
            exit
            end if
            kr=pre +kr
            sav=w(f_En(i))-mu
         end do
         dum_z(1:2)=z(1:2,f_En(i))**2

      write (nout+i,*) sita,dum_z(1),dum_z(2)
      write (nout2+i,*) k(1),k(2),dum_z(1)
      end do
      

      end if
      end do
 
      do i=1,3
         close(nout+i)
         close(nout2+i)
      end do

!=====================================================================!
!	Making master_file					      !
!=====================================================================!
      OPEN(UNIT=nout2+1,FILE =FM(1),STATUS="old",IOSTAT=status2)
      OPEN(UNIT=nout2+2,FILE =FM(2),STATUS="old",IOSTAT=status2)
      OPEN(UNIT=nout2+3,FILE =FM(3),STATUS="old",IOSTAT=status2)

      do i=1,3
      if(f(i)==1) then
      do sita=0,360,5
         read(nout2+i,*) k(1),k(2),dum_z(1)
         write(114,*) k(1),k(2),dum_z(1)
      end do
      write(114,*)
      write(114,*)
      close(nout2+i)

      if(i==2) then
      OPEN(UNIT=nout2+i,FILE =FM(i),STATUS="old",IOSTAT=status2)
      do sita=0,360,5
         read(nout2+i,*) k(1),k(2),dum_z(1)
         write(114,*) -k(1),k(2),dum_z(1)
      end do
      write(114,*)
      write(114,*)
      close(nout2+i)

      OPEN(UNIT=nout2+i,FILE =FM(i),STATUS="old",IOSTAT=status2)
      do sita=0,360,5
         read(nout2+i,*) k(1),k(2),dum_z(1)
         write(114,*) k(2),k(1),dum_z(2)
      end do
      write(114,*)
      write(114,*)
      close(nout2+i)

      OPEN(UNIT=nout2+i,FILE =FM(i),STATUS="old",IOSTAT=status2)
      do sita=0,360,5
         read(nout2+i,*) k(1),k(2),dum_z(1)
         write(114,*) k(2),-k(1),dum_z(2)
      end do
      write(114,*)
      write(114,*)
      close(nout2+i)

      else if(i==3) then
      OPEN(UNIT=nout2+i,FILE =FM(i),STATUS="old",IOSTAT=status2)
      do sita=0,360,5
         read(nout2+i,*) k(1),k(2),dum_z(1)
         write(114,*) -k(1),k(2),dum_z(1)
      end do
      write(114,*)
      write(114,*)
      close(nout2+i)

      OPEN(UNIT=nout2+i,FILE =FM(i),STATUS="old",IOSTAT=status2)
      do sita=0,360,5
         read(nout2+i,*) k(1),k(2),dum_z(1)
         write(114,*) k(1),-k(2),dum_z(1)
      end do
      write(114,*)
      write(114,*)
      close(nout2+i)

      OPEN(UNIT=nout2+i,FILE =FM(i),STATUS="old",IOSTAT=status2)
      do sita=0,360,5
         read(nout2+i,*) k(1),k(2),dum_z(1)
         write(114,*) -k(1),-k(2),dum_z(1)
      end do
      write(114,*)
      write(114,*)
      close(nout2+i)
         
      end if
      end if
      end do

      close(114)
      


      	 
      	 
      
      CONTAINS
      
      SUBROUTINE Hamiltonian_2(k,ap)
      	real(kind(0d0)),INTENT(IN) :: k(2)
      	real(kind(0d0)),INTENT(OUT):: ap(3)
      	
      	ap(1)=-2*t1*cos(k(1))-2*t2*cos(k(2))
        ap(3)=-2*t1*cos(k(2))-2*t2*cos(k(1))
        ap(1)=ap(1)-4*t3*cos(k(1))*cos(k(2))
        ap(3)=ap(3)-4*t3*cos(k(1))*cos(k(2))
        ap(2)=-4d0*t4*sin(k(1))*sin(k(2))
        
      END SUBROUTINE Hamiltonian_2
      
      
      SUBROUTINE kx_ky2r_sita(k)
      			real(kind(0d0)),INTENT(INOUT) :: k(2)
      			real(kind(0d0)) :: a(2)
      			a(1) = k(1)**2 + k(2)**2
      			a(1) = a(1)**(0.5)
      			a(2)=atan(k(2)/k(1))
      			k(1)=a(1)
      			k(2)=a(2)
      
      END SUBROUTINE kx_ky2r_sita	       

      SUBROUTINE N_Es(a, n, E,ne)
        real (kind(0d0)),INTENT(OUT) :: ne
        real (kind(0d0)) :: Weight
        integer, INTENT(IN) :: n
        real (kind(0d0)),INTENT(IN) ::E, a(-n:n,-n:n,2)
        integer :: i,j,k,m

        Weight= 1d0/ (2*n+1)**2

        m=0
        do i= -n,n
           do j=-n,n
              do k=1,2
                 if(a(i,j,k)< E) then
                    m = m + 1
                 end if
              end do
           end do
        end do
        ne = m * weight
      END SUBROUTINE N_Es

      SUBROUTINE E_two_law(E,E_min, E_max, c, n, n_t)
        real (kind(0d0)) :: ne, a, b
        real (kind(0d0)), INTENT(IN) :: E_min, E_max,n_t
        real (kind(0d0)), INTENT(OUT) :: E
        integer, INTENT(IN) :: n
        real (kind(0d0)),INTENT(IN):: c(-n:n,-n:n,2)
        integer :: i=0,k

        if(E_min>E_max) then
        print *, 'error'
        stop
        end if

        a=E_min
        b=E_max
        k=n

        do i=1,20
       !    if(MOD(i,100)==0) print *,i
        !   i=i+1
           E = (a +b) / 2.0
           CALL N_Es(c, k, E,ne)
           
           IF(ne > n_t) Then
              b = E
           ELSE 
              a = E
           END IF
           
      !     IF(i>20) then
    !          print *, ne,i
       !       exit
      !     end if
        
        end do
        print *,'ne=',ne

      END SUBROUTINE E_two_law 


      end program void        