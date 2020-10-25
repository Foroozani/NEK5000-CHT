	program average
	implicit none
	    integer ip,ipic,ibin,ibin1,np,np1,nbin,nn,nn1,ns,ns1,ns1a,ns1b
	    parameter(ns1a=26,ns1b=0,ns=ns1a+ns1b,ns1=1,nn1=96)
        
        integer ncount(1:nn1)
        real*8 x,y,z,zold,zini
        real*8 dummy1,dummy2,dummy3,Ra,Pr

        real*8 zz1(1:nn1),dz(1:nn1),eta(1:nn1),ratio(1:nn1)
        real*8 mean_uzt(1:nn1),mean_uzt_all(1:nn1)
        real*8 mean_epst(1:nn1),mean_epst_all(1:nn1)
        real*8 mean_epsv(1:nn1),mean_epsv_all(1:nn1)
        real*8 mean_dtdz(1:nn1),mean_dtdz_all(1:nn1)
        real*8 mean_temp(1:nn1),mean_temp_all(1:nn1)

        Ra=1.0d+7
        Pr=0.7d0

	dz(:)=0.0d0
        eta(:)=0.0d0
        ratio(:)=0.0d0
        mean_uzt_all(:)=0.0d0
        mean_epst_all(:)=0.0d0
        mean_epsv_all(:)=0.0d0
        mean_dtdz_all(:)=0.0d0
        mean_temp_all(:)=0.0d0

        OPEN(10,file='ver_uzte.dat')
        OPEN(12,file='ver_epst.dat')
        OPEN(14,file='ver_epsv.dat')
        OPEN(16,file='ver_dtdz.dat')
        OPEN(18,file='ver_temp.dat')


	DO ipic=1,ns1a
         do ibin1=1,nn1
           read(10,*)zz1(ibin1),mean_uzt(ibin1)   
           read(12,*)zz1(ibin1),mean_epst(ibin1)   
           read(14,*)zz1(ibin1),mean_epsv(ibin1)
           read(16,*)zz1(ibin1),mean_dtdz(ibin1)   
           read(18,*)zz1(ibin1),mean_temp(ibin1)

              
            mean_uzt_all(ibin1)=mean_uzt_all(ibin1)+mean_uzt(ibin1) 
            mean_epst_all(ibin1)=mean_epst_all(ibin1)+mean_epst(ibin1) 
            mean_epsv_all(ibin1)=mean_epsv_all(ibin1)+mean_epsv(ibin1) 
            mean_dtdz_all(ibin1)=mean_dtdz_all(ibin1)+mean_dtdz(ibin1) 
            mean_temp_all(ibin1)=mean_temp_all(ibin1)+mean_temp(ibin1)
         
  
         enddo
        ENDDO

        CLOSE(10)
        CLOSE(12)
        CLOSE(14)
        CLOSE(16)
        CLOSE(18)

        mean_uzt_all(:)=mean_uzt_all(:)/dble(ns-ns1+1) 
        mean_epst_all(:)=mean_epst_all(:)/dble(ns-ns1+1) 
        mean_epsv_all(:)=mean_epsv_all(:)/dble(ns-ns1+1) 
        mean_dtdz_all(:)=mean_dtdz_all(:)/dble(ns-ns1+1) 
        mean_temp_all(:)=mean_temp_all(:)/dble(ns-ns1+1) 

!------ calculate Local dissipation scale
        do ibin1=1,nn1-1
          dz(ibin1)=zz1(ibin1+1)-zz1(ibin1)
          eta(ibin1)=(Ra/Pr)**(-3.0/8.0)*mean_epsv_all(ibin1)**(-0.25)
          ratio(ibin1)=dz(ibin1)/eta(ibin1)
        enddo

!------ Output file
        OPEN(20,file='ver_all.dat')
        do ibin1=1,nn1
           write(20,'(8(1x,e16.8))')real(zz1(ibin1)),&
                                    real(mean_uzt_all(ibin1)),&
                                    real(mean_epst_all(ibin1)),&
                                    real(mean_epsv_all(ibin1)),&
                                    real(mean_dtdz_all(ibin1)),&
                                    real(mean_temp_all(ibin1)),&
                                    real(eta(ibin1)),&
                                    real(ratio(ibin1))
        enddo
        CLOSE(20)
         
        
       end program average
