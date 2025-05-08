!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   fhn :     FitzHugh-Nagumo
!   author: Nicolas Verschueren
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!     ---------- ---- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION v,d,w,p,s,del,alp,gam,eps,efe

      v=U(1)
      d=U(2)
      w=U(3)

      p=PAR(1)
      s=PAR(2)
      del=PAR(3)
      alp=PAR(4)
      gam=PAR(5)
      eps=PAR(6)
      efe=v*(v-1)*(alp-v)

      F(1)= d
      F(2)= (s*d+w-p-efe)/del
      F(3)= eps*(v-gam*w)/s

      ! jacobian

            IF(IJAC.EQ.0)RETURN

      DFDU(1,1)=0.0
      DFDU(1,2)=1.0
      DFDU(1,3)=0.0

!      DFDU(2,1)=-((v-1)*(alp-v)+v*(alp-v)-v*(v-1))/del

      DFDU(2,1)=(3*v*v-2*(alp-1)*v+alp)/del
      DFDU(2,2)=s/del
      DFDU(2,3)=1.0/del

      DFDU(3,1)=eps/s
      DFDU(3,2)=0.0
      DFDU(3,3)=-eps*gam/s


      IF(IJAC.EQ.1)RETURN 

      DFDP(1,1:6)=0.0
!      DFDP(1,2:4)=0

      DFDP(2,1)=-1/del
      DFDP(2,2)=d/del
      DFDP(2,3)=-(s*d+w-p-efe)/del/del
      DFDP(2,4)=-v*(v-1)/del
      DFDP(2,5:6)=0.0


      DFDP(3,1)=0.0
      DFDP(3,2)=-eps*(v-gam*w)/s/s
      DFDP(3,3:4)=0.0
      DFDP(3,5)=-eps*w/s
      DFDP(3,6)=(v-gam*w)/s
      

      END SUBROUTINE FUNC

      SUBROUTINE STPNT(NDIM,U,PAR,T)
!     ---------- ----- 

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

       PAR(1)=0.01!p
       PAR(2)=1.06 !(s)peed
       PAR(3)=5.0!(del)ta
       PAR(4)=0.1 !(alp)ha
       PAR(5)=1.0 !(gam)ma
       PAR(6)=0.01 !(eps)ilon
       PAR(11)=511.50
       U(1)=0.1! In newton we trust
       U(2)=0.0
       U(3)=0.1

      END SUBROUTINE STPNT

      SUBROUTINE BCND 
      END SUBROUTINE BCND

      SUBROUTINE ICND 
      END SUBROUTINE ICND

      SUBROUTINE FOPT 
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
