cf ----------------------------------------------------------------------------
cf
cf    ORTHOG() will re-normalize a 3D rotation matrix to remove small problems.
cf    This routine is modelled on that from RESTRAIN Ver 12.
cf
cf    subroutine orthog(rm)
cf
cf ----------------------------------------------------------------------------

      subroutine orthog(rm)
c
c     THIS SUBROUTINE CHECKS DETERMINANT OF 3*3 ORTHOGONAL ROTATION
c     MATRIX AND RENORMALISES
c     THE CHECKING CAN BE BYPASSED BY CALLING ENTRY RENORM
c
      implicit none
#include "io.cst"
      real small
      parameter (small = 1.0e-4)
      integer i,j,k
      real rm(3,3),s(3,3),t(3,3),rsq(3),csq(3),detrm,temp
      real a(3,3),d(3),v(3,3)

c      real aa(3,3)
c      do  i = 1, 3
c        do  j = 1, 3
c          aa(i,j) = rm(i,j)
c        end do
c      end do

c
c   COMPUTE DETERMINANT OF A 3*3 MATRIX
c
      detrm=      rm(1,1)*rm(2,2)*rm(3,3)
      detrm=detrm+rm(2,1)*rm(3,2)*rm(1,3)
      detrm=detrm+rm(1,2)*rm(2,3)*rm(3,1)
      detrm=detrm-rm(3,1)*rm(2,2)*rm(1,3)
      detrm=detrm-rm(2,1)*rm(1,2)*rm(3,3)
      detrm=detrm-rm(3,2)*rm(2,3)*rm(1,1)
C
C   DETERMINE MATRIX ELEMENT CAUSING ORTHOGONALITY ERROR
C
      if (abs(detrm-1.00000).gt.small) then
        write(iolog,900) detrm,((rm(i,j),j=1,3),i=1,3)
900     format(//'Determinant ',f10.5,' for rotation matrix :'//
     &           3(3x,3f8.4/))
C
        rsq(1)=rm(1,1)*rm(1,1)+rm(1,2)*rm(1,2)+rm(1,3)*rm(1,3)
        rsq(2)=rm(2,1)*rm(2,1)+rm(2,2)*rm(2,2)+rm(2,3)*rm(2,3)
        rsq(3)=rm(3,1)*rm(3,1)+rm(3,2)*rm(3,2)+rm(3,3)*rm(3,3)
        csq(1)=rm(1,1)*rm(1,1)+rm(2,1)*rm(2,1)+rm(3,1)*rm(3,1)
        csq(2)=rm(1,2)*rm(1,2)+rm(2,2)*rm(2,2)+rm(3,2)*rm(3,2)
        csq(3)=rm(1,3)*rm(1,3)+rm(2,3)*rm(2,3)+rm(3,3)*rm(3,3)
C
        write(iolog,905)
905     format(1x,t2,'matrix element',t20,'sum of squares for row',
     &            t48,'sum of squares for column'/)

        do 920 i=1,3
          do 930 j=1,3
            if (abs(rsq(i)-1.00000).gt.small.and.
     &          abs(csq(j)-1.00000).gt.small) then
              write(iolog,940) i,j,rsq(i),csq(j)
940           format(1x,t6,i2,',',i2,t26,f10.5,t54,f10.5)
            endif
930       continue
920     continue
C
        write(iolog,950)
950     format(/'Error[orthog]: orthogonality error in rotation matrix'
     &          ,' - check if serious')
      endif
C
      entry renorm(rm)
C
C     NORMALISE 3*3 ROTATION MATRIX
C
C---FORM (A)=(RM).(RM-TRANSPOSE)
C
      do 1055 i=1,3
        do 1050 j=1,3
          a(i,j)=0.0
          do 1060 k=1,3
            a(i,j)=a(i,j)+rm(i,k)*rm(j,k)
1060      continue
1050    continue
1055  continue
C
C---DETERMINE EIGENVALUES AND EIGENVECTORS OF REAL SYMMETRIC MATRIX A
C
      call meign(a,d,v,3,3)

c      write(iolog,'(3g15.5)') (a(1,j), j=1,3)
c      write(iolog,'(3g15.5)') (a(2,j), j=1,3)
c      write(iolog,'(3g15.5)') (a(3,j), j=1,3)
c      write(iolog,*)
c      write(iolog,'(3g15.5)') (v(1,j), j=1,3)
c      write(iolog,'(3g15.5)') (v(2,j), j=1,3)
c      write(iolog,'(3g15.5)') (v(3,j), j=1,3)
c      write(iolog,*)
c      write(iolog,'(3g15.5)') (d(j), j=1,3)

C
C---DETERMINE (EIGENVALUES)**(-1/2)
C
      d(1)=1.0/sqrt(max(d(1),1.0e-12))
      d(2)=1.0/sqrt(max(d(2),1.0e-12))
      d(3)=1.0/sqrt(max(d(3),1.0e-12))
C
C---FORM (RM-RENORM)=(V).D.(V-TRANSPOSE).(RM)
C
C---(S)=(V).D
      do 970 i=1,3
        do 965 j=1,3
          s(i,j)=v(i,j)*d(j)
965     continue
970   continue

C---(T)=(V-TRANSPOSE).(RM)
      do 980 i=2,3
        do 975 j=1,i-1
          temp=v(i,j)
          v(i,j)=v(j,i)
          v(j,i)=temp
975     continue
980   continue
C
      do 990 i=1,3
        do 985 j=1,3
          temp=0.0
          do 983 k=1,3
            temp=temp+v(i,k)*rm(k,j)
983       continue
          t(i,j)=temp
985     continue
990   continue

C---(RM-RENORM)=(S).(T)
      do 1015 i=1,3
        do 1010 j=1,3
          temp=0.0
          do 1020 k=1,3
            temp=temp+s(i,k)*t(k,j)
1020      continue
          rm(i,j)=temp
1010    continue
1015  continue
C

c      do  i = 1, 3
c        write(iolog,*) ((rm(i,j)-aa(i,j)),j=1,3)
c      end do

      return
      end


