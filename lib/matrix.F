c --- This file contains subroutines and functions operating on matrices:

c --- Adds scalar s and matrix a(n,m) to a(n,m):
c     subroutine sadd2(s,a,n,m,n1max,m1max)

c --- Subtracts matrix a(n,m) from scalar s to matrix a(n,m)
c     subroutine ssub2(s,a,n,m,n1max,m1max)

c --- Multiplies scalar s and matrix a(n,m) to a(n,m);
c     subroutine smult2(s,a,n,m,n1max,m1max)

c --- Adds a(n,m) and b(n,m) to matrix a(n,m):
c     subroutine madd2(a,b,n,m,n1max,m1max,n2max,m2max)

c --- Subtracts b(n,m) from a(n,m) to a(n,m)
c     subroutine msub2(a,b,n,m,n1max,m1max,n2max,m2max)

c --- Multiplies a(n1,m2) and b(n2,m2) to a(n1,m2); m1=n2
c     subroutine mmult2(a,b,n1,m1,n2,m2,n1max,m1max,n2max,m2max)

c --- a(n,m) is transposed to a(m,n)
c     subroutine mtrans2(a,n,m,n1max,m1max)

c --- a(n,n) is inverted to a(n,n)
c     subroutine minv2(a,n,n1max)

c --- Adds scalar s and matrix b(n,m) to matrix c(n,m)
c     subroutine sadd(s,a,c,n,m,n1max,m1max,n2max,m2max)

c --- Subtracts matrix a(n,m) from scalar s to matrix c(n,m)
c     subroutine ssub(s,a,c,n,m,n1max,m1max,n2max,m2max)

c --- Multiplies scalar s and matrix b(n,m) to matrix c(n,m);
c     subroutine smult(s,b,c,n,m,n1max,m1max,n2max,m2max)

c --- Adds matrix a(n,m) and b(n,m) to matrix c(n,m):
c     subroutine madd(a,b,c,n,m,n1max,m1max,n2max,m2max,n3max,m3max)

c --- Subtracts matrix b(n,m) from matrix a(n,m) to matrix c(n,m)
c     subroutine msub(a,b,c,n,m,n1max,m1max,n2max,m2max,n3max,m3max)

c --- Multiplies a(n1,m2) and b(n2,m2) to c(n1,m2); m1=n2
c     subroutine mmult(a,b,c,n1,m1,n2,m2,n1max,m1max,n2max,m2max,n3max,m3max)

c --- a(n,m) is transposed to b(m,n)
c     subroutine mtrans(a,b,n,m,n1max,m1max,n2max,m2max)

c --- matrix a(n,n) is initialized to the unit matrix
c     subroutine mid(a,n,nmax)

c --- matrix a(n,m) is initialized to a zero matrix
c     subroutine mzero(a,n,m,nmax,mmax)
c
c --- vector a(n) is initialized to a zero vector
c     subroutine vzero(a,n)
c
c --- vector a(n) is initialized to set
c     subroutine vset(a,n,set)
c
c --- vector ia(n) is initialized to iset
c     subroutine ivset(ia,n,iset)
c
c --- matrix a(n,m) is copied to matrix b(n,m)
c     subroutine mcopy(a,b,n,m,n1max,m1max,n2max,m2max)
c
c --- vector a(n) is copied to vector b(n)
c     subroutine vcopy(a,b,n)
c
c --- matrix a(n,n) is inverted to matrix b(n,n)
c     subroutine minv(a,b,n,n1max,n2max)

c --- returns a determinant of matrix a(n,n)
c     real function detm(a,n,nmax)

c --- returns a trace of matrix a(n,n)
c     real function tracem(a,n,nmax)

c --- returns eigenvalues and normalized eigenvectors of symmetric matrix a(n,n)
c     subroutine meign(a,eval,evec,n,nmax)

c --- returns sorted eigenval and normalized eigenvec of symmetric matrix a(n,n)
c     subroutine meigns(a,eval,evec,n,nmax)

c --- matrices a(n1,m1) and b(n2,m2) are used to get c(n1,n1) = A * B * A^t
c     subroutine m3mult(a,b,c,n1,m1,n2,m2,nmax1,mmax1,nmax2,mmax2,nmax3,mmax3)


c --- In general: contents dimensions: n x m
c                 physical dimensions: nmax x mmax

c --- Maximal dimensions have to be supplied for each matrix, because
c     matrices can be initiated in different program units and therefore
c     they may not have the same physical dimensions


c --- Adds scalar s and matrix a to a:
      subroutine sadd2(s,a,n,m,n1max,m1max)
        implicit none
        integer i,j,n,m,n1max,m1max
        real s, a(n1max,m1max)
        do 20  i = 1, n
          do 10  j = 1, m
            a(i,j) = s + a(i,j)
10        continue
20      continue
        return
      end

c --- Subtracts matrix a from scalar s to matrix a
      subroutine ssub2(s,a,n,m,n1max,m1max)
        implicit none
        integer i,j,n,m,n1max,m1max
        real a(n1max,m1max), s
        do 20  i = 1, n
          do 10  j = 1, m
            a(i,j) = s - a(i,j)
10        continue
20      continue
        return
      end

c --- Multiplies scalar s and a(n,m) to a(n,m);
      subroutine smult2(s,a,n,m,n1max,m1max)
        implicit none
        integer i,j,n,m,n1max,m1max
        real s, a(n1max,m1max)
        do 30  i = 1, n
          do 20  j = 1, m
            a(i,j) = s*a(i,j)
20        continue
30      continue
        return
      end

c --- Adds a and b to a:
      subroutine madd2(a,b,n,m,n1max,m1max,n2max,m2max)
        implicit none
        integer i,j,n,m,n1max,m1max,n2max,m2max
        real a(n1max,m1max), b(n2max,m2max)
        do 20  i = 1, n
          do 10  j = 1, m
            a(i,j) = a(i,j) + b(i,j)
10        continue
20      continue
        return
      end

c --- Subtracts b from a to a
      subroutine msub2(a,b,n,m,n1max,m1max,n2max,m2max)
        implicit none
        integer i,j,n,m,n1max,m1max,n2max,m2max
        real a(n1max,m1max), b(n2max,m2max)
        do 20  i = 1, n
          do 10  j = 1, m
            a(i,j) = a(i,j) - b(i,j)
10        continue
20      continue
        return
      end

c --- Multiplies a(n1,m2) and b(n2,m2) to a(n1,m2); m1=n2
      subroutine mmult2(a,b,n1,m1,n2,m2,n1max,m1max,n2max,m2max)
        implicit none
        integer nmmax,mmmax
        parameter (nmmax=100,mmmax=100)
        integer i,j,k,n1max,m1max,n2max,m2max,n1,m1,n2,m2
        real a(n1max,m1max), b(n2max,m2max), c(nmmax,mmmax)
        if (m1 .ne. n2) stop 'Error [mmut2]: m1 <> n2'
        call mcopy(a,c,n1,m1,n1max,m1max,nmmax,mmmax)
        do 30  i = 1, n1
          do 20  j = 1, m2
            a(i,j) = 0.0
            do 10  k = 1, m1
              a(i,j) = a(i,j) + c(i,k)*b(k,j)
10          continue
20        continue
30      continue
        return
      end

c --- a(n,m) is transposed to a(m,n)
      subroutine mtrans2(a,n,m,n1max,m1max)
        implicit none
        integer n,m,n1max,m1max,i,j
        real a(n1max,m1max)
        do 10  i = 1, n-1
          do 5  j = i+1, m
            call swap(a(j,i),a(i,j))
5         continue
10      continue
        return
      end

      subroutine swap(a,b)
      implicit none
      real a,b,temp
      temp = a
      a = b
      b = temp
      return
      end

c --- a is inverted to a
      subroutine minv2(a,n,n1max)
        implicit none
        integer nmmax
c ----- maximal n1max:
        parameter (nmmax = 100)
        integer n,n1max,j, indx(nmmax)
        real a(n1max,n1max), b(nmmax,nmmax), d
        call mid(b,n,nmmax)
        call ludcmp(a,n,n1max,indx,d)
        do 13  j = 1, n
          call lubksb(a,n,n1max,indx,b(1,j))
13      continue
        call mcopy(b,a,n,n,nmmax,nmmax,n1max,n1max)
        return
      end


c --- Adds scalar s and matrix b(n,m) to matrix c(n,m)
      subroutine sadd(s,b,c,n,m,n1max,m1max,n2max,m2max)
        implicit none
        integer n,m,n1max,m1max,n2max,m2max,i,j
        real s, b(n1max,m1max), c(n2max,m2max)
        do 20  i = 1, n
          do 10  j = 1, m
            c(i,j) = s + b(i,j)
10        continue
20      continue
        return
      end

c --- Subtracts matrix a(n,m) from scalar s to matrix c(n,m)
      subroutine ssub(s,a,c,n,m,n1max,m1max,n2max,m2max)
        implicit none
        integer n,m,n1max,m1max,n2max,m2max,i,j
        real a(n1max,m1max), s, c(n2max,m2max)
        do 20  i = 1, n
          do 10  j = 1, m
            c(i,j) = s - a(i,j)
10        continue
20      continue
        return
      end

c --- Multiplies scalar s and matrix b(n,m) to matrix c(n,m);
      subroutine smult(s,b,c,n,m,n1max,m1max,n2max,m2max)
        implicit none
        integer n,m,n1max,m1max,n2max,m2max,i,j
        real s, b(n1max,m1max), c(n2max,m2max)
        do 30  i = 1, n
          do 20  j = 1, m
            c(i,j) = s*b(i,j)
20        continue
30      continue
        return
      end

c --- Adds matrix a(n,m) and b(n,m) to matrix c(n,m):
      subroutine madd(a,b,c,n,m,n1max,m1max,n2max,m2max,n3max,m3max)
        implicit none
        integer n,m,n1max,m1max,n2max,m2max,i,j,n3max,m3max
        real a(n1max,m1max), b(n2max,m2max), c(n3max,m3max)
        do 20  i = 1, n
          do 10  j = 1, m
            c(i,j) = a(i,j) + b(i,j)
10        continue
20      continue
        return
      end

c --- Subtracts matrix b(n,m) from matrix a(n,m) to matrix c(n,m)
      subroutine msub(a,b,c,n,m,n1max,m1max,n2max,m2max,n3max,m3max)
        implicit none
        integer n,m,n1max,m1max,n2max,m2max,i,j,n3max,m3max
        real a(n1max,m1max), b(n2max,m2max), c(n3max,m3max)
        do 20  i = 1, n
          do 10  j = 1, m
            c(i,j) = a(i,j) - b(i,j)
10        continue
20      continue
        return
      end

c --- Multiplies a(n1,m2) and b(n2,m2) to c(n1,m2); m1=n2
      subroutine mmult(a,b,c,n1,m1,n2,m2,n1max,m1max,n2max,m2max,
     &                 n3max,m3max)
        implicit none
        integer n1,m1,n2,m2,n1max,m1max,n2max,m2max,i,j,n3max,m3max,k
        real a(n1max,m1max), b(n2max,m2max), c(n3max,m3max)
        if (m1 .ne. n2) stop 'Error [mmut]: m1 <> n2'
        do 30  i = 1, n1
          do 20  j = 1, m2
            c(i,j) = 0.0
            do 10  k = 1, m1
              c(i,j) = c(i,j) + a(i,k)*b(k,j)
10          continue
20        continue
30      continue
        return
      end

c --- a(n,m) is transposed to b(m,n)
      subroutine mtrans(a,b,n,m,n1max,m1max,n2max,m2max)
        implicit none
        integer n,m,n1max,m1max,n2max,m2max,i,j
        real a(n1max,m1max), b(n2max,m2max)
        do 10  i = 1, n
          do 5  j = 1, m
            b(j,i) = a(i,j)
5         continue
10      continue
        return
      end

c --- matrix a(n,n) is initialized to the unit matrix
      subroutine mid(a,n,nmax)
      implicit none
      integer i,j,n,nmax
      real a(nmax,nmax)
      do 20  i = 1, n
        do 10  j = 1, n
          a(i,j) = 0.0
10      continue
        a(i,i) = 1.0
20    continue
      return
      end

c --- matrix a(n,m) is initialized to a zero matrix
      subroutine mzero(a,n,m,nmax,mmax)
        implicit none
        integer i,j,n,nmax,m,mmax
        real a(nmax,mmax)
        do 20  i = 1, n
          do 10  j = 1, m
            a(i,j) = 0.0
10        continue
20      continue
        return
      end


c --- vector a(n) is initialized to set
      subroutine vset(a,n,set)
        implicit none
        integer i,n
        real a(n),set
        do  i = 1, n
          a(i) = set
        end do
        return
      end

c --- vector ia(n) is initialized to iset
      subroutine ivset(ia,n,iset)
        implicit none
        integer i,n,ia(n),iset
        do  i = 1, n
          ia(i) = iset
        end do
        return
      end

c --- vector a(n) is copied to vector b(n)
      subroutine vcopy(a,b,n)
        implicit none
        integer i,n
        real a(n), b(n)
        do i = 1, n
          b(i) = a(i)
        end do
        return
      end

cf ----------------------------------------------------------------------------
cf
cf    VCOPY2() copies X1() into X2(), but only the NI elements indexed in IND().cf    The dimension of X1() is N.
cf
cf    subroutine vcopy2(x1,y1,z1,x2,y2,z2,ind,ni,n)
cf
cf ----------------------------------------------------------------------------

      subroutine vcopy2(x1,y1,z1,x2,y2,z2,ind,ni,n)
        implicit none
        integer n, ni, ind(ni), j, i
        real x1(n), y1(n), z1(n)
        real x2(n), y2(n), z2(n)
        do  j = 1, ni
          i = ind(j)
          x2(i) = x1(i)
          y2(i) = y1(i)
          z2(i) = z1(i)
        end do
        return
      end



c --- vector v(n) is copied to vector b(n)
      subroutine ivcopy(a,b,n)
        implicit none
        integer i,n
        integer a(n), b(n)
        do i = 1, n
          b(i) = a(i)
        end do
        return
      end


c --- matrix a(n,m) is copied to matrix b(n,m)
      subroutine mcopy(a,b,n,m,n1max,m1max,n2max,m2max)
        implicit none
        integer i,j,n,m,n1max,m1max,n2max,m2max
        real a(n1max,m1max), b(n2max,m2max)
        do 10  i = 1, n
          do 5  j = 1, m
            b(i,j) = a(i,j)
5         continue
10      continue
        return
      end

c --- matrix a(n,n) is inverted to matrix b(n,n)
      subroutine minv(a,b,n,n1max,n2max)
        implicit none
        integer nmmax
c ----- maximal n1max:
        parameter (nmmax = 100)
        integer j,n,n1max,n2max
        integer indx(nmmax)
        real a(n1max,n1max), b(n2max,n2max), c(nmmax, nmmax), d
        call mcopy(a,c,n,n,n1max,n1max,nmmax,nmmax)
        call mid(b,n,n2max)
        call ludcmp(c,n,nmmax,indx,d)
        do 13  j = 1, n
          call lubksb(c,n,nmmax,indx,b(1,j))
13      continue
        return
      end

c --- returns a determinant of matrix a(n,n)
      real function detm(a,n,nmax)
        implicit none
        integer nmaxm
c ----- maximal nmax:
        parameter (nmaxm = 100)
        integer indx(nmaxm),j,n,nmax
        real a(nmax,nmax), b(nmaxm,nmaxm), d
        call mcopy(a,b,n,n,nmax,nmax,nmaxm,nmaxm)
        call ludcmp(b,n,nmaxm,indx,d)
        do 11  j = 1, n
          d = d*b(j,j)
11      continue
        detm = d
        return
      end

c --- returns a trace of matrix a(n,n)
      real function tracem(a,n,nmax)
        implicit none
        integer n,nmax,i
        real a(nmax,nmax),tr
        tr = 0.0
        do 11  i = 1, n
          tr = tr + a(i,i)
11      continue
        tracem = tr
        return
      end

c --- returns eigenvalues and normalized eigenvectors of a symmetric 
c     matrix A(n,n)
      subroutine meign(a,eval,evec,n,nmax)
        implicit none
        integer n,nmax,i,j,nrot
        real a(nmax,nmax),eval(nmax),evec(nmax,nmax)

        call jacobi(a,n,nmax,eval,evec,nrot)

c ----- restore matrix A half-destroyed by the jacobi() routine:
        do  i = 1, n-1
          do  j = i+1, n
            a(i,j) = a(j,i)
          end do
        end do

        return
      end

c --- returns sorted eigenvalues and normalized eigenvectors
c     of a symmetric matrix A(n,n)
      subroutine meigns(a,eval,evec,n,nmax)
        implicit none
        integer n,nmax
        real a(nmax,nmax),eval(nmax),evec(nmax,nmax)

        call meign(a,eval,evec,n,nmax)
c        call eignorm(evec,n,nmax)
        call eigsrt(eval,evec,n,nmax)

c        write(iolog,*) 'Vec_I: A x - L x'
c        s = 0.0
c        do  i = 1, n
c          do  j = 1, n
c            p1 = 0.0
c            do  k = 1, n
c              p1 = p1 + a(j,k) * evec(k,i)
c            end do
c            d(j) = p1 - eval(i)*evec(j,i)
c            s = s + abs(d(j))
c          end do
c          write(iolog,'(i4, 999g15.5)') i, (d(j),j=1,n)
c        end do
c        write(iolog,'(a,g15.5)') 'SUM DEVIATIONS FROM 0: ', s

        return
      end

c --- matrices a(n1,m1) and b(n2,m2) are used to get c(n1,n1) = A * B * A^t
      subroutine m3mult(a,b,c,n1,m1,n2,m2,nmax1,mmax1,nmax2,mmax2,
     &                  nmax3,mmax3)
        implicit none
        integer nmmax,mmmax
c ----- maximal nmax1,mmax1:
        parameter (nmmax=100, mmmax=100)
        integer n1,m1,n2,m2,nmax1,mmax1,nmax2,mmax2,nmax3,mmax3
        real a(nmax1,mmax1), b(nmax2,mmax2), c(nmax3,mmax3)
        real at(mmmax,nmmax), d(nmmax,mmmax)
        call mtrans(a,at,n1,m1,nmax1,mmax1,mmmax,nmmax)
        call mmult(b,at,d,n2,m2,m1,n1,nmax2,mmax2,mmmax,nmmax,nmmax,
     &             mmmax)
        call mmult(a,d,c,n1,m1,n2,n1,nmax1,mmax1,nmmax,mmmax,nmax3,
     &             mmax3)
        return
      end

c --- Numerical Recipes routine:
      subroutine lubksb(a,n,np,indx,b)
      implicit none
      integer n,indx(n),ii,np,i,ll,j
      real a(np,np),b(n),sum
      ii=0
      do 12 i=1,n
        ll=indx(i)
        sum=b(ll)
        b(ll)=b(i)
        if (ii.ne.0)then
          do 11 j=ii,i-1
            sum=sum-a(i,j)*b(j)
11        continue
        else if (sum.ne.0.) then
          ii=i
        endif
        b(i)=sum
12    continue
      do 14 i=n,1,-1
        sum=b(i)
        if(i.lt.n)then
          do 13 j=i+1,n
            sum=sum-a(i,j)*b(j)
13        continue
        endif
        b(i)=sum/a(i,i)
14    continue
      return
      end


c --- Numerical Recipes routine:
      subroutine ludcmp(a,n,np,indx,d)
      implicit none
      integer nmax
      real tiny
      parameter (nmax=100,tiny=1.0e-20)
      integer n,np,indx(n),j,i,k,imax
      real a(np,np),vv(nmax),d,aamax,sum,dum
      d=1.
      do 12 i=1,n
        aamax=0.
        do 11 j=1,n
          if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
11      continue
        if (aamax.eq.0.) stop 'Singular matrix.'
        vv(i)=1./aamax
12    continue
      do 19 j=1,n
        if (j.gt.1) then
          do 14 i=1,j-1
            sum=a(i,j)
            if (i.gt.1)then
              do 13 k=1,i-1
                sum=sum-a(i,k)*a(k,j)
13            continue
              a(i,j)=sum
            endif
14        continue
        endif
        aamax=0.
        do 16 i=j,n
          sum=a(i,j)
          if (j.gt.1)then
            do 15 k=1,j-1
              sum=sum-a(i,k)*a(k,j)
15          continue
            a(i,j)=sum
          endif
          dum=vv(i)*abs(sum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(j.ne.n)then
          if(a(j,j).eq.0.)a(j,j)=tiny
          dum=1./a(j,j)
          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
      if(a(n,n).eq.0.)a(n,n)=tiny
      return
      end


c --- from Numerical Recipes, Edition 2

      subroutine jacobi(a,n,np,d,v,nrot)
      implicit none
#include "io.cst"
      integer maxiter, nmax
      parameter (maxiter = 100)
      parameter (nmax=500)

      integer n,np,nrot
      real a(np,np),d(np),v(np,np)
      integer i,ip,iq,j
      real c,g,h,s,sm,t,tau,theta,tresh,b(nmax),z(nmax)

      if (n.gt.nmax) stop 'jacobi__E> increase NMAX'

      do 12 ip=1,n
        do 11 iq=1,n
          v(ip,iq)=0.
11      continue
        v(ip,ip)=1.
12    continue
      do 13 ip=1,n
        b(ip)=a(ip,ip)
        d(ip)=b(ip)
        z(ip)=0.
13    continue
      nrot=0
      do 24 i=1,maxiter
        sm=0.
        do 15 ip=1,n-1
          do 14 iq=ip+1,n
            sm=sm+abs(a(ip,iq))
14        continue
15      continue
        if(sm.eq.0.)return
        if(i.lt.4)then
          tresh=0.2*sm/n**2
        else
          tresh=0.
        endif
        do 22 ip=1,n-1
          do 21 iq=ip+1,n
            g=100.*abs(a(ip,iq))
            if((i.gt.4).and.(abs(d(ip))+
     *g.eq.abs(d(ip))).and.(abs(d(iq))+g.eq.abs(d(iq))))then
              a(ip,iq)=0.
            else if(abs(a(ip,iq)).gt.tresh)then
              h=d(iq)-d(ip)
              if(abs(h)+g.eq.abs(h))then
                t=a(ip,iq)/h
              else
                theta=0.5*h/a(ip,iq)
                t=1./(abs(theta)+sqrt(1.+theta**2))
                if(theta.lt.0.)t=-t
              endif
              c=1./sqrt(1+t**2)
              s=t*c
              tau=s/(1.+c)
              h=t*a(ip,iq)
              z(ip)=z(ip)-h
              z(iq)=z(iq)+h
              d(ip)=d(ip)-h
              d(iq)=d(iq)+h
              a(ip,iq)=0.
              do 16 j=1,ip-1
                g=a(j,ip)
                h=a(j,iq)
                a(j,ip)=g-s*(h+g*tau)
                a(j,iq)=h+s*(g-h*tau)
16            continue
              do 17 j=ip+1,iq-1
                g=a(ip,j)
                h=a(j,iq)
                a(ip,j)=g-s*(h+g*tau)
                a(j,iq)=h+s*(g-h*tau)
17            continue
              do 18 j=iq+1,n
                g=a(ip,j)
                h=a(iq,j)
                a(ip,j)=g-s*(h+g*tau)
                a(iq,j)=h+s*(g-h*tau)
18            continue
              do 19 j=1,n
                g=v(j,ip)
                h=v(j,iq)
                v(j,ip)=g-s*(h+g*tau)
                v(j,iq)=h+s*(g-h*tau)
19            continue
              nrot=nrot+1
            endif
21        continue
22      continue
        do 23 ip=1,n
          b(ip)=b(ip)+z(ip)
          d(ip)=b(ip)
          z(ip)=0.
23      continue
24    continue

      write(iolog,'(a,i7)')'jacobi__E> too many iterations: ',maxiter
      stop

      return
      end




      subroutine eignorm(evec, nobj, maxobj)
      implicit none
      integer nobj, maxobj, i, j
      real evec(maxobj,maxobj), ss

      do  j = 1, nobj
        ss = 0.0
        do  i = 1, nobj
          ss = ss + evec(i,j)*evec(i,j)
        end do
        do  i = 1, nobj
          evec(i,j) = evec(i,j) / ss
        end do
      end do

      return
      end



c --- from Numerical Recipes, Edition 2

      subroutine eigsrt(d,v,n,np)
      integer n,np
      real d(np),v(np,np)
      integer i,j,k
      real p
      do 13 i=1,n-1
        k=i
        p=d(i)
        do 11 j=i+1,n
          if(d(j).ge.p)then
            k=j
            p=d(j)
          endif
11      continue
        if(k.ne.i)then
          d(k)=d(i)
          d(i)=p
          do 12 j=1,n
            p=v(j,i)
            v(j,i)=v(j,k)
            v(j,k)=p
12        continue
        endif
13    continue
      return
      end


c --- swaps reals
      subroutine rswap(x,y)
        implicit none
        real t,x,y
        t = x
        x = y
        y = t
        return
      end



c --- swaps integers
      subroutine iswap(i,j)
        implicit none
        integer k,i,j
        k = i
        i = j
        j = k
        return
      end



c --- swaps strings
      subroutine sswap2(i,j)
      implicit none
      character i*(*), j*(*), itemp*(255)
      itemp = i
      i = j
      j = itemp
      return
      end



c --- swaps logicals
      subroutine lswap(i,j)
      implicit none
      logical i, j, itemp
      itemp = i
      i = j
      j = itemp
      return
      end
