c --- General purpose geometric routines:
c
c --- Conventions (not strict):
c
c      a, b, c ...     a(3,n)
c      x, y, z ...     x(n)
c
c      routine1(x)
c      routine2(a,b)
c      routine3(x,y,z)
c
c
c --- The list:
c
c --- root mean square error between vectors x1 and x2 of length n
c      real function rmsq1(x1,x2,n)
c
c --- root mean square error between 3d points a(3,n) and b(3,n)
c      real function rmsq2(a,b,n)
c
c --- root mean square error between 3d points x1(n),y1(n),z1(n) and
c     x2(n),y2(n),z2(n)
c      real function rmsq3(x1,y1,z1,x2,y2,z2,n)
c
c --- root mean square error between 3d points x1(n),y1(n),z1(n) and
c     x2(n),y2(n),z2(n)
c      real function rmsq4(x1,y1,z1,x2,y2,z2,n,cut,neqvp)
c
c --- root mean square error of points a(n)
c      real function rmsq5(a,n)
c
c --- largest distance between 3d points x1(n),y1(n),z1(n) and
c     x2(n),y2(n),z2(n)
c     real function rmsq6(x1,y1,z1,x2,y2,z2,n)
c
c --- root mean square error between 3d points x1(n),y1(n),z1(n) and
c     x2(n),y2(n),z2(n), selected by ind(nind)
c     real function rmsq7(x1,y1,z1,x2,y2,z2,n,ind,nind,cut,neqvp)
c
c --- average of values in vector x(n) returned in xaver and
c     x(n) = x(n) - xaver
c      subroutine centr1(x,n,xaver)
c
c --- average of values in vectors a(3,n) returned in aver(3) and
c     a(i,n) = a(i,n) - aver(i)
c      subroutine centr2(a,n,aver)
c
c --- average of values in vectors x(n),y(n),z(n) returned in aver(3) and
c     x(n) = x(n) - aver(1), ...
c      subroutine centr3(x,y,z,n,aver)
c
c --- averages of x(n),y(n),z(n) are returned in trans(3) and
c     xnew(i) = x(i) - aver(1), ...
c      subroutine centr4(x, y, z, xnew, ynew, znew, n, aver)
c
c --- averages of x(n),y(n),z(n) are returned in xc,yc,zc and
c     x(i) = x(i) - xc, ...
c      subroutine centr5(x, y, z, n, xc, yc, zc)
c
c --- returns an average of x(i)
c      real function aver1(x,n)
c
c --- returns the averages of a(1,n), a(2,n) and a(3,n) in aver(3)
c      subroutine aver2(a,n,aver)
c
c --- returns the averages of x(n), y(n) and z(n) in aver(3)
c      subroutine aver3(x,y,z,n,aver)
c
c --- returns the averages of x(n), y(n) and z(n) in aver(3)
c      subroutine aver4(x,y,z,nxyz,ind,n,xa,ya,za)
c
c --- returns the euclidean distance between x1,y1,z1 and x2,y2,z2
c      real function dist1(x1,y1,z1,x2,y2,z2)
c
c --- standard error of the mean:
c     real function errvec(x, n)
c
c --- returns the euclidean distance between a(3) and b(3)
c      real function dist2(a,b)
c
c --- returns the square of the euclidean distance
c      real function dist1sq(x1,y1,z1,x2,y2,z2)
c
c --- returns the square of the euclidean distance between a(3) and b(3)
c      real function dist2sq(a,b)
c
c --- returns the distance rms on the symmetric matrix of distances
c     spanned by 3d euclidean points x1(n),y1(n),z1(n) and x1(n),y2(n),z2(n)
c      real function drmsq1(x1,y1,z1,x2,y2,z2,n)
c
c --- returns the distance rms on the symmetric matrix of distances
c     spanned by 3d euclidean points a(3,n) and b(3,n)
c      real function drmsq2(a,b,n)
c
c --- returns the distance rms on the symmetric matrix of distances
c     spanned by 3d euclidean points a(3,n) and b(3,n); the difference
c     contributes to the sum only if smaller than cut (neqvp such differences)
c      real function drmsq3(x1,y1,z1,x2,y2,z2,n,cut,neqvp)
c
c --- translates vectors x(n),y(n),z(n) for xt, yt, zt
c      subroutine trans1(x,y,z,n,xt,yt,zt)
c
c --- translates 3d points in a(3,n) for trans(3)
c      subroutine trans2(a,n,trans)
c
c --- applies linear transformation to 3d points x(n),y(n),z(n):
c     newxyz = rotmat*oldxyz + trans
c      subroutine trnsf1(x,y,z,n,rotmat,trans)
c
c --- applies linear transformation to 3d points a(3,n)
c     newa = rotmat*olda + trans
c      subroutine trnsf2(a, n, rotmat, trans)
c
c --- applies linear transformation to 3d points x(n),y(n),z(n):
c     xyztrf = rotmat*xyz + trans
c      subroutine trnsf3(x,y,z,xtrf,ytrf,ztrf,n,rotmat,trans)
c
c --- rotates 3d points in x,y,z according to rotation matrix r(3,3)
c      subroutine rotat1(x,y,z,n,rotmat)
c
c --- rotates 3d points in a(3,n) according to rotation matrix r(3,3):
c     b = rotmat * a
c      subroutine rotat2(a,b,n,rotmat)
c
c --- given a unit vector on a screw through origin and an angle of rotation
c     around the screw, this routine returns the equivalent rotation matrix r:
c      subroutine scr2mat(s,ang,r)
c
c --- this subroutine rotates n points in xyz around vector xyzaxis through
c     origin for an angle ang [rad]: exact formula is used and the routine is
c     therefore appropriate for large angles too: result is in xyz
c      subroutine scrot1(x,y,z,n,xaxis,yaxis,zaxis,ang)
c
c --- this subroutine rotates n points in a around unit vector s through origin
c     for an angle ang [rad]: exact formula is used and the routine is
c     therefore appropriate for large angles too: result is in b
c      subroutine scrot2(a,b,n,s,ang)
c
c --- vector (x1,x2,x3) is normalized to unit length
c      subroutine norml1(x1,x2,x3)
c
c --- takes the two vectors and calculates the vector product
c      subroutine vectr1(a1,a2,a3,b1,b2,b3,ab1,ab2,ab3)
c
c --- takes the two vectors and calculates the vector product
c      subroutine vectr2(a,b,c)
c
c --- returns the angle, in radians, spanned by vectors v1(3) and v2(3)
c      real function angle2(v1,v2)
c
c --- this subroutine calculates right handed angles of rotation along
c     the axes of orthogonal coord. system
c     it assumes the following order: x, y, z - phi(1,2,3)
c     angle is positive if counterclockwise looking from the end of the
c     axis to the center
c      subroutine rotphi(r, phi)
c
c --- takes the vector a(3) and returns its size:
c      real function vsize2(a)
c
c --- takes the two vectors a(3) and b(3) and calculates the scalar product
c      real function scalr2(a,b)
c
c
c -----------------------------------------------------------------------
c Source
c -----------------------------------------------------------------------
c
c
c --- root mean square error between vectors x1 and x2 of length n
      real function rmsq1(x1,x2,n)
      implicit none
      integer n, i
      real x1(n), x2(n), dx, r
      r = 0.0
      do  i = 1, n
        dx = x2(i) - x1(i)
        r = r + dx*dx
      end do
      if (n .gt. 0) then
        rmsq1 = sqrt(r/n)
      else
        rmsq1 = 0.0
      end if
      return
      end


c --- root mean square error between 3d points a(3,n) and b(3,n)
      real function rmsq2(a,b,n)
      implicit none
      integer n, i
      real a(3,n), b(3,n), r, dist2sq
      r = 0.0
      do  i = 1, n
        r = r + dist2sq(a(1,i),b(1,i))
      end do
      if (n .gt. 0) then
        rmsq2 = sqrt(r/n)
      else
        rmsq2 = 0.0
      end if
      return
      end

c --- root mean square error between 3d points x1(n),y1(n),z1(n) and
c     x2(n),y2(n),z2(n)
      real function rmsq3(x1,y1,z1,x2,y2,z2,n)
      implicit none
      integer n, i
      real x1(n), y1(n), z1(n), x2(n), y2(n), z2(n), r, dist1sq
      r = 0.0
      do  i = 1, n
        r = r + dist1sq(x1(i),y1(i),z1(i),x2(i),y2(i),z2(i))
      end do
      if (n .gt. 0) then
        rmsq3 = sqrt(r/n)
      else
        rmsq3 = 0.0
      end if
      return
      end

c --- root mean square error between 3d points x1(n),y1(n),z1(n) and
c     x2(n),y2(n),z2(n)
      real function rmsq4(x1,y1,z1,x2,y2,z2,n,cut,neqvp)
      implicit none
#include "numbers.cst"
      integer n, i, neqvp
      real x1(n), y1(n), z1(n), x2(n), y2(n), z2(n), r, d, cut, dist1sq
      real cut2
      r = 0.0
      neqvp = 0
      cut2 = cut*cut
      do  i = 1, n
        d = dist1sq(x1(i),y1(i),z1(i),x2(i),y2(i),z2(i))
        if (d .le. cut2) then
          neqvp = neqvp + 1
          r = r + d
        end if
      end do
      if (neqvp .gt. 0) then
        rmsq4 = sqrt(r/neqvp)
      else
        rmsq4 = 0.0
      end if
      return
      end


c --- root mean square error of points a(n)
      real function rmsq5(a,n)
      implicit none
      integer n, i
      real a(n), avr, aver1, s

      avr = aver1(a, n)

      s = 0.0
      do  i = 1, n
        s = s + (a(i)-avr)**2
      end do
      rmsq5 = sqrt(s / max(n,1))

      return
      end


c --- root mean square error between 3d points x1(n),y1(n),z1(n) and
c     x2(n),y2(n),z2(n), selected by ind(nind)
      real function rmsq7(x1,y1,z1,x2,y2,z2,n,ind,nind,cut,neqvp)
      implicit none
#include "numbers.cst"
      integer n, i, neqvp, nind, ind(nind), ic
      real x1(n), y1(n), z1(n), x2(n), y2(n), z2(n), r, d, cut, dist1sq
      real cut2
      r = 0.0
      neqvp = 0
      cut2 = cut*cut
      do  ic = 1, nind
        i = ind(ic)
        d = dist1sq(x1(i),y1(i),z1(i),x2(i),y2(i),z2(i))
        if (d .le. cut2) then
          neqvp = neqvp + 1
          r = r + d
        end if
      end do
      if (neqvp .gt. 0) then
        rmsq7 = sqrt(r/neqvp)
      else
        rmsq7 = 0.0
      end if
      return
      end


c --- largest distance between 3d points x1(n),y1(n),z1(n) and
c     x2(n),y2(n),z2(n)
      real function rmsq6(x1,y1,z1,x2,y2,z2,n)
      implicit none
      integer n, i
      real x1(n), y1(n), z1(n), x2(n), y2(n), z2(n), r, dist1sq
      real rmax

      r = 0.0
      do  i = 1, n
        r = dist1sq(x1(i),y1(i),z1(i),x2(i),y2(i),z2(i))
        if (r .gt. rmax) rmax = r
      end do
      rmsq6 = sqrt(rmax)

      return
      end

c --- average of values in vector x(n) returned in xaver and
c     x(n) = x(n) - xaver
      subroutine centr1(x, n, xaver)
      implicit none
      integer n, i
      real x(n), xaver, aver1
      xaver = aver1(x, n)
      do  i = 1, n
        x(i) = x(i) - xaver
      end do
      return
      end


c --- average of values in vectors a(3,n) returned in aver(3) and
c     a(i,n) = a(i,n) - aver(i)
      subroutine centr2(a, n, aver)
      implicit none
      integer n, i, j
      real a(3,n), aver(3)
      call aver2(a, n, aver)
      do  i = 1, n
        do  j = 1, 3
          a(j,i) = a(j,i) - aver(j)
        end do
      end do
      return
      end

c --- average of values in vectors x(n),y(n),z(n) returned in aver(3) and
c     x(n) = x(n) - aver(1), ...
      subroutine centr3(x, y, z, n, aver)
      implicit none
      integer n
      real x(n), y(n), z(n), aver(3)
      call centr1(x,n,aver(1))
      call centr1(y,n,aver(2))
      call centr1(z,n,aver(3))
      return
      end

c --- negative averages of x(n),y(n),z(n) are returned in trans(3) and
c     xnew(i) = x(i) - aver(1), ...
      subroutine centr4(x, y, z, xnew, ynew, znew, n, aver)
      implicit none
      integer n, i
      real x(n), y(n), z(n), aver(3), xnew(n), ynew(n), znew(n)
      call aver3(x,y,z,n,aver)
      do  i = 1, n
        xnew(i) = x(i) - aver(1)
        ynew(i) = y(i) - aver(2)
        znew(i) = z(i) - aver(3)
      end do
      return
      end


c --- averages of x(n),y(n),z(n) are returned in xc,yc,zc and
c     x(i) = x(i) - xc, ...
      subroutine centr5(x, y, z, n, xc, yc, zc)
      implicit none
      integer n
      real x(n), y(n), z(n), xc, yc, zc
      call centr1(x,n,xc)
      call centr1(y,n,yc)
      call centr1(z,n,zc)
      return
      end

c --- returns an average of x(i)
      real function aver1(x, n)
      implicit none
      integer n,j
      real x(n),a
      a = 0.0
      do  j = 1, n
        a = a + x(j)
      end do
      if (n .gt. 0) then
        aver1 = a / n
      else
        aver1 = 0.0
      end if
      return
      end

c --- returns the averages of a(1,n), a(2,n) and a(3,n) in aver(3)
      subroutine aver2(a, n, aver)
      implicit none
      integer n, i, j
      real a(3,n), aver(3), avr
      do  i = 1, 3
        avr = 0.0
        do  j = 1, n
          avr = avr + a(i,j)
        end do
        if (n .gt. 0) then
          aver(i) = avr / n
        else
          aver(i) = 0.0
        end if
      end do
      return
      end

c --- returns the averages of x(n), y(n) and z(n) in aver(3)
      subroutine aver3(x,y,z,n,aver)
      implicit none
      integer n
      real x(n), y(n), z(n), aver(3), aver1
      aver(1) = aver1(x,n)
      aver(2) = aver1(y,n)
      aver(3) = aver1(z,n)
      return
      end


c --- returns the averages of x(n), y(n) and z(n) in xa, ya, za
      subroutine aver4(x,y,z,nxyz,ind,n,xa,ya,za)
      implicit none
      integer n, ind(n), i, nxyz
      real x(nxyz), y(nxyz), z(nxyz), xa, ya, za
      xa = 0.0
      ya = 0.0
      za = 0.0
      do  i = 1, n
        xa = xa + x(ind(i))
        ya = ya + y(ind(i))
        za = za + z(ind(i))
      end do
      xa = xa / n
      ya = ya / n
      za = za / n
      return
      end


c --- returns the euclidean distance between x1,y1,z1 and x2,y2,z2
      real function dist1(x1,y1,z1,x2,y2,z2)
        implicit none
        real x1,y1,z1,x2,y2,z2,dx,dy,dz
        dx = x2-x1
        dy = y2-y1
        dz = z2-z1
        dist1 = sqrt(dx*dx + dy*dy + dz*dz)
        return
      end

c --- returns the euclidean distance between a(3) and b(3)
      real function dist2(a,b)
        implicit none
        real a(3), b(3),dx,dy,dz
        dx = a(1)-b(1)
        dy = a(2)-b(2)
        dz = a(3)-b(3)
        dist2 = sqrt(dx*dx + dy*dy + dz*dz)
        return
      end

c --- returns the square of the euclidean distance
      real function dist1sq(x1,y1,z1,x2,y2,z2)
        implicit none
        real x1,y1,z1,x2,y2,z2,dx,dy,dz
        dx = x2-x1
        dy = y2-y1
        dz = z2-z1
        dist1sq = dx*dx + dy*dy + dz*dz
        return
      end


c --- returns the square of the euclidean distance between a(3) and b(3)
      real function dist2sq(a,b)
        implicit none
        real a(3),b(3),dx,dy,dz
        dx = a(1)-b(1)
        dy = a(2)-b(2)
        dz = a(3)-b(3)
        dist2sq = dx*dx + dy*dy + dz*dz
        return
      end


c --- returns the distance rms on the symmetric matrix of distances
c     spanned by 3d euclidean points x1(n),y1(n),z1(n) and x1(n),y2(n),z2(n)
      real function drmsq1(x1,y1,z1,x2,y2,z2,n)
        implicit none
#include "io.cst"
        integer i,j,n
        real x1(n), y1(n), z1(n), dist1
        real x2(n), y2(n), z2(n), d1, d2, d, sd

c ----- get the distance rms
        if (n .lt. 2) then
          write(iolog,'(a)') 'drmsq1__W> n<2'
          drmsq1 = -1.0
          go to 100
        end if
        sd = 0.0
        do  i = 1, n-1
          do  j = i+1, n
            d1 = dist1(x1(i),y1(i),z1(i),x1(j),y1(j),z1(j))
            d2 = dist1(x2(i),y2(i),z2(i),x2(j),y2(j),z2(j))
            d = d1 - d2
            sd = sd + d*d
          end do
        end do

        drmsq1 = sqrt(sd / (((n-1)*n)/2))

100     return
      end


c --- returns the distance rms on the symmetric matrix of distances
c     spanned by 3d euclidean points a(3,n) and b(3,n)
      real function drmsq2(a,b,n)
        implicit none
        integer i,j,n
        real a(3,n), b(3,n), d1, d2, d, sd, dist2

c ----- get the distance rms
        if (n .lt. 2) stop 'error[drmsq2]; n<2.'
        sd = 0.0
        do  i = 1, n-1
          do  j = i+1, n
            d1 = dist2(a(1,i),a(1,j))
            d2 = dist2(b(1,i),b(1,j))
            d = d1 - d2
            sd = sd + d*d
          end do
        end do

        drmsq2 = sqrt(sd / (((n-1)*n)/2))

        return
      end

c --- returns the distance rms on the symmetric matrix of distances
c     spanned by 3d euclidean points a(3,n) and b(3,n); the difference
c     contributes to the sum only if smaller than cut (neqvp such differences)
      real function drmsq3(x1,y1,z1,x2,y2,z2,n,cut,neqvp)
        implicit none
#include "io.cst"
        integer i,j,n,neqvp
        real x1(n), y1(n), z1(n)
        real x2(n), y2(n), z2(n), d1, d2, d, sd, cut, dist1

c ----- get the distance rms
        if (n .lt. 2) then
          write(iolog,'(a)') 'drmsq3__W> n<2'
          drmsq3 = -1.0
          go to 100
        end if
        sd = 0.0
        neqvp = 0
        do  i = 1, n-1
          do  j = i+1, n
            d1 = dist1(x1(i),y1(i),z1(i),x1(j),y1(j),z1(j))
            d2 = dist1(x2(i),y2(i),z2(i),x2(j),y2(j),z2(j))
            d = d1 - d2
            if (abs(d) .le. cut) then
              neqvp = neqvp + 1
              sd = sd + d*d
             end if
          end do
        end do

        if (neqvp .gt. 0) then
          drmsq3 = sqrt(sd / neqvp)
        else
          drmsq3 = 0.0
        end if

100     return
      end




c --- translates vectors x(n),y(n),z(n) for xt, yt, zt
      subroutine trans1(x,y,z,n,xt,yt,zt)
        implicit none
        integer i,n
        real x(n),y(n),z(n),xt,yt,zt
        do  i = 1, n
          x(i) = x(i) + xt
          y(i) = y(i) + yt
          z(i) = z(i) + zt
        end do
        return
      end


c --- translates vectors x(n),y(n),z(n) for xt, yt, zt
      subroutine trans3(x,y,z,n,xt,yt,zt,ind,nind)
        implicit none
        integer j,i,n,nind,ind(nind)
        real x(n),y(n),z(n),xt,yt,zt
        do  j = 1, nind
          i = ind(j)
          x(i) = x(i) + xt
          y(i) = y(i) + yt
          z(i) = z(i) + zt
        end do
        return
      end

c --- translates 3d points in a(3,n) for trans(3)
      subroutine trans2(a,n,trans)
        implicit none
        integer i,k,n
        real a(3,n),trans(3)
        do  i = 1, n
          do  k = 1, 3
            a(k,i) = a(k,i) + trans(k)
          end do
        end do
        return
      end

c --- applies linear transformation to 3d points x(n),y(n),z(n):
c     newxyz = rotmat*oldxyz + trans
      subroutine trnsf1(x, y, z, n, rotmat, trans)
      implicit none
      integer i,n
      real x(n), y(n), z(n), b1, b2, b3, rotmat(3,3), trans(3)
      do  i = 1, n
        b1=rotmat(1,1)*x(i)+rotmat(1,2)*y(i)+rotmat(1,3)*z(i) + trans(1)
        b2=rotmat(2,1)*x(i)+rotmat(2,2)*y(i)+rotmat(2,3)*z(i) + trans(2)
        b3=rotmat(3,1)*x(i)+rotmat(3,2)*y(i)+rotmat(3,3)*z(i) + trans(3)
        x(i)=b1
        y(i)=b2
        z(i)=b3
      end do
      return
      end

c --- applies linear transformation to 3d points a(3,n)
c     newa = rotmat*olda + trans
      subroutine trnsf2(a, n, rotmat, trans)
      implicit none
      integer i,n
      real a(3,n), b1, b2, b3, rotmat(3,3), trans(3)
      do  i = 1, n
        b1=rotmat(1,1)*a(1,i)+rotmat(1,2)*a(2,i)+rotmat(1,3)*a(3,i) +
     &          trans(1)
        b2=rotmat(2,1)*a(1,i)+rotmat(2,2)*a(2,i)+rotmat(2,3)*a(3,i) +
     &          trans(2)
        b3=rotmat(3,1)*a(1,i)+rotmat(3,2)*a(2,i)+rotmat(3,3)*a(3,i) +
     &          trans(3)
        a(1,i)=b1
        a(2,i)=b2
        a(3,i)=b3
      end do
      return
      end

c --- applies linear transformation to 3d points x(n),y(n),z(n):
c     xyztrf = rotmat*xyz + trans
      subroutine trnsf3(x, y, z, xtrf, ytrf, ztrf, n, rotmat, trans)
      implicit none
      integer i,n
      real x(n), y(n), z(n), rotmat(3,3), trans(3)
      real xtrf(n), ytrf(n), ztrf(n)
      do  i = 1, n
        xtrf(i)=rotmat(1,1)*x(i)+rotmat(1,2)*y(i)+rotmat(1,3)*z(i) +
     &          trans(1)
        ytrf(i)=rotmat(2,1)*x(i)+rotmat(2,2)*y(i)+rotmat(2,3)*z(i) +
     &          trans(2)
        ztrf(i)=rotmat(3,1)*x(i)+rotmat(3,2)*y(i)+rotmat(3,3)*z(i) +
     &          trans(3)
      end do
      return
      end


c --- rotates 3d points in x,y,z according to rotation matrix r(3,3)
      subroutine rotat1(x,y,z,n,r)
        implicit none
        integer i,n
        real x(n), y(n), z(n), r(3,3), b1,b2,b3
        do  i = 1, n
          b1 = x(i)*r(1,1) + y(i)*r(1,2) + z(i)*r(1,3)
          b2 = x(i)*r(2,1) + y(i)*r(2,2) + z(i)*r(2,3)
          b3 = x(i)*r(3,1) + y(i)*r(3,2) + z(i)*r(3,3)
          x(i) = b1
          y(i) = b2
          z(i) = b3
        end do
        return
      end


c --- rotates 3d points in a(3,n) according to rotation matrix r(3,3):
c     b = rotmat * a
      subroutine rotat2(a,b,n,r)
        implicit none
        integer i,n
        real a(3,n), r(3,3), b(3,n)
        do  i = 1, n
          b(1,i) = a(1,i)*r(1,1) + a(2,i)*r(1,2) + a(3,i)*r(1,3)
          b(2,i) = a(1,i)*r(2,1) + a(2,i)*r(2,2) + a(3,i)*r(2,3)
          b(3,i) = a(1,i)*r(3,1) + a(2,i)*r(3,2) + a(3,i)*r(3,3)
        end do
        return
      end


c --- given a unit vector on a screw through origin and an angle of rotation
c     around the screw, this routine returns the equivalent rotation matrix r:
      subroutine scr2mat(s,ang,r)
        implicit none
        real s(3), r(3,3), ang, cosa, sina, cosa1, r1, r2

        cosa = cos(ang)
        sina = sin(ang)
        cosa1 = 1.0-cosa

        r(1,1) = cosa+cosa1*s(1)*s(1)
        r(2,2) = cosa+cosa1*s(2)*s(2)
        r(3,3) = cosa+cosa1*s(3)*s(3)

        r1 = cosa1*s(1)*s(2)
        r2 = sina*s(3)
        r(1,2) = r1 - r2
        r(2,1) = r1 + r2

        r1 = cosa1*s(1)*s(3)
        r2 = sina*s(2)
        r(1,3) = r1 + r2
        r(3,1) = r1 - r2

        r1 = cosa1*s(2)*s(3)
        r2 = sina*s(1)
        r(2,3) = r1 - r2
        r(3,2) = r1 + r2

        return
      end


c --- returns the angle, in radians, spanned by vectors v1(3) and v2(3)
      real function angle2(v1,v2)
        implicit none
        real v1(3), v2(3), v1s, v2s, aangle
        v1s = sqrt(v1(1)*v1(1) + v1(2)*v1(2) + v1(3)*v1(3))
        v2s = sqrt(v2(1)*v2(1) + v2(2)*v2(2) + v2(3)*v2(3))
        aangle = (v1(1)*v2(1)+v1(2)*v2(2)+v1(3)*v2(3))/(v1s*v2s)
        angle2 = acos(aangle)
c        angle2 = acosfix(aangle)
        return
      end


c --- this subroutine rotates n points in xyz around vector xyzaxis through
c     origin for an angle ang [rad]: exact formula is used and the routine is
c     therefore appropriate for large angles too: result is in xyz
      subroutine scrot1(x,y,z,n,xaxis,yaxis,zaxis,ang)
      implicit none
      integer n,i
      real x(n), y(n), z(n), xaxis, yaxis, zaxis, v(3), rtl(3)
      real sinl,cosl,rl,ang,xa,ya,za
      xa = xaxis
      ya = yaxis
      za = zaxis
      call norml1(xa,ya,za)
      sinl = sin(ang)
      cosl = 1.0-cos(ang)
      do  i = 1, n
        call vectr1(xa,ya,za,x(i),y(i),z(i),v(1),v(2),v(3))
        rl = x(i)*xa + y(i)*ya + z(i)*za
        rtl(1) = rl*xa
        rtl(2) = rl*ya
        rtl(3) = rl*za
        x(i) = x(i) + v(1)*sinl + (rtl(1)-x(i))*cosl
        y(i) = y(i) + v(2)*sinl + (rtl(2)-y(i))*cosl
        z(i) = z(i) + v(3)*sinl + (rtl(3)-z(i))*cosl
      end do
      return
      end


c --- this subroutine rotates n points in a around unit vector s through origin
c     for an angle ang [rad]: exact formula is used and the routine is
c     therefore appropriate for large angles too: result is in b
      subroutine scrot2(a,b,n,s,ang)
      implicit none
      integer n,i,k
      real a(3,n), b(3,n), s(3), v(3), rtl(3),sinl,cosl,rl,ang
      sinl = sin(ang)
      cosl = 1.0-cos(ang)
      do  i = 1, n
        call vectr1(s(1),s(2),s(3),a(1,i),a(2,i),a(3,i),v(1),v(2),v(3))
        rl = a(1,i)*s(1) + a(2,i)*s(2) + a(3,i)*s(3)
        rtl(1) = rl*s(1)
        rtl(2) = rl*s(2)
        rtl(3) = rl*s(3)
        do  k = 1, 3
          b(k,i) = a(k,i) + v(k)*sinl + (rtl(k)-a(k,i))*cosl
        end do
      end do
      return
      end


cf -- this subroutine rotates specified n points in xyz around vector xyzaxis
cf    through origin for an angle ang [rad]: exact formula is used and the
cf    routine is therefore appropriate for large angles too: result is in xyz
      subroutine scrot3(x,y,z,n,xaxis,yaxis,zaxis,ang,ind,nind)
      implicit none
      integer n,j,i,nind,ind(nind)
      real x(n), y(n), z(n), xaxis, yaxis, zaxis, v(3), rtl(3)
      real sinl,cosl,rl,ang
      call norml1(xaxis,yaxis,zaxis)
      sinl = sin(ang)
      cosl = 1.0-cos(ang)
      do  j = 1, nind
        i = ind(j)
        call vectr1(xaxis,yaxis,zaxis,x(i),y(i),z(i),v(1),v(2),v(3))
        rl = x(i)*xaxis + y(i)*yaxis + z(i)*zaxis
        rtl(1) = rl*xaxis
        rtl(2) = rl*yaxis
        rtl(3) = rl*zaxis
        x(i) = x(i) + v(1)*sinl + (rtl(1)-x(i))*cosl
        y(i) = y(i) + v(2)*sinl + (rtl(2)-y(i))*cosl
        z(i) = z(i) + v(3)*sinl + (rtl(3)-z(i))*cosl
      end do
      return
      end


c --- vector (x1,x2,x3) is normalized to unit length
      subroutine norml1(x1,x2,x3)
      implicit none
#include "numbers.cst"
      real size,x1,x2,x3
      size = sqrt(x1*x1+x2*x2+x3*x3)
      if (abs(size) .lt. tenm8) then
        x1 = x1
        stop 'norml1_E> size < tenm8.'
      end if
      x1 = x1/size
      x2 = x2/size
      x3 = x3/size
      return
      end


c --- takes the two vectors and calculates the vector product
      subroutine vectr1(a1,a2,a3,b1,b2,b3,ab1,ab2,ab3)
      implicit none
      real a1,a2,a3,b1,b2,b3,ab1,ab2,ab3
      ab1 = a2*b3 - a3*b2
      ab2 = a3*b1 - a1*b3
      ab3 = a1*b2 - a2*b1
      return
      end


c --- this subroutine calculates right handed angles of rotation along
c     the axes of orthogonal coord. system
c     it assumes the following order: x, y, z - phi(1,2,3)
c     angle is positive if counterclockwise looking from the end of the
c     axis to the center
      subroutine rotphi(r, phi)
      implicit none
#include "numbers.cst"
      real r(3,3), phi(3), deg, rad
      deg(rad) = 57.29577951*rad

      if ((1.0-abs(r(3,1))).lt.tenm5) then

c       special case: assume x = 0
c                     cos(phi(2)) = 0

        phi(1) = 0.0
        phi(2) = sign(90.0, -r(3,1))
        phi(3) = deg(atan2(-r(1,2), r(2,2)))

      else

c       general case: -90 < phi(2) < 90

        phi(2) = deg(asin(-r(3,1)))
        phi(3) = deg(atan2(r(2,1), r(1,1)))
        phi(1) = deg(atan2(r(3,2), r(3,3)))
      end if

      return
      end


c --- takes the vector a(3) and returns its size:
      real function vsize2(a)
      implicit none
      real a(3)
      vsize2 = sqrt(a(1)*a(1)+a(2)*a(2)+a(3)*a(3))
      return
      end


c --- takes the two vectors a(3) and b(3) and calculates the scalar product
      real function scalr2(a, b)
      implicit none
      real a(3), b(3)
      scalr2 = a(1)*b(1) + a(2)*b(2) + a(3)*b(3)
      return
      end


c --- takes the two vectors a(3), b(3) and calculates the vector product c(3)
      subroutine vectr2(a, b, c)
      implicit none
      real a(3), b(3), c(3)
      c(1) = a(2)*b(3) - a(3)*b(2)
      c(2) = a(3)*b(1) - a(1)*b(3)
      c(3) = a(1)*b(2) - a(2)*b(1)
      return
      end


c --- standard error of the mean:
      real function errvec(x, n)
        implicit none
        integer n,i
        real x(n),a,aver1,e

        a = aver1(x,n)

        e = 0.0
        do  i = 1, n
          e = e + (x(i)-a)**2
        end do
        if (n.gt.0) then
c ------- standard deviation:
          e = sqrt(e / n)
c ------- standard error of the mean (conversion to SP or DP):
          e = e / sqrt(1.*n)
        end if
        errvec = e

        return
      end



      subroutine rotmat(dcos,angle,r)
c
c     Compute rotation matrix corresponding to rotation about an axis.
c     DCOS specifies direction cosines of rotation axis (i.e. a unit vector),
c     ANGLE is the rotation angle (in radians), and R is the output 3x3 matrix.
c
      real dcos(3),angle,r(3,3),sinang,cosang,cosa1,sin,cos

      sinang = sin(angle)
      cosang = cos(angle)
      cosa1 = 1.0 - cosang
      r(1,1) = cosang + dcos(1)**2 * cosa1
      r(2,2) = cosang + dcos(2)**2 * cosa1
      r(3,3) = cosang + dcos(3)**2 * cosa1
      r(1,2) = dcos(1)*dcos(2)*cosa1 - dcos(3)*sinang
      r(1,3) = dcos(1)*dcos(3)*cosa1 + dcos(2)*sinang
      r(2,1) = dcos(2)*dcos(1)*cosa1 + dcos(3)*sinang
      r(2,3) = dcos(2)*dcos(3)*cosa1 - dcos(1)*sinang
      r(3,1) = dcos(3)*dcos(1)*cosa1 - dcos(2)*sinang
      r(3,2) = dcos(3)*dcos(2)*cosa1 + dcos(1)*sinang
      return
      end
