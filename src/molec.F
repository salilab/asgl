c --- This subroutine draws ball and stick model of the selected atoms.
c
      subroutine draw_ball_stick(perspective,x,y,z,isel,numsel,numat,
     -           atrad,acolor,ialine,ibond,bcolor,ibline,numbond,
     -           bond_fact,taper)
      implicit none
      integer maxpoint,maxatm
c --- maximal number of bonds times 2
      parameter (maxpoint=10000)
      parameter (maxatm=5000)
c --- this is the measure of the bond width that enables using bond_factor
c     on the order of 1 to scale bond widths
      real width
      parameter (width = 0.3)
      integer numsel,ia,numat,ialine(numat),numbond,is
      integer ibline(numbond),isel(numsel),numpoint,inibond,k
      integer ibond(2,numbond),i,j,l,lastbond(maxatm)
      integer nbonds(maxpoint),iseq(maxatm),irank(maxatm)
      real atrad(numat),acolor(numat),bcolor(numbond),taper(numbond)
      real x(numat),y(numat),z(numat),bond_fact(numbond)
      logical perspective

c --- assuming each bond is connected to two atoms only
      if (numbond*2 .gt. maxpoint) 
     -   stop 'Error[draw_ball_stick]; increase maxpoint'
      if (numsel .gt. maxatm) 
     -   stop 'Error[draw_ball_stick]; increase maxatm'

c --- for each atom, get the bonds connected to it
      call bond_list(ibond,numbond,lastbond,numpoint,numat,nbonds,
     -               maxpoint,isel,numsel)

c      do  i = 1, numbond
c        write(*,'(a,4i5)') 'BOND: ',i,ibond(1,i),ibond(2,i),ibline(i)
c      end do
c      l = 1
c      do  i = 1, numat
c        j = lastbond(i)
c        write(*,'(a,4i5)') 'ATOM: ',i,(nbonds(k),k=l,j)
c        l = j + 1
c      end do

c --- get the rank of each atom in the structure (by its z coordinate)
      call rank_seq(z,numat,isel,numsel,iseq,irank)

c --- plot selected atoms and bonds connected to them,
c     from the most distant to the closest atom:
      do i = 1, numsel

c ------ its index among the selected atoms:
         is=iseq(i)

c ------ its index among all atoms:
         ia = isel(is)

c ------ plot it
         if (atrad(ia) .gt. 1.0E-3) 
     -     call draw_at(perspective,x(ia),y(ia),z(ia),atrad(ia),
     -                  acolor(ia),ialine(ia))

         inibond=1
         if (is.gt.1) inibond=lastbond(is-1)+1
         do k=inibond,lastbond(is)
c -------- bond l is bonded to atom ia
           l=nbonds(k)
           if (ia.eq.ibond(1,l)) then
c ---------- atom ia occurs behind atom j; draw bond l between them
             j=ibond(2,l)
c             write(*,*)l,bcolor(l),ibline(l),bond_fact(l),taper(l)
c             write(*,*)x(ia),y(ia),z(ia),x(j),y(j),z(j)
             call draw_bond(perspective,x(ia),y(ia),z(ia),
     -            x(j),y(j),z(j),atrad(ia),atrad(j),bcolor(l),
     -            ibline(l),width,bond_fact(l),taper(l))
           end if

         end do
      end do

      return
      end




c --- This subroutine draws a colored circle for the given point.
      subroutine draw_at(perspective,x,y,z,atrad,color,iline)
      implicit none
      integer iline
      real x, y, z, persp, atrad, color
      logical perspective

      if (perspective) then
         call circle(persp(x,z),persp(y,z),persp(atrad,z),iline,color)
      else
         call circle(x,y,atrad,iline,color)
      end if

      return
      end


c --- plot the bond
      subroutine draw_bond(perspective,xi,yi,zi,xj,yj,zj,ri,rj,
     -           color,iline,width,bond_fact,taper)
      implicit none
      integer maxpbond
      parameter (maxpbond = 20)
      integer iline,numpbond
      real xb(maxpbond),yb(maxpbond),color,x1,y1,x2,y2,wpx,wpy,taper
      real xi,yi,zi,xj,yj,zj,ri,rj,width,bond_fact
      logical perspective

      if (zi .lt. zj ) then
        call calc_bond(perspective,xi,yi,zi,xj,yj,zj,ri,rj,width,
     -                 bond_fact,xb,yb,numpbond,maxpbond,taper)
      else
        call calc_bond(perspective,xj,yj,zj,xi,yi,zi,rj,ri,width,
     -                 bond_fact,xb,yb,numpbond,maxpbond,taper)
      end if

c      write(*,*) 'iline: ', iline
      if (iline .gt. 0) then
        call pgone(xb,yb,numpbond,iline,color)
      else
        x1 = wpx(0.5*(xb(1)+xb(4)))
        y1 = wpy(0.5*(yb(1)+yb(4)))
        x2 = wpx(0.5*(xb(2)+xb(3)))
        y2 = wpy(0.5*(yb(2)+yb(3)))
c        write(*,*) xb(1),yb(1),xb(2),yb(2),xb(3),yb(3),xb(4),yb(4)
c        write(*,*) x1,y1,x2,y2
        call line(x1,y1,x2,y2,-iline)
      end if

      return
      end



c     This subroutine calculates the vertices of the polygon that
c     represent the bonds.  Briefly, it takes the 3D-single-line bond,
c     calculates an iso-Z line orthogonal to it at each
c     end and selects two equidistant points 0.3*R away from it. This
c     gives the required four points.  If rounded bonds are required
c     the "cap" corresponding to the lower atom is expanded
c
c     points 1 and 4 are the iso-z endpoints in the bottom atom,
c     points 2 and 3  "   "    "       "      "  "  top     "
c
c     the lines 1-2 and 3-4 are the sides of the bond, while
c     the lines 2-3 and 4-?-1 correspond to the "caps" of the bond .
c
      subroutine calc_bond(perspective,xi,yi,zi,xj,yj,zj,ri,rj,
     -           width,bond_fact,xb,yb,numpbond,maxpbond,taper)
      implicit none
      integer numpbond,maxpbond,numpcap
      real xb(maxpbond),yb(maxpbond),dz,taper
      real xi,yi,zi,xj,yj,zj,ri,rj,width,bond_fact
      real x1,y1,z1,x2,y2,z2,slope,yint,add1,add2,w
      real costheta,sintheta,theta,persp
      logical perspective,vertical

c --- point i has a smaller z than point j:
      if (zi .gt. zj) stop 'Error[calc_bond] zi>zj !'

      call calc_slb(perspective,xi,yi,zi,xj,yj,zj,ri,rj,width,
     -              bond_fact,x1,y1,z1,x2,y2,z2)
c
c     building a trapeze :
c
      call line_eq(x1,y1,x2,y2,slope,yint,vertical)
      w=width*bond_fact*bond_fact
      add1=w
      add2=w
      if (perspective) then
        add1 = persp(add1,zi)*persp(1.0,zi)
        add2 = persp(add2,zj)*persp(1.0,zj)
      end if

c --- do additional tapering of the bond (thicker larger Z and thinner
c     for smaller Z:
      if (abs(taper) .gt. 0.001) then
c ----- dz > 0, always
        dz = zj - zi
        add1 = add1 - taper*dz*w
        add2 = add2 + taper*dz*w
      end if 

c      
      if (vertical) then
        xb(1) = x1-add1
        yb(1) = y1
        xb(2) = x2-add2
        yb(2) = y2
        xb(3) = x2+add2
        yb(3) = y2
        xb(4) = x1+add1
        yb(4) = y1
      else
        theta = atan(slope)
        costheta = cos(theta)
        sintheta = sin(theta)
c
        xb(1)=x1-add1*sintheta
        yb(1)=y1+add1*costheta
        xb(4)=x1+add1*sintheta
        yb(4)=y1-add1*costheta
        xb(2)=x2-add2*sintheta
        yb(2)=y2+add2*costheta
        xb(3)=x2+add2*sintheta
        yb(3)=y2-add2*costheta
      end if
c
c     rounded bonds are needed; go and calculate them, add the
c     calculated points at the end of the xb and yb arrays.  The
c     bonds where the two atoms have the same Z don't have to be
c     calculated.
c
      if (zj.ne.zi) then
        call calc_cap(x1,y1,z1,x2,y2,z2,add1,numpcap,xb(5),yb(5))
        numpbond=4+numpcap
      else
        numpbond=4
      end if

      return
      end




c --- Calculation of the rounded "caps" of bonds :
c
c     input  :  BOTTOM     contains the coordinates of the center of
c                          the bond cap for the atom in the bottom
c               TOP        contains the coordinates of the center of
c                          the bond cap for the atom on top
c               WIDTH      the half width of the bond
c
c     output :  NUMPCAP    the number of points to be added to the
c                          bond-polygon
c               XCAP, YCAP 2-D coordinates of these points.
c
c     Calculation :
c
c           a 3-D unitary cap (values are sines and cosines) centered
c              at the origin in the Y-Z plane is used as a template
c           this template is then multiplied by the wanted width
c           the cap is then rotated in the y axis according to the
c           angle that the central bond line makes with the X-Y plane
c           after that it is rotated in the z axis to match the
c           orthogonal to the slope of the line in the X-Y plane
c           3-D translated to coincide with the center of the bond, and
c           finally, the 2-D values extracted.
c
      subroutine calc_cap(xbottom,ybottom,zbottom,xtop,ytop,ztop,
     -                    width,numpcap,xcap,ycap)
      implicit none
      integer ncap
c --- number of points describing the cap curve
      parameter (ncap = 9)
      real rad_to_deg
      parameter (rad_to_deg = 57.295779515)
c
      integer i,numpcap
      real unity(ncap),unitz(ncap),xcap(ncap),ycap(ncap),cap(3,ncap)
      real xbottom,ybottom,zbottom,xtop,ytop,ztop,width,theta,phi
c
      data unity/-0.95106,-0.80902,-0.58779,-0.30902, 0.0e0,
     &            0.30902, 0.58779, 0.80902, 0.95106/
      data unitz/0.30902, 0.58779, 0.80902,0.95106, 1.0e0,
     &           0.95106, 0.80902, 0.58779, 0.30902/
c
c     scale the cap according to the width :
c
      do i=1, ncap
        cap(1,i)=0.0e0
        cap(2,i)=unity(i)*width
        cap(3,i)=unitz(i)*width
      end do
c
c     do the rotations :
c
      phi=atan((ztop-zbottom)/sqrt((xtop-xbottom)**2+
     -         (ytop-ybottom)**2))*rad_to_deg
      if (xtop.gt.xbottom) phi = -phi
      if (xtop.eq.xbottom) then
         theta=90.0
         if (ytop.gt.ybottom) phi = phi+180.0
      else
           theta=atan((ytop-ybottom)/(xtop-xbottom))*rad_to_deg 
      end if
      call rot_3d(cap,9,0.0,-phi,0.0)
      call rot_3d(cap,9,0.0,0.0,theta)
c
c     translate
c
      do i=1,ncap
        xcap(i)=cap(1,i)+xbottom
        ycap(i)=cap(2,i)+ybottom
      end do
      numpcap = ncap

      return
      end


c --- Calculation of the equation of a line (slope & Y-intercept).
      subroutine line_eq(x1,y1,x2,y2,slope,yint,vertical)
      implicit none
      real x1,y1,x2,y2,slope,yint
      logical vertical

      if (x1.eq.x2) then
        vertical=.true.
        slope=0.0
        yint=0.0
      else
        vertical=.false.
        slope=(y1-y2)/(x1-x2)
        yint=y1-(slope*x1)
      end if

      return
      end



c --- Input : perspective,xi,yi,zi,xj,yj,zj,ri,rj,bond_fact
c
c --- Output: x1,y1,z1,x2,y2,z2
c
      subroutine calc_slb(perspective,xi,yi,zi,xj,yj,zj,ri,rj,
     -           width,bond_fact,x1,y1,z1,x2,y2,z2)
      implicit none
      real x1,y1,z1,x2,y2,z2
      real xi,yi,zi,ri,xj,yj,zj,rj
      real xii,yii,zii,rii,xjj,yjj,zjj,rjj
      real rnorm,ridis,rjdis,persp
      real bond_fact,w,wi,width,wj
      logical perspective
c
      xii = xi
      xjj = xj
      yii = yi
      yjj = yj
      zii = zi
      zjj = zj
      rii = ri
      rjj = rj
      if (perspective) then
        xii = persp(xii,zi)
        xjj = persp(xjj,zj)
        yii = persp(yii,zi)
        yjj = persp(yjj,zj)
        rii = persp(rii,zi)
        rjj = persp(rjj,zj)
      end if

      w=width*bond_fact*bond_fact
      wi=w
      wj=w
      if (perspective) then
        wi=persp(wi,zi)*persp(1.0,zi)
        wj=persp(wj,zj)*persp(1.0,zj)
      end if

      rnorm = 1.0/((xii-xjj)**2+(yii-yjj)**2+(zii-zjj)**2)
c --- this dependence of bond width on ri and rj pulls out the bond
c     end from the center of an atom (necessary when atom radius > 0)
      ridis = sqrt(max(0.0, (rii*rii-wi*wi))*rnorm)
      rjdis = sqrt(max(0.0, (rjj*rjj-wj*wj))*rnorm)
      x1=xii+(xjj-xii)*ridis
      y1=yii+(yjj-yii)*ridis
      z1=zii+(zjj-zii)*ridis
      x2=xjj+(xii-xjj)*rjdis
      y2=yjj+(yii-yjj)*rjdis
      z2=zjj+(zii-zjj)*rjdis

      return
      end



      real function persp(var,zpoint)
      implicit none
      real var, zpoint
#include "top.cmn"
      
      persp = (rprm(1,49) * var) / 
     -        (rprm(1,49)+rprm(1,50) + (rprm(6,32)-zpoint))

      return
      end



c --- Rotation is performed about cartesian coordinate axes,
c     ANGXD, ANGYD and ANGZD are assumed to be in degrees.
c
      subroutine rot_3d(cart,number,angxd,angyd,angzd)
      implicit none
      real rad_to_deg
      parameter (rad_to_deg = 57.295779515)
      
      integer number,ia
      real cart(3,number),angxd,angyd,angzd,c1,s1,c2,s2,c3,s3
      real coxold,coyold,cozold
      real rot11,rot12,rot13,rot21,rot22,rot23,rot31,rot32,rot33
c
c     Set up the transformation matrix :
c
      if(angxd .eq. 0.0e0) then
         c1 = 1.0e0
         s1 = 0.0e0
      else
         c1=cos(angxd/rad_to_deg)
         s1=sin(angxd/rad_to_deg)
      end if
      if(angyd .eq. 0.0e0) then
         c2= 1.0e0
         s2= 0.0e0
      else
         c2=cos(angyd/rad_to_deg)
         s2=sin(angyd/rad_to_deg)
      end if
      if(angzd .eq. 0.0e0) then
         c3= 1.0e0
         s3= 0.0e0
      else
         c3=cos(angzd/rad_to_deg)
         s3=sin(angzd/rad_to_deg)
      end if
c
      rot11=c2*c3
      rot21=c2*s3
      rot32=s1*c2
      rot33=c1*c2

      rot12=-s1*s2*c3-c1*s3
      rot13=s1*s3-c1*s2*c3
      rot22=c1*c3-s1*s2*s3
      rot23=-c1*s2*s3-s1*c3

      rot31=s2
c
c     Do the transformation :
c
      do ia=1,number
        coxold=cart(1,ia)
        coyold=cart(2,ia)
        cozold=cart(3,ia)
        cart(1,ia)=rot11*coxold+rot12*coyold+rot13*cozold
        cart(2,ia)=rot21*coxold+rot22*coyold+rot23*cozold
        cart(3,ia)=rot31*coxold+rot32*coyold+rot33*cozold
      end do

      return
      end

 

      subroutine getcov(x,y,z,numat,isel,numsel,ibond,
     -           numbond,maxbond,iattyp,vdwcnt,nvdwtyp,coval)
      implicit none
      real vdwred
c --- there is a covalent bond if: distance < (sum of vdw radii)*vdwred
      parameter (vdwred=0.55)
      integer maxbond,numat,numbond,numsel,isel(numsel),i,j,ia,ja
      integer ibond(2,maxbond),iattyp(numat),nvdwtyp
      real x(numat),y(numat),z(numat),dist,d,coval(2)
      real vdwcnt(nvdwtyp),bndlen1,bndlen2
      logical undefd

      do  i = 1, numsel-1
        ia = isel(i)
        do  j = i+1, numsel
          ja = isel(j)
          d = dist(x(ia),y(ia),z(ia),x(ja),y(ja),z(ja))
          if (undefd(coval(1)) .or. undefd(coval(2))) then
            bndlen2 = vdwred*(vdwcnt(iattyp(ia))+vdwcnt(iattyp(ja)))
            bndlen1 = 0.5*bndlen2
c            write(*,'(4i4,2f8.3)') ia,ja,iattyp(ia),iattyp(ja),
c     &                 vdwcnt(iattyp(ia)),vdwcnt(iattyp(ja))
          else
            bndlen1 = coval(1)
            bndlen2 = coval(2)
          end if  
          if ((d .ge. bndlen1) .and. (d .le. bndlen2)) then
            numbond = numbond + 1
            if (z(ia) .lt. z(ja)) then
              ibond(1,numbond) = ia
              ibond(2,numbond) = ja
            else
              ibond(2,numbond) = ia
              ibond(1,numbond) = ja
            end if
          end if
        end do
      end do

      return
      end



      subroutine calc_conn(x,y,z,numat,isel,numsel,ibond,numbond,
     -           maxbond,addbnd,bndtyp,iattyp,vdwcnt,nvdwtyp,
     -           coval)
      implicit none
      integer maxbond,numat,numbond,numsel,isel(numsel)
      integer ibond(2,maxbond), indexw,iattyp(numat),nvdwtyp
      real x(numat),y(numat),z(numat),vdwcnt(nvdwtyp)
      real coval(2)
      character bndtyp*(*)
      logical addbnd

      if (.not. addbnd) numbond = 0

      if (indexw(bndtyp, 'COVALENT') .gt. 0) then
        call getcov(x,y,z,numat,isel,numsel,ibond,numbond,maxbond,
     -              iattyp,vdwcnt,nvdwtyp,coval)
        write(*,'(a,2i6)') 
     -  'calc_conn> Selctd atoms, COVALENT bonds calculated: ',
     -  numsel,numbond
      end if

      if (indexw(bndtyp, 'SEQUENTIAL') .gt. 0) then
        call getseq(x,y,z,numat,isel,numsel,ibond,numbond,maxbond,
     -              iattyp,vdwcnt,nvdwtyp,coval)
        write(*,'(a,2i6)') 
     -  'calc_conn> Selctd atoms, SEQUENTIAL bonds used: ', 
     -  numsel,numbond
      end if

      return
      end


      subroutine getseq(x,y,z,numat,isel,numsel,ibond,numbond,maxbond,
     -                  iattyp,vdwcnt,nvdwtyp,coval)
      implicit none
      real vdwred
c --- there is a covalent bond if: distance < (sum of vdw radii)*vdwred
      parameter (vdwred=0.55)
      integer maxbond,numat,numbond,numsel,isel(numsel),i,j,ia,ja
      integer ibond(2,maxbond),iattyp(numat),nvdwtyp
      real vdwcnt(nvdwtyp)
      real x(numat),y(numat),z(numat),dist,d,coval(2),bndlen1,bndlen2
      logical undefd

      do  i = 1, numsel-1
        ia = isel(i)
        j = i + 1
        ja = isel(j)
        d = dist(x(ia),y(ia),z(ia),x(ja),y(ja),z(ja))
        if (undefd(coval(1)) .or. undefd(coval(2))) then
          bndlen2 = vdwred*(vdwcnt(iattyp(ia))+vdwcnt(iattyp(ja)))
          bndlen1 = 0.5*bndlen2
        else
          bndlen1 = coval(1)
          bndlen2 = coval(2)
        end if  
        if ((d .ge. bndlen1) .and. (d .le. bndlen2)) then
          numbond = numbond + 1
          if (z(ia) .lt. z(ja)) then
            ibond(1,numbond) = ia
            ibond(2,numbond) = ja
          else
            ibond(2,numbond) = ia
            ibond(1,numbond) = ja
          end if
        end if
      end do

      return
      end


      subroutine getseq2(x,y,z,numat,isel,numsel,ibond,
     -           numbond,maxbond)
      implicit none
      integer maxdiff
      real dbond
c --- maximal length of the virtual bond, maximal difference in
c     atom indices for a virtual bond
      parameter (dbond = 4.5, maxdiff=50)
      integer maxbond,numat,numbond,numsel,isel(numsel),i,j,ia,ja
      integer ibond(2,maxbond)
      real x(numat),y(numat),z(numat),dist,d

      do  i = 1, numsel-1
        ia = isel(i)
        do  j = i+1, numsel
         ja = isel(j)
          d = dist(x(ia),y(ia),z(ia),x(ja),y(ja),z(ja))
          if ((d .le. dbond) .and. (abs(ja-ia) .lt. maxdiff)) then
            numbond = numbond + 1
            if (z(ia) .lt. z(ja)) then
              ibond(1,numbond) = ia
              ibond(2,numbond) = ja
            else
              ibond(2,numbond) = ia
              ibond(1,numbond) = ja
            end if
          end if
        end do
      end do

      return
      end

 
      real function dist(x1,y1,z1,x2,y2,z2)
      implicit none
      real x1,y1,z1,x2,y2,z2,dx,dy,dz
      dx = x1 - x2 
      dy = y1 - y2 
      dz = z1 - z2 
      dist = sqrt(dx*dx + dy*dy + dz*dz)
      return
      end



c     This is an auxiliary subroutine for sorting.  Given a real
c     array CO of (3 X N) elements, it will create two complementary 
c     pointer lists (ISEQ and IRANK) that will aid in either sorting or 
c     ranking of the input array.  
c            
c          The ISEQ array contains pointers to the positions of the 
c     elements in CO in order of increasing magnitude, i. e. CO(3,SEQ(1))
c     is the smallest element of CO, X(3,SEQ(2)) is the second, etc.
c        
c          The RANK array contains the relative ranking of its 
c     corresponding element of CO, again in order of increasing 
c     magnitude, i. e. CO(3,1) is the RANK(1)'th smallest element of CO,
c     CO(3,i) is the smallest element of CO if RANK(i)=1, etc.
c     
c          Neither RANK nor SEQ need be initialized before calling this
c     subroutine, since they will be overwritten; CO and N will be 
c     returned unchanged.
c     
c          The lists are generated via a modified double-ended search/sort
c     algorithm, in which the logical array USED keeps track of the elements
c     already sequenced/ranked.
c     

      subroutine rank_seq(z,numat,isel,numsel,iseq,irank)
      implicit none
      integer numat,numsel,isel(numsel),iseq(numsel),irank(numsel)
      integer j,ia,ifor,iback,imin,imax,itemp
      real z(numat)
      logical foundmin,foundmax
c
c     Initializing the arrays :
c
      do j=1,numsel
        iseq(j)  = 0
        irank(j) = 0
      end do
c
c     Initializing pointers :
c
c          IFOR  : current forward position in ISEQ
c          IBACK : current backward position in ISEQ
c          IMIN  : position of current minimum in X
c          IMAX  : position of current maximum in X
c
      ifor  = 1
      iback = numsel
      imin  = ifor
      imax  = iback
c
c     since this is a double-ended search, loop until the backward counter
c     is smaller than the forward.  At this point the fills in both
c     directions have already met in the middle.
c
      do while (iback.ge.ifor)
c
c       search for current minimum and maximum :
c
        foundmin=.false.
        foundmax=.false.
        do j=1,numsel
           ia = isel(j)
           if (irank(j).eq.0) then
              if((j.ne.imin) .and. (z(ia).le.z(isel(imin))))then
                 imin = j
                 foundmin=.true.
              end if
              if((j.ne.imax) .and. (z(ia).gt.z(isel(imax))))then
                 imax = j
                 foundmax=.true.
              end if
           end if
        end do
        if (.not.foundmin) foundmin = irank(imin).eq.0
        if (.not.foundmax) foundmax = irank(imax).eq.0
           
        if ((.not.foundmin) .or. (.not.foundmax)) then
           do j=1,numsel
              if (irank(j).eq.0) then
                 if (.not.foundmin) then
                    foundmin=.true.
                    imin=j
                 else if (.not.foundmax) then
                    foundmax=.true.
                    imax=j
                 end if
              end if
           end do
        end if

c       fill the ISEQ array with the positions of current minimum and
c       maximum :
        iseq(ifor)=imin
        iseq(iback)=imax
c
c       set the ranks of these elements :
c
        irank(imin)=ifor
        irank(imax)=iback
c
c       increment the pointers :
c
        ifor=ifor+1
        iback=iback-1
c
c       swap minimum and maximum to insure finding correct values in
c       the next iteration :
c
        itemp=imin
        imin=imax
        imax=itemp
      end do

      return
      end



c --- This subroutine generates a list of ALL atoms bonded to each 
c     other and stores it in the NBONDS array.  It utilizes a pointer 
c     array, LASTBOND to keep track of the origin atom. i. e. :
c
c        - bonds NBONDS(1,2,...,LASTBOND(1)) are bonded to atom 1,
c
c        - bonds NBONDS(LASTBOND(I-1)+1,...,LASTBOND(I)) are
c                bonded to atom I.
c
c          NUMPOINT is the total number of pointers generated.
c
c     This subroutine requires the previous generation of the IBOND
c     array.  This information is generated in the CALC_CONN routine.
c
      subroutine bond_list(ibond,numbond,lastbond,numpoint,
     -                     numat,nbonds,maxpoint,isel,numsel)
      implicit none
      integer numat,maxpoint,numpoint,numbond,numsel,isel(numsel)
      integer ibond(2,numbond),lastbond(maxpoint),nbonds(maxpoint)
      integer i,is,j

      do i=1,maxpoint
        nbonds(i)=0
      end do
c
      do i=1,numsel
        lastbond(i)=0
      end do
      numpoint=0
c
      do is=1,numsel
        i = isel(is)
        do j=1,numbond
          if ((ibond(1,j).eq.i) .or. (ibond(2,j).eq.i)) then
            numpoint=numpoint+1
            nbonds(numpoint)=j
          end if
        end do
        lastbond(is)=numpoint
      end do                     

      return
      end
