c ----- There are 5 coordinate systems:
c
c
c       1. Base PostScript coordinate system in 1/72'' units.
c          (bbx0,bby0,bbx1,bby1)
c
c       2. Auxillary PostScript coordinate system in cm's that overlaps 
c          with Base PostScript coordinate system.
c          (x0cm,y0cm,x1cm,y1cm)
c
c       3. Paper coordinate system expressed in the Auxiallary coordinate 
c          system (used to position the Plot on the paper). The coordinate
c          axes box corresponds to this area.
c          (x0pap,y0pap,x1pap,y1pap)
c          (x0ps,y0ps,x1ps,y1ps)
c
c       4. World coordinate system of the data plotted (used throughout
c          at the upper level of dealing with the data such as plotting
c          points, lines, ticks, ...). Overlaps with the Paper coordinate
c          system.
c          (x0wrl,y0wrl,x1wrl,y1wrl)
c
c       5. Plot coordinate system used for actual plotting and appearance
c          management - it should be {{0,0},{1,y}} where y is determined
c          from the ratio of y and x spans in the Paper coordinate system.
c          This enables easy coding for all appearence constants such as
c          line spaces, margins widths, etc; overlaps with the Paper 
c          coordinate system.
c          (x0plt,y0plt,x1plt,y1plt)
c
c
c --- Depending on the value of INP, there are three ways of calculating 
c     the World' extents (in all of them, if non-default World coordinates 
c     are given in the input, no calculation is done):
c       a) INP=1 ... x,y vectors of the plotting table are used for x and 
c                    y axes, respectively;
c       b) INP=2 ... nxdim and nydim dimensions of the plotting array are used; 
c       c) INP=3 ... PDB data are used.
c
c
c --- Input:
c       inp,nxdim,nydim,tab
c       x0wrl,y0wrl,x1wrl,y1wrl  ... World window corresponding to the plot
c       x0pap,y0pap,x1pap,y1pap  ... Paper window corresponding to the plot
c       x0plt,y0plt,x1plt,y1ltp  ... Plot window corresponding to the plot
c       angle       ... rotation of the World & Plot coordinate systems
c                       relative to the Base PS, Auxiallary PS and Paper
c                       coordinate systems.
c       a4bound     ... whether to draw a line around the largest 
c                       BoundingBox possible (ie the whole drawing area on
c                       the A4 sheet)
c       
      subroutine world(ixy,nwxy,iwxy,ndim,npnts,
     -                 x0wrl,y0wrl,x1wrl,y1wrl,
     -                 x0pap,y0pap,x1pap,y1pap,
     -                 angle,a4bound,center)
        implicit none
        real small
        parameter (small = 1.0E-10)
#include "asgl.cmn"
#include "io.cst"
        integer npnts,ndim,ixy(2),nwxy(2),iwxy(ndim)
        real x0pap,y0pap,x1pap,y1pap,x0ps,y0ps,x1ps,y1ps
        real x0wrl,y0wrl,x1wrl,y1wrl,dxwrl,dywrl,cmps
        real angle,rminv,rmaxv,radians,center
        logical a4bound,deflt

c ----- World extent calculation depends on the type of the last input:
        go to (1,2,3) inp
c ------- 2D plot
1         call range1(tab,ixy,nwxy,iwxy,ndim,maxpnt,maxclm,npnts,
     -                  x0wrl,y0wrl,x1wrl,y1wrl,center)
          go to 10
c ------- histogram
2         call range2(nxdim,nydim,x0wrl,y0wrl,x1wrl,y1wrl)
          go to 10
c ------- molecule:
3         if (deflt(x0wrl)) x0wrl = rminv(tab(1,1), npnts)
          if (deflt(y0wrl)) y0wrl = rminv(tab(1,2), npnts)
          if (deflt(x1wrl)) x1wrl = rmaxv(tab(1,1), npnts)
          if (deflt(y1wrl)) y1wrl = rmaxv(tab(1,2), npnts)
10      continue
        write(iolog,'(a,4g12.4)')
     -  'world____> World window: ', x0wrl, y0wrl, x1wrl, y1wrl


c ----- Workout the linear transformation of the World coordinates resulting
c       in the Plot coordinates:
c           X_Plot = X_World*xwpscale + xwptrans
c           Y_Plot = Y_World*ywpscale + ywptrans
c
c ----- dxwrl and dywrl can be 0.0 if no points read in and no World
c       window set explicitly; set them here to 1;

        dxwrl = (x1wrl-x0wrl)
        if (dxwrl .lt. small) then
          dxwrl = 1.0
          x1wrl = x0wrl + dxwrl
        end if
        xwpscale = (x1plt-x0plt) / dxwrl
        dywrl = (y1wrl-y0wrl)
        if (dywrl .lt. small) then
          dywrl = 1.0
          y1wrl = y0wrl + dywrl
        end if
        ywpscale = (y1plt-y0plt) / dywrl

c ----- make sure to have the same x/y scaling for molecules
        if (inp .eq. 3) then
          xwpscale = min(xwpscale,ywpscale)
          ywpscale = xwpscale
        end if

        xwptrans = x0plt - x0wrl*xwpscale
        ywptrans = y0plt - y0wrl*ywpscale


c ----- Workout the linear transformation of the Plot coordinates resulting
c       in the Base PS coordinates:
c           X_Base = X_Plot*xpbscale + xpbtrans
c           Y_Base = Y_Plot*ypbscale + ypbtrans
c           and rotate point (X_Base, Y_Base) to get the final Base point.


c ----- change Paper coordinates from cm to 1/72" (the Base PS units)
        x0ps = cmps(x0pap)
        y0ps = cmps(y0pap)
        x1ps = cmps(x1pap)
        y1ps = cmps(y1pap)

c ----- workout the scales in x and y directions
        xpbscale = (x1ps-x0ps) / (x1plt-x0plt)
        ypbscale = (y1ps-y0ps) / (y1plt-y0plt)
c ----- workout the translation in the original paper system
        xpbtrans = x0ps - x0plt*xpbscale
        ypbtrans = y0ps - y0plt*ypbscale
c ----- rotation matrix elements:
        pbrot11 =  cos(radians(angle))
        pbrot12 = -sin(radians(angle))
        pbrot21 =  sin(radians(angle))
        pbrot22 =  cos(radians(angle))
        pbangle =  angle

c ----- optionally draw the A4 box:
        if (a4bound) then
          if (epsf) then
c --------- extend the BoundingBox to include all A4 area:
            bbox0 = bbx0
            bboy0 = bby0
            bbox1 = bbx1
            bboy1 = bby1
          end if
          write(iops, '(a,f5.2,a/a,4(2f7.2,a)/a)')
     -    '0.0 setgray ', 2*w2lin, ' setlinewidth',
     -    'NP ', 
     -    bbx0+w2lin, bby0+w2lin, ' M2 ', 
     -    bbx1-w2lin, bby0+w2lin, ' L2 ',
     -    bbx1-w2lin, bby1-w2lin, ' L2 ',
     -    bbx0+w2lin, bby1-w2lin, ' L2 ',
     -    'closepath stroke'
        else
         if (epsf) then
          call updbox(x0plt-0.03*(x1plt-x0plt),y0plt-0.03*(y1plt-y0plt))
          call updbox(x1plt+0.03*(x1plt-x0plt),y1plt+0.03*(y1plt-y0plt)) 
         end if
        end if

        return
      end



c --- update the BoundingBox given the (x,y) point in the Plot cs
      subroutine updbox(x, y)
        implicit none
#include "asgl.cmn"
        real x, y, x2, y2

c ----- convert from the Plot cs to Base PS cs
        call plot2base(x, y, x2, y2)

c ----- update the BoundingBox
        bbox0 = min(bbox0, x2)
        bboy0 = min(bboy0, y2)
        bbox1 = max(bbox1, x2)
        bboy1 = max(bboy1, y2)

c        write(iolog,'(a,8f8.2)')'UPDBOX: ',x,y,x2,y2,bbox0,bboy0,bbox1,bboy1

        return
      end


   

c --- This routine should be called before each of the TOP drawing commands
c     is executed. It prepares the transformation properly, given current
c     WORLD definitions. It assumes that the PS graphics engine is in the
c     default state (0 0 translate 0 0 scale).
      subroutine topgset(comment)
        implicit none
#include "asgl.cmn"
        integer lenr
        character comment*(*)

        write(iops, '(2a)' ) '% set: ', comment(1:lenr(comment))

c ----- save for a restore by topgrestore()
        call gsave

        call translate(xpbtrans, ypbtrans)
        call scale(xpbscale, ypbscale)
        call rotate(pbangle)

        call undefps

        return
      end



c --- This routine should be called after each of the TOP drawing commands
c     is executed. It restores the default transformation properly, given 
c     current WORLD definitions (the default state is 0 0 translate 0 0 
c     scale).
      subroutine topgrestore(comment)
        implicit none
#include "psgl.cmn"
        integer lenr
        character comment*(*)

        write(iops, '(2a)' ) '% restore: ', comment(1:lenr(comment))
        call grestore

        return
      end


c --- from a World distance to Plot distance
      real function wpdx(x)
        implicit none
#include "asgl.cmn"
        real x
        wpdx = x*xwpscale
        return
      end

 
      real function wpdy(y)
        implicit none
#include "asgl.cmn"
        real y
        wpdy = y*ywpscale
        return
      end


c --- from World coordinates to Plot coordinates:
c --- use this simple equation because you know that no rotation involved
      real function wpx(x)
        implicit none
#include "asgl.cmn"
        real x
        wpx = x*xwpscale + xwptrans
        return
      end


c --- from World coordinates to Plot coordinates:
c --- use this simple equation because you know that no rotation involved
      real function wpy(y)
        implicit none
#include "asgl.cmn"
        real y
        wpy = y*ywpscale + ywptrans
        return
      end


      subroutine plot2base(xp,yp,xb,yb)
        implicit none
#include "asgl.cmn"
        real x, y, xp, yp, xb, yb
 
        x = pbrot11*xp + pbrot12*yp
        y = pbrot21*xp + pbrot22*yp
 
        xb = x*xpbscale + xpbtrans
        yb = y*ypbscale + ypbtrans

        return
      end
