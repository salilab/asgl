c --- There are two types of routines in this file:
c 
c     1)    Application level: These routines can be used directly from 
c           an application, provided the PS system was initialized properly 
c           with INITPS and PSHEAD routines, in this order. The application
c           level routines use the application's coordinate system.
c 
c           Some of these routines are very PostScript specific are 
c           usually not called from the application.
c
c     2)    PostScript level: These routines are called by application
c           level routines. They include routines that draw objects,
c           but depend on the pre-processed PS coordinates. They are 
c           generally named ps_ROUTINE, where ROUTINE is the name of its
c           application level equivalent that transforms the application 
c           coordinates into PS coordinates.
c
c
c ------------------------------------------------------------------------
c --- APPLICATION level routines:
c ------------------------------------------------------------------------

c --- This routine will write the head of the PS file:
      subroutine headps(psf, datetime)
#include "psgl.cmn"
#include "asgl.cst"
        integer lenr
        character psf*(*), datetime*(*)

c ----- PostScript or Encapsulated PostScript can be produced. Note,
c       however, that these two standards are followed only to a degree
c       that allows the use of the following NeXT programs: Preview,
c       texview, Draw and NeXT PS printer. I do not know what happens
c       otherwise.

c       Remark: PS Reference manual forbids or at least advises not to use 
c               initmatrix in Encapsulated PS, but it works just fine 
c               with NeXT programs and printer?! Also, I do not use %%Pages:.

        call openf(iops, psf, 'unknown')

        if (epsf) then
          write(iops,'(a)') '%!PS-Adobe-3.0 EPSF-3.0'
        else
          write(iops,'(a)') '%!PS-Adobe-3.0'
        end if

        if (epsf) then
          write(iops,'(a)') '%%BoundingBox: (atend)'
        else
          write(iops,'(a)') '%%BoundingBox: 0 0 612 792'
        end if

        write(iops,7) version, author, version_date, 
     -                psf, datetime(1:lenr(datetime))
7       format( '%%LanguageLevel: 2' /
     -          '%%Creator: ', a, ', by ', a, ', ', a /
     -          '%%Title: ', a /
     -          '%%CreationDate: ', a /
     -          '%%EndComments')

c        call save 

        if (.not. epsf) write(iops,'(a)') '%%BeginProlog'

c ----- definitions of the sizes:
        call wrdefs(sizstr, nsize)

c ----- typefont definitions
        call wrdefs(ftdef, nfont)

c ----- line type definitions
        call wrdefs(ltdef, nlintp)

c ----- symbol definitions
        call wrdefs(sbdef, nsybtp)

c ----- general definitions
        call wrdefs(defs, ndefs)

        if (.not. epsf) write(iops,'(a)') '%%EndProlog'

        if (.not. epsf) write(iops,'(a)') '%%BeginSetup'
c ----- Styles for joining lines and ending lines:
        write(iops,10) lnjond, lncapd
10      format(i1, ' setlinejoin' /
     -         i1, ' setlinecap')
        if (.not. epsf) write(iops,'(a)') '%%EndSetup'

        ipage = 1
        if (.not. epsf) call pspage

        return
      end


c --- This procedure will read line type and font definitions from the
c     psgl.ini file:
      subroutine initps
#include "io.cst"
#include "psgl.cmn"
        character line*(lencod+lencmd), code*(lencod), contents*(lencmd)
        logical new

        call undefps

        nfont  = 0
        nlintp = 0
        nsybtp = 0
c ----- number of font and linetype sizes:
        nsize = 0
c ----- number of definition lines:
        ndefs = 0

        call openf(ioinp, inifl, 'old')
        new = .true.
10      read(ioinp, 20, end=100) line
20        format(a)
c ------- Ignore comment lines:
          if (line(1:1) .ne. '#') then
            code     = line(1:lencod)
            contents = line(lencod+1:)

            if(code .eq. 'FONT      =') then
              if (new) nfont = nfont + 1
              call prslin(contents, ftdef(nfont), ftcmnd(nfont), new)
              new = contents(lenr(contents):) .ne. acontin
c ----------- read the associated sub/superscript and greek fonts at the
c             end of the font specification:
              if(new)read(ioinp,*)ifsub(nfont),ifsup(nfont),ifgrk(nfont)
            end if

            if(code .eq. 'SIZE      =') then
              nsize = nsize + 1
              call ljust(contents)
              sizstr(nsize) = contents
            end if

            if(code .eq. 'LINE_TYPE =') then
              if (new) nlintp = nlintp + 1
              call prslin(contents, ltdef(nlintp), ltcmnd(nlintp), new)
              new = contents(lenr(contents):) .ne. acontin
            end if

            if(code .eq. 'SYMBOL    =') then
              if (new) nsybtp = nsybtp + 1
              call prslin(contents, sbdef(nsybtp), sbcmnd(nsybtp), new)
              new = contents(lenr(contents):) .ne. acontin
            end if

            if(code .eq. 'DEFINITION=') then
              ndefs = ndefs + 1
              defs(ndefs) = contents
            end if

          end if
          go to 10
100     continue
        close(ioinp)

c ----- process the sizes for the linetypes and fonts:
        call getsiz(sizstr, nsize, 'setlinewidth',ltdef,linwdt,nlintp)
        call getsiz(sizstr, nsize, 'scalefont', ftdef, fnthgt, nfont)

        return
      end


      subroutine setfont(ifont)
#include "psgl.cmn"
c ----- change the fonttype only if different from the current one and
c       the new one is defined:
        if((ifontc.ne.ifont).and.(ifont.ge.1).and.(ifont.le.nfont))then
          ifontc = ifont
          write(iops, 10) ftcmnd(ifont)(1:lenr(ftcmnd(ifont)))
10        format(a)
        end if
        return
      end

      subroutine setpen(lintyp)
#include "psgl.cmn"
c ----- change the linetype only if different from the current one 
c       and the new one is defined:
c        if((lintyc.ne.lintyp).and.(lintyp.ge.1).and.
        if((lintyp.ge.1).and.
     -     (lintyp.le.nlintp))then
          lintyc = lintyp
          write(iops,'(a)') ltcmnd(lintyp)(1:lenr(ltcmnd(lintyp)))
        end if
        return
      end


c --- strikes the current PS path with the current linetype
      subroutine stroke
#include "psgl.cst"
        write(iops, '(a)') 'ST'
c        write(iops, '(a)') 'stroke'
        return
      end


c c --- Note: this wont overdraw an area if the colour is set to 1.0 
c c           (i.e. white), which a straighforward fill command would!
      subroutine fill(gray)
#include "psgl.cst"
        character s1*(lennum)
c         if (gray .lt. 0.9999) then
c          call gsave
          call condns(gray,s1,l1)
          write(iops, '(a,1x,a)') s1(:l1),'setgray fill'
c          call grestore
c         end if
        return
      end 


c --- rotate PS coordinate system
      subroutine rotate(angle)
#include "psgl.cst"
        character s1*(lennum)
        call condns(angle,s1,l1)
        write(iops, '(a,1x,a)') s1(:l1),'rotate'
        return
      end


c --- save PS graphics state
      subroutine gsave
#include "psgl.cst"
        write(iops, '(a)') 'gsave'
        return
      end

c --- restore PS graphics state
      subroutine grestore
#include "psgl.cst"
        write(iops, '(a)') 'grestore'
        return
      end

      subroutine newpath
#include "psgl.cst"
        write(iops, '(''NP'')')
c 10      format('newpath')
        return
      end

      subroutine initmatrix
#include "psgl.cst"
c        stop 'Warning[initmatrix]: avoid initmatrix'
        write(iops, '(a)') 'initmatrix'
        return
      end

c --- writes a symbol
      subroutine symbol(x,y,isybtp,itfont,i)
        real x,y,wpx,wpy
        integer isybtp,i
        call ps_symbol(wpx(x),wpy(y),-1.,isybtp,itfont,i)
        return
      end

      subroutine symbolc(x,y,c,isybtp,itfont,i)
        integer isybtp,i
        real x,y,wpx,wpy,c
        call ps_symbol(wpx(x),wpy(y),c,isybtp,itfont,i)
        return
      end

c --- writes N symbols:
      subroutine psymbol(x,y,n,isybtp,itfont)
        real x(n), y(n)
        integer isybtp
        do  i = 1, n
          call symbol(x(i),y(i),isybtp,itfont,i)
        end do
        return
      end

c --- writes N symbols in color:
      subroutine psymbolc(x,y,c,n,isybtp,itfont)
        real x(n), y(n), c(n)
        integer isybtp
        do  i = 1, n
          call symbolc(x(i),y(i),c(i),isybtp,itfont,i)
        end do
        return
      end


c --- back to the device's default (full page):
      subroutine initclip
#include "psgl.cst"
c        stop 'Warning[initclip]: avoid initclip'
        write(iops, '(a)') 'initclip'
c        return
      end


c --- form feed: 
      subroutine showpage(ncopies)
#include "psgl.cmn"

        if (epsf) then
          write(iolog,'(a)') 'showpag_E> not allowed in encapsulated PS'
          stop
        end if

        call undefps

        write(iops, '(a,1x,i2,1x,a)') '/#copies', ncopies, 
     -              'def gsave showpage grestore'

        ipage = ipage + 1

        return
      end

c --- scale any coordinate system
      subroutine scale(xscale,yscale)
#include "psgl.cst"
        character s1*(lennum), s2*(lennum)
        call condns(xscale,s1,l1)
        call condns(yscale,s2,l2)
        write(iops, '(3(a,1x))') s1(:l1), s2(:l2), 'scale'
        return
      end


c --- draw a circle
      subroutine circle(x,y,rad,ltype,color)
        implicit none
        integer ltype
        real x,y,rad,color,wpx,wpy,wpdx
        call ps_circle(wpx(x),wpy(y),wpdx(rad),ltype,color)
        return
      end


c --- draw a poly-line of type lintyp:
      subroutine pline(x,y,n,lintyp)
        implicit none
#include "psgl.cst"
        integer lintyp,n,i,i1,i2,npl,ni,j
        real x(n),y(n),xps(maxpoly),yps(maxpoly),wpx,wpy

        npl = n / maxpoly
        if (mod(n, maxpoly) .ne. 0) npl = npl + 1

        do  i = 1, npl
          i1 = max(1, (i-1)*maxpoly)
          i2 = min(i*maxpoly, n)

          ni = 0
          do  j = i1, i2
            ni = ni + 1
            xps(ni) = wpx(x(j))
            yps(ni) = wpy(y(j))
          end do
          call ps_pline(xps,yps,ni,lintyp)

        end do

        return
      end

c --- draw a polygone
      subroutine pgone(x,y,n,lintyp,color)
        implicit none
#include "psgl.cst"
        integer lintyp,i,n,i1,i2,npl,ni,j
        real x(n),y(n),xps(maxpoly),yps(maxpoly),color,wpx,wpy

        npl = n / maxpoly
        if (mod(n, maxpoly) .ne. 0) npl = npl + 1

        do  i = 1, npl
          i1 = max(1, (i-1)*maxpoly)
          i2 = min(i*maxpoly, n)

          ni = 0
          do  j = i1, i2
            ni = ni + 1
            xps(ni) = wpx(x(j))
            yps(ni) = wpy(y(j))
          end do
          call ps_pgone(xps,yps,ni,lintyp,color)

        end do

        return
      end


c --- writes the text in the Application coordinate system using font ifont 
c     in horizontal justification ihstyl (1 left justified, 2 centered, 
c     3 right justified) and vertical justification ivstyl (1 bottom
c     aligned, 2 centered, 3 top aligned):
      subroutine text(txt,x,y,ifont,angle,ihstyl,ivstyl,wrld)
        implicit none
#include "psgl.cmn"
        integer ihstyl,ivstyl,ifont
        real x, y, angle, wpx, wpy
        character txt*(*)
        logical wrld

c ----- get the point in the Plot coordinate system
        if (wrld) then
          call ps_text(txt,wpx(x),wpy(y),ifont,angle,ihstyl,ivstyl)
        else
          call ps_text(txt,x,y,ifont,angle,ihstyl,ivstyl)
        end if

        return
      end

c ------------------------------------------------------------------------
c --- PostScript level routines and their support routines:
c ------------------------------------------------------------------------


      subroutine box(x1, y1, x2, y2, lintyp, gray)
#include "psgl.cmn"

c ----- is there anything to do:
        if (((lintyp .ge. 1).and.(lintyp.le.nlintp)).or.
     -      (gray .lt. 0.9999)) then

c ----- draw a new and closed path:
        call nmoveto(x1,y1)
        call lineto(x1,y2)
        call lineto(x2,y2)
        call lineto(x2,y1)
        call lineto(x1,y1)

c ----- fill the area if so selected:
        if (gray .lt. 0.9999) call fill(gray)

c ----- strike a line if so selected:
        if((lintyp.ge.1).and.(lintyp.le.nlintp))then
          call nmoveto(x1,y1)
          call lineto(x1,y2)
          call lineto(x2,y2)
          call lineto(x2,y1)
          call lineto(x1,y1)
          call setpen(lintyp)
          call stroke
        end if
      
        end if

        return
      end


c --- a poly-line of type lintyp:
      subroutine ps_pline(x,y,n,lintyp)
#include "psgl.cmn"
        integer lintyp
        real x(n),y(n)
        logical first

        if((lintyp.ge.1).and.(lintyp.le.nlintp))then
   
          first = .true. 
          do  i = 2, n

c --------- the first point in the section?
            if (first) then
              call nmoveto(x(i-1), y(i-1))
              first = .false.
            end if

            call lineto(x(i), y(i))
        
c --------- some printers do not like very long poly-lines -- divide them
c           into shorter ones
            if (mod(i,1000) .eq. 0) then 
              call setpen(lintyp)
              call stroke
              first = .true.
            end if

          end do

          if (mod(n,1000) .ne. 0) then
            call setpen(lintyp)
            call stroke
          end if

        end if

        return
      end


c --- a symbol of type isymb at (x,y)
      subroutine ps_symbol(x,y,c,isymb,itfont,i)
c ----- style for the integer point indices:
        parameter (angle=0.0, ihs=2, ivs=2)
c ----- because of maxpnt
#include "asgl.cst"
#include "psgl.cmn"
        integer itfont,i,l1
        real x,y,c
        character txt*10,s1*(lennum)

c ----- plot the symbol only if it is defined:
c
        if (isymb .eq. -1) then
c ------- plot the index of a point in the line (for psymbol calls of symbol 
c         only):
c
c ------- this error occurs if symbol not called from psymbol and
c         i in indpnt common was not initialized:
          if((i.lt. 1).or.(i.gt.maxpnt))
     -       stop 'Error[symbol] i not initialized.'
c
          write(txt,'(i10)') i
          call ljust(txt)
          call ps_text(txt,x,y,itfont,angle,ihs,ivs)
        else
          if((isymb.ge.1).and.(isymb.le.nsybtp))then
c --------- plot one of the predefined symbols:
            call newpath
            call gsave
c --------- do it with translate, not moveto, to enable easy specification
c           of objects like circles in the symbol definitions:
            call translate(x,y)
            if (c.ge.0. .and. c.le.1.) then
             call condns(c,s1,l1)
             write(iops,'(a,1x,a,1x,a)') '/H', s1(:l1), 
     &         'def /S 1.0 def /B 0.9 def'
            else
              write(iops,'(a)') '/H 0 def /S 0 def /B 0 def'
            end if
            write(iops,'(a)') 'H S B sethsbcolor'
            write(iops, '(a)') sbcmnd(isymb)(1:lenr(sbcmnd(isymb)))
            call grestore
            isybtc = isymb
          end if
        end if

        return
      end



c --- a polygone:
      subroutine ps_pgone(x,y,n,lintyp,color)
        implicit none
#include "psgl.cmn"
        integer lintyp,n,i
        real x(n),y(n),color

c ----- NOTE: some printers do not like very long poly-lines -- can not
c       divide them into shorter ones -- BAD LUCK -- NO FUN HERE

c ----- trace the polygon:
        if((lintyp.ge.1).and.(lintyp.le.nlintp))then
   
          call nmoveto(x(1), y(1))
          do  i = 2, n
            call lineto(x(i), y(i))
          end do
          call gsave
          call fill(color)
          call grestore
          call setpen(lintyp)
          call stroke

        end if

        return
      end

c --- writes N symbols:
      subroutine ps_psymbol(x,y,n,isybtp,itfont)
        real x(n), y(n)
        integer isybtp
        do  i = 1, n
          call ps_symbol(x(i),y(i),-1.,isybtp,itfont,i)
        end do
        return
      end

c --- clips an area on the page
      subroutine clip(x1,y1,x2,y2)
#include "psgl.cst"
c ----- just establish the clipping path: do not draw anything
        call nmoveto(x1,y1)
        call lineto(x1,y2)
        call lineto(x2,y2)
        call lineto(x2,y1)
        call lineto(x1,y1)
        write(iops, '(a)')  'clip'
        return
      end


      subroutine ps_circle(x,y,rad,ltype,color)
        implicit none
#include "psgl.cst"
        integer ltype,l1,l2,l3
        real x,y,rad,color
        character s1*(lennum), s2*(lennum), s3*(lennum)
        call condns(x,s1,l1)
        call condns(y,s2,l2)
        call condns(rad,s3,l3)
        call newpath
        write(iops, '(4(a,1x))') s1(:l1), s2(:l2), s3(:l3), ' 0 360 arc'
        call gsave
        call fill(color)
        call grestore
        call setpen(ltype)
        call stroke
        return
      end


c --- translate PostScript coordinate system
      subroutine translate(xtrans,ytrans)
        implicit none
#include "psgl.cst"
        integer l1, l2
        real xtrans, ytrans
        character s1*(lennum), s2*(lennum)
        call condns(xtrans,s1,l1)
        call condns(ytrans,s2,l2)
        write(iops, '(3(a,1x))') s1(:l1), s2(:l2), 'translate'
        return
      end



c --- a line of type lintyp, from (x1,y1) to (x2,y2); 
      subroutine line(x1,y1,x2,y2,lintyp)
#include "psgl.cmn"
        integer lintyp
        real x1,y1,x2,y2

c ----- write it out only if the linetype is defined:
        if((lintyp.ge.1).and.(lintyp.le.nlintp))then
          call nmoveto(x1,y1)
          call lineto(x2,y2)
          call setpen(lintyp)
          call stroke
        end if

        return
      end

c --- a line from (x1,y1) to (x2,y2) with width and grayness; 
      subroutine line2(x1,y1,x2,y2,width,gray)
#include "psgl.cst"
        real x1,y1,x2,y2
        character s1*(lennum), s2*(lennum)

        call nmoveto(x1,y1)
        call lineto(x2,y2)
        call condns(width,s1,l1)
        call condns(gray,s2,l2)
        write(iops, 10) s1(:l1), s2(:l2)
10      format('gsave ', a, ' setlinewidth ', a,
     -         ' setgray stroke grestore')

        return
      end
      subroutine box2(x1,y1,x2,y2,x3,y3,x4,y4,lintyp,gray)
#include "psgl.cmn"

c ----- is there anything to do:
        if (((lintyp .ge. 1).and.(lintyp.le.nlintp)).or.
     -      (gray .lt. 0.9999)) then

c ----- draw a new and closed path:
        call nmoveto(x1,y1)
        call lineto(x2,y2)
        call lineto(x3,y3)
        call lineto(x4,y4)
        call lineto(x1,y1)

c ----- fill the area if so selected:
        if (gray .lt. 0.9999) call fill(gray)

c ----- strike a line if so selected:
        if((lintyp.ge.1).and.(lintyp.le.nlintp))then
          call setpen(lintyp)
          call stroke
        end if
      
        end if

        return
      end



      subroutine getsiz(sizstr, nsize, sclcmd, ltcmnd, linwgt, nlintp)
#include "psgl.cst"
        integer ierr
        real linwgt(nlintp), size(maxltp)
        character sizstr(nsize)*(*), ltcmnd(nlintp)*(*), ss*(lencmd)
        character lowcmd*(lencmd), sclcmd*(*), sizsyb(maxltp)*(lencmd)
 
        do 10  is = 1, nsize
c ------- to make it case-insensitive
          lowcmd = sizstr(is)
          call lower(lowcmd)
c ------- extract the number after the /var command (that's the scale):
          i = indexb(lowcmd) 
          call str_r(lowcmd(i+1:), size(is), ierr)
c ------- extract the variable itself (after the '/' character):
          sizsyb(is) = lowcmd(2:i)
10      continue

        do 30  il = 1, nlintp
c ------- to make it case-insensitive
          lowcmd = ltcmnd(il)
          call lower(lowcmd)
c ------- extract the variable before the size command (that's the scale):
          i = index(lowcmd, sclcmd) 
          call rdleft(lowcmd, i, ss)
c ------- find which size symbol this is:
          do 20  is = 1, nsize
            isize = is
            if (ss .eq. sizsyb(is)) go to 25
20        continue
          write(iolog,*)'Error[dosiz]: symbol not found: ', ss
          go to 30
25        linwgt(il) = size(isize)
30      continue

        return
      end


c --- parses the line containing the definition into the definition and
c     the command symbol that is defined:
      subroutine prslin(contents, def, cmnd, new)
#include "psgl.cst"
        character def*(*), cmnd*(*), dummy*(lencmd), contents*(*)
        logical new
        call ljust(contents)
        if (new) then
c ------- full PS definition list for a command:
          def = contents
c ------- the first character on the line is '\'; you need just a command, 
c         which is the first string on the line after '\':
          call str_s(contents, dummy)
          cmnd = dummy(2:) 
        else
          def(lenr(def)+1:) = ' ' // contents(lenl(contents):)
        end if
        return
      end 


c --- This routine will write n definitions in def to the PS file:
      subroutine wrdefs(def, n)
#include "psgl.cst"
        character def(n)*(*), line*(lencmd), ch*1
        do 10  i = 1, n
          ipos = 0
          lr = lenr(def(i))
          do 20  j = 1, lr
            ch = def(i)(j:j)
            if (ch .ne. acontin) then
              ipos = ipos + 1
              line(ipos:) = ch
            else
              write(iops, 5) line(lenl(line):ipos)
5             format(a)
              ipos = 0
            end if
20        continue
          if (ipos .gt. 0) write(iops, 5) line(lenl(line):ipos)
10      continue 
        return
      end


c --- returns real x in string strx written compactly in the 
c     first lenx positions:
      subroutine condns(x,strx,lenx)
#include "psgl.cst"
        character strx*(*), str*(lennum)
c       single precision real(Flennum.ndec):
        write(str,'(f20.4)') x
c        write(str,'(f10.4)') x
c ----- discard all zeroes at the end before the decimal point, except
c       for the one after the decimal point:
        ll = lenl(str)
        do 20  i = lennum, 1, -1
          lr = i
          if (index('123456789.', str(i:i)) .gt. 0) go to 30
20      continue
        write(iolog,*) 'condns__E> no number in str: ', x, ' | ', str
        return
30      continue
c ----- there is always a dot when you write a number in a f14.6 format:
        idot = index(str,'.')
        lr = max(idot+1,lr)
        strx = str(ll:lr)
c ----- do not return insignificant digits, but everything before the dot 
c       and the dot has to be returned (+123456. = 8 places)
        lenx = min(lennum-ndec, lr - ll + 1)
        return
      end

      subroutine moveto(x,y)
#include "psgl.cmn"
        character s1*(lennum), s2*(lennum)
        call condns(x,s1,l1)
        call condns(y,s2,l2)
        write(iops, '(3(a,1x))') s1(:l1), s2(:l2), 'M2'
c        write(iops, '(3(a,1x))') s1(:l1), s2(:l2), 'moveto'
        return
      end

      subroutine rmoveto(x,y)
#include "psgl.cmn"
        character s1*(lennum), s2*(lennum)
        call condns(x,s1,l1)
        call condns(y,s2,l2)
        write(iops, '(3(a,1x))') s1(:l1), s2(:l2), 'RM'
c        write(iops, '(3(a,1x))') s1(:l1), s2(:l2), 'rmoveto'
        return
      end

      subroutine lineto(x,y)
#include "psgl.cmn"
        character s1*(lennum), s2*(lennum)
        call condns(x,s1,l1)
        call condns(y,s2,l2)
        write(iops, '(3(a,1x))') s1(:l1), s2(:l2), 'L2'
c        write(iops, '(3(a,1x))') s1(:l1), s2(:l2), 'lineto'
        return
      end

c --- args: x-tail,y-tail,x-tip,y-tip,thick-tail,thick-arrow,len-arrow
c --- calls a routine arrow defined in PS
      subroutine arrow(xtail,ytail,xtip,ytip,ttail,tarrow,darrow)
#include "psgl.cmn"
        character s1*(lennum), s2*(lennum), s3*(lennum), s4*(lennum)
        character s5*(lennum), s6*(lennum), s7*(lennum)
        call condns(xtail, s1,l1)
        call condns(ytail, s2,l2)
        call condns(xtip,  s3,l3)
        call condns(ytip,  s4,l4)
        call condns(ttail, s5,l5)
        call condns(tarrow,s6,l6)
        call condns(darrow,s7,l7)
        call newpath
        write(iops, 10)
     -    s1(:l1),s2(:l2),s3(:l3),s4(:l4),s5(:l5),s6(:l6),s7(:l7)
10      format(7(a,1x), 'arrow' / 'fill') 
        return
      end

      subroutine unspec(txt,txt2,lr,lr2)
#include "psgl.cst"
        character txt*(*), txt2*(*), ch*1
        lr2 = 0
        do  i = 1, lr
          ch = txt(i:i) 
          if ((ch.ne.subscr).and.(ch.ne.supscr).and.(ch.ne.grkscr)) then
            lr2 = lr2 + 1
            txt2(lr2:lr2) = ch
          end if
        end do
        return
      end

c --- manages the stack of fonts:
      subroutine poppush(nsub,ifntst,istack,mstack,ifnt,sgndy)
        parameter (eps = 1.0E-10)
#include "psgl.cmn"
        integer ifntst(mstack)
        nsub = nsub + 1
        if (mod(nsub,2) .eq. 1) then
c ------- start of a new style:
          ifntst(istack+1) = ifnt
          istack = istack + 1
          dy = sgndy * fnthgt(ifntst(istack))
        else
c ------- end of a new style:
          dy = -sgndy * fnthgt(ifntst(istack))
          istack = istack - 1
        end if
        if (abs(dy) .gt. eps) call rmoveto(0.0,dy)
        call setfont(ifntst(istack)) 
        return
      end


      subroutine nmoveto(x,y)
#include "psgl.cst"
        character s1*(lennum), s2*(lennum)
        call condns(x,s1,l1)
        call condns(y,s2,l2)
        call newpath
        write(iops, '(3(a,1x))') s1(:l1), s2(:l2), 'M2'
c        write(iops, '(3(a,1x))') s1(:l1), s2(:l2), 'moveto'
        return
      end


c --- writes a Pi shaped box (like a bar for histograms):
      subroutine pbox(x1, y1, x2, y2, lintyp, gray)
#include "psgl.cmn"

c ----- is there anything to do:
        if (((lintyp.ge.1).and.(lintyp.le.nlintp)).or.
     -      (gray .lt. 0.9999)) then

        call nmoveto(x1,y1)
        call lineto(x1,y2)
        call lineto(x2,y2)
        call lineto(x2,y1)
        call lineto(x1,y1)

c ----- fill the current path if so selected and restore the graphics state:
        call fill(gray)

c ----- strike a Pi shaped path if so selected:
        if((lintyp.ge.1).and.(lintyp.le.nlintp))then
          call nmoveto(x1,y1)
          call lineto(x1,y2)
          call lineto(x2,y2)
          call lineto(x2,y1)
          call setpen(lintyp)
          call stroke
        end if

        end if

        return
      end


c --- writes the text to the current PS coordinate system using font ifont in
c     horizontal justification ihstyl (1 left justified, 2 centered, 
c     3 right justified) and vertical justification ivstyl (1 bottom
c     aligned, 2 centered, 3 top aligned):
      subroutine ps_text(txt,x,y,ifont,angle,ihstyl,ivstyl)
c ----- maximal number of font changes in a text, max numb of char in text:
        parameter (maxstk = 50, maxlen=255, nspecial=3)
#include "psgl.cmn"
        integer ifntst(maxstk)
        character txt*(*),form(3)*150,ch*1,form2(3)*150,txt2*(maxlen)
        character txt3*(maxlen),special(nspecial)*(1)
        character escspec(nspecial)*(2)
        character s1*(lennum), s2*(lennum), escapc*1
        logical saved

c ----- The PostScript special characters:
c       The ASGL special characters (_,@) must not be PS spec chars!
c ----- '\'
        escapc     = char(92)
        special(1) = char(92)
        special(2) = '('
        special(3) = ')'

        txt3 = txt

c ----- do not allow printing of \ in order to have simple printing of
c       octally coded characters such as Aring (in Angstrom) (\308).
        do  i = 2, nspecial
c        do  i = 1, nspecial
          escspec(i) = escapc // special(i) 
          call subs2(txt3,special(i),escspec(i))
        end do

        form(1)='(2(a,1x), ''M2'' / ''('', a, '') show'')'
        form(2)='(2(a,1x), ''M2'' / ''('', a, '') dup'' /' //
     -    '''stringwidth neg 2 div exch neg 2 div exch rmoveto show'')'
        form(3)='(2(a,1x), ''M2'' / ''('', a, '') dup'' / ' //
     -    '''stringwidth neg exch neg exch rmoveto show'')'

        form2(1)='(2(a,1x), ''M2'')'
        form2(2)='(2(a,1x), ''M2'' / ''('', a, '') '' / ' //
     -    '''stringwidth neg 2 div exch neg 2 div exch rmoveto '')'
        form2(3)='(2(a,1x), ''M2'' / ''('', a, '') '' / ' //
     -    '''stringwidth neg exch neg exch rmoveto '')'

c ----- ignore, if the length of the text is zero:
        lr = lenr(txt)
        lr3 = lenr(txt3)

        if (lr.gt.0) then

          if (epsf) then
            go to (101,102,103) ivstyl
101         yt2 = y + 1.5*fnthgt(ifont)
            yt1 = y - 0.5*fnthgt(ifont)
            go to 110
102         yt2 = y + 1.0*fnthgt(ifont)
            yt1 = y - 1.0*fnthgt(ifont)
            go to 110
103         yt2 = y + 0.5*fnthgt(ifont)
            yt1 = y - 1.5*fnthgt(ifont)
110         continue

c --------- assuming that the average char width is 0.8 of the height
            go to (201,202,203) ihstyl
201         xt2 = x + 0.8*(lr3+1)*fnthgt(ifont)
            xt1 = x - 0.8*fnthgt(ifont)
            go to 210
202         xt2 = x + 0.8*(lr3+1)*0.5*fnthgt(ifont)
            xt1 = x - 0.8*(lr3+1)*0.5*fnthgt(ifont)
            go to 210
203         xt2 = x + 0.8*fnthgt(ifont)
            xt1 = x - 0.8*(lr3+1)*fnthgt(ifont)
210         continue

c --------- rotate the two window defining points:
            c = cos(radians(angle))
            s = sin(radians(angle))
            xt1r = c*(xt1-x) - s*(yt1-y) + x
            yt1r = s*(xt1-x) + c*(yt1-y) + y
            xt2r = c*(xt2-x) - s*(yt2-y) + x
            yt2r = s*(xt2-x) + c*(yt2-y) + y

c            write(iolog,'(a,f8.4,2i3,2f8.4,i4,1x,a)') 
c     -      'TXT: ', fnthgt(ifont),ihstyl,ivstyl,x,y,lr3,txt3(1:lr3)
            call updbox(xt1r, yt1r)
            call updbox(xt2r, yt2r)
          end if


          call setfont(ifont)

c ------- calculate the shift in y-direction due to the vertical alignment:
          go to (1,2,3) ivstyl
1           dy = 0.0
            go to 4
c --------- use 0.38 here (not 0.5) because PS base is at bottom of p while
c           a normal person would start at bottom of a
2           dy = -0.38*fnthgt(ifontc)
            go to 4
3           dy = -0.76*fnthgt(ifontc)
4         continue

c ------- rotate the coordinate system if so required (use makefont instead?!):
          if (angle.ne.0.0) then
c --------- save the graphics state 
            saved = .true.
            call gsave
            call translate(x,y)
            call rotate(angle)
            s1 = '0'
            l1 = 1
            call condns(dy,s2,l2) 
          else
            saved = .false.
            call condns(x,s1,l1)
            call condns(y+dy,s2,l2)
          end if

          i2 =         index(txt(1:lr), subscr)
          i2 = max(i2, index(txt(1:lr), supscr))
          i2 = max(i2, index(txt(1:lr), grkscr))
          if (i2 .eq. 0) then
c --------- do it in a short way: no special effects:
            write(iops,form(ihstyl))s1(:l1),s2(:l2),txt3(1:lr3)
          else
c --------- there are some sub/superscripts and/or greek characters:
c --------- get the string without ASGL control characters
            call unspec(txt3,txt2,lr3,lr2)
c --------- determine the correct horizontal position (warning: it may not
c           be precise, because the special characters have different
c           widths from the ones that will actually show on the paper in
c           their place):
            if (ihstyl .gt. 1) then
              write(iops,form2(ihstyl)) s1(:l1),s2(:l2),txt2(1:lr2)
            else
              write(iops,form2(ihstyl)) s1(:l1),s2(:l2)
            end if
            ipos      = 1
            istack    = 1
            ifntst(1) = ifont
            nsub      = 0
            nsup      = 0
            ngrk      = 0
c --------- for every character: it either changes a font or is printed
c           in a current font:
50          if (ipos .gt. lr3) go to 100 
              ch = txt3(ipos:ipos)
              if (ch .eq. escapc) then
c ------------- write out the whole \code in one show command:
                if(index('()\\',txt3(ipos+1:ipos+1)).gt.0) then
                  iii = ipos + 2
                  go to 45
                else
                  do ii = ipos+1, min(lr3, ipos+3)
                    iii = ii
                    if(index('1234567890',txt3(ii:ii)).lt.1)go to 45
                  end do
                  write(iops,81) txt3(ipos:iii)
                  ipos = iii+1
                  go to 46
                end if
45              write(iops,81) txt3(ipos:iii-1)
                ipos = iii
46              continue
              else
                if (ch .eq. subscr) then
                  call poppush(nsub,ifntst,istack,maxstk,
     -                         ifsub(ifntst(istack)),dysub)
                else
                  if (ch .eq. supscr) then
                    call poppush(nsup,ifntst,istack,maxstk,
     -                           ifsup(ifntst(istack)),dysup)
                  else
                    if (ch .eq. grkscr) then
                      call poppush(ngrk,ifntst,istack,maxstk,
     -                             ifgrk(ifntst(istack)),0.0)
                    else
                      if (ch .ne. escapc) then
                        write(iops,81) ch
                      else
                        write(iops,81) txt3(ipos:ipos+1)
                        ipos = ipos + 1
                      end if
81                    format('(', a, ') show')
                    end if
                  end if
                end if
                ipos = ipos + 1
              end if
              go to 50
100         continue
          end if

c ------- restore the graphics state, if necessary
          if (saved) call grestore
        end if

        return
      end


      subroutine restore
#include "psgl.cmn"
        write(iops, '(a)') 'restore'
        return
      end


      subroutine save
#include "psgl.cmn"
        write(iops, '(a)') 'save'
        return
      end


      subroutine pspage
#include "psgl.cmn"
        if (.not. epsf) then
          write(iops, '(a,2i4)') '%%Page: ', ipage,ipage
        else
          write(iolog,*) 
     -    'Warning[pspage]: cannot use SHOWPAGE when in EPS format.'
        end if
        return
      end


      real function cmps(x)
      cmps = x / 2.54 * 72.0
      return
      end


      real function pscm(x)
      pscm = x * 2.54 / 72.0
      return
      end


c --- write the head and definitions of procedures and variables
      subroutine closeps(ncopies, psf, bbox0, bboy0, bbox1, bboy1)
#include "psgl.cmn"
        integer ncopies
        real bbox0, bboy0, bbox1, bboy1
        character psf*(*), cmd*(255)

c ----- form feed
        if (.not. epsf) call showpage(ncopies)

c c ----- restore the state of the PS machine before possibly returning to
c        the calling PostScript program (if this is EPS file).
c        call restore

c ----- %%Trailer is necessary for the Page Forward/Backward facility to
c       work in Preview for the non-EPSF files (does not hurt EPSF files).
        write(iops, '(a)') '%%Trailer'

        write(iops, '(a)') '%%EOF'

        close(iops)

c ----- do I have to fix the smallest BoundingBox?
        if (epsf) then
          bbox0 = min(bbx1, max(bbx0,bbox0))
          bboy0 = min(bby1, max(bby0,bboy0))
          bbox1 = min(bbx1, max(bbx0,bbox1))
          bboy1 = min(bby1, max(bby0,bboy1))

          write(iolog,'(a,4i5)') 
     -    'closeps__> %%BoundingBox: ', 
     -    nint(bbox0),nint(bboy0),nint(bbox1),nint(bboy1)

c ------- this shit is neccessary because (atend) does not work with
c         dvips.

#ifdef WIN
c         Windows doesn't have sed... or mv...
          write(cmd,'(a,4i4)') 
     -      '%%BoundingBox: ',
     -      nint(bbox0),nint(bboy0),nint(bbox1),nint(bboy1)
          call fixbbox(psf(1:lenr(psf)), cmd(1:lenr(cmd)))
#else
          write(cmd,'(a,4i4,a,a,a,a)') 
     -      'sed -e "s/%%BoundingBox: (atend)/%%BoundingBox: ',
     -      nint(bbox0),nint(bboy0),nint(bbox1),nint(bboy1),
     -      '/" ', psf(1:lenr(psf)),
     -      ' > asgl.sed.tmp ; mv asgl.sed.tmp ',
     -      psf(1:lenr(psf))

          call mysystem(cmd)
#endif

        end if

        return
      end

#ifdef WIN
c --- Replaces the BoundingBox information with the correct values
      subroutine fixbbox(filenm, bbox)
      use dfport
      implicit none
      character filenm*(*), bbox*(*)
      character line*(300)
      integer inerr, outerr
      integer(4) renstat

      open(unit=21,file=filenm,status="old",iostat=inerr)
      open(unit=22,file="asgl.tmp.out",status="replace",iostat=outerr)
      if (inerr .eq. 0 .and. outerr .eq. 0) then
        do while (inerr .eq. 0)
          read(unit=21,fmt="(A)",iostat=inerr) line
          if (inerr .eq. 0) then
            if (trim(line) .eq. "%%BoundingBox: (atend)") then
              write(22, "(A)") bbox
            else
              write(22, "(A)") trim(line)
            endif
          endif
        end do
        inerr = 0
        close(21)
        close(22)
      endif

c     Only overwrite the original if both files were opened _and_
c     no write errors were encountered
      if (inerr .eq. 0 .and. outerr .eq. 0) then
        renstat = rename("asgl.tmp.out",filenm)
      endif
      end
#endif

c --- undefines ASGL PostScript state
      subroutine undefps
#include "psgl.cmn"

c ----- current linetype (not defined)
        lintyc = -1
c ----- current fonttype (not defined)
        ifontc = -1
c ----- current symbol type
        isybtc = -1

        return
      end
