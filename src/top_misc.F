      subroutine topinit 
        implicit none
#include "top_appl.cmn"
        integer lenr
        character dummy*(lenf), encaps*(5)

c ----- The application initialization code here (it is run only once
c       at the very beginning of the TOP execution)

        vdwfnam = 'egromos.vdw'
        grbrkf  = 'egromos.brk'
        inifl   = 'psgl.ini'
        drawlib = '3d.lib'

        call add4str(libdir, vdwfnam)
        call add4str(libdir, grbrkf)
        call add4str(libdir, inifl)
        call add4str(libdir, drawlib)

c ----- initialize PS package:
        call initps

c ----- is this going to be an encapsulated PS or PS file
        call concat(commfile, '.top', dummy)
        call openf(ioinp, dummy, 'old')
        read(ioinp, '(a,a)') encaps, ext
        close(ioinp)
        call upper(encaps)
        if (encaps .eq. '#EPSF') then
          epsf = .true.
          if (lenr(ext) .lt. 1) ext = '.ps'
          write(*,'(a)') 'top.ini__> Encapsulated PostScript'
          call initbb
c ------- by default, turn the mark page off:
c ------- top.ini specific
          lprm(1,38) = .false.
        else
          epsf = .false.
          ext = '.ps'
          write(*,'(a)') 'top.ini__> PostScript'
        end if
        call rootfn(commfile, dummy)
        call concat(dummy, ext, psfil)
        call headps(psfil, datetime)

c ----- read the correspondence between Brookhaven and GROMOS names:
c       (nresdc and resdc are local to init1 - use ndcres and dcres outside)
        call rdgrbrk(ioinp,grbrkf,groatm,brkatm,mrestyp,mapres,
     -               nresdc,natmdc,resdc)

c ----- read the table of the van der Waals radii:
        call readvdw(ioinp,vdwfnam,vdwatm,vdwcnt,nvdwtyp,mattyp,
     -               polar,electr)

c ----- read the default atom and bond colors and line types
        call rddraw(ioinp,drawlib,nvdwtyp,mbndtyp,nbndtyp,
     -               atypc,iatypl,btypc,ibtypl)

c ----- no data in Table:
        nstrs = 0
        ncolmn = 0

        return
      end



      subroutine cmdspec
        implicit none
#include "top_appl.cmn"
c        integer lr, lenr

c        lr = min(lenr(action(indxca)),68)
c        write(iolog,'(2a/)') 'TOP______> ', action(indxca)(1:lr)

        return
      end



c --- exceptions to general parameter setup:
      subroutine spec_prm
      implicit none
#include "top.cmn"

c --- post-processing:

      return
      end


      subroutine finish
        implicit none
#include "top_appl.cmn"

        call stamp
        call closeps(iprm(1,45), psfil, bbox0, bboy0, bbox1, bboy1)
        write(iolog, '(/2a)') 'Starting time: ', datetime
        call sys('date', datetime)
        write(iolog, '(2a)') 'Closing time : ', datetime
        close(iolog)

        return
      end

      subroutine topappl1
      return
      end

      subroutine topappl2
      return
      end
