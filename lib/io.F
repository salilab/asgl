cf ----------------------------------------------------------------------------
cf
cf    OPENF4() routine opens a possibly compressed file for various
cf    I/O operations. It is equivalent to VMS open with
cf    carriagecontrol='list', ie the write format statement should
cf    omit 1x as the first item if you do not want a blank as the
cf    first character on the line.
cf
cf    The arguments are:
cf
cf    iochn             I/O stream
cf
cf    fname             filename
cf
cf    if stat='old'     if the file does not exist then it complains;
cf           ='unknown' the file may or may not exist;
cf           ='new'     the file must not exist;
cf
cf    if access='append' then the file pointer is positioned at the end
cf                       (irrespective of the file's existence or not);
cf                       only for fmt='FORMATED'; the other possibility
cf                       if SEQUENTIAL.
cf
cf    format       can be FORMATTED or UNFORMATTED.
cf
cf    if verbose=  0 No messages written out;
cf                 1 Error messages written out;
cf                 2 Error and Warning messages written out;
cf                 3 Error, Warning, and Note messages written out;
cf                 4 Note messages written out;
cf
cf    if estop=.true.  then it will stop on Error.
cf
cf    An output status variable IERR is <> 0 if something was wrong.
cf
cf    An output argument COMPRESSED is returned .T. if the file was
cf    compressed before opened.
cf
cf    subroutine openf4(iochn,fname,stat,access,format,verbose,
cf   &                  estop,ierr,compressed,iolog)
cf
cf ----------------------------------------------------------------------------

      subroutine openf4(iochn,fname,stat,access,format,verbose,
     &                  estop,ierr,compressed,iolog)
        implicit none
#include "lenf.cst"
        integer iochn, ierr, verbose, iolog
        character fname*(*), stat*(*), access*(*), format*(*)
        character nozf*(lenf)
        logical estop, compressed

c ----- possibly uncompress the file if it is compressed
        call prepfil(fname,compressed,nozf)

        call openf5(iochn,nozf,stat,access,format,verbose,estop,
     &              ierr,iolog)

        return
      end




cf ----------------------------------------------------------------------------
cf
cf    subroutine openf5(iochn,fname,stat,access,format,verbose,estop,
cf   &                  ierr,iolog)
cf
cf ----------------------------------------------------------------------------

      subroutine openf5(iochn,fname0,stat,access,format,verbose,estop,
     &                  ierr,iolog)
        implicit none
#include "lenf.cst"
        integer iochn, lr, lenr2, ierr, verbose, iolog, lenr
        character fname0*(*), stat*(*), access*(*), ios*3
        character format*(*), fname2*(lenf), fname1*(lenf)
        logical exst, estop, iowr, strgin
        external strgin, lenr2, iowr, lenr

        lr = lenr(fname0)
c ----- always stop on this error message:
        if (lr .gt. lenf) then
          write(iolog,'(a)') 'openf5__E> increase LENF'
          stop
        end if

        fname1 = fname0
        call ljust(fname1)

        if (verbose.ge.3) then
          if (lr .eq. 0) then
            fname1 = 'fort.'
            write(ios, '(i3)') iochn
            call addstr(fname1, ios)
            lr = lenr(fname1)
          end if

          if (iowr(2)) then
            write(iolog,'(a,i2,8a)')
     &      'openf5___> ',iochn,
     &      ',',stat(1:3), ',',access(1:3), ',',format(1:4),
     &      ', ',fname1(1:lr)
          end if
        end if

c ----- expand the filename by expanding environment variables in it:
        call expand(fname1,fname2)

c ----- does the file exist?
        inquire(file=fname2, exist=exst)
        if ((.not. exst) .and. strgin(stat,'OLD')) then
          if (verbose.gt.0 .and. verbose .le. 3) then
            write(iolog,'(2a)') 'openf5__E> file does not exist: ',
     &                          fname2(1:lenr2(fname2))
            if (estop) stop
          else
            if (verbose .eq. 4) then
              write(iolog,'(2a)') 'openf5___> file does not exist: ',
     &                            fname2(1:lenr2(fname2))
              if (estop) stop
            end if
          end if
          ierr = 1
          return
        end if

        if (exst .and. strgin(stat,'NEW')) then
          if (verbose.gt.0 .and. verbose .le. 3) then
            write(iolog,'(a)') 'openf5__E> file exists'
            if (estop) stop
          end if
          ierr = 2
          return
        end if

        if ((.not.strgin(format,'FORMATTED')) .and.
     &      (strgin(access,'APPEND'))) then
          if (verbose.gt.0 .and. verbose .le. 3) then
            write(iolog,'(a)')
     &      'openf5__E> access="APPEND" not allowed for UNFORMATTED'
            if (estop) stop
          end if
          ierr = 3
          return
        end if

c ----- open the file (always in access='sequential')
#if defined(f90)
c ----- for f90 (on Ultrix 5.5) (is this enough?):
        open(iochn, form=format, file=fname2, status=stat, err=10,
     &       access=access, recl=4096)
#else
        open(iochn, form=format, file=fname2, status=stat, err=10,
     &       access=access)
#endif
        ierr = 0
        return

10      if (verbose.gt.0 .and. verbose.le.3) then
          write(iolog,'(a)')'openf5__E> error openning file'
          if (estop) stop
        end if
        ierr = 4
        return

      end





cf ----------------------------------------------------------------------------
cf
cf    OPENF() uses OPENF5 assuming sequential access, formatted form,
cf    maximal verbose, a stop on error, and stream 6 for log output.
cf
cf    subroutine openf(iochn, fname)
cf
cf ----------------------------------------------------------------------------

      subroutine openf(iochn, fname, stat)
        implicit none
        integer iochn, ierr
        character fname*(*), stat*(*)

        call openf5(iochn,fname,stat,'SEQUENTIAL','FORMATTED',
     &              3,.true.,ierr,6)

        return
      end



cf ----------------------------------------------------------------------------
cf
cf    PREPFIL() finds whether INFIL with or without .Z exists, and possibly
cf    uncompresses the compressed file and so prepares it for the I/O
cf    operations. COMPRESSED returns .T. if the file was compressed.
cf    FNAME returns INFIL without .Z.
cf
cf    subroutine prepfil(infil,compressed,fname)
cf
cf ----------------------------------------------------------------------------

      subroutine prepfil(infil,compressed,fname)
        implicit none
#include "lenf.cst"
        character fname*(*), zfname*(lenf), cmd*(lenf+11), infil*(*)
        logical compressed, filexs

        call zfnam(infil, zfname)
        call nozfnam(infil, fname)

        if (filexs(fname)) then
          compressed = .false.
        else
          if (filexs(zfname)) then
            cmd = 'uncompress '
            cmd(12:) = zfname
            call mysystem(cmd)
            compressed = .true.
          end if
        end if

        return
      end


cf ----------------------------------------------------------------------------
cf
cf    UNPREPFIL() puts the file in the original state; if it was compressed,
cf    (COMPRESSED = .T.) it compresses it. FNAME can be with or without the
cf    .Z extension.
cf
cf    subroutine unprepfil(fname,compressed)
cf
cf ----------------------------------------------------------------------------

      subroutine unprepfil(fname,compressed)
        implicit none
#include "lenf.cst"
        character fname*(*), cmd*(lenf+13), nozf*(lenf)
        logical compressed, filexs

        if (compressed) then
          cmd = 'compress -f '
          call nozfnam(fname,nozf)
          cmd(13:) = nozf
          if (filexs(nozf)) call mysystem(cmd)
        end if

        return
      end


cf ----------------------------------------------------------------------------
cf
cf    FILEXS() returns .T. if file FNAME exists.
cf
cf    logical function filexs(fname)
cf
cf ----------------------------------------------------------------------------

      logical function filexs(fname)
        implicit none
#include "lenf.cst"
        character fname*(*), fname2*(lenf)
        logical exst
        call expand(fname,fname2)
        inquire(file=fname2, exist=exst)
        filexs=exst
        return
      end


      subroutine zfnam(infil,zfname)
        implicit none
        integer lenr, lr
        character infil*(*), zfname*(*)
        external lenr

        lr = lenr(infil)
        if (lr .ge. 2) then
          if (infil(lr-1:lr) .eq. '.Z') then
            zfname=infil(1:lr)
          else
            zfname=infil(1:lr) // '.Z'
          end if
        else
          zfname=infil(1:lr) // '.Z'
        end if

        return
      end


      subroutine nozfnam(infil,fname)
        implicit none
        integer lenr, lr
        character infil*(*), fname*(*)
        external lenr

        lr = lenr(infil)
        if (lr .ge. 2) then
          if (infil(lr-1:lr) .eq. '.Z') then
            fname=infil(1:lr-2)
          else
            fname=infil(1:lr)
          end if
        else
          fname=infil(1:lr)
        end if

        return
      end


cf ----------------------------------------------------------------------------
cf
cf    EXPAND() expands up the environment variables in a filename.
cf    Environment variables must be of the form ${VARNAME} or
cf    $(VARNAME).
cf
cf    subroutine expand(infil,outfil)
cf
cf ----------------------------------------------------------------------------

      subroutine expand(infil,outfil)
        implicit none
#include "lenf.cst"
        integer lenr, i1, i2, l
        character infil*(*),outfil*(*),envval*(lenf),outfil2*(lenf)
        external lenr

c ----- replace the ${VARNAME}'s:

        outfil = infil
10      continue
          outfil2 = outfil
          i1 = index(outfil2, '${')
          i2 = index(outfil2, '}')
          if (i1.lt.1.or.i2.lt.1.or.i2.lt.i1) go to 100
          call getenv(outfil2(i1+2:i2-1), envval)
          l = lenr(envval)
          if (l .gt. 0) then
           call subs2(outfil,outfil2(i1:i2),envval(1:l))
          else
           call rmstrs(outfil,outfil2(i1:i2))
          end if
          go to 10
100     continue

c ----- now replace the $(VARNAME)'s:

20      continue
          outfil2 = outfil
          i1 = index(outfil2, '$(')
          i2 = index(outfil2, ')')
          if (i1.lt.1.or.i2.lt.1.or.i2.lt.i1) go to 200
          call getenv(outfil2(i1+2:i2-1), envval)
          l = lenr(envval)
          if (l .gt. 0) then
           call subs2(outfil,outfil2(i1:i2),envval(1:l))
          else
           call rmstrs(outfil,outfil2(i1:i2))
          end if
          go to 20
200     continue

        return
      end



cf ----------------------------------------------------------------------------
cf
cf    FULLFN() routine returns full filename of a file specified by
cf    CODE (if it exists). The full filename is constructed by adding
cf    all directories listed in string DIRS (separated by ':', as in a
cf    UNIX shell) and all extensions in string EXTS (also separated by ':').
cf    Additionally, it tries compressed .Z versions of the filename, too.
cf    Additionally, it tries to prepend 'pdb', too.
cf    If file is not found IFNDFLG=1, otherwise 0. If compressed,
cf    COMPRESSED=.T., otherwise .F. If ISTOP=1, it stops on no
cf    find, if ISTOP=0, it does not stop. If IVERBOSE=1, it writes
cf    the filenames on standard output as it tries them out.
cf
cf    subroutine fullfn(dirs,fname,exts,fullfil,ifndflg,compressed,
cf   &                  istop,iverbose)
cf
cf ----------------------------------------------------------------------------

      subroutine fullfn(dirs,fname0,exts,fullfil,ifndflg,compressed,
     &                  istop,iverbose)
        implicit none
#include "lenf.cst"
        integer mdir, mext
        parameter (mdir=20, mext=20)
        integer i, ifndflg, ndir, istop, iext, next, lenr, lr, ll
        integer iverbose, lenl, lenr2
        character dirs*(*), dir(mdir)*(lenf), ext(mext)*(20)
        character fname0*(*), fullfil*(*), dcode*(lenf), exts*(*)
        character code*(lenf), fname*(lenf), dcode2*(lenf)
        logical filexs,compressed
        external lenr, lenr2, lenl

        ifndflg = 0

        fname = fname0
        call ljust(fname)

c ----- in case the arguments fname and fulfill are the same variable
        code = fname

c ----- Try the original name without and with .Z (note that this is generally
c       not the same as using current directory and no extension):
        fullfil = fname
        compressed = .false.
        if (iverbose.eq.1) write(*,'(2a)')
     &    'fullfn___> trying: ', fullfil(1:lenr2(fullfil))
        if (filexs(fullfil)) return
        call addstr(fullfil, '.Z')
        compressed = .true.
        if (iverbose.eq.1) write(*,'(2a)')
     &    'fullfn___> trying: ', fullfil(1:lenr2(fullfil))
        if (filexs(fullfil)) return

c ----- Try adding pdb before the filename:
        call concat('pdb', fname, fullfil)
        compressed = .false.
        if (iverbose.eq.1) write(*,'(2a)')
     &    'fullfn___> trying: ', fullfil(1:lenr2(fullfil))
        if (filexs(fullfil)) return
        call addstr(fullfil, '.Z')
        compressed = .true.
        if (iverbose.eq.1) write(*,'(2a)')
     &    'fullfn___> trying: ', fullfil(1:lenr2(fullfil))
        if (filexs(fullfil)) return

c ----- get all directories:
        ll = lenl(code)
        if (code(ll:ll) .ne. '/') then
          call str_sn5(dirs, dir, mdir, ndir, ':')
        else
          ndir = 0
        end if
c ----- add current directory if no directories specified:
        if (ndir .eq. 0) then
          ndir = 1
          dir(1) = ' '
        end if

c ----- get all extensions:
        call str_sn5(exts, ext, mext, next, ':')
c ----- add no extension if no extensions specified:
        if (next .eq. 0) then
          next = 1
          ext(1) = ' '
        end if

        do  i = 1, ndir
          lr = lenr(dir(i))
          if (lr .gt. 0) then
            if (dir(i)(lr:lr).ne.'/') call addstr(dir(i),'/')
            call concat(dir(i), code, dcode)
            call concat('pdb', code, dcode2)
            call add3str(dir(i), dcode2)
          else
            dcode = code
            call concat('pdb', code, dcode2)
          end if

          do  iext = 1, next

c            write(*,'(//a,2i4,1x/a,a70/a,a70)')
c     &      'IDIR, IEXT: ', idir,iext,'  DIR: ',dir(i)(1:lenr2(dir(i))),
c     &      '  EXT: ', ext(iext)(1:lenr2(ext(iext)))

c --------- append the extension and see if it exists in directory i:
            call concat(dcode, ext(iext), fullfil)
            compressed = .false.
            if (iverbose.eq.1) write(*,'(2a)')
     &        'fullfn___> trying: ', fullfil(1:lenr2(fullfil))
            if (filexs(fullfil)) return

            call addstr(fullfil, '.Z')
            compressed = .true.
            if (iverbose.eq.1) write(*,'(2a)')
     &        'fullfn___> trying: ', fullfil(1:lenr2(fullfil))
            if (filexs(fullfil)) return

c --------- Try pre-pending 'pdb':

            call concat(dcode2, ext(iext), fullfil)
            compressed = .false.
            if (iverbose.eq.1) write(*,'(2a)')
     &        'fullfn___> trying: ', fullfil(1:lenr2(fullfil))
            if (filexs(fullfil)) return

            call addstr(fullfil, '.Z')
            compressed = .true.
            if (iverbose.eq.1) write(*,'(2a)')
     &        'fullfn___> trying: ', fullfil(1:lenr2(fullfil))
            if (filexs(fullfil)) return
          end do

        end do

        if (istop .eq. 1) then
          write(*,'(2a/2a/2a/2a)')
     &    'fullfn__E> file not found: ', code(1:lenr2(code)),
     &    '           directories   : ', dirs(1:lenr2(dirs)),
     &    '           extensions    : ', exts(1:lenr2(exts)),
     &    '           prefixes      : nothing, pdb'
          stop
        end if
        ifndflg = 1

        return
      end



c --- if FNAME = 'DEFAULT' then
c       FNAME = {ROOT_NAME}{FILE_ID}{ID1}{ID2}{FILE_EXT}
c     else
c       FNAME = FNAME
c     end if
c
      subroutine gennam2(root,fileid,deffileid,id1,id2,fname,filext)
        implicit none
        integer id1, id2, lr, lenr, len
        character fname*(*), fileid*(*), root*(*)
        character filext*(*), deffileid*(*)
        logical strgin
        external strgin, lenr

        if (strgin(fname, 'DEFAULT')) then

          if (strgin(fileid, 'DEFAULT')) then
            call concat(root, deffileid, fname)
          else
            call concat(root, fileid, fname)
          end if

          lr = lenr(fname)
          if (len(fname)-lr.lt.5)
     &      call wrtmsg(4,.false.,.true.,1,'gennam2',
     &                  'Increase LEN(FNAME).')

c ------- the first ID
          write(fname(lr+1:), '(i4)') id1
          if (fname(lr+1:lr+1) .eq. ' ') fname(lr+1:lr+1) = '0'
          if (fname(lr+2:lr+2) .eq. ' ') fname(lr+2:lr+2) = '0'
          if (fname(lr+3:lr+3) .eq. ' ') fname(lr+3:lr+3) = '0'

c ------- the second ID
          write(fname(lr+5:), '(i4)') id2
          if (fname(lr+5:lr+5) .eq. ' ') fname(lr+5:lr+5) = '0'
	  if (fname(lr+6:lr+6) .eq. ' ') fname(lr+6:lr+6) = '0'
	  if (fname(lr+7:lr+7) .eq. ' ') fname(lr+7:lr+7) = '0'

c ------- now add the explicit last extension:
          call addstr(fname, filext)
        end if

        return
      end



cf ----------------------------------------------------------------------------
cf
cf    ADDDIR() adds a '/' at the end of the directory name if necessary.
cf
cf    subroutine adddir(dir)
cf
cf ----------------------------------------------------------------------------

      subroutine adddir(dir)
        implicit none
        integer lenr, lr
        character dir*(*)
        external lenr

        lr = lenr(dir)
        if (lr.gt.0) then
          if (dir(lr:lr) .ne. '/') dir(lr+1:lr+1) = '/'
        end if

        return
      end


      subroutine prefix(dir,fn)
        implicit none
        character dir*(*), fn*(*)

        if (fn(1:1) .ne. '/') then
          call adddir(dir)
          call add3str(dir,fn)
        end if

        return
      end
