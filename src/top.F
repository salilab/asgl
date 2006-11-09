cf ----------------------------------------------------------------------------
cf
cf    This file contains the main TOP program. No changes required here.
cf
cf    Copyright by Andrej Sali, 1990 -- 2006.
cf
cf
cf    The other parts of the TOP framework are:
cf
cf
cf    top.cst, top.cmn                      : no changes necessary;
cf
cf    top_appl.cmn, top_act?.F, top_misc.F  : application dependent.
cf
cf
cf    Top1 common block is used for the top level communication.
cf
cf    Application common is used for communication between TOP and
cf    application in the act* routines.
cf
cf    Search for 'top.ini specific' to find top.ini specific parts.
cf
cf
cf ----------------------------------------------------------------------------

      program top1
        implicit none
#include "top_appl.cmn"
        integer icmd,lr,iargc,lenr,i,lenr2,istat
        character key*100, str*14, outfile*256
        logical iowr
        external lenr, lenr2, iowr

c ----- specify whether this application needs a key
        logical dokey
c ----- ASGL:
        data dokey /.false./
c c ----- MODELLER:
c         data dokey /.true./

        iprm(1,7) = 0
        iprm(2,7) = 0
        iprm(3,7) = 0
        iprm(4,7) = 1

        if (dokey) then

        call getenv(keyenv, key)
        if (lenr(key) .gt. 0) then
          if (key .ne. license) then
            call wrtmsg(4,.true.,.true.,1,'TOP',
     &      'Not a valid key: ' // key(1:lenr2(key)) //
     &      '$Make sure that "setenv ' // keyenv //
     &      ' ' // key(1:lenr2(key)) // '" is defined properly. $' //
     &      'E-mail to Andrej Sali to get the key ' //
     &      '(sali@rockvax.rockefeller.edu).')
          end if
        else 
          call wrtmsg(4,.true.,.true.,1,'TOP',
     &    'Undefined license; see file INSTALLATION.$'//
     &    'Possible reasons: Incomplete installation, too small '//
     &    'a swap space.')
        end if

        end if

c ----- get the TOP program name:
        if (iargc() .ne. 1) then
          commfile = applic
        else
          call getarg(1, commfile)
        end if
        job = commfile
        lr = lenr(job)
        if (job(max(1,lr-3):lr) .eq. '.top') then
          job=job(1:lr-4)
          outfile = job(1:lr-4) // '.log'
        else
          outfile = job(1:lr) // '.log'
        end if
        CALL openf5(iolog,outfile,'REPLACE','SEQUENTIAL','FORMATTED',
     &              2,1,istat,iolog)

c        else
c          commfile = call addstr(commfile, '.top')
c        end if

c ----- top directory for all program files:
        call getenv(appenv(1:lenr2(appenv)), topdir)
        if (lenr(topdir) .eq. 0)
     &   call wrtmsg(4,.true.,.true.,1,'TOP',
     &   'Environment variable for application directory undefined.'//
     &   'Program probably not installed correctly.')
        call addstr(topdir, '/')
        call getenv(binenv(1:lenr2(binenv)), bindir)
        call addstr(bindir, '/')
        call getenv(libenv(1:lenr2(libenv)), libdir)
        call addstr(libdir, '/')

c ----- the filename of the TOP initialization file:
        call concat(topdir, 'top.ini', inifil)

c        write(iolog, '(2a/a)')
c     &  'TOP______> if it crashes here, you do not have enough swap ',
c     &  'space;',
c     &  '           see file INSTALLATION.'

c ----- get the time and date
        call dattim(datetime)

c ----- read in the top.ini file (commands, keys, default values)
        call rditop
        call wrtmsg(2,.false.,.false.,0,'TOP','TOP initialized.')

c ----- write the header:
        if (iowr(1)) call head(iolog)

        do  i = 1, nio2-nio1+1
          ioopnd(i) = .false.
        end do

c ----- initialize application
        call wrtmsg(2,.false.,.false.,0,'TOP',
     &              'Initializing application ' // applic)
        call topinit

c ----- read actions in all relevant TOP program files:
        call wrtmsg(2,.false.,.false.,0,'TOP',
     &  'Script to be executed: '//commfile(1:lenr2(commfile)))
        call getprogram

        write(str,'(2i7)') nactions, nrout
        call wrtmsg(2,.false.,.false.,0,'TOP',
     &  'Number of command lines, routines: '//str)

c ----- compile the TOP program
        call wrtmsg(2,.false.,.false.,0,'TOP',
     &  'Compiling the TOP program.')
        call compile

        call wrtmsg(2,.false.,.false.,0,'TOP',
     &  'Executing the TOP program.')

c ----- start with the beginning of the program
        indxca = 0
c ----- currently at the level of the main program
        ilevel = 1

        icmd = 0
500     if (indxca .ge. nactions) go to 600

c ------- get the index of the next statement
          indxca = indxca + 1
          icmd = icmd + 1

c ------- write out the command to be executed
c ------- top.ini specific
          if (iowr(1)) then
            if (action(indxca)(1:10).ne.'SUBROUTINE') then
              if (iowr(2)) call wract(iolog,icmd,indxca,
     &                                action(indxca))
            end if
          end if

c ------- execute the command
          call cmdexec(iaction(indxca), action(indxca))

          go to 500
600     continue

        call wrtmsg(2,.false.,.false.,0,'TOP','EXIT(0).')

c ----- finish the APPLICATION business
        call finish

        stop
      end



      subroutine getprogram
        implicit none
#include "top.cmn"
        integer lr, lenr, indexwr, i, irout, ifindword, ig, iuniq
        integer lenr2, ierr
        character suniq*3
        logical iowr
        external lenr, lenr2, indexwr, ifindword, iowr

        iuniq = 0

c ----- read all command lines, including those from the INCLUDE files
        call rdactions

c ----- assemble the subroutines' table:
        rout(1)   = commfile
c ----- the number of routines, including the main program
        nrout     = 1

        i = 0
10      if (i.ge.nactions) go to 100
          i = i + 1

c ------- is it a SUBROUTINE header:
c ------- top.ini specific
          ig = indexwr(action(i), cmdlst(17))
          if (ig .gt. 0) then
c --------- top.ini specific
            call assgn(17,action(i),.true.,ierr,.true.)
            if (lenr(sprm(1,3)) .lt. 1) then
             if (iowr(4))
     &        write(iolog,'(2a)') 'getprog_E> no ROUTINE name in: ',
     &        action(i)(1:lenr2(action(i)))
              stop
            end if

            if (nrout .ge. maxrout) then
              if (iowr(4))
     &        write(iolog,'(a)')
     &        'getprog_E> too many ROUTINEs; increase maxrout'
              stop
            end if

            nrout = nrout + 1
            rout(nrout) = sprm(1,3)
c --------- beginning of this subroutine
            indrout(1,nrout) = i

c --------- use the last of the routines in the source file if several
c           of them have the same name; remove this block if you want
c           to use the first one.
            if (nrout .gt. 1) then
              irout = ifindword(rout(nrout),rout,nrout-1)
              if (irout .gt. 0) then
                lr = lenr(rout(nrout))
                if (iowr(3))
     &            write(iolog,'(2a)')
     &            'getprog_W> routine redefined: ',rout(nrout)(1:lr)
                iuniq = iuniq + 1
                write(suniq, '(i3)') iuniq
                call concat('_@#%$', suniq, rout(irout))
                call subs2(action(indrout(1,irout)),rout(nrout)(1:lr),
     &                     rout(irout)(1:lenr(rout(irout))))
              end if
            end if

c --------- find the first END_SUBROUTINE === end of the current SUBROUTINE
20          continue
              i = i + 1
c ----------- is it an END_SUBROUTINE command:
c ----------- top.ini specific
              ig = indexwr(action(i), cmdlst(18))
            if (ig .lt. 1) go to 20
            indrout(2,nrout) = i

          end if

          go to 10
100     continue

        indrout(1,1) = 1
        indrout(2,1) = nactions

        return
      end



c --- User interface routines for top1:
c
c --- read the top1 program or routine; including all INCLUDE files where
c     they occur.
c
c --- Input: ioi ... input channel
c            commfile ... input file name (top program)
c            eofline, commch ... EOLN and comment control characters
c            maxact ... maximal number of actions possible
c            lenact ... maximal allowed length of commands
c
c --- Output:nactions ... number of commands
c            actions ... commands array
c
c --- The processing of the commands is also done:
c       1. Ignore all text after the comment character.
c       2. Ignore all lines empty after step 1.
c       3. Join lines from 2 that have to be joined into one command.
c       4. In all commands, transform everything but the strings
c          between two successive unprotected primes into upper case.
c
      subroutine rdactions
        implicit none
#include "top.cmn"
        integer ig,ioj,i,lena,lenr,lenline,ierr
        integer indexwr,lenr2
        character line*(lenact)
        logical finished, commnt, prime, isprime

c ----- Character COMMCH behaves like a comment character: everything
c       on the line after this char is ignored;
c ----- Character EOFLINE is a continuation character: you can have a comment
c       on the same line after it, but it also means the command continues
c       on the following physical line;

        ioj = iomin
        call opentop(ioj,commfile)

c ----- will be the number of non-empty lines containing possible commands
        nactions = 1
        lena = 0
10      read(ioj, '(a)', end = 100) line
c ------- is this whole line a comment:
          if (commnt(line, commch)) go to 10
c ------- is anything in this line:
          lenline = lenr(line)
          if (lenline .eq. 0) go to 10
c ------- by default, there will be one command per one line
          finished = .true.
c ------- copy characters on the line to the action string
          prime = .false.
          do  i = 1, lenline
            if (isprime(line, i)) prime = .not. prime
c --------- if a continuation character is reached, the command continues
c           on the next line
            if ((line(i:i) .eq. eofline) .and. (.not. prime)) then
              finished = .false.
              go to 40
            end if
c --------- if an end of command is reached, continue with the next command
            if ((line(i:i) .eq. commch) .and. (.not. prime)) go to 40
            lena = lena + 1
c --------- TAB goes into a blank
            if (ichar(line(i:i)) .eq. 9) then
              action(nactions)(lena:lena) = ' '
            else
              action(nactions)(lena:lena) = line(i:i)
            end if
          end do

40        continue

          if (finished) then

           if ((lena .gt. 0).and.(lena.lt.lenact)) then
            call blank(action(nactions)(lena+1:lenact))

c --------- process all actions to upper case, except the strings quoted
c           in unprotected primes
            call upperp(action(nactions))
c --------- left justify the action
            call ljust(action(nactions))
c --------- put a blank before each unquoted '=' that does not have a blank
c           before it
            call eqnbln(action(nactions))

c --------- is it an INCLUDE command:

c --------- top.ini specific
            ig = indexwr(action(nactions), cmdlst(19))
            if (ig .gt. 0) then
c ----------- top.ini specific
              call assgn(19,action(nactions),.true.,ierr,.true.)
              if (lenr(sprm(1,11)) .lt. 1) then
                write(iolog,'(2a)')
     &          'rdactio_E> no include file name in: ',
     &          action(nactions)(1:lenr2(action(nactions)))
                stop
              end if

c ----------- open the INCLUDE file
              ioj = ioj + 1
              if (ioj .gt. iomax) then
                write(iolog,'(a/a)')
     &          'rdactio_E> too many nested INCLUDE calls',
     &          '           change the gap between iomin and iomax'
              end if
              call opentop(ioj,sprm(1,11))

              lena = 0

            else

              if (nactions .lt. maxact) then
                nactions = nactions + 1
                lena = 0
              else
                write(iolog,'(a)') 'rdactio_E> increase MAXACT'
                stop
              end if

            end if

           end if

          end if

          go to 10

100     continue

        close(ioj)
c ----- any files left
        ioj = ioj - 1
        if (ioj .ge. iomin) go to 10

        if (lena .eq. 0) nactions = nactions - 1

        return
      end



      subroutine opentop(ioj, infile)
#include "top.cmn"
        integer ioj, ierr
        character infile*(*), dirs*(lenfil), infile2*(lenfil)
        logical cmpr

        call gennam2(sprm(1,4),sprm(1,6),'_TOP',iprm(1,2),iprm(1,3),
     &               infile,sprm(1,17))

c ----- add '.top' because fullfn() tries original name first:
        call concat(infile,'.top',infile2)
        call concat(sprm(1,5), ':', dirs)
        call addstr(dirs, bindir)
        call fullfn(dirs, infile2, ' ', infile2, ierr, cmpr, 0, 0)
        if (ierr .ne. 0) then
          call fullfn(dirs, infile, '.top:', infile, ierr, cmpr, 1, 0)
        else
          infile = infile2
        end if
        call openf4(ioj,infile,'OLD','SEQUENTIAL','FORMATTED',3,.true.,
     &              ierr,cmpr,iolog)

        return
      end


c --- insert a blank between a non-blank and '=' outside a string
      subroutine eqnbln(action)
        implicit none
#include "top.cst"
        integer lcont,lenr,i,lnew
        character ch*1
        character dummy*(lenact), action*(*)
        logical prime, isprime

        prime = .false.
        lcont = lenr(action)
        lnew = 0
        do  i = 1, lcont
          ch = action(i:i)
          if (isprime(action, i)) prime = .not. prime
          if ((ch.eq.varelm).and.(.not.prime)) then
            if (i .gt. 1) then
              if (action(i-1:i-1) .ne. blankc) then
                lnew = lnew + 1
                dummy(lnew:lnew) = blankc
              end if
            end if
          end if
          lnew = lnew + 1
          dummy(lnew:lnew) = ch
        end do

        if (lnew .le. len(action)) then
          if (lnew .gt. 0) then
            action = dummy(1:lnew)
          else
            action = ' '
          end if
        else
          write(iolog,'(a)')
     &    'eqnbln__E> increase length of action (LENACT)'
          stop
        end if

        return
      end


      integer function indact(cmdlin)
        implicit none
#include "top.cmn"
        integer indexw,j,lenr2
        character cmdlin*(*), cmd*(25)
        external lenr2

c ----- the first word must be a command:
        call str_s(cmdlin, cmd)
        do  j = 1, ntypcom
          if (indexw(cmd,cmdlst(j)) .gt. 0) then
            indact = j
            return
          end if
        end do

        indact = 1
        if (cmd.ne.'undefined') then
          write(iolog,'(a,a)')
     &    'indact__E> command not recognized: ', 
     &    cmdlin(1:lenr2(cmdlin))
          stop
        end if
        return
      end


      subroutine doenddo
        implicit none
#include "top.cmn"
        integer ierr,i,ifind2word,ind,ir,ieq,lenr,indexwr,nloop,il
        integer lenr2
        real cntrl
        character scntrl(4)*(lenkey)
        external lenr, lenr2, ifind2word

        nloop = 0
        do  indxca = 1, nactions
c ------- top.ini specific:
          if (iaction(indxca) .eq. 9) then
            nloop = nloop + 1
            if (nloop .gt. maxloop) then
              write(iolog,'(a)')
     &        'doenddo_E> too many DO loops; increase MAXLOOP'
              stop
            end if
            ibegloop(nloop) = indxca

c --------- top.ini specific:
            il = indexwr(action(indxca), cmdlst(9))
            ir = lenr(action(indxca))

            ieq = index(action(indxca), '=')
            if (ieq .eq. 0) then
              write(iolog,'(2a)')'doenddo_E> no = in: ', action(indxca)
              stop
            end if

c --------- the loop variable:
            if ((ieq-1)-(il+1) .ge. 0) then
              call str_s(action(indxca)(il+1:ieq-1), scntrl(1))
              call rmchrs(scntrl(1), ',')
            else
              write(iolog,'(2a)')
     &        'doenddo_E> no loop cntrl variable in: ',action(indxca)
              stop
            end if

c --------- symbolic value of the loop var: find its type and index in ?prm:
c --------- set it to a scalar type:
            ind = ifind2word(scntrl(1),prmrlst,nrprm)
            if (ind .eq. 0) then
              ind = ifind2word(scntrl(1),prmilst,niprm)
              if (ind .eq. 0) then
                write(iolog,'(2a/2a)')
     &          'doenddo_E> real/integer var undefined: ',
     &          scntrl(1)(1:lenr2(scntrl(1))),
     &          '           TOP line: ',
     &          action(indxca)(1:lenr2(action(indxca)))
                stop
              else
                tcntrl(1,nloop) = 'I'
                chki(ind) = .false.
                nviprm(ind) = 1
              end if
            else
              tcntrl(1,nloop) = 'R'
              chkr(ind) = .false.
              nvrprm(ind) = 1
            end if
            ictrloop(1,nloop) = ind

c --------- the starting value, final value, step:
            if (ir-(ieq+1) .ge. 0) then
              call subs2(action(indxca)(ieq+1:ir), ',', blankc)
              call str_sn(action(indxca)(ieq+1:ir), scntrl(2), 3)
c              call rmchrs(scntrl(2), ',')
c              call rmchrs(scntrl(3), ',')
c              call rmchrs(scntrl(4), ',')
            else
              write(iolog,'(2a)')
     &        'doenddo_E> no loop cntrl variables in: ',action(indxca)
              stop
            end if

c --------- types and indices:
            do  i = 2, 4
              call str_r2(scntrl(i), cntrl, ierr)
              if (ierr .eq. 0) then
c ------------- real value, create a real variable corresponding to it:
                call defvar(prmrlst,'doloopvar',chkr,nvrprm,nrprm,mrprm)
                ictrloop(i,nloop) = nrprm
                rprm(1,nrprm) = cntrl
              else
c ------------- symbolic value: find the variable index in ?prm:
                ind = ifind2word(scntrl(i),prmrlst,nrprm)
                if (ind .eq. 0) then
                 ind = ifind2word(scntrl(i),prmilst,niprm)
                 if (ind .eq. 0) then
                  write(iolog,'(2a/2a)')
     &            'doenddo_E> real/integer var undefined: ',
     &            scntrl(i)(1:lenr2(scntrl(i))),
     &            '           in TOP line: ',
     &            action(indxca)(1:lenr2(action(indxca)))
                  stop
                 else
                  tcntrl(i,nloop) = 'I'
                 end if
                else
                 tcntrl(i,nloop) = 'R'
                end if
                ictrloop(i,nloop) = ind
              end if

c ----------- check (if ctrl var is integer, then start and step must be
c             integer too):
              if ((tcntrl(1,nloop) .eq. 'I') .and.
     &           ((tcntrl(2,nloop) .eq. 'R') .or.
     &            (tcntrl(4,nloop) .eq. 'R'))) then
                write(iolog,'(a/2a)')
     &          'doenddo_E> real/int clash in loop variables',
     &          '           in TOP line: ',
     &          action(indxca)(1:lenr2(action(indxca)))
                stop
              end if

            end do

          end if
        end do
        return
      end


      subroutine lblgoto
        implicit none
#include "top.cmn"
        integer lenr,ir,indexwr,il
        external lenr, indexwr

        nlbl = 0
        do  indxca = 1, nactions
c ------- top.ini specific:
          if (iaction(indxca) .eq. 4) then
c --------- top.ini specific:
            il = indexwr(action(indxca), cmdlst(4))
            ir = lenr(action(indxca))
            if (ir.le.il) then
              write(iolog,'(2a/a,2i4)')
     &        'lblgoto_E> no label in: ', action(indxca),
     &        '           il, ir: ', il, ir
              stop
            end if
            if (il .gt. 0) then
              nlbl = nlbl + 1
              if (nlbl .gt. maxlbl) then
                write(iolog,'(a)')
     &          'lblgoto_E> too many labels; increase MAXLBL'
                stop
              end if
              call str_s(action(indxca)(il+1:ir), lbl(nlbl))
              ilbl(nlbl) = indxca
            end if
          end if
        end do
        return
      end



      subroutine rditop
      implicit none
#include "top.cmn"
      integer nvalues,iend,i,lr,indc,lenr,iend2,ie,indexb,indx,ierr
      integer lenr2
      character line*(lenact)
      logical iowr, cmpr
      external lenr, indexb, iowr, lenr2

      do  i = 1, mtypcom
        cmdlst(i) = 'undefinedC'
      end do
      do  i = 1, mrprm
        prmrlst(i) = 'undefinedR'
      end do
      do  i = 1, miprm
        prmilst(i) = 'undefinedI'
      end do
      do  i = 1, msprm
        prmslst(i) = 'undefinedS'
      end do
      do  i = 1, mlprm
        prmllst(i) = 'undefinedL'
      end do

      call openf4(ioinp,inifil,'OLD','SEQUENTIAL','FORMATTED',3,.true.,
     &            ierr,cmpr,iolog)
      read(ioinp, '(a)') line

c --- read the command names
      ntypcom = 0
20    read(ioinp, '(a)') line
        if (line(5:13).eq.'KEYWORDS:') go to 30
        read(line, '(i4)') indx
        ntypcom = max(ntypcom, indx)
        if (ntypcom .gt. mtypcom) then
          write(iolog,'(a)') 'rditop__E> increase MTYPCOM'
          stop
        end if
        cmdlst(indx) = line(5:lenr(line))
        call ljust(cmdlst(indx))
        go to 20
30    continue

c --- read the keyword type, name (line 1) and then the number of dimensions and
c     default values themselves:
c --- FORMAT: i4, TYPE(a8), NAME,  NO OF DIMENSIONS,  DEFAULTS
c --- TYPE must be left justified, NAME must start in column 13:
c --- comment character is allowed, continuation character is not!

      nrprm = 0
      niprm = 0
      nsprm = 0
      nlprm = 0
40    read(ioinp, '(a)') line

        if (line(5:15).eq.'END OF FILE') go to 50

c ----- remove the comment part of the line, if there
        indc = index(line, commch)
        if (indc .gt. 0) then
          lr = lenr(line)
          do  i = indc, lr
            line(i:i) = ' '
          end do
        end if

        read(line, '(i4)') indx

        if (line(5:12) .eq.'REAL    ') then
          nrprm = max(nrprm, indx)
          if (nrprm .gt. mrprm) then
            write(iolog,'(a)') 'rditop__E> increase MRPRM'
            stop
          end if
          iend = indexb(line(13:))
          ie = index(line(13:), commch)
          if (ie .gt. 0) iend = ie - 1
          prmrlst(indx) = line(13:12+iend)
          call str_i(line(13+iend:37), nvrprm(indx), ierr)
          if (nvrprm(indx) .gt. 0) then
            chkr(indx) = .true.
          else
            chkr(indx) = .false.
          end if
          iend2 = indexb(line(13+iend:))
          call str_rn2(line(13+iend+iend2:),rprm(1,indx),mvrprm,nvalues,
     &                 ierr)
c-------- check the number of arguments, if fixed
          if (chkr(indx)) then
            if (nvalues .ne. nvrprm(indx)) then
              write(iolog,'(a,2i4/2a)')
     &        'rditop__E> # expected args <> # actual args: ',
     &        nvrprm(indx), nvalues,
     &        '           in line: ', line(1:lenr2(line))
              stop
            end if
          else
            nvrprm(indx) = nvalues
          end if
        end if

        if (line(5:12) .eq.'INTEGER ') then
          niprm = max(niprm,indx)
          if (niprm .gt. miprm) then
            write(iolog,'(a)') 'rditop__E> increase MIPRM'
            stop
          end if
          iend = indexb(line(13:))
          ie = index(line(13:), commch)
          if (ie .gt. 0) iend = ie - 1
          prmilst(indx) = line(13:12+iend)
          call str_i(line(13+iend:37), nviprm(indx), ierr)
          if (nviprm(indx) .gt. 0) then
            chki(indx) = .true.
          else
            chki(indx) = .false.
          end if
          iend2 = indexb(line(13+iend:))
          call str_in2(line(13+iend+iend2:),iprm(1,indx),mviprm,nvalues,
     &                 ierr)
c-------- check the number of arguments, if fixed
          if (chki(indx)) then
            if (nvalues .ne. nviprm(indx)) then
              write(iolog,'(a,2i4/2a)')
     &        'rditop__E> # expected args <> # actual args: ',
     &        nviprm(indx), nvalues,
     &        '           in line: ', line(1:lenr2(line))
              stop
            end if
          else
            nviprm(indx) = nvalues
          end if
        end if

        if (line(5:12) .eq.'STRING  ') then
          nsprm = max(nsprm,indx)
          if (nsprm .gt. msprm) then
            write(iolog,'(a)') 'rditop__E> increase MSPRM'
            stop
          end if
          iend = indexb(line(13:))
          ie = index(line(13:), commch)
          if (ie .gt. 0) iend = ie - 1
          prmslst(indx) = line(13:12+iend)
          call str_i(line(13+iend:37), nvsprm(indx), ierr)
          if (nvsprm(indx) .gt. 0) then
            chks(indx) = .true.
          else
            chks(indx) = .false.
          end if
          iend2 = indexb(line(13+iend:))
          call str_sn2(line(13+iend+iend2:),sprm(1,indx),mvsprm,nvalues)
          if (chks(indx)) then
            if (nvalues .gt. nvsprm(indx)) then
              write(iolog,'(a,2i4/2a)')
     &        'rditop__E> # expected args < # actual args: ',
     &        nvsprm(indx), nvalues,
     &        '           in line: ', line(1:lenr2(line))
              stop
            else
c ----------- blank assignments are allowed in a quick&dirty way:
              do  i = nvalues+1, nvsprm(indx)
                sprm(i,indx) = ' '
              end do
            end if
          else
            nvsprm(indx) = nvalues
          end if

c ------- deal with macros in strings:
          do  i = 1, nvalues
            call macros(sprm(i,indx))
          end do
        end if

        if (line(5:12) .eq.'LOGICAL ') then
          nlprm = max(nlprm,indx)
          if (nlprm .gt. mlprm) then
            write(iolog,'(a)') 'rditop__E> increase MLPRM'
            stop
          end if
          iend = indexb(line(13:))
          ie = index(line(13:), commch)
          if (ie .gt. 0) iend = ie - 1
          prmllst(indx) = line(13:12+iend)
          call str_i(line(13+iend:37), nvlprm(indx), ierr)
          if (nvlprm(indx) .gt. 0) then
            chkl(indx) = .true.
          else
            chkl(indx) = .false.
          end if
          iend2 = indexb(line(13+iend:))
          call str_ln2(line(13+iend+iend2:),lprm(1,indx),mvlprm,
     &                 nvalues,'O',ierr)
          if (chkl(indx)) then
            if (nvalues .ne. nvlprm(indx)) then
              write(iolog,'(a,2i5)')
     &        'rditop__E> # expected args <> # actual args: ',
     &        nvlprm(indx), nvalues,
     &        '           in line: ', line(1:lenr2(line))
              stop
            end if
          else
            nvlprm(indx) = nvalues
          end if
        end if

        go to 40

50    continue

      close(ioinp)

c --- no actions executed yet:
      do  i = 1, mtypcom
        called(i) = .false.
      end do

      if (iowr(2)) then
        write(iolog,'(a,5i4)')
     &        'rditop___> cmds,reals,ints,strs,logs: ',
     &        ntypcom, nrprm, niprm, nsprm, nlprm
      end if

      return
      end



      subroutine cmdexec(indexa, cmdlin)
        implicit none
#include "top.cmn"
        integer indexa,ierr
        character cmdlin*(*)

c ----- parse the command: assign values to variables, if any
        call assgn(indexa, cmdlin, .true.,ierr,.true.)

c ----- do anything application specific:
        call topappl1

c ----- in common block, you now have all the variables that you need for the
c       execution of the indexa-th action:

        go to (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
     &         22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
     &         40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,
     &         58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,
     &         76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,
     &         94,95,96,97,98,99,100) indexa
        write(iolog, '(a,i4)') 'cmdexec__E> wrong INDEXA: ',indexa
        stop

c ------- no action
1         continue
          go to 1000

c ------- SET: set parameters (already done above)
2         continue
          go to 1000

c ------- EXIT
3         call act3
          go to 1000

c ------- LABEL
4         continue
          go to 1000

c ------- GO_TO
5         call act5(cmdlin)
          go to 1000

c ------- DEFINE_INTEGER
6         continue
          go to 1000

c ------- DEFINE_REAL
7         continue
          go to 1000

c ------- END_DO of the DO loop
8         call act8
          go to 1000

c ------- DO loop, start
9         call act9
          go to 1000

c ------- CALL routine
10        call act10
          go to 1000

c ------- RESET TOP variables & ...
11        call act11
          go to 1000

c ------- WRITE TOP variables
12        call act12
          go to 1000

13        call act13
          go to 1000

14        call act14
          go to 1000

15        continue
          go to 1000

16        continue
          go to 1000

17        call act17
          go to 1000

18        call act18
          go to 1000

19        continue
          go to 1000

20        call act20
          go to 1000

21        call act21
          go to 1000

22        call act22
          go to 1000

23        call act23
          go to 1000

c ------- IF THEN ELSE
24        call act24
          go to 1000

25        call act25
          go to 1000

26        call act26
          go to 1000

27        call act27
          go to 1000

28        call act28
          go to 1000

29        call act29
          go to 1000

30        call act30
          go to 1000

31        call act31
          go to 1000

32        call act32
          go to 1000

33        call act33
          go to 1000

34        call act34
          go to 1000

35        call act35
          go to 1000

36        call act36
          go to 1000

37        call act37
          go to 1000

38        call act38
          go to 1000

39        call act39
          go to 1000

40        call act40
          go to 1000

41        call act41
          go to 1000

42        call act42
          go to 1000

43        call act43
          go to 1000

44        call act44
          go to 1000

45        call act45
          go to 1000

46        call act46
          go to 1000

47        call act47
          go to 1000

48        call act48
          go to 1000

49        call act49
          go to 1000

50        call act50
          go to 1000

51        call act51
          go to 1000

52        call act52
          go to 1000

53        call act53
          go to 1000

54        call act54
          go to 1000

55        call act55
          go to 1000

56        call act56
          go to 1000

57        call act57
          go to 1000

58        call act58
          go to 1000

59        call act59
          go to 1000

60        call act60
          go to 1000

61        call act61
          go to 1000

62        call act62
          go to 1000

63        call act63
          go to 1000

64        call act64
          go to 1000

65        call act65
          go to 1000

66        call act66
          go to 1000

67        call act67
          go to 1000

68        call act68
          go to 1000

69        call act69
          go to 1000

70        call act70
          go to 1000

71        call act71
          go to 1000

72        call act72
          go to 1000

73        call act73
          go to 1000

74        call act74
          go to 1000

75        call act75
          go to 1000

76        call act76
          go to 1000

77        call act77
          go to 1000

78        call act78
          go to 1000

79        call act79
          go to 1000

80        call act80
          go to 1000

81        call act81
          go to 1000

82        call act82
          go to 1000

83        call act83
          go to 1000

84        call act84
          go to 1000

85        call act85
          go to 1000

86        call act86
          go to 1000

87        call act87
          go to 1000

88        call act88
          go to 1000

89        call act89
          go to 1000

90        call act90
          go to 1000

91        call act91
          go to 1000

92        call act92
          go to 1000

93        call act93
          go to 1000

94        call act94
          go to 1000

95        call act95
          go to 1000

96        call act96
          go to 1000

97        call act97
          go to 1000

98        call act98
          go to 1000

99        call act99
          go to 1000

100       call act100
          go to 1000

1000    continue

c ----- do anything application specific:
        call topappl2
        return

      end



c --- in principle I could use rditop code and conventions (for arrays and
c     initializations too)
      subroutine defvar(prmlst,sprm,chk,nvprm,nprm,mprm)
        implicit none
#include "io.cst"
        integer mprm,nprm,nvprm(mprm)
        character prmlst(mprm)*(*), sprm*(*)
        logical chk(mprm)

c ----- new no of variables:
        nprm = nprm + 1
        if (nprm .gt. mprm) then
          write(iolog,'(a)') 'defvar__E> increase MPRM'
          stop
        end if
c ----- name:
        prmlst(nprm) = sprm

c ----- variable no of dimensions
        chk(nprm) = .false.
c ----- no of dimensions
        nvprm(nprm) = 0

        return
      end


c --- define integer, real, string or logical variables
      subroutine defv
        implicit none
#include "top.cmn"
        integer i, ierr

        do  indxca = 1, nactions

c ------- top.ini specific
          if (iaction(indxca) .eq. 6) then
c --------- top.ini specific
            call assgn(9999,action(indxca),.true.,ierr,.true.)
            do  i = 1, nvsprm(2)
c ----------- top.ini specific
              call defvar(prmilst,sprm(i,2),chki,nviprm,niprm,miprm)
            end do
          else
            if (iaction(indxca) .eq. 7) then
              call assgn(9999,action(indxca),.true.,ierr,.true.)
              do  i = 1, nvsprm(2)
                call defvar(prmrlst,sprm(i,2),chkr,nvrprm,nrprm,mrprm)
              end do
            else
              if (iaction(indxca) .eq. 15) then
                call assgn(9999,action(indxca),.true.,ierr,.true.)
                do  i = 1, nvsprm(2)
                  call defvar(prmslst,sprm(i,2),chks,nvsprm,nsprm,msprm)
                end do
              else
                if (iaction(indxca) .eq. 16) then
                  call assgn(9999,action(indxca),.true.,ierr,.true.)
                  do  i = 1, nvsprm(2)
                    call defvar(prmllst,sprm(i,2),chkl,nvlprm,nlprm,
     &                          mlprm)
                  end do
                end if
              end if
            end if
          end if
        end do

        return
      end


      subroutine compile
        implicit none
#include "top.cmn"
        integer indact, ierr
        logical err

c ----- process the actions: assign an index to each action and see if
c       you can assign them in advance to reduce waiting time before reporting
c       syntactic errors in the top1 program:
        do  indxca = 1, nactions
c ------- action code
          iaction(indxca) = indact(action(indxca))
        end do

c ----- define variables
        call defv

c ----- process the goto and label statements (incl those from routines)
c       (they are global)
        call lblgoto

c ----- process the do and end_do statements (incl those from routines)
c       (they are global)
        call doenddo

c ----- parse the command: assign values to variables, if any
        err = .false.
        do  indxca = 1, nactions
          call assgn(iaction(indxca),action(indxca),
     &               .false.,ierr,.false.)
          err = err .or. ierr.gt.0
        end do

        if (err) then
         write(iolog,'(a)')
     &   'compile_E> please correct the errors reported above and rerun'
         stop
        end if

        return
      end


c --- parse the command line to assign the values to the variables
      subroutine assgn(indact, cmdlin, stp, ierr, ass)
        implicit none
c
c ----- characters in the separators for the assignment command:
c
c       VARIABLE varelm ELEM1 elmelm ELEM2 ... elmelm ELEMN varvar):
c
c          where ELEMi can itself be a vector in which case its
c          elements are assigned to VARIABLE and VARIABLE is then
c          a vector with more than N components.
c
c       (any number of blanks can also be added anywhere)
c       (any number of such commands can be on one line; the last varvar on
c        the  line is optional)
c       (elmelm must be a blank)
c
#include "top.cmn"
        integer ival(mviprm),indxe,ielmtp,i,nvalues,lr,ivarvar,indexr
        integer indx,ivartp,lenr,lencmd,ll,indact,indexs,nv,iv,nve
        integer lenr2,ierr
        real rval(mvrprm)
        character cmdlin*(*), cmdlin2*(lenact)
        character varnam*(lenkey), sval(mvsprm)*(lensprm)
        character buffr(mvsprm)*(lensprm),elm2*(lensprm)
c        character buffr(max(mvrprm,mviprm,mvsprm,mvlprm))*(max(lensprm,20))
        logical lval(mvlprm), symass, stp, ass
        external lenr, lenr2, indexr, indexs

        ierr = 0
        cmdlin2 = cmdlin(max(1,index(cmdlin,' ')):)

c        if (mvsprm .ne. max(mvrprm,mviprm,mvsprm,mvlprm))
c     &    write(iolog,'(a, 4i5)')
c     &    'assgn___w> there may be too many F,I,L elms for BUFFR: ',
c     &    mvrprm,mviprm,mvsprm,mvlprm
c
c        if (lensprm .lt. 20)
c     &    write(iolog,'(a)')
c     &    'assgn___w> there may be too long F,I,L elms for BUFFR'

c ----- no assignments at all in parsing for:
c       DEFINE_REAL/INTEGER/STRING/LOGICAL,
c       DO and END_DO, GO_TO and LABEL, and STOP
c       END_SUBROUTINE
c ----- top.ini specific
        if (((indact.ge.3).and.(indact.le.9)) .or.
     &      ((indact.ge.15).and.(indact.le.16)) .or.
     &       (indact.eq.18)) return

        ll = 1
        lencmd = lenr(cmdlin2)
10      if (ll .ge. lencmd) go to 100
c ------- get the name of the variable:
          call str_s(cmdlin2(ll:), varnam)

c ------- get the type of the variable and its index in TOP1.INI:
          call vartyp(ivartp,varnam,indx)

          if (ivartp .eq. 0) then
            write(iolog,'(2a/2a)')
     &      'assgn___E> variable name not recognized: ', varnam,
     &      '           in CMD: ', cmdlin(1:lenr2(cmdlin))
            ierr = 1
            if (stp) then 
              stop
            else
              return
            end if
          end if

c ------- get the values/scalar variables assigned to the elements of the
c         current variable

c ------- find the first position of the first value/variable
          ll = ll + indexr(cmdlin2(ll:), varnam(1:lenr(varnam)))
20        if (ll .gt. lencmd) go to 300
            if (varvar .eq. cmdlin2(ll:ll)) go to 300
            if((blankc.ne.cmdlin2(ll:ll)).and.
     &         (varelm.ne.cmdlin2(ll:ll))) go to 30
            ll = ll + 1
          go to 20

300       continue
          write(iolog,'(2a/2a)')
     &    'assgn___E> no values for variable: ', varnam,
     &    '           in CMD: ', cmdlin(1:lenr2(cmdlin))
          ierr = 1
          if (stp) then 
            stop
          else
            return
          end if

30        continue

c ------- find the last position of the value/variable assigned to the
c         last element of the current variable
          ivarvar = indexs(cmdlin2(ll:), varvar)
          if (ivarvar .gt. 0) then
            lr = ll + ivarvar-2
          else
            lr = lencmd
          end if
          if (lr.lt.ll) then
            write(iolog,'(a/2a)') 'assgn___E> internal error 1',
     &      '           in CMD: ', cmdlin(1:lenr2(cmdlin))
            ierr = 1
            if (stp) then 
              stop
            else
              return
            end if
          end if

c ------- extract the elements as strings (delimited with 'elem' optionally):
c ------- omit unprotected primes from the values; protected primes \'
c         into primes '
          call str_sn6(cmdlin2(ll:lr), buffr, mvsprm, nvalues)

c ------- process each element:
          nv = 0
          do  i = 1, nvalues

c --------- get: the type: ielmtp = 1,2,3,4
c                the variable/value flag: indxe > 0 if variable (top.ini indx)
c                                                 0 if value
c                the value: ival, rval, sval, lval (only if indxe=0)
c
            call etype(buffr(i),elm2,ielmtp,indxe,ival,rval,sval,lval,
     &                 nve)

c --------- assign and check the types:
            go to (21,22,23,24) ivartp
            write(iolog,'(a,i4/2a)')'assgn___E> wrong IVARTP: ',ivartp,
     &      '           in CMD: ', cmdlin(1:lenr2(cmdlin))
            ierr = 1
            if (stp) then 
              stop
            else
              return
            end if

21            continue
                go to (51,52,53,53) ielmtp
                write(iolog,'(a,i4/2a)')'assgn___E> wrong IELMTP: ',
     &          ielmtp,
     &          '           in CMD: ',cmdlin(1:lenr2(cmdlin))
                ierr = 1
                if (stp) then 
                  stop
                else
                  return
                end if
51              do  iv = 1, nve
                  nv = nv + 1
                  if (ass) rprm(nv,indx) = rval(iv)
                end do
                go to 25
52              do  iv = 1, nve
                  nv = nv + 1
                  if (ass) rprm(nv,indx) = ival(iv)
                end do
                go to 25
53              write(iolog,'(2a/2a,1x,i4/2a)')
     &          'assgn___E> wrong element type for variable: ',varnam,
     &          '           element, element type: ',
     &                      buffr(i)(1:lenr2(buffr(i))),ielmtp,
     &          '           in CMD: ', cmdlin(1:lenr2(cmdlin))
                ierr = 1
                if (stp) then
                  stop
                else
                  return
                end if

22            continue
                go to (61,62,63,63) ielmtp
                write(iolog,'(a,i4/2a)')
     &          'assgn___E> wrong IELMTP: ',ielmtp,
     &          '           in CMD: ', cmdlin(1:lenr2(cmdlin))
                ierr = 1
                if (stp) then
                  stop
                else
                  return
                end if
61              do  iv = 1, nve
                  nv = nv + 1
                  if (ass) iprm(nv,indx) = nint(rval(iv))
                end do
                go to 25
62              do  iv = 1, nve
                  nv = nv + 1
                  if (ass) iprm(nv,indx) = ival(iv)
                end do
                go to 25
63              write(iolog,'(2a/2a,1x,i4/2a)')
     &          'assgn___E> wrong element type for variable: ',varnam,
     &          '           element, element type: ',
     &                      buffr(i)(1:lenr2(buffr(i))),ielmtp,
     &          '           in CMD: ', cmdlin(1:lenr2(cmdlin))
                ierr = 1
                if (stp) then
                  stop
                else
                  return
                end if


23            continue

c ----------- different behaviour in OPERATE for a RESULT variable:
c             the name of a string variable is assigned, not its contents!
c             top.ini specific

              if (symass(indact, indx)) then

                if (indxe .gt. 0) then
                  nv = nv + 1
                  if (ass) sprm(nv,indx) = elm2
                else
                  write(iolog,'(2a/2a)')
     &            'assgn___E> cannot assign a value to a value: ', 
     &            elm2(1:lenr2(elm2)),
     &            '           in CMD: ', cmdlin(1:lenr2(cmdlin))
                  ierr = 1
                  if (stp) then
                    stop
                  else
                    return
                  end if
                end if

              else

c ------------- is it a variable or a value:
                if (indxe .gt. 0) then
                  if (ielmtp .eq. 3) then
                    do  iv = 1, nve
                      nv = nv + 1
c ------------------- assign the contents of the named string variable
                      if (ass) sprm(nv,indx) = sval(iv)
                    end do
                  else
                    if (ielmtp .eq. 4) then
                      do  iv = 1, nve
                        nv = nv + 1
                        if (lval(iv)) then
                          if (ass) sprm(nv,indx) = 'ON'
                        else
                          if (ass) sprm(nv,indx) = 'OFF'
                        end if
                      end do
                    else
                      if (ielmtp .eq. 2) then
                        do  iv = 1, nve
                          nv = nv + 1
c ----------------------- top.ini specific
                          if (ass) call i_str(ival(iv),sprm(nv,indx),
     &                             iprm(1,4))
                        end do
                      else
                        do  iv = 1, nve
                          nv = nv + 1
c ----------------------- top.ini specific
                          if (ass) call r_str(rval(iv),sprm(nv,indx),
     &                             iprm(1,4),iprm(2,4))
                        end do
                      end if
                    end if
                  end if
                else
c --------------- any value can be assigned to a string variable
                  nv = nv + 1
                  if (ass) then
                    sprm(nv,indx) = sval(1)
                    call macros(sprm(nv,indx))
                  end if
                end if
              end if
              go to 25


24            continue
              go to (82,82,82,81) ielmtp
                write(iolog,'(a,i4/2a)')
     &          'assgn___E> wrong IELMTP: ',ielmtp,
     &          '           in CMD: ', cmdlin(1:lenr2(cmdlin))
                ierr = 1
                if (stp) then
                  stop
                else
                  return
                end if
81              do  iv = 1, nve
                  nv = nv + 1
                  if (ass) lprm(nv,indx) = lval(iv)
                end do
                go to 25
82              write(iolog,'(2a/2a,1x,i4/2a)')
     &          'assgn___E> wrong element type for variable: ',varnam,
     &          '           element, element type: ',
     &                      buffr(i)(1:lenr2(buffr(i))),ielmtp,
     &          '           in CMD: ', cmdlin(1:lenr2(cmdlin))
                ierr = 1
                if (stp) then
                  stop
                else
                  return
                end if

25          continue

          end do

c ------- check and assign the number of elements for the current variable
          go to (11,12,13,14) ivartp
            write(iolog,'(a,i4/2a)')'assgn___E> wrong IVARTP: ',ivartp,
     &      '           in CMD: ', cmdlin(1:lenr2(cmdlin))
            ierr = 1
            if (stp) then
              stop
            else
              return
            end if
11          call chknum(nv,chkr(indx),nvrprm(indx),cmdlin2,mvrprm,ass)
            go to 15
12          call chknum(nv,chki(indx),nviprm(indx),cmdlin2,mviprm,ass)
            go to 15
c --------- to allow for empty string assignments in variables with
c           fixed number of elements
13          if ((nv .lt. nvsprm(indx)) .and. chks(indx)) then
              do  i = nv+1, nvsprm(indx)
                if (ass) sprm(i,indx) = ' '
              end do
              nv = nvsprm(indx)
            end if
            call chknum(nv,chks(indx),nvsprm(indx),cmdlin2,mvsprm,ass)
            go to 15
14          call chknum(nv,chkl(indx),nvlprm(indx),cmdlin2,mvlprm,ass)
15        continue

c ------- skip the var/var delimiter!
          ll = lr + 2
          go to 10

100     continue

c ----- special processing
        if (ass) call spec_prm

        return

      end


      subroutine vartyp(ivtype,varnam,indx)
        implicit none
#include "top.cmn"
        integer i,indx,indexw,ivtype
        character varnam*(*)

        ivtype = 0

c ----- is it a real variable:
        do  i = 1, nrprm
          if (indexw(prmrlst(i), varnam) .ge. 1) then
            ivtype = 1
            indx = i
            return
          end if
        end do

c ----- is it an integer variable:
        do  i = 1, niprm
          if (indexw(prmilst(i), varnam) .ge. 1) then
            ivtype = 2
            indx = i
            return
          end if
        end do

c ----- is it a string variable:
        do  i = 1, nsprm
          if (indexw(prmslst(i), varnam) .ge. 1) then
            ivtype = 3
            indx = i
            return
          end if
        end do

c ----- is it a logical variable:
        do  i = 1, nlprm
          if (indexw(prmllst(i), varnam) .ge. 1) then
            ivtype = 4
            indx = i
            return
          end if
        end do

        return
      end


      subroutine chknum(nvalues,chk,nvprm,cmdlin,mvprm,ass)
        implicit none
#include "io.cst"
        integer nvalues,nvprm,mvprm,lenr2
        character cmdlin*(*)
        logical chk,ass
        external lenr2

        if (mvprm .lt. nvalues) then
          write(iolog,'(a,i4)')
     &    'chknum__E> too many elements; increase MVPRM: ',nvalues
          stop
        end if

c ----- check the number of arguments, if fixed (only works if assignemnt done)
        if (chk .and. ass) then
          if (nvalues .ne. nvprm) then
            write(iolog,'(a,2i5/2a)')
     &      'chknum__E> # expected arguments <> # actual args: ',
     &      nvprm, nvalues,
     &      '           in line: ', cmdlin(1:lenr2(cmdlin))
          end if
        else
          if (ass) nvprm = nvalues
        end if

        return
      end


c --- Input : elm   ... string whose value is sought
c
c     Output: elm2  ... variable name without -, if a variable; blank otherwise
c             iet   ... type of a variable (real,integer,string,logical)
c             indx  ... if symbolic, its indx in ?prm(1,indx); 0 otherwise
c             ival  ... value, if integer; otherwise undefined
c             rval  ... value, if real; otherwise undefined
c             sval  ... always equal to elm;
c             lval  ... value, if logical; otherwise undefined
c             nve   ... dimension of ival,rval,lval,sval
c
      subroutine etype(elm,elm2,iet,indx,ival,rval,sval,lval,nve)
        implicit none
#include "top.cmn"
        integer indx, ival(mviprm), ierr, iet, il, nve
        integer ir, imin, lenl, lenr, is, i
        real rval(mvrprm),sgn,fp
        character elm*(*),sval(mvsprm)*(*),elm2*(*)
        logical lval(mvlprm), equal

c ----- to simplify number --> string conversions and the code in str_i for
c       reading strings like '28file.ext'
        sval(1) = elm
        nve = 1
        indx = 0

c ----- if the first character is '-' then remember the sign
        imin = index(elm, '-')

c ----- Checks:
        il = lenl(elm)
        ir = lenr(elm)

c ----- is it only a '-' sign in the element, or an empty string?
        if (((imin .eq. ir).and.(il.eq.ir)).or.(ir.eq.0)) then
c ------- a string, by definition:
          iet = 3
          return
        end if

        if (imin .eq. il) then
          sgn = -1.0
          is  =  imin + 1
        else
          sgn =  1.0
          is  =  1
        end if

c ----- is it any of the variable names:
        call vartyp(iet,elm(is:),indx)
        if (iet .gt. 0) then
          elm2 = elm(is:)
          go to (1,2,3,4) iet
          write(iolog,'(a,i4)')'etype____E> wrong IET: ',iet
          stop
1           nve = nvrprm(indx)
            do  i = 1, nve
              rval(i) = sgn*rprm(i,indx)
            end do
            return
2           nve = nviprm(indx)
            do  i = 1, nve
              ival(i) = nint(sgn)*iprm(i,indx)
            end do
            return
3           nve = nvsprm(indx)
            do  i = 1, nve
              sval(i) = sprm(i,indx)
            end do
            return
4           nve = nvlprm(indx)
            do  i = 1, nve
              lval(i) = lprm(i,indx)
            end do
            return
        else
          call blank(elm2)
        end if


c ----- No, so it must be the actual value; but which?

        call str_i2(elm, ival(1), ierr)
        if (ierr .eq. 0) then
          call str_r2(elm, rval(1), ierr)
          fp = ival(1)
          if (equal(fp, rval(1)) .and.
     &        (index(elm, '.').lt.1)) then
c --------- Integer:
            iet = 2
          else
c --------- Real:
            iet = 1
          end if
          return
        else
c ------- only for [-].nnnnn situations:
          call str_r2(elm, rval(1), ierr)
          iet = 1
          if (ierr .eq. 0) return
        end if

        call str_l(elm, lval(1), 'O',ierr)
        iet = 4
        if (ierr .eq. 0) return

c ----- If it is not any of the above, it must be a string:
c        sval(1) = elm
        iet = 3

        return
      end



c --- go to statement
      subroutine act5(cmdlin)
        implicit none
#include "top.cmn"
        integer ig, il, indexwr, ifind2word
        character cmdlin*(*)

        ig = indexwr(cmdlin, cmdlst(5))
        if (ig .lt. 1) then
          write(iolog,'(a)') 'act5___E> no GO_TO code in the command'
          stop
        end if
        il = ifind2word(cmdlin(ig+1:), lbl, nlbl)
        if (il .lt. 1) then
          write(iolog,'(2a)') 'act5____E> label not found: ',
     &               cmdlin(ig+1:)
          stop
        else
          indxca = ilbl(il)
        end if
c ----- top.ini specific
        called(5) = .true.

        return
      end


c --- call subroutine
      subroutine act10
        implicit none
#include "top.cmn"
        integer ifind2word, irout

c ----- top.ini specific
        irout            = ifind2word(sprm(1,3), rout, nrout)
        if (irout .lt. 1) then
          write(iolog,'(2a)') 'act10___E> no such routine: ', sprm(1,3)
          stop
        end if
        indlev(ilevel)   = indxca
        ilevel           = ilevel + 1
        indxca           = indrout(1,irout)
c ----- top.ini specific
        called(10) = .true.

        return
      end



c --- end_do statement
      subroutine act8
        implicit none
#include "top.cmn"
        integer iloop, i1, i3, i4, indenddo
        real v1, v3, v4, selct

c ----- top.ini specific
        called(8) = .true.

c ----- current loop index in the list of loops (have to scan all to allow
c       jumping out of loops by GO_TO; i.e. loop stack does not work)
        iloop = indenddo(iaction, indxca)

        i1 = ictrloop(1,iloop)
        i3 = ictrloop(3,iloop)
        i4 = ictrloop(4,iloop)

        v1 = selct(1, i1, iloop)
        v3 = selct(3, i3, iloop)
        v4 = selct(4, i4, iloop)

c ----- new value of the loop control variable
        v1 = v1 + v4
        if (tcntrl(1,iloop) .eq. 'I') then
          iprm(1,i1) = nint(v1)
        else
          rprm(1,i1) = v1
        end if

c ----- is the loop finished:
        if (v4 .gt. 0) then
         if((v1-tsmall).gt.v3) go to 100
        else
         if((v1+tsmall).lt.v3) go to 100
        end if

c ----- loop has not finished yet
c ----- index of the next statement (not executing DO statement this time)
        indxca = ibegloop(iloop)
        return

c ----- loop has finished, continue with the next statement
100     continue
        return

      end

      real function selct(it, i, iloop)
        implicit none
#include "top.cmn"
        integer it, i, iloop
        if (tcntrl(it, iloop) .eq. 'I') then
          selct = iprm(1,i)
        else
          selct = rprm(1,i)
        end if
        return
      end

c --- do statement
      subroutine act9
        implicit none
#include "top.cmn"
        integer inddo,iloop
        integer i1, i2
        real v2, selct

c ----- top.ini specific
        called(9) = .true.

c ----- loop index
c ----- top.ini specific
        iloop = inddo(iaction, indxca)

        i1 = ictrloop(1,iloop)
        i2 = ictrloop(2,iloop)
        v2 = selct(2, i2, iloop)

c ----- starting value of the loop control variable
        if (tcntrl(1,iloop) .eq. 'I') then
          iprm(1,i1) = nint(v2)
        else
          rprm(1,i1) = v2
        end if

        return
      end


      integer function inddo(iaction, indxca)
        implicit none
        integer indxca,iaction(indxca),ind,i
        ind = 0
        do  i = 1, indxca
          if (iaction(i) .eq. 9) ind = ind + 1
        end do
        inddo = ind
        return
      end

      integer function indenddo(iaction, indxca)
        implicit none
#include "top.cst"
        integer indxca,iaction(indxca),i,j,ind,ndolp
        logical dolp(maxloop)

c ----- each end_do must be matched with the last un-matched DO above it:

        ndolp = 0
        ind   = 0
        do  i = 1, indxca
c ------- top.ini specific
          if (iaction(i) .eq. 9) then
            ndolp = ndolp + 1
c --------- this DO is not matched yet:
            dolp(ndolp) = .true.
          end if
          if (iaction(i) .eq. 8) then
c --------- match this end_do with the last un-matched DO above it:
            do  j = ndolp, 1, -1
              if (dolp(j)) then
                ind = j
                dolp(j) = .false.
                go to 10
              end if
            end do
            write(iolog,'(a)')
     &      'indendd_E> cannot have END_DO before free DO'
            stop
10          continue
          end if
        end do

        indenddo = ind

        return
      end


      subroutine act3
        implicit none
#include "top.cmn"
        logical iowr
c ----- top.ini specific
        if (iowr(2))
     &    write(iolog,'(a)') 'TOP______> EXIT(0)'
        call finish
        called(3) = .true.
        stop
      end


      subroutine act11
        implicit none
#include "top.cmn"
        call rditop
c ----- top.ini specific
        called(11) = .true.
        return
      end


c --- write statement
      subroutine act12
        implicit none
#include "top.cmn"
        integer i, lenr2
        external lenr2

        if (iprm(1,1) .ge. nio1 .and. iprm(1,1) .le. nio2) then
          if (.not. ioopnd(iprm(1,1)-nio1+1)) then
            write(iolog, '(a,i6)') 'act12____E> I/O stream not opened',
     &            iprm(1,1)
            stop
          end if
        else
          write(iolog, '(a,i6)') 'act12____E> I/O stream out of range',
     &          iprm(1,1)
          stop
        end if

c ----- top.ini specific
        write(iprm(1,1), '(999(a,1x))')
     &       (sprm(i,1)(1:lenr2(sprm(i,1))),i=1,nvsprm(1))
c        close(iprm(1,1))

c ----- top.ini specific
        called(12) = .true.

        return
      end


      logical function commnt(line, cmmch)
        implicit none
        integer ic, lenr
        character line*(*), cmmch*(*)

        commnt = .false.

        ic = index(line, cmmch)

        if (ic .eq. 1) then
          commnt = .true.
        else
          if (ic .gt. 1) then
            if (lenr(line(1:ic-1)) .eq. 0) commnt = .true.
          end if
        end if

        return
      end


c --- math operations on real numbers
      subroutine act13
        implicit none
        integer nopr
        parameter (nopr= 5)
#include "top.cmn"
        integer ifind5word
        integer iet,ind,ival(mviprm),nve,imath
        real rval(mvrprm),rvar
        character oprlst(nopr)*(lensprm),sval(mvsprm)*(lensprm)
        character elm2*(lensprm)
        logical lval(mvlprm)
        external ifind5word
        data oprlst / 'SUM', 'MULTIPLY', 'DIVIDE', 'POWER', 'MOD'/

c ----- the whole routine is very top.ini specific


c ----- Check:
        if (nvsprm(8) .ne. 1) then
          write(iolog,'(a,i4)')
     &    'act13___E> not 1 result variable: ', nvsprm(8)
          stop
        end if

        call etype(sprm(1,8),elm2,iet,ind,ival,rval,sval,lval,nve)

        call upper(sprm(1,7))
        imath = ifind5word(sprm(1,7),oprlst,nopr)

c ----- Check:
        if ((iet .ne. 1) .and. (iet .ne. 2)) then
          write(iolog,'(a,i4)')
     &    'act13___E> result variable not REAL or INTEGER: ', iet
          stop
        end if

        go to (1,2,3,4,5) imath
        write(iolog,'(a,i4)')'act13____E> wrong IMATH: ',imath
        stop

1       continue
          call sumf(rprm(1,1),nvrprm(1),rvar)
          go to 100

2       continue
          call prodf(rprm(1,1),nvrprm(1),rvar)
          go to 100

3       continue
          call divf(rprm(1,1),nvrprm(1),rvar)
          go to 100

4       continue
          call expf(rprm(1,1),nvrprm(1),rvar)
          go to 100

5       continue
          call modf(rprm(1,1),nvrprm(1),rvar)

100     continue

        if (iet .eq. 2) then
          iprm(1,ind) = rvar
          nviprm(ind) = 1
        else
          rprm(1,ind) = rvar
          nvrprm(ind) = 1
        end if

        called(13) = .true.

        return
      end


      subroutine act14
        implicit none
#include "top.cmn"
        integer nopr
        parameter (nopr= 1)
        integer imath,ifind2word,iet,ind,ival(mviprm),nve
        real rval(mvrprm)
        character oprlst(nopr)*(lensprm),sval(mvsprm)*(lensprm)
        character elm2*(lensprm)
        logical lval(mvlprm)
        data (oprlst(imath),imath=1,nopr)
     &       / 'CONCATENATE' /

c ----- the whole routine is very top.ini specific


c ----- Check:
        if (nvsprm(8) .ne. 1) then
          write(iolog,'(a,i4)')
     &    'act14____E> not 1 result variable: ', nvsprm(8)
          stop
        end if

        call etype(sprm(1,8),elm2,iet,ind,ival,rval,sval,lval,nve)

        imath = ifind2word(sprm(1,7),oprlst,nopr)

c ----- Check:
        if (iet .ne. 3) then
          write(iolog,'(a,i4)')
     &    'act14____E> result variable not STRING: ', iet
          stop
        end if

        go to (1) imath
        write(iolog,'(a,i4/2a)')
     &    'act14____E> wrong IMATH: ',imath,
     &    '            sprm(1,7): ', sprm(1,7)
        stop

1       continue
c ------- always put into the first element of the string variable
          call concat3(sprm(1,9),nvsprm(9),sprm(1,ind))
c ------- replace all dummy '@' characters with blanks:
          call subs2(sprm(1,ind), '@', ' ')
          nvsprm(ind) = 1
          go to 100

100     continue

        called(14) = .true.

        return
      end


c --- SUBROUTINE
      subroutine act17
        implicit none
#include "top.cmn"
        integer irout,ifind2word,ierr

c ----- if in the main program: possibly jump over a subroutine definition:
c ----- top.ini specific
        if (ilevel .eq. 1) then
c ------- top.ini specific
          call assgn(iaction(indxca), action(indxca),
     &               .true.,ierr,.true.)
          irout = ifind2word(sprm(1,3),rout,nrout)
          indxca = indrout(2, irout)
        else
          write(iolog,'(a)') 'act17___E> SUBROUTINE nesting not allowed'
          stop
        end if

        called(17) = .true.

        return
      end


c --- END_SUBROUTINE
      subroutine act18
        implicit none
#include "top.cmn"

c ----- return properly from the subroutine, if the end reached
c       (the main program goes from 1 to nactions; the subroutines
c       go from SUBROUTINE to END_SUBROUTINE or RETURN)

c ----- exiting from the current routine
        ilevel = ilevel - 1
        indxca = indlev(ilevel)

        called(18) = .true.

        return
      end



c --- RETURN
      subroutine act20
        implicit none
#include "top.cmn"

c ----- exiting from the current routine
        ilevel = ilevel - 1
        indxca = indlev(ilevel)

        called(20) = .true.

        return
      end


c --- read statement
      subroutine act21
        implicit none
#include "top.cmn"

c ----- top.ini specific

        if (iprm(1,1) .ge. nio1 .and. iprm(1,1) .le. nio2) then
          if (.not. ioopnd(iprm(1,1)-nio1+1)) then
            write(iolog, '(a,i6)') 'act21____E> I/O stream not opened',
     &            iprm(1,1)
            stop
          end if
        else
          write(iolog, '(a,i6)') 'act21____E> I/O stream out of range',
     &          iprm(1,1)
          stop
        end if

c ----- read the line into a string scalar
        read(iprm(1,1), '(a)') sprm(1,13)

c ----- top.ini specific
        called(21) = .true.

        return
      end



c --- open statement
      subroutine act22
        implicit none
#include "top.cmn"
        integer ierr
        logical cmpr

        if (iprm(1,1) .lt. nio1 .or. iprm(1,1) .gt. nio2) then
          write(iolog, '(a,i6)') 'act22____E> I/O stream out of range',
     &          iprm(1,1)
          stop
        end if

c ----- top.ini specific
        call gennam2(sprm(1,4),sprm(1,6),'TOP',iprm(1,2),
     &               iprm(1,3),sprm(1,10),sprm(1,17))
        call prefix(sprm(1,18), sprm(1,10))
        call openf4(iprm(1,1), sprm(1,10), sprm(1,20), sprm(1,19),
     &              'FORMATTED',3,.true.,ierr,cmpr,iolog)
        ioopnd(iprm(1,1)-nio1+1) = .true.

c ----- top.ini specific
        called(22) = .true.

        return
      end

c --- close a file
      subroutine act23
        implicit none
#include "top.cmn"

        close(iprm(1,1))
        called(23) = .true.

        return
      end

      subroutine act24
        implicit none
#include "top.cmn"
        integer indact,inda
        logical cond

c ----- top.ini specific

        call upper(sprm(1,7))
        cond = .false.
        if (sprm(1,7) .eq. 'NE') then
          if (abs(rprm(1,1)-rprm(2,1)) .gt. 1.0e-10) cond = .true.
        end if
        if (sprm(1,7) .eq. 'EQ') then
          if (abs(rprm(1,1)-rprm(2,1)) .lt. 1.0e-10) cond = .true.
        end if
        if (sprm(1,7) .eq. 'GT') then
          if (rprm(1,1).gt.rprm(2,1))  cond = .true.
        end if
        if (sprm(1,7) .eq. 'LT') then
          if (rprm(1,1).lt.rprm(2,1))  cond = .true.
        end if
        if (sprm(1,7) .eq. 'GE') then
          if (rprm(1,1).ge.rprm(2,1)-1.0e-10)  cond = .true.
        end if
        if (sprm(1,7) .eq. 'LE') then
          if (rprm(1,1).le.rprm(2,1)+1.0e-10)  cond = .true.
        end if

c        write(iolog,*) sprm(1,7), cond, rprm(1,1), rprm(2,1)
c        write(iolog,*) 'then=',sprm(1,14)
c        write(iolog,*) 'else=',sprm(1,15)

        if (cond) then
          inda = indact(sprm(1,14))
          if (inda.gt.1) call cmdexec(inda, sprm(1,14))
        else
          inda = indact(sprm(1,15))
          if (inda.gt.1) call cmdexec(inda, sprm(1,15))
        end if

        called(24) = .true.

        return
      end


c --- write all TOP statements
      subroutine act25
        implicit none
#include "top.cmn"
        integer ierr,i,lenr2
        logical cmpr
        external lenr2

        call gennam2(sprm(1,4),sprm(1,6),'top',iprm(1,2),
     &               iprm(1,3),sprm(1,12),sprm(1,17))
        call prefix(sprm(1,18), sprm(1,12))
        call openf4(ioinp, sprm(1,12), 'UNKNOWN', sprm(1,19),
     &              'FORMATTED', 3, .true., ierr, cmpr, iolog)
        do  i = 1, nactions
          write(ioinp, '(a)') action(i)(1:lenr2(action(i)))
        end do
        close(ioinp)
        called(25) = .true.

        return
      end
      subroutine act26
        implicit none
#include "top.cmn"

        call mysystem(sprm(1,16))
        called(26) = .true.

        return
      end


      subroutine act27
        implicit none
        logical filexs
#include "top.cmn"

        call gennam2(sprm(1,4),sprm(1,6),'.B',iprm(1,2),
     &              iprm(1,3),sprm(1,12),sprm(1,17))
        call prefix(sprm(1,18), sprm(1,12))

        if (filexs(sprm(1,12))) then
          iprm(1,5) = 1
        else
          iprm(1,5) = 0
        end if

        called(27) = .true.

        return
      end



      subroutine act28
        implicit none
#include "top.cmn"
        integer indact,inda,lenr2
        logical cond
        external lenr2, indact

c ----- top.ini specific

        call upper(sprm(1,7))
        cond = .false.
        if (sprm(1,7) .eq. 'NE')
     &    cond =  sprm(1,9) .ne. sprm(2,9)
        if (sprm(1,7) .eq. 'EQ')
     &    cond = sprm(1,9) .eq. sprm(2,9)
        if (sprm(1,7) .eq. 'INDEX')
     &    cond = index(sprm(1,9),sprm(2,9)(1:lenr2(sprm(2,9)))).gt.0

        if (cond) then
          inda = indact(sprm(1,14))
          if (inda.gt.1) call cmdexec(inda, sprm(1,14))
        else
          inda = indact(sprm(1,15))
          if (inda.gt.1) call cmdexec(inda, sprm(1,15))
        end if

        called(28) = .true.

        return
      end



      subroutine act29
        implicit none
c #include "top.cmn"

c         called(29) = .true.

        return
      end


      subroutine act30
        implicit none
c #include "top.cmn"

c         called(30) = .true.

        return
      end



      subroutine sumf(args,n,res)
        implicit none
#include "io.cst"
        integer i,n
        real args(n),r,res
        if (n .eq. 0) write(iolog,'(a)') 'sumf____w> no operands'
        r = args(1)
        do  i = 2, n
          r = r + args(i)
        end do
        res = r
        return
      end

      subroutine prodf(args,n,res)
        implicit none
#include "io.cst"
        integer i,n
        real args(n),r,res
        if (n .eq. 0) write(iolog,'(a)') 'prodf___w> no operands'
        r = args(1)
        do  i = 2, n
          r = r * args(i)
        end do
        res = r
        return
      end


      subroutine divf(args,n,res)
        implicit none
#include "top.cst"
        integer n
        real args(n),res
        if (n .ne. 2) then
          write(iolog,'(a,i4)')
     &    'divf____E> not 2 operands for division: ',n
          stop
        end if
        if (abs(args(2)) .lt. teps) then
          write(iolog,'(a,f15.5)')
     &    'divf____E> |divisor| too small: ', args(2)
          stop
        end if
        res = args(1) / args(2)
        return
      end


      subroutine expf(args, n, res)
        implicit none
#include "io.cst"
        integer n
        real args(n),res
        if (n .ne. 2) then
          write(iolog,'(a,i4)')
     &    'expf____E> not 2 operands for the exponent: ',n
          stop
        end if
        res = args(1)**args(2)
        return
      end

      subroutine modf(args, n, res)
        implicit none
#include "io.cst"
        integer n
        real args(n),res
        if (n .ne. 2) then
          write(iolog,'(a,i4)')
     &    'expf____E> not 2 operands for the modulus: ',n
          stop
        end if
        res = mod(nint(args(1)), nint(args(2)))
        return
      end

      logical function symass(indact, indx)
        implicit none
#include "io.cst"
        integer indact, indx
        symass = .false.
c ----- top.ini specific
c ----- symbolic name for (OPERATE | STRING_OPERATE) & RESULT
        if (((indact .eq. 13) .or. (indact .eq. 14)) .and.
     &      (indx .eq. 8)) symass = .true.
        return
      end



      subroutine macros(str)
        implicit none
#include "top.cmn"
        character str*(*), deff*(lenfil)

        call submac(str, '${LIB}', libdir)
        call submac(str, '$(LIB)', libdir)

        call submac(str, '${DIR}', sprm(1,5))
        call submac(str, '$(DIR)', sprm(1,5))

        call submac(str, '${JOB}', job)
        call submac(str, '$(JOB)', job)

c ----- do this test to avoid unnecessary call gennam2():
        if (index(str,'${DEFAULT}').gt.0 .or.
     &      index(str,'$(DEFAULT)').gt.0) then
          deff = 'default'
          call gennam2(sprm(1,4),sprm(1,6),'X',iprm(1,2),
     &                 iprm(1,3),deff,sprm(1,17))
          call submac(str,'${DEFAULT}',deff)
          call submac(str,'$(DEFAULT)',deff)
        end if

        return
      end


      subroutine submac(str, targ, subs)
        implicit none
        integer lr, lenr
        character str*(*), targ*(*), subs*(*)
        external lenr

        lr = lenr(subs)
        if (lr .gt. 0) then
          call subs2(str, targ, subs(1:lr))
        else
          call rmstrs(str, targ)
        end if

        return
      end


      subroutine wract(iolog,icmd,indxca,action)
        implicit none
        integer lencmd
        parameter (lencmd = 57)
        integer iolog, icmd, indxca, lenr2, l1, lr, l
        character action*(*), cont*(1)
        external lenr2

        lr = lenr2(action)

        l1 = 1
        l  = min(lr, lencmd)

        if (l .eq. lr) then
          cont = ' '
        else
          cont = ';'
        end if
        write(iolog, '(/a,i6,i5,1x,2a)') 
     &     'TOP______>',icmd,indxca,action(l1:l),cont

10      if (l .eq. lr) go to 100
          l1 = l+1
          l = min(lr, l+lencmd)
          if (l .eq. lr) then
            cont = ' '
          else
            cont = ';'
          end if
          write(iolog, '(22x,2a)') action(l1:l),cont
          go to 10
100     continue
        write(iolog, *)

        return
      end
