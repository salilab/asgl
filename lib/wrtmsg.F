cf    ILEV   ... level of a message:
cf                1 ... long output from commands
cf                2 ... short notes (flow control ...)
cf                3 ... warnings
cf                4 ... errors
cf
cf    ALWAYS  ... whether or not to write the message always, irrespective of
cf                OUTPUT_CONTROL
cf
cf    SETSTAT ... whether or not to update the maximal error level so far
cf
cf    ISTAT   ... the level of error seriousness (0 is no error, >0 is error)
cf                (modeller stops when ISTAT >= STOP_ON_ERROR)
cf
cf    ROUT    ... the calling routine name
cf
cf    CARD    ... the message ($ splits the intput into a new line)
cf
cf Examples:
cf      error, write_message_anyway, update_status_to_1:
cf      call wrtmsg(4,.true.,.true.,1,'ROUT','MSG')
cf
cf      error, use_OUTPUT_CONTROL, update_status_to_1:
cf      call wrtmsg(4,.false.,.true.,1,'ROUT','MSG')
cf
cf      warning, use_OUTPUT_CONTROL, keep_status_unchanged:
cf      call wrtmsg(3,.false.,.false.,0,'ROUT','MSG')
cf
cf      note, use_OUTPUT_CONTROL, keep_status_unchanged:
cf      call wrtmsg(2,.false.,.false.,0,'ROUT','MSG')

      subroutine wrtmsg(ilev,always,setstat,istat,rout,card)
        implicit none
#include "io.cst"
        integer lenr, ilev, i, lr, nr, i2b, i1, i2, istat
        character card*(*), rout*(*), what*(10)
        logical iowr, setstat, recover, stp, always
        external lenr, iowr, recover

        if (always .or. iowr(ilev)) then

        what = rout
        if (ilev .le. 2) what(8:10) = '__>'
        if (ilev .eq. 3) what(8:10) = '_W>'
        if (ilev .eq. 4) what(8:10) = '_E>'
        do  i = max(1,lenr(rout)), 7
          if (what(i:i) .eq. ' ') what(i:i) = '_'
        end do
    
        lr = lenr(card)
c ----- first position of the printed range:
        i1 = 1
        nr = 0
10      continue
c ------- last position of the printed range:
          i2b = index(card(i1:), '$')
          if (i2b .eq. 0) then
            i2 = lr
          else
            i2 = i1 + i2b - 2
          end if
c ------- print out range nr:
          if (i2 .ge. i1) then
            nr = nr + 1
            if (nr .eq. 1) then
              write(iolog, '(a,1x,a)') what, card(i1:i2)
            else
              write(iolog, '(11x,a)') card(i1:i2)
            end if
          end if
c ------- the beginning of the next range:
          if (i2b .eq. 0) then
            i1 = i2 + 1
          else
            i1 = i2 + 2
          end if
          if (i1 .le. lr) go to 10
        continue

        end if

        stp = recover(setstat, istat)

        return
      end



c --- OUTPUT_CONTROL = output, notes&flow_control, warnings, errors

      logical function iowr(iwhat)
        implicit none
#include "top_appl.cmn"
        integer iwhat

        if (iwhat.lt.1 .or. iwhat.gt.4) then
          write(iolog, '(a,i5)') 'iowr____E> iwhat out of range: ',iwhat
          stop
        end if

        iowr = iprm(iwhat,6) .ne. 0

        return
      end


cf -----------------------------------------------------------------------------
cf
cf    RECOVER() sets, tests, and/or acts on the error status.
cf
cf    logical function recover(setstat, istat)
cf
cf -----------------------------------------------------------------------------

      logical function recover(setstat, istat)
        implicit none
        integer istat
        logical setstat
#include "top_appl.cmn"

c ----- see if we are changing the STATUS variable in this call
        if (setstat) then
c ------- new worst error so far:
          iprm(1,8) = max(istat, iprm(1,8))
        end if

c ----- always test and act if necessary:
        if (iprm(1,8) .ge. iprm(1,7)) then
          write(iolog, '(a,2i3)')
     &    'recover__> ERROR_STATUS >=  STOP_ON_ERROR: ',
     &    iprm(1,8),iprm(1,7)
          stop
        end if

        if (iprm(1,8) .gt. 0) then
          recover = .true.
        else
          recover = .false.
        end if

        return
      end
