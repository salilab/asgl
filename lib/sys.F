cf ---------------------------------------------------------------------------
cf
cf    SYS() routine takes a UNIX shell command cmd and returns its output
cf    in string scmd. The command is executed with the system() f77
cf    routine, its output redirected to a file, and the first line of
cf    the file is read in into the scmd string. Useful for date, pwd, ...
cf
cf    subroutine sys(cmd, scmd)
cf
cf ---------------------------------------------------------------------------


      subroutine sys(cmd, scmd)
        implicit none
        character sysf*10
        integer lenr2,time
        character cmd*(*), scmd*(*), ccmd*255
        external lenr2,time

#if defined(f2c) || defined (AIX) || defined(WIN)
          sysf = 'sys.tmp.file'
#else
c ------- need argument 0 for an IBM AIX 3.2 xlf (but this doesn
c         not work because of a clash with call time() below:
          write(sysf,'(i10)') time(0)
          call ljust(sysf)
#endif

        ccmd = cmd(1:lenr2(cmd)) // ' > ' // sysf
        call mysystem(ccmd)

        close(99)
        open(99, file=sysf, status='old', err=10)
        read(99, '(a)') scmd
        close(99)
        ccmd = 'rm -f ' // sysf
        call mysystem(ccmd)
        return

10      continue
        write(*,'(2a)') 'sys______E> File probably does not exist: ',
     &                  sysf
        write(*,'(2a)') '            command: ', ccmd(1:lenr2(ccmd))
        write(*,'(a)')
     &  '            Perhaps you have to increase swap space size.'
        scmd = ' '

        return
      end



cf ----------------------------------------------------------------------------
cf
cf    MYSYSTEM() routine calls the right system() routine
cf
cf    call mysystem(cmd)
cf
cf ----------------------------------------------------------------------------

      subroutine mysystem(cmd)
        implicit none
        character cmd*(*)

#if defined(cray)
          call ishell(cmd)
#else
          call system(cmd)
#endif

        return
      end


      subroutine dattim(datetime)
        implicit none
        character datetime*(*)

        datetime(9:10) = '  '

#if defined(f90)
          call date_and_time(datetime(1:8), datetime(11:20))
#else
#if defined(f2c) || defined(AIX)
          datetime = 'unknown'
          datetime(11:) = 'unknown'
#else
          call date(datetime)
          call time(datetime(11:18))
          datetime(19:) = ' '
#endif
#endif

        return
      end
