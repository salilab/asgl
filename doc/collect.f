c --- will collect all latex files included or input in master latex file
c     into one latex file;

c --- only one input or include command per line is allowed;

c --- only one file name argument per each input/include command is allowed;

c --- included file names can be without .tex;



      program collect
        implicit none
        integer maxlen
        parameter (maxlen=255)
        integer lenr,inp,ind,ind1,ind2,lr,iargc
        character line*(maxlen), infil*60, outfil*60, infil2*60

        if (iargc() .ne. 2) stop 'usage: input_file output_file'
        call getarg(1, infil)
        call getarg(2, outfil)

        inp = 2
        open(inp, file = infil, status='old', err=150)
        open(1, file = outfil, status='unknown')

        write(*,*) 'Opening: ', infil 

10      read(inp, '(a)', end=100) line
          ind = max(index(line, '\input{'), index(line, '\include{'))
          ind1 = index(line, '%')
          if (ind1 .lt. 1) ind1 = 99999
          if ((ind .gt. 0) .and. (ind .lt. ind1)) then
            inp = inp + 1
            ind1 = index(line(ind:), '{')
            ind2 = index(line(ind:), '}')
            infil2 = line(ind+ind1:ind+ind2-2)
            if (index(infil2(min(3,lenr(infil2)):), '.') .eq. 0) then
              call concat(infil2, '.tex', infil)
              call ljust(infil)
            else
              infil = infil2
            end if
            write(*,*) 'Opening: ', infil
            open(inp, file = infil, status='old', err=15)
            go to 20
15          write(*,*) 'Opening: ', infil2
            open(inp, file = infil2, status='old', err=150)
20          continue
          else
            lr = lenr(line)
            if (lr .gt. 0) then
              write(1, '(a)') line(1:lenr(line))
            else
              write(1, '()')
            end if
          end if
          go to 10
100     continue
        close(inp)
        if (inp .eq. 2) go to 200
        inp = inp - 1
        go to 10
        
200     close(1)
        stop
 
150     write(*,'(a)') 'Error; File does not exist: ', infil
        stop

        end



c --- joins s1 and s2 strings into string s in such a way that there are
c     no blanks inbetween; s has the same idententation as s1;
c --- user has to ensure that the string s is long enough;
      subroutine concat(s1, s2, s)
      implicit none
      integer len1r,l1str,len2r,len2l,l2str,l0,i,lenl,lenr
      character*(*) s1, s2, s
c
c --- first right and left printable character indices:
      len1r = lenr(s1)
      if (len1r .lt. 1) then
        s = s2
        return
      end if
c --- number of printable characters:
      l1str = len1r
c
      len2r = lenr(s2)
      if (len2r .lt. 1) then
        s = s1
        return
      end if
      len2l = lenl(s2)
      l2str = len2r - len2l + 1
      l0 = len(s)
c
c --- do tests:
      if ((l1str+l2str) .gt. l0) then
        write(*,*) 'len(s1), len(s2), len(s3): ', l1str, l2str, l0
        stop 'Error:[concat] output string too short'
      end if
c
c --- retain leading blanks in s1:
      s(1:l1str) = s1(1:len1r)
c
      s(l1str+1:l1str+l2str) = s2(len2l:len2r)
c
      do 10  i = l1str+l2str+1, l0
10      s(i:) = ' '
c
      return
      end

c
c --- left justifies card:
      subroutine ljust(card)
      implicit none
      character blank*1
      parameter (blank = ' ')
      integer nl,nr,i,lenr,lenl,lenc
      character card*(*)
      nl = lenl(card)
      nr = lenr(card)
      if (nr .ge. nl) then
        card(1:(nr-nl+1)) = card(nl:nr)
        lenc = len(card)
        do 5  i = nr-nl+2, lenc
5         card(i:i) = blank
      end if
      return
      end



c
c --- the index of the first printable character from the right:
c       (if no printable character, 0 is returned)
c       (if string argument incorrect, it aborts)
      integer function lenr(str)
      implicit none
      integer lens,i,lenl,lenlft,irndsp,ilast
      character str*(*)
      lens = len(str)
      if (lens .gt. 0) then
        lenlft = lenl(str)
        if (lenlft .le. lens) then
          i = lenlft
10        if (irndsp(str(i:i)) .eq. 1) go to 100
            if (str(i:i) .ne. ' ') ilast = i + 1
            i = i + 1
            if (i .le. lens) go to 10
100       continue
          lenr = ilast - 1
        else
          lenr = 0
        end if
      else
c        stop 'Error[lenr]: string has length 0'
        lenr = 0
      end if
      return
      end
c
c --- the index of the first printable character from the left:
c       (if no printable character, length+1 is returned)
c       (if string argument incorrect, it aborts)
      integer function lenl(str)
      implicit none
      integer lens,ilndsp,i
      character str*(*)
      lens = len(str)
      if (lens .gt. 0) then
        i = 1
10      if (ilndsp(str(i:i)) .eq. 0) go to 100
          i = i + 1
          if (i .le. lens) go to 10
100     continue
        lenl = i
      else
c        stop 'Error[lenl]: string has length 0'
        lenl = 0
      end if
      return
      end

      integer function ilndsp(ch)
        implicit none
        integer ich
        character ch*1
        ich=ichar(ch)
        if((ich.lt.33).or.(ich.gt.127)) then
          ilndsp = 1
        else
          ilndsp = 0
        end if
        return
      end
c
      integer function irndsp(ch)
        implicit none
        integer ich
        character ch*1
        ich=ichar(ch)
        if((ich.lt.32).or.(ich.gt.127)) then
          irndsp = 1
        else
          irndsp = 0
        end if
        return
      end
