c --- the following routines are general purpose string handlers
c
c --- summary: ----------------------------------------------------
c
c
c --- joins s1 and s2 strings into string s in such a way that
c     there are no blanks inbetween; s has the same idententation
c     as s1;
c     subroutine concat(s1, s2, s)
c
c --- joins s1 and s2 strings into string s in such a way that
c     no blanks at the end are taken from s1, but all the beginning
c     blanks in s2 are still put between s1 and s2; s has the same
c     idententation as s1;
c     subroutine concat2(s1, s2, s)
c
c --- concatenate N ARGS into RES
c     subroutine concat3(args,n,res)
c
c --- shifts text to the right of string
c     subroutine rjust(s)
c
c --- shifts text to the left of string
c     subroutine ljust(s)
c
c --- transforms all characters in string s into lower case
c     subroutine lower(s)
c
c --- transforms all characters in the string s into upper case
c     subroutine upper(s)
c
c --- transforms all characters in the string s1 into upper case in s2
c     subroutine upper2(s1,s2)
c
c --- transforms all characters in string s into lower case except for
c     the characters between two successive primes
c     subroutine lowerp(s)
c
c --- transforms all characters in string s into upper case except for
c     the characters between two successive primes
c     subroutine upperp(s)
c
c --- index of the last printable character different
c     from a blank with no non-printable characters before it;
c     (if no printable character, 0 is returned)
c --- integer function lenr(s)
c
c --- index of the last printable character different
c     from a blank with no non-printable characters before it;
c     (if no printable character, 1 is returned)
c --- integer function lenr2(s)
c
c --- the index of the first printable character from the left:
c       (if no printable character, 1 is returned)
c       (if string argument incorrect, it aborts)
c     integer function lenl2(str)
c
c --- index of the first printable character different from blank;
c     (if no printable character, length+1 is returned)
c     printable characters: ascii>31 and ascii<128 ;
c --- integer function lenl(s)
c
c --- returns the position of the last character of target in the
c     search string (0 if not found); using index()
c     integer function indexr(search, target)
c
c --- this function returns the first occurence of target in str, but
c     only using those parts of string which are not quoted in ''
c     integer function indexs(str, target)
c
c --- returns the position of the last character of target word in the
c     search string (0 if not found); using indexw()
c     integer function indexwr(search, target)

c --- this routine returns the index of the word in str; the word
c     must be bounded by the beginning or end of the str or by a
c     blank; logical word is only the part of the physical word
c     as determined by lenl and lenr
c     integer function indexw(str,word)
c
c --- returns the index of the last contiguous printable character
c     different from blank (scan started at the first such character
c     in line) (if no printable char in the string, 0 is returned):
c     integer function indexb(line)
c
c --- returns the index of the position of the first character
c     of any of those specified in delim; if not found, 0 is returned;
c     integer function index2(line, delim)
c
c --- converts integer i to string s, write out in the Inpre format.
c     subroutine i_str(i, s, npos)
c
c --- converts real r to string s with npre and npost places for
c     pre-decimal and post-decimal digits, respectively.
c     subroutine r_str(r, s, npre, npost)
c
c --- reads n integers from string line into integer vector ibuffr
c     subroutine str_in(line, ibuffr, n, ierr)
c
c --- converts the first number in string s to integer i
c     subroutine str_i(s, i, ierr)
c
c --- converts the number in string s to real r (faster):
c     subroutine str_r2(s, r, ierr)
c
c --- converts the first number in string s to real r (slower):
c     subroutine str_r(s, r, ierr)
c
c --- reads n reals from string line into real vector buffr:
c     subroutine str_rn(line, buffr, n, ierr)
c
c --- returns the continuous stream of reals from the beginning
c     of the line in vector vec, and n, the number of them; maxn is
c     the dimension of vec
c     subroutine str_rn2(line, vec, maxn, n, ierr)
c
c --- reads a number of logicals from string line (if type='O',
c     logicals are on and off instead of *t* and *f*);
c     case insensitive;
c     subroutine str_ln(line, buffr, n, type, ierr)
c
c --- reads one logical variable from line (see above for t and f):
c     subroutine str_l(line, arg, type, ierr)
c
c --- reads n blocks of contigous printable non-blank characters
c     from line and returns them in a string array buffr;
c     subroutine str_sn(line, buffr, n)
c
c --- returns the first contiguous block of printable non-blank
c     characters in line to string buffr;
c     subroutine str_s(line, buffr)
c
c --- returns the directory+root of the unix file name:
c     subroutine diroot(file, rot)
c
c --- returns the root of the unix file name (directory and everything after
c     the last dot is eliminated):
c     subroutine rootfn(file, rot)
c
c --- returns the root of the unix file name (directory and everything after
c     the first dot after the last '/' is eliminated):
c     subroutine rootfn2(file, rot)
c
c --- returns the root of the unix file name (directory only is eliminated):
c     subroutine rootfn3(file, rot)
c
c --- returns the directory of the unix file name:
c     subroutine direct(file, dir)
c
c --- returns the number of blocks of contiguous non-blank characters
c     in line:
c     integer function nstr(line)
c
c --- pads the string strin with blanks
c     subroutine padbln(strin,strout)
c
c --- will substitute all occurences of str1 with str2, in str
c     subroutine subs2(str,str1,str2)
c
c --- will remove all occurences of str1 in str
c     subroutine rmstrs(str,str1)
c
c --- will substitute all occurences of str1 with str2, in each element
c     of str(n)
c     subroutine subs2n(str,n,str1,str2)
c
c --- will substitute cont1(istart+1:istart+n) for sub
c     subroutine subs(str,istart,n,sub)
c
c --- substitute string str1 with str2 in all n1 x n2 elements of
c     str(n1,n2)*(*) 2D matrix
c     subroutine subs2nn(str,n1,n2,str1,str2)
c
c --- puts blanks into string str
c     subroutine blank(str)
c
c --- returns the positions of target in line in ipos(maxpos); npos
c     such positions are returned; beware: in 'andandand' target
c     andand occurs twice!
c     subroutine getpos2(line, target, ipos, maxpos, npos)
c
c --- reads words from line, returns them in buffr and the number of
c     them in n; the word is: if there is a ' character in the line,
c     then the word is everything between the successive ',
c     otherwise it is everything non-blank between two blanks.
c     Non-primed words can also occur between the primed ones.
c     subroutine str_sn2(line, buffr, maxn, n)
c
c --- like str_sn2, except that a prime can be protected by \.
c     subroutine str_sn6(line, buffr, maxn, n)
c
c --- reads words from line, returns them in buffr and the number of
c     them in n; the word is: everything non-blank between two blanks.
c     subroutine str_sn3(line, buffr, maxn, n)
c
c --- reads words from line, returns them in buffr and the number of
c     them in n; the word is: everything non-delimiter between two
c     delimiter characters; many of those can be in string delim.
c     subroutine str_sn5(line, buffr, maxn, n, delim)
c
c --- If there are '(' or ')' in line, it deals only with the part
c     between the parentheses, ignoring the parentheses. It then reads
c     words from a selected part of line, returns them in buffr and the
c     number of them in n; the word is: if there is a ' character in the text,
c     then the word is everything between the two successive ',
c     otherwise it is everything non-blank between two blanks. Non-primed
c     words can also occur between the primed ones. This later part is
c     like str_sn2.
c     subroutine str_sn4(line, buffr, maxn, n)
c
c --- returns the first continuous stream of integers from the beginning
c     of the line in vector ivec, and n, the number of them; maxn is
c     the dimension of ivec
c     subroutine str_in2(line, ivec, maxn, n, ierr)
c
c --- returns the first continuous stream of logicals from the beginning
c     of the line in vector vec, and n, the number of them; maxn is
c     the dimension of vec
c     subroutine str_ln2(line, vec, maxn, n, type, ierr)
c
c --- returns the string item to the left of the strin(ipos:ipos):
c     the string item starts at the first non-blank character to the
c     left of the ipos and ends at the first blank character to
c     the left of ipos or at the beginning of the strin, whichever first.
c     subroutine rdleft(strin, ipos, strout)
c
c --- to remove primes from the string
c      subroutine rmprimes(str)
c
c --- adds string add to the end of string str (using concat; output: str)
c     (no blanks inbetween str and add)
c     subroutine addstr(str,add)
c
c --- adds string add to the end of string str (using concat2; output: str)
c     (blanks from add are kept)
c     subroutine add2str(str,add)
c
c --- adds string str to the beginning of string add (concat; output: add)
c     (no blanks inbetween str and add)
c     subroutine add3str(str,add)
c
c --- adds string str to the beginning of string add (concat2; output: add)
c     (blanks from add are kept)
c     subroutine add4str(str,add)
c
c --- to remove chars from the string
c      subroutine rmchrs(str,ch)

c --- finds the index of an array element that is exactly equal to WORD
c     integer function ifindword(word,array,narray)
c
c --- finds the index of an array element that contains WORD (index())
c     integer function ifind3word(word,array,narray)
c
c --- finds an index of an array element that contains a
c     'blank-separated' word WORD;
c     integer function ifind2word(word,array,narray)
c
c --- a vector version of ifindword; exact matches;
c     subroutine ifindwordn(word,nword,array,narray,iword)
c
c --- finds indices of array elements that contain word WORD (according
c     to index3(), where WORD can contain character # which will match
c     any digit [0..9] in ARRAY(I));
c     subroutine ifind4word(word,array,narray,ind,mind,nind)
c
c --- finds an index of an array element that contains a
c     'blank-separated' word WORD (strgin); case insensitive;
c     integer function ifind5word(word,array,narray)
c
c --- finds an index of an array element that contains a
c     'blank-separated' word WORD; starts from the end;
c     integer function ifind6word(word,array,narray)
c
c --- will find position of TARGET within WORD (like index()), except that
c     character # in TARGET matches any digit [0..9] in WORD.
c     integer function index3(word, target)
c
c --- is string TARGET in string SINP, case insensitive
c     subroutine strgin(sinp, target)
c
c code: ======================================================================
c
c --- puts blanks into string str
      subroutine blank(str)
        implicit none
        integer l, i
        character str*(*)
        l = len(str)
        if (l .gt. 0) then
          do  i = 1, l
            str(i:i) = ' '
          end do
        else
          stop 'blank___E> string 0 length'
        end if
        return
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
        write(*,'(a/a,3i4)')
     &  'concat__E> output string too short',
     &  '           len(s1), len(s2), len(s3): ', l1str, l2str, l0
        do  i = 1, len(s1)
           write(*,*) i, ' ', s1(i:i), ' ', ichar(s1(i:i))
        end do
        stop
      end if
c
c --- retain leading blanks in s1:
      s(1:l1str) = s1(1:len1r)
c
      s(l1str+1:l1str+l2str) = s2(len2l:len2r)
c
      do  i = l1str+l2str+1, l0
        s(i:) = ' '
      end do
c
      return
      end
c
c --- right justifies card:
      subroutine rjust(card)
      implicit none
      integer i, j, lenc, maxi, npos, lenr
      character blank*1
      parameter (blank = ' ')
      character card*(*)
      lenc = len(card)
      npos = lenr(card)
      if (lenc.gt.npos) then
        j = lenc
        do 10  i = npos, 1, -1
          card(j:j) = card(i:i)
          j = j - 1
10      continue
        maxi = lenc - npos
        do 30 i=1,maxi
          card(i:i)=blank
30      continue
      end if
      return
      end


c --- joins s1 and s2 strings into string s in such a way that
c     no blanks at the end are taken from s1, but all the beginning
c     blanks in s2 are still put between s1 and s2; s has the same
c     idententation as s1;
      subroutine concat2(s1, s2, s)
      implicit none
      integer len1r,l1str,len2r,len2l,l2str,l0,i,lenr
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
      len2l = 1
      l2str = len2r - len2l + 1
      l0 = len(s)
c
c --- do tests:
      if ((l1str+l2str) .gt. l0) then
        write(*,'(a/a,3i4)')
     &  'concat2_E> output string too short',
     &  '           len(s1), len(s2), len(s3): ', l1str, l2str, l0
        stop
      end if
c
c --- retain leading blanks in s1:
      s(1:l1str) = s1(1:len1r)
c
c --- retain leading blanks in s2:
      s(l1str+1:l1str+l2str) = s2(len2l:len2r)
c
      do  i = l1str+l2str+1, l0
        s(i:) = ' '
      end do
c
      return
      end


c --- will substitute all occurences of str1 with str2, in each element
c     of str(n)
      subroutine subs2n(str,n,str1,str2)
        implicit none
        integer n, i
        character str(n)*(*), str1*(*), str2*(*)

        do  i = 1, n
          call subs2(str(i),str1,str2)
        end do

        return
      end


c --- will substitute all occurences of str1 with str2, in str
      subroutine subs2(str,str1,str2)
        implicit none
        integer lr, lenr, lr2, ipos, inew, l
        character str*(*), str1*(*), str2*(*)

        l = lenr(str)
        if (l .le. 0) return
        lr = lenr(str1)
        if (lr .le. 0) stop 'subs2___E> str1 is empty'
        lr2 = lenr(str2)

        inew = 1
 10     ipos = index(str(inew:), str1)
          if (ipos .le. 0) return
          call subs(str(inew:),ipos-1,lr,str2)
          inew = inew + ipos + lr2 - 1
          if (inew .gt. l) return
        go to 10

      end


c --- will remove all occurences of str1 in str
      subroutine rmstrs(str,str1)
        implicit none
        integer lenr, lr, ipos
        character str*(*), str1*(*)

        lr = lenr(str1)
        if (lr .le. 0) stop 'rmstrs__E> str1 is empty'

10      ipos = index(str, str1)
          if (ipos .le. 0) return
          str(ipos:) = str(ipos+lr:)
        go to 10

      end

c
c
c --- will substitute cont1(istart+1:istart+n) for sub
      subroutine subs(str,istart,n,sub)
        implicit none
        integer maxlen,lstr,lsub,n,j,l,i,lenr,istart
        parameter (maxlen=255)
        character str*(*), sub*(*), buffer*(maxlen)

        lstr = lenr(str)
        lsub = len(sub)
        if (lstr.eq.0) stop 'Error[subs] lenr(str) = 0'
c        if (lsub.eq.0) stop 'Error[subs] len(sub) = 0'
        if (lstr .lt. istart) stop 'Error[subs] istart > lenr(str)'
        if ((lstr-n+lsub) .gt. len(str)) then
          write(*,*) 'Error[subs] str too short: ',len(str),lstr,n,lsub
          stop
        end if
        if ((lstr-istart-n+1) .gt. maxlen)
     &       stop 'Error[subs] increase maxlen'

c ----- str = begin + substituted + end

        j = 1
c ----- save the end
        do  i = istart+n+1, lstr
          buffer(j:j) = str(i:i)
          j = j + 1
        end do

c ----- copy the sub to str after begin
        j = istart+1
        do  i = 1, lsub
          str(j:j) = sub(i:i)
          j = j + 1
        end do

c ----- append the end to str:
        j = istart + lsub + 1
        do  i = 1, lstr-istart-n
          str(j:j) = buffer(i:i)
          j = j + 1
        end do

c ----- padd the str with blanks
        l = len(str)
        do  i = lstr-n+lsub+1, l
          str(i:i) = ' '
        end do

        return
      end
c
c --- left justifies card:
      subroutine ljust(card)
      implicit none
      character blank*1
      parameter (blank = ' ')
      integer nl,nr,i,lenr,lenl,j
c     integer lenc
      character card*(*)
      nl = lenl(card)
      if (nl .gt. 1) then
        nr = lenr(card)
        if (nr .ge. nl) then
c          card(1:(nr-nl+1)) = card(nl:nr)
          j = 1
          do  i = nl, nr
            card(j:j) = card(i:i)
            j = j + 1
          end do
          card(nr-nl+2:) = blank
c          lens = len(card)
c          do  i = nr-nl+2, lenc
c            card(i:i) = blank
c          end do
        end if
      end if
      return
      end
c
c --- transforms all characters in string s into lower case
      subroutine lower(s)
      implicit none
      integer i,l1,ich
      character s*(*), ch*1
      l1 = len(s)
      do  i = 1, l1
        ch = s(i:i)
        ich = ichar(ch)
        if ((ich .ge. 65) .and. (ich .le. 90)) s(i:i) = char(ich+32)
      end do
      return
      end
c
c --- transforms all characters in string s into lower case except for
c     the characters between two successive primes
      subroutine lowerp(s)
      implicit none
      integer iprime
      parameter (iprime = 39)
      integer lens,i,ich
      logical prime
      character s*(*), ch*1

      lens = len(s)
      prime = .false.
      do  i = 1, lens
        ch = s(i:i)
        ich = ichar(ch)
        if (ich .eq. iprime) prime = .not. prime
        if((ich.ge.65).and.(ich.le.90).and.(.not.prime))ich=ich+32
        s(i:i) = char(ich)
      end do

      return
      end
c
c --- to transform all characters in the input string s into upper case;
      subroutine upper(s)
        implicit none
        integer l1,lenr,ich,i
        character s*(*), ch*1
        l1 = lenr(s)
        do  i = 1, l1
          ch = s(i:i)
          ich = ichar(ch)
          if((ich.ge.97).and.(ich.le.122)) s(i:i) = char(ich-32)
        end do
        return
      end
c
c --- transforms all characters in the string s1 into upper case in s2
      subroutine upper2(s1,s2)
        implicit none
        integer l1,lenr,ich,i,l2
        character s1*(*), s2*(*), ch*1
        l1 = lenr(s1)
        l2 = len(s2)
        if (l2.lt.l1) then
          write(*,'(a,2i5)') 'upper2__E> len(s2)<lenr(s1): ', l1, l2
          stop
        end if
        do  i = 1, l1
          ch = s1(i:i)
          ich = ichar(ch)
          if((ich.ge.97).and.(ich.le.122)) then
            s2(i:i) = char(ich-32)
          else
            s2(i:i) = ch
          end if
        end do
        do  i = l1+1, l2
          s2(i:i) = ' '
        end do
        return
      end
c
c --- to transform all characters in the input string s into upper case, except
c     for parts between two unprotected primes;
      subroutine upperp(s)
        implicit none
        integer iprime
        parameter (iprime = 39)
        integer lcont,lenr,i,ich
        character s*(*), ch*1
        logical prime, isprime

        lcont = lenr(s)
        prime = .false.
        do  i = 1, lcont
          ch = s(i:i)
          ich = ichar(ch)
          if (isprime(s, i)) prime = .not. prime
          if((ich.ge.97).and.(ich.le.122).and.(.not.prime))ich=ich-32
          s(i:i) = char(ich)
        end do
        return
      end


#if defined(f90)

      integer function lenr(text)
      implicit none
      character text*(*)
      lenr = len_trim(text)
      return
      end

#else

c --- the index of the first printable character from the right:
c       (if no printable character, 0 is returned)
      integer function lenr(text)
      implicit none
      logical again
      integer iasc,nchr,itmp
      character text*(*)

      nchr = len(text)
      again = (nchr.gt.0)
      do while (again)
        iasc = ichar(text(nchr:nchr))
        if (iasc.lt.33) then
C         non-printable characters & blank
          itmp = nchr - 1
          nchr = itmp
        elseif (iasc.gt.254) then
C         null character
          itmp = nchr - 1
          nchr = itmp
        else
          again = .false.
        endif
        if (nchr.lt.1) then
C         Nothing left to check
          again = .false.
        endif
      enddo
      lenr = nchr

      return
      end

#endif


c --- this one is about 3 times slower than lenr()
c
c --- the index of the first printable character from the right:
c       (if no printable character, 0 is returned)
      integer function lenr_org(str)
      implicit none
      character str*(*)
      integer l,i

c --- try to pre-empt completely empty strings:
      l = len(str)
      if (l .gt. 0) then
       if (ichar(str(1:1)).gt.0) then

        do  i = l, 1, -1
c          if (str(i:i) .ne. ' ') then
          if((str(i:i).ne.' ').and.(ichar(str(i:i)).ne.0)) then
            lenr_org = i
            return
          end if
        end do

       end if
      end if

      lenr_org = 0
c
      return
      end

      integer function lenr2(str)
      implicit none
      integer lenr
      character str*(*)
      external lenr
      lenr2 = max(1,lenr(str))
      return
      end


      integer function lenl2(str)
      implicit none
      integer lenl, l
      character str*(*)
      external lenl
      l = lenl(str)
      if (l .gt. len(str)) then
        lenl2 = 1
      else
        lenl2 = l
      end if
      return
      end


c      integer function lenl(str)
c      implicit none
c      integer l1, l2, lenl_tst, lenl_org
c      character str*(*)
c
c      l1 = lenl_tst(str)
c      l2 = lenl_org(str)
c      if (l1 .ne. l2) then
c        write(*,*) 'SHIT: L1, L2: ', l1, l2
c        write(*,*) 'STRING: ', str
c        stop
c      end if
c      lenl = l1
c
c      return
c      end


c
c
c --- the index of the first printable character from the left:
c       (if no printable character, length+1 is returned)
      integer function lenl(str)
      implicit none
      integer lens,i
      character str*(*)
      logical ilndsp
      lens = len(str)
      if (lens .gt. 0) then
        i = 1
10      if (.not.ilndsp(str(i:i))) go to 100
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
c
c
c --- returns the position of the last character of target in the
c     search string (0 if not found)
      integer function indexr(search, target)
      implicit none
      integer i
      character search*(*), target*(*)
c --- due to a bug in lpi-fortran, test for this:
      if (len(search) .ge. len(target)) then
        i = index(search, target)
        if (i .gt. 0) then
          i = i + len(target) - 1
        end if
        indexr = i
      else
        indexr = 0
      end if
      return
      end

c
c --- returns the position of the last character of target word in the
c     search string (0 if not found)
      integer function indexwr(search, target)
      implicit none
      integer i,indexw,lenr
      character search*(*), target*(*)
      i = indexw(search, target)
      if (i .gt. 0) then
        i = i + lenr(target) - 1
      end if
      indexwr = i
      return
      end

c --- returns the index of the position of the first character
c     of any of those specified in delim; if not found, 0 is returned;
      integer function index2(line, delim)
        implicit none
        integer lenr,npos,i
        character line*(*), delim*(*)

        npos = lenr(line)
        if (npos .gt. 0) then
          do  i = 1, npos
            if (index(delim, line(i:i)) .gt. 0) then
              index2 = i
              return
            end if
          end do
          index2 = 0
        else
          index2 = 0
        end if

        return
      end

c
c --- returns the index of the last contiguous printable and non-blank
c     character in line, scan starting at the first such character:
c     if no non-blank printable character, 0 is returned;
      integer function indexb(line)
        implicit none
        integer lenr,npos,i,ii,lenl
        character line*(*)
        logical ilndsp

        npos = lenr(line)
        if (npos .gt. 0) then
c ------- (if lenr in range, then lenl in range too -> (if npos>0 -> do
c         loop is executed at least once))
          do 100  i = lenl(line), npos
            ii = i
            if (ilndsp(line(ii:ii))) go to 200
100       continue
          indexb = ii
          return
200       indexb = ii-1
        else
          indexb = 0
        end if

        return
      end
c
c
c --- converts real r to string s with npre and npost places for
c     pre-decimal and post-decimal digits, respectively.
      subroutine r_str(r, s, npre, npost)
      implicit none
      integer npre, npost, ls, i
      real r, rround
      character s*(*), fmt*8, fmti*5
      save fmt, fmti
      data fmt /'(F  .  )'/
      data fmti /'(I  )'/

      if (npre .lt. 1) stop 'r_str___E> npre < 1'

      if (npost .lt. 0) then
        write(fmti(3:4), '(i2)') npre
        write(s, fmti, err=100) nint(r)
      else

c ----- Not necessary to do explicit rounding on iris4d/convex/rs6000/
c       sun4/alpha/f2c&next which do rounding automatically in the write
c       statement.
c ----- The above statement is not true, but I do not know exactly why not.
c        rround = r

c ----- round the number to npost digits:
        rround = r + sign(1.0,r)*0.5*10.0**(-npost-1)
c        write(*,'(99f15.7)') r,rround,sign(1.0,r)*0.5*10.0**(-npost),
c     &       sign(1.0,r)*0.5*10.0**(-npost-1)

        write(fmt(3:4),  '(i2)') npre+npost
        write(fmt(6:7), '(i2)') npost
        write(s, fmt, err=100) rround

        if (npre.gt.1) then
c ------- make sure that _.1231 does not occur:
          if (s(npre-1:npre) .eq. ' .') s(npre-1:npre) =  '0.'
          if (s(npre-1:npre) .eq. '-.') then
            if (npre.gt.2) s(npre-2:npre) = '-0.'
          end if
        end if
      end if

      return

100   ls = len(s)
      do  i = 1, ls
        s(i:i) = '*'
      end do
      return

c 100   stop 'r_str___E> error writing to string'
      end


c --- converts integer i to string s, write out in the Inpre format.
      subroutine i_str(i, s, npre)
      implicit none
      integer i, npre, l
      character s*(*), fmt*5
      save fmt
      data fmt /'(I  )'/

      l = len(s)
      if (npre.gt.l) then
        write(*,'(a,i4)') 'i_str___E> string too short: ', l, npre
        stop
      end if

      if ((npre.ge.1).and.(npre.le.99)) then
        write(fmt(3:4), '(i2)') npre
      else
        write(*,'(a,i4)') 'i_str___E> npre out of range: ', npre
        stop
      end if

      write(s, fmt) i

      return
      end



c --- reads a number of integers from string line
      subroutine str_in(line, ibuffr, n, ierr)
        implicit none
        integer n,ibuffr(n),ilast,iend,i,indexb,ierr
        character line*(*)
        ilast = 0
        do  i = 1, n
          call str_i(line(ilast+1:), ibuffr(i), ierr)
          iend = indexb(line(ilast+1:))
          ilast = ilast + iend
        end do
        return
      end
c
c
c --- reads n reals from string line into real array buffr:
      subroutine str_rn(line, buffr, n, ierr)
        implicit none
        integer ilast, i, n, iend, indexb, ierr
        real buffr(n)
        character line*(*)
        ilast = 0
        do  i = 1, n
          call str_r(line(ilast+1:), buffr(i), ierr)
          iend = indexb(line(ilast+1:))
          ilast = ilast + iend
        end do
        return
      end
c
c
c --- reads a number of logicals from string line:
      subroutine str_ln(line, buffr, n, type, ierr)
        implicit none
        integer n, ilast, i, iend, indexb, ierr
        character line*(*), type*(*)
        logical buffr(n)
        ilast = 0
        do  i = 1, n
          call str_l(line(ilast+1:), buffr(i), type, ierr)
          iend = indexb(line(ilast+1:))
          ilast = ilast + iend
        end do
        return
      end

c
c --- reads words from line, returns them in buffr and the number of
c     them in n; the word is: everything non-blank between two blanks.
      subroutine str_sn3(line, buffr, maxn, n)
        implicit none
        integer maxn,n,ilast,l,iend,indexb
        character line*(*), buffr(maxn)*(*)
        ilast = 0
        n = 0
        l = len(line)
5       if (ilast+1 .gt. l) go to 10
          iend = indexb(line(ilast+1:))
c ------- is there no string left in the line?
          if (iend .eq. 0) go to 10
          n = n + 1
          call str_s(line(ilast+1:), buffr(n))
          ilast = ilast + iend
          go to 5
10      continue
        return
      end


c
c --- reads words from line, returns them in buffr and the number of
c     them in n; the word is: if there is a ' character in the line,
c     then the word is everything between the successive ',
c     otherwise it is everything non-blank between two blanks.
c     Non-primed words can also occur between the primed ones.
      subroutine str_sn2(line, buffr, maxn, n)
        implicit none
        integer maxpos
        parameter (maxpos = 1024)
        integer lenr
        integer ipos(maxpos),maxn,n,ind1,ind2,ip,ni,i1,i2,i,npos,nw
        character line*(*), buffr(maxn)*(*)

        n = 0

        ind1 = 1
        ind2 = len(line)
        if (ind2 .eq. 0) return

        ip = index(line(ind1:ind2), '''')
        if (ip .gt. 0) then
c ------- find positions of all '
          call getpos2(line(ind1:ind2), '''', ipos, maxpos, npos)
c ------- testing:
          ni = npos / 2
          if(ni*2 .ne. npos) then
            write(*,'(a,i4)') 'str_sn2_E> # of '' <> even:, ',npos
            write(*,'(a,i4,1x,a)') 'Len,String: ', lenr(line),
     &                             line(1:max(1,lenr(line)))
            stop
          end if
c ------- words before the first prime:
          i1 = ind1
          i2 = ind1+ipos(1)-2
          if (i2 .ge. i1) call str_sn3(line(i1:i2), buffr, maxn, n)
c ------- words after the first prime
          ipos(npos+1) = 9999
          do i = 1, npos, 2
            i1 = ind1+ipos(i)
            i2 = ind1+ipos(i+1)-2
            n = n + 1
            if (i2.ge.i1) then
              buffr(n)=line(i1:i2)
            else
              call blank(buffr(n))
            end if
c --------- words before the next prime (and after the last prime in the last
c           cycle):
            i1 = ind1+ipos(i+1)
            i2 = min(ind1+ipos(i+2)-2, ind2)
            if((i2.ge.i1).and.(n+1.le.maxn)) then
              call str_sn3(line(i1:i2), buffr(n+1), maxn, nw)
              n = n + nw
            end if
          end do
        else
          if (ind2.ge.ind1) call str_sn3(line(ind1:ind2),buffr,maxn,n)
        end if
        return
      end
c
c --- reads n blocks of contigous printable non-blank characters from
c     line and returns them in an array buffr;
      subroutine str_sn(line, buffr, n)
        implicit none
        integer n,ilast,i,iend,indexb
        character line*(*),buffr(n)*(*)
        ilast = 0
        do  i = 1, n
          call str_s2(line(ilast+1:), buffr(i), iend)
          iend = indexb(line(ilast+1:))
          ilast = ilast + iend
        end do
        return
      end

c --- returns the first contiguous block of printable non-blank
c     characters in line to buffr;
      subroutine str_s2(line, buffr, iend)
        implicit none
        character blank*1
        parameter (blank=' ')
        integer ibeg,iend,lenl,indexb
        character buffr*(*), line*(*)

        ibeg = lenl(line)
        iend = indexb(line)
        if (iend .ge. ibeg) then
          buffr = line(ibeg:iend)
        else
          buffr = blank
        end if

        return
      end

c
c --- returns the first contiguous block of printable non-blank
c     characters in line to buffr;
      subroutine str_s(line, buffr)
        implicit none
        integer iend
        character buffr*(*), line*(*)

        call str_s2(line, buffr, iend)

        return
      end
c
c
c --- returns the directory+root of the unix file name:
      subroutine diroot(file, rot)
        implicit none
        integer lenr,lr,ibeg,iend,i
        character file*(*), rot*(*)
        lr = lenr(file)
        ibeg = 0
c ----- position of the last dot:
        do 30  i = lr, 1, -1
          iend = i
          if (file(i:i) .eq. '.') go to 40
30      continue
        iend = lr+1
40      continue
c ----- hidden files:
        if (iend .eq. 1) iend = lr+1
        if (iend-ibeg-1 .le. 0) stop 'Error[root]: length of root <= 0'
        rot = file(ibeg+1:iend-1)
        return
      end
c
c --- returns the root of the unix file name (directory and everything after
c     the first dot after the last '/' is eliminated):
      subroutine rootfn2(file, rot)
        implicit none
        integer lr,lenr,i,ibeg,iend
        character file*(*), rot*(*)

        lr = lenr(file)
c ----- position of the last slash:
        do 10  i = lr, 1, -1
          ibeg = i
          if (file(i:i) .eq. '/') go to 20
10      continue
        ibeg = 0

20      continue
c ----- position of the first dot after the last slash:
        do 30  i = ibeg+1, lr
          iend = i
          if (file(i:i) .eq. '.') go to 40
30      continue
        iend = lr+1
        go to 50
40      continue
c ----- for files in the form '../file' and '.file'
        if (iend .le. 2) then
          if (file(1:1) .eq. '.') iend = lr+1
        end if
50      continue
        if (iend-ibeg-1 .le. 0) then
          write(*,*) 'file: ', file
          write(*,*) 'iend,ibeg: ', iend, ibeg
          stop 'Error[rootfn]: length of root <= 0'
        end if
        rot = file(ibeg+1:iend-1)
        return
      end


c --- returns the root of the unix file name (directory and everything after
c     the last dot is eliminated):
      subroutine rootfn(file, rot)
        implicit none
        integer lr,lenr,i,ibeg,iend
        character file*(*), rot*(*)
        lr = lenr(file)
c ----- position of the last slash:
        do 10  i = lr, 1, -1
          ibeg = i
          if (file(i:i) .eq. '/') go to 20
10      continue
        ibeg = 0

20      continue
c ----- position of the last dot:
        do 30  i = lr, 1, -1
          iend = i
          if (file(i:i) .eq. '.') go to 40
30      continue
        iend = lr+1
        go to 50
40      continue
c ----- for files in the form '../file' and '.file'
        if (iend .le. 2) then
          if (file(1:1) .eq. '.') iend = lr+1
        end if
50      continue
        if (iend-ibeg-1 .le. 0) then
c          write(*,*) 'file: ', file
c          write(*,*) 'iend,ibeg: ', iend, ibeg
c          stop 'Error[rootfn]: length of root <= 0'
          rot = ' '
        else
          rot = file(ibeg+1:iend-1)
        end if
        return
      end

c --- returns the root of the unix file name (directory only is eliminated):
      subroutine rootfn3(file, rot)
        implicit none
        integer lr,lenr,i,ibeg
        character file*(*), rot*(*)

        lr = lenr(file)
c ----- position of the last slash:
        do  i = lr, 1, -1
          ibeg = i
          if (file(i:i) .eq. '/') go to 10
        end do
        ibeg = 0

10      continue

c ----- for files in the form '.file'
        rot = file(ibeg+1:lr)

        return
      end

c
c
c --- returns the directory of the unix file name:
      subroutine direct(file, dir)
        implicit none
        integer l,lr,lenr,iend,i
        character file*(*), dir*(*)
        lr = lenr(file)
c ----- position of the last  slash:
        do 10  i = lr, 1, -1
          iend = i
          if (file(i:i) .eq. '/') go to 20
10      continue
        iend = 0
20      continue
        if (iend .gt. 0) then
          dir = file(1:iend)
        else
          l = len(dir)
          do  i = 1, l
            dir(i:i) = ' '
          end do
        end if
        return
      end
c
c --- returns the number of blocks of contiguous non-blank characters
c     in line:
      integer function nstr(line)
        implicit none
        integer n,i1,i2,indexb
        character line*(*)
        n = 0
        i1 = 0
10      i2 = indexb(line(i1+1:))
        if (i2 .eq. 0) go to 20
          n = n + 1
          i1 = i1 + i2
          go to 10
20      continue
        nstr = n
        return
      end
c
c --- padd the string str with blanks
      subroutine padbln(str)
        implicit none
        character blank*1
        parameter (blank = ' ')
        integer l,i,lenr
        character str*(*)
        l = len(str)
        if (l .eq. 0) stop 'Error[padbln]: len(string) = 0'
        do i = lenr(str)+1, l
          str(i:) = blank
        end do
        return
      end



c
c
c --------------------------------------------------------------
c miscelaneous routines, not very useful on their own:
c --------------------------------------------------------------
c
c
      logical function ilndsp(ch)
        implicit none
        integer ich
        character ch*1
        ich=ichar(ch)
        ilndsp = (ich.lt.33).or.(ich.gt.254)
        return
      end


      logical function irndsp(ch)
        implicit none
        integer ich
        character ch*1
        ich=ichar(ch)
        irndsp = (ich.lt.32).or.(ich.gt.254)
        return
      end

c --- returns the positions of target in line in ipos(maypos), npos
c     such positions are returned; beware: in 'andandand' target
c     andand occurs twice!
      subroutine getpos2(line, target, ipos, maxpos, npos)
        implicit none
        integer maxpos,npos,ipos(maxpos),ll,lenr,lt,i
        character line*(*), target*(*)
        ll = lenr(line)
        lt = lenr(target)
        npos = 0
        do  i = 1, ll-lt+1
          if (line(i:i+lt-1) .eq. target(1:lt)) then
            npos = npos + 1
            if (npos .gt. maxpos) stop 'getpos2__E> increase maxpos'
            ipos(npos) = i
          end if
        end do
        return
      end

c --- returns the first continuous stream of reals from the beginning
c     of the line in vector vec, and n, the number of them;
      subroutine str_rn2(line, vec, maxn, n, ierr)
        implicit none
        integer maxlen,maxbuf
        parameter (maxlen=40, maxbuf=500)
        integer maxn,n,ierr,i,nwords
        real vec(maxn)
        character buff(maxbuf)*(maxlen), line*(*)

c ----- get the words in the line (no ' taken into account):
        call str_sn3(line, buff, maxbuf, nwords)

c ----- check them for being integers and stop when reaching the first
c       non-integer:
        n = 0
        ierr = 0
        do  i = 1, nwords
          call str_r2(buff(i), vec(i), ierr)
          if (ierr .gt. 0) return
          n = i
        end do

        return
      end

c --- returns the first continuous stream of integers from the beginning
c     of the line in vector ivec, and n, the number of them;
      subroutine str_in2(line, ivec, maxn, n, ierr)
        implicit none
        integer maxlen,maxbuf
        parameter (maxlen=60, maxbuf=200)
        integer maxn,n,i,nwords,ierr
        integer ivec(maxn)
        character buff(maxbuf)*(maxlen), line*(*)

c ----- get the words in the line (no ' taken into account):
        call str_sn3(line, buff, maxbuf, nwords)

c ----- check them for being integers and stop when reaching the first
c       non-integer:
        n = 0
        ierr = 0
        do  i = 1, nwords
          call str_i(buff(i), ivec(i), ierr)
          if (ierr .gt. 0) return
          n = i
        end do

        return
      end




c --- returns the first continuous stream of integers from the beginning
c     of the line in vector vec, and n, the number of them;
      subroutine str_ln2(line, vec, maxn, n, type, ierr)
        implicit none
        integer maxlen,maxbuf
        parameter (maxlen=60, maxbuf=200)
        integer maxn,n,i,nwords,ierr
        logical vec(maxn)
        character buff(maxbuf)*(maxlen), line*(*), type*(*)

c ----- get the words in the line (no ' taken into account):
        call str_sn3(line, buff, maxbuf, nwords)

c ----- check them for being logicals and stop when reaching the first
c       non-logical:
        n = 0
        do  i = 1, nwords
          call str_l(buff(i), vec(i), type, ierr)
          n = i
        end do

        return
      end


c --- the same as str_i, except with error returned as an argument
c     if the contents of s not recognized as an integer variable:
c --- integer form is: [+-] [digits]
      subroutine str_i2(s, i, ierr)
        implicit none
        integer ierr, i
        real r
        character s*(*)

        call str_r2(s, r, ierr)
        i = nint(r)

        return
      end



c --- the same as str_i, except with error returned as an argument
c     if the contents of s not recognized as an integer variable:
c --- integer form is: [+-] [digits]
      subroutine str_i(str, i, ierr)
        implicit none
        integer ierr, i
        real r
        character str*(*)

        call str_r(str, r, ierr)
        i = nint(r)

        return
      end

#if defined(AIX)

      subroutine str_r2(s, r, ierr)
        implicit none
        integer ierr
        real r, l
        character s*(*), f*9

        l = len(s)
        f = '(f'
        if (l.lt.10) then
          write(f(3:6), '(f3.1,a1)') l, ')'
        elseif (l.lt.100) then 
            write(f(3:7), '(f4.1,a1)') l, ')'
          elseif (l.lt.1000) then 
              write(f(3:8), '(f5.1,a1)') l, ')'
            else
              write(f(3:9), '(f6.1,a1)') l, ')'
        end if
     
        read(s, f, err=100, end=100, iostat=ierr) r
        return

100     ierr = 1
        return
      end

#else

c --- the same as str_r, except with error returned as an argument if
c     the contents of s not recognized as a real variable;
c     real number is: [+-] [digits] [.] [digits] [eEdD] [+-] [digits]
      subroutine str_r2(s, r, ierr)
        implicit none
        integer ierr
        real r
        character s*(*)

        read(s, '(f40.0)', err=100, end=100, iostat=ierr) r
        return

100     ierr = 1
        return
      end

#endif



c --- the same as str_r, except with error returned as an argument if
c     the contents of s not recognized as a real variable;
c     real number is: [+-] [digits] [.] [digits] [eEdD] [+-] [digits]

      subroutine str_r(str, r, ierr)
        implicit none
        integer ierr, iend
        real r
        character str*(*), s*(50)

c ----- this call increases CPU time relative to str_r2 for more than 50%:
        call str_s2(str, s, iend)
        call str_r2(s, r, ierr)

        return
      end


c --- older & slower str_r():
      subroutine str_r3(str, r, ierr)
        implicit none
        integer ierr
        real r
        character s*(40), fmt*10, str*(*)
        save fmt
        data fmt /'(BN,F  .0)'/

c ----- this call increases CPU time relative to str_r2 for 50%.
        call str_s(str, s)

        write(fmt(6:7), '(i2)') len(s)

c --- This routine does not work on hermosa when the STR does not contain
c     only a single legitimate number and absolutely nothing else: it gives
c     a warning and then continues with all 0 in the field so ierr = 0 and
c     r = 0.0. Nothing helps; IOSTAT is also 0.

        read(s, fmt, err=100) r
        ierr = 0
        return

100     ierr = 1
        return
      end


c
c     the same as str_l, except that error is returned as an argument,
c     if the contents of line not recognized as a logical variable:
c --- reads one logical variable from line (on/off in upper or
c     lower case are true and false, respectively, if type='O')
      subroutine str_l(line, arg, type, ierr)
        implicit none
        integer ierr
        character line*(*), type*(*), str*(255)
        logical arg

        ierr = 0

        call str_s(line, str)
        call upper(str)
      
c ----- default value is .f.:
        arg = .false.

        if (type.eq.'o' .or. type.eq.'O') then
          if (str .eq. 'ON') then
            arg = .true.
          else
            if (str .eq. 'OFF') then
              arg = .false.
            else
              ierr = 1
            end if
          end if
        else
c ------- at least a '.t' or '.T' will mean .T.:
          if (str(1:2) .eq. '.T') then
            arg = .true.
          else
            if (str(1:2) .eq. '.F') then
              arg = .false.
            else
              ierr = 1
            end if
          end if
        end if

        return
      end



c --- this routine returns the index of the word in str; the word
c     must be bounded by the beginning or end of the str or by a
c     blank; logical word is only the part of the physical word
c     as determined by lenl and lenr
      integer function indexw(str,word)
      implicit none
      integer ibeg,iend,ipos,llw,lrw,lrs,i1,i2,lenl,lenr
      character str*(*), word*(*)
      lrw = lenr(word)
      if (lrw .eq. 0) then
        indexw = 1
        return
      end if
      llw = lenl(word)
      lrs = lenr(str)
      iend= lrs - (lrw-llw)
      ibeg = 1
      if ((lrs .lt. ibeg) .or. (lrw.lt.llw)) go to 100
10    if(ibeg .gt. iend) go to 100
        ipos = index(str(ibeg:lrs), word(llw:lrw))
        if (ipos .gt. 0) then
          i1 = ibeg + ipos - 1
          i2 = i1 + lrw - llw
          if (i1 .eq. 1) then
            if (i2 .eq. lrs) go to 200
            if (str(i2+1:i2+1).eq.' ') go to 200
          else
            if (str(i1-1:i1-1).eq.' ') then
              if (i2 .eq. lrs) go to 200
              if (str(i2+1:i2+1).eq.' ') go to 200
            end if
          end if
        else
          go to 100
        end if
        ibeg = ibeg + ipos
      go to 10
100   continue
      indexw = 0
      return
200   continue
      indexw = i1
      return
      end

c --- returns the string item to the left of the strin(ipos:ipos):
c     the string item starts at the first non-blank character to the
c     left of the ipos and ends at the first blank character to
c     the left of ipos or at the beginning of the strin, whichever first.
      subroutine rdleft(strin, ipos, strout)
        implicit none
        integer ipos,i,il,ib
        character strin*(*), strout*(*)
        logical ilndsp

        if ((ipos .ge. 2) .and. (ipos .le. len(strin))) then

c ------- find the last position of the string item to the left:
          do 10  i = ipos-1, 1, -1
            il = i
            if (.not. ilndsp(strin(il:il))) go to 15
10        continue
c ------- all blanks to the left: return
          strout = ' '
          return
15        continue

c ------- find the first position of the string to the left:
          do 20  i = il, 1, -1
            ib = i
            if (ilndsp(strin(ib:ib))) go to 30
20        continue
          ib = 0
30        ib = ib + 1

          if (il-ib+1 .gt. len(strout)) then
            write(*,*) 'Error[rdleft]: strout too short.'
            return
          end if

          strout = strin(ib:il)
        else
          strout = ' '
        end if
        return
      end

c --- to remove chars from the string
      subroutine rmchrs(str,ch)
      implicit none
      integer lr,lenr,i,k
      character str*(*),ch*1
      lr = lenr(str)
10    if (lr .eq. 0) return
        i = index(str, ch)
        if (i .eq. 0) return
        lr = lr - 1
        do  k = i, lr
          str(k:k) = str(k+1:k+1)
        end do
        str(lr+1:lr+1) = ' '
      go to 10
      end

c --- to remove all primes from the string
      subroutine rmprimes(str)
      implicit none
      integer lr,lenr,i,k
      character str*(*)

      lr = lenr(str)
10    if (lr .eq. 0) return
        i = index(str, '''')
        if (i .eq. 0) return
        lr = lr - 1
        do  k = i, lr
          str(k:k) = str(k+1:k+1)
        end do
        str(lr+1:lr+1) = ' '
      go to 10

      end
c
c --- If there are '(' or ')' in line, it deals only with the part
c     between the parentheses, ignoring the parentheses. It then reads
c     words from a selected part of line, returns them in buffr and the
c     number of them in n; the word is: if there is a ' character in the text,
c     then the word is everything between the two successive ',
c     otherwise it is everything non-blank between two blanks. Non-primed
c     words can also occur between the primed ones. This later part is
c     like str_sn2.
      subroutine str_sn4(line, buffr, maxn, n)
        implicit none
        integer maxpos
        parameter (maxpos = 100)
        integer i,n,maxn,ind1,ind2,lenr,ipos(maxpos),ip,ni,i1,i2,nw,npos
        character line*(*), buffr(maxn)*(*)

        n = 0

c ----- these two are the only occurences of the delimiter symbols:
        ind1 = index(line,'(') + 1
        ind2 = index(line,')')
        if (ind2 .eq. 0) then
          ind2 = lenr(line)
        else
          ind2 = ind2 - 1
        end if
        if (ind2 .eq. 0) return

        ip = index(line(ind1:ind2), '''')
        if (ip .gt. 0) then
c ------- find positions of all '
          call getpos2(line(ind1:ind2), '''', ipos, maxpos, npos)
c ------- testing:
          ni = npos / 2
          if(ni*2 .ne. npos) then
            write(*,'(a,i4)') 'str_sn4_E> # of '' <> even:, ',npos
            write(*,'(a,i4,1x,a)') 'Len,String: ', lenr(line),
     &                             line(1:max(1,lenr(line)))
            stop
          end if
c ------- words before the first prime:
          i1 = ind1
          i2 = ind1+ipos(1)-2
          if (i2 .ge. i1) call str_sn3(line(i1:i2), buffr, maxn, n)
c ------- words after the first prime
          ipos(npos+1) = 9999
          do i = 1, npos, 2
            i1 = ind1+ipos(i)
            i2 = ind1+ipos(i+1)-2
            n = n + 1
            if (i2.ge.i1) then
              buffr(n)=line(i1:i2)
            else
              call blank(buffr(n))
            end if
c --------- words before the next prime (and after the last prime in the last
c           cycle):
            i1 = ind1+ipos(i+1)
            i2 = min(ind1+ipos(i+2)-2, ind2)
            if(i2.ge.i1) then
              call str_sn3(line(i1:i2), buffr(n+1), maxn, nw)
              n = n + nw
            end if
          end do
        else
          if (ind2.ge.ind1)
     &        call str_sn3(line(ind1:ind2), buffr, maxn, n)
        end if
        return
      end
c
c
c --- reads words from line, returns them in buffr and the number of
c     them in n; the word is: everything non-delimiter between two
c     delimiters, including nothing.
      subroutine str_sn5(line, buffr, maxn, n, delim)
        implicit none
        integer maxn,n,lr,lenr,idelim2,idelim1
        character line*(*), buffr(maxn)*(*), delim*(*)

        n  = 0
        lr = lenr(line)
        if (lr .eq. 0) return

        idelim1 = 0
        do  idelim2 = 1, lr
          if (line(idelim2:idelim2) .eq. delim) then
            n = n + 1
            if (n .gt. maxn) then
              write(*,'(a,i4)') 'str_sn5___E> increase MAXN: ', maxn
              stop
            end if
            if (idelim2.gt.idelim1+1) then
              buffr(n) = line(idelim1+1:idelim2-1)
            else
              buffr(n) = ' '
            end if
            idelim1 = idelim2
          end if
        end do

c ------- The last word:
          idelim2 = lr + 1

          n = n + 1
          if (n .gt. maxn) then
            write(*,'(a,i4)') 'str_sn5___E> increase MAXN: ', maxn
            stop
          end if
          if (idelim2.gt.idelim1+1) then
            buffr(n) = line(idelim1+1:idelim2-1)
          else
            buffr(n) = ' '
          end if

        return
      end
c
c
c --- adds string add to the end of string str (using concat)
      subroutine addstr(str,add)
        implicit none
        integer maxstr
        parameter (maxstr=1024)
        integer lr1,lr2,lenr,lr
        character dummy*(maxstr), str*(*), add*(*)

        lr1 = lenr(str)
        lr2 = lenr(add)
        lr  = len(str)
        if (lr1+lr2 .gt. maxstr) stop 'Error[addstr]; increase maxstr.'
        if (lr1+lr2 .gt. lr)
     &     stop 'Error[addstr]; increase the length of str.'
        call concat(str, add, dummy)
        str = dummy

        return
      end


c --- adds string add to the end of string str (using concat2)
      subroutine add2str(str,add)
        implicit none
        integer maxstr
        parameter (maxstr=1024)
        integer lr1,lr2,lenr,lr
        character dummy*(maxstr), str*(*), add*(*)

        lr1 = lenr(str)
        lr2 = lenr(add)
        lr  = len(str)
        if (lr1+lr2 .gt. maxstr) stop 'Error[addstr]; increase maxstr.'
        if (lr1+lr2 .gt. lr)
     &     stop 'Error[addstr]; increase the length of str.'
        call concat2(str, add, dummy)
        str = dummy

        return
      end


c --- adds string str to the beginning of string add (concat; output: add)
      subroutine add3str(str,add)
        implicit none
        integer maxstr
        parameter (maxstr=1024)
        integer lr1,lr2,lenr,lr
        character dummy*(maxstr), str*(*), add*(*)

        lr1 = lenr(str)
        lr2 = lenr(add)
        lr  = len(add)
        if (lr1+lr2 .gt. maxstr) stop 'Error[addstr]; increase maxstr.'
        if (lr1+lr2 .gt. lr)
     &     stop 'Error[addstr]; increase the length of add.'
        call concat(str, add, dummy)
        add = dummy

        return
      end


c --- adds string str to the beginning of string add (concat2; output: add)
      subroutine add4str(str,add)
        implicit none
        integer maxstr
        parameter (maxstr=1024)
        integer lr1,lr2,lenr,lr
        character dummy*(maxstr), str*(*), add*(*)

        lr1 = lenr(str)
        lr2 = lenr(add)
        lr  = len(add)
        if (lr1+lr2 .gt. maxstr) stop 'Error[addstr]; increase maxstr.'
        if (lr1+lr2 .gt. lr) then
          write(*,*) 'Error[add4str]; increase the length of add: ',
     &    lr1, lr2, lr
          write(*,*) '      str :', str
          write(*,*) '      add :', add
          stop
        end if
        call concat2(str, add, dummy)
        add = dummy

        return
      end


c --- finds the index of an array element that is exactly equal to WORD
      integer function ifindword(word,array,narray)
        implicit none
        integer narray,i
        character array(narray)*(*)
        character word*(*)

        do  i = 1, narray
          if (word .eq. array(i)) then
C --------- found it, return its index ;
            ifindword = i
            return
          end if
        end do

C ----- search not successful; return 0 ;
        ifindword = 0

        return
      end

c --- finds the index of an array element that contains WORD (index())
      integer function ifind3word(word,array,narray)
        implicit none
        integer narray,i
        character array(narray)*(*)
        character word*(*)

        do  i = 1, narray
          if (index(array(i), word) .gt. 0) then
C --------- found it, return its index ;
            ifind3word = i
            return
          end if
        end do

C ----- search not successful; return 0 ;
        ifind3word = 0

        return
      end

c --- finds an index of an array element that contains a
c     'blank-separated' word WORD; starts from the end;
      integer function ifind6word(word,array,narray)
        implicit none
        integer i, narray, indexw
        character array(narray)*(*)
        character word*(*)

        do  i = narray, 1, -1
c ------- old:
c          if (indexw(word, array(i)) .gt. 0) then
c ------- new:
          if (indexw(array(i),word) .gt. 0) then
c --------- found it, return its index ;
            ifind6word = i
            return
          end if
        end do

c ----- search not successful; return 0 ;
        ifind6word = 0

        return
      end


c --- finds an index of an array element that contains a
c     'blank-separated' word WORD;
      integer function ifind2word(word,array,narray)
        implicit none
        integer i, narray, indexw
        character array(narray)*(*)
        character word*(*)

        do  i = 1, narray
c ------- old:
c          if (indexw(word, array(i)) .gt. 0) then
c ------- new:
          if (indexw(array(i),word) .gt. 0) then
c --------- found it, return its index ;
            ifind2word = i
            return
          end if
        end do

c ----- search not successful; return 0 ;
        ifind2word = 0

        return
      end

c --- a vector version of ifindword; exact matches;
      subroutine ifindwordn(word,nword,array,narray,iword)
        implicit none
        integer nword,iword(nword),ityp,narray,i,ifindword
        character word(nword)*(*), array(narray)*(*)

        do 200  i = 1, nword
          ityp = ifindword(word(i),array,narray)
          if (ityp.ne.0) then
            iword(i) = ityp
          else
            write(*,190) word(i)
190         format('Error[ifindwordn]: Word not found in array; ', a)
          end if
200     continue

        return
      end


c --- this function returns the first occurence of target in str, but
c     only using those parts of string which are not quoted in unprotected ''
      integer function indexs(str, target)
        implicit none
        character str*(*), target*(*)
        integer ip1, i, ind, lr, lenr
        logical prime,isprime

        prime = .false.
        ip1 = 1
        lr = lenr(str)
        do  i = 1, lr
          if (isprime(str,i)) then
            prime = .not. prime
            if (prime) then
              if (i .gt. ip1) then
                ind = index(str(ip1:i-1), target)
                if (ind .gt. 0) then
                  indexs = ip1 + ind - 1
                  return
                end if
              end if
            else
              ip1 = i + 1
            end if
          end if
        end do

        if (.not. prime) then
          ind = index(str(ip1:), target)
          if (ind .gt. 0) then
            indexs = ip1 + ind - 1
          else
            indexs = 0
          end if
        else
          indexs = 0
        end if

        return
      end


c --- substitute string str1 with str2 in all n1 x n2 elements of
c     str(n1,n2)*(*)

      subroutine subs2nn(str,n1,n2,str1,str2)
        implicit none
        integer i1, i2, n1, n2, indexw
        character str(n1,n2)*(*), str1*(*), str2*(*)
        do  i1 = 1, n1
          do  i2 = 1, n2
            if (indexw(str(i1,i2), str1) .gt. 0) str(i1,i2) = str2
          end do
        end do
        return
      end

c --- concatenate N ARGS into RES
      subroutine concat3(args,n,res)
        implicit none
        integer i,n
        character args(n)*(*), res*(*)
        if (n .eq. 0) write(*,'(a)') 'concat3_w> no operands'
        res = args(1)
        do  i = 2, n
          call add2str(res, args(i))
        end do
        return
      end


c --- is string TARGET in string SINP, case insensitive
      logical function strgin(sinp, target)
        implicit none
        integer indexw
        character sinp*(*), target*(*), sinp2*(512), target2*(512)

        sinp2 = sinp
        call upper(sinp2)
        target2 = target
        call upper(target2)
        strgin = indexw(sinp2, target2) .gt. 0

        return
      end


c --- finds indices of array elements that contain word WORD (according
c     to index3(), where WORD can contain character # which will match
c     any digit [0..9] in ARRAY(I));
      subroutine ifind4word(word,array,narray,ind,mind,nind)
        implicit none
        integer i, narray, index3, mind, ind(mind), nind
        character array(narray)*(*)
        character word*(*)

        nind = 0
        do  i = 1, narray
          if (index3(array(i), word) .gt. 0) then
            nind = nind + 1
            ind(nind) = i
          end if
        end do

        return
      end


c --- will return 1 if TARGET is equal to WORD, where
c     character # in TARGET matches any digit [0..9] in WORD.
c --- assuming both are left justified;
      integer function index3(word, target)
        implicit none
        integer ir1, ir2, j, lenr
        character word*(*), target*(*)
        external lenr

        ir1 = lenr(word)
        ir2 = lenr(target)

        if (ir2 .eq. ir1) then
          do  j = 1, ir2
            if (word(j:j) .ne. target(j:j) .and.
     &          ((index('0123456789',word(j:j)) .lt. 0) .or.
     &           (target(j:j) .ne. '#'))) then
              index3 = 0
              return
            end if
          end do
          index3 = 1
        else
          index3 = 0
        end if

        return
      end


c --- finds an index of an array element that contains a
c     'blank-separated' word WORD (strgin); case insensitive;
      integer function ifind5word(word,array,narray)
        implicit none
        integer i, narray
        character array(narray)*(*), word*(*)
        logical strgin

        do  i = 1, narray
          if (strgin(array(i),word)) then
c --------- found it, return its index ;
            ifind5word = i
            return
          end if
        end do

c ----- search not successful; return 0 ;
        ifind5word = 0

        return
      end


c --- like str_sn2, except that a prime can be protected by \.
      subroutine str_sn6(line, buffr, maxn, n)
        implicit none
        integer maxn,n,i
        character line*(*), buffr(maxn)*(*), line2*(4096)

        line2 = line

        call subs2(line2,char(92)//'''',char(92)//char(92))
        call str_sn2(line2, buffr, maxn, n)
        do  i = 1, n
          call subs2(buffr(i),char(92)//char(92),'''')
        end do

        return
      end


      logical function isprime(line, i)
        implicit none
        integer i, im
        character line*(*)

        im = max(1,i-1)
        isprime = line(i:i).eq.'''' .and. line(im:im).ne.char(92)

        return
      end

