      program smooth1
      parameter (maxn=1000, maxcol=10)
      integer n,isegm,ncolumn,ierr
      real y(maxn,maxcol),ys(maxn,maxcol)
      character ifile*40, ofile*40, swind*20, scolumn*20

      if (iargc() .ne. 4) stop 'usage: inp_file out_file window columns'
      call getarg(1, ifile)
      write(iolog,*) 'Input file: ', ifile
      call getarg(2, ofile)
      write(iolog,*) 'Output file: ', ofile
      call getarg(3, swind)
      call str_i(swind, isegm, ierr)
      write(iolog,*) 'Half-window size: ', isegm
      call getarg(4, scolumn)
      call str_i(scolumn, ncolumn, ierr)
      write(iolog,*) 'Number of columns: ', ncolumn

      open(11, file=ifile, status='old')
      n = 1
10    read(11,*,end=100)(y(n,k),k=1,ncolumn)
        n = n + 1
        go to 10
100   continue
      n = n - 1
      close(11)

      do  i = 1, ncolumn
        call smth2(y(1,i),n,isegm,ys(1,i),maxn)
      end do

      open(11, file=ofile, status='unknown')
      do  i = 1, n
        write(11, '(99f8.3)') (ys(i,ic),ic=1,ncolumn)
      end do
      close(11)

      stop
      end


      subroutine smth2(y,n,isegm,ys,maxn)
      integer n, isegm
      real y(maxn),ys(maxn)
      do i = 1, n
        ys(i) = average2(y,i,isegm,n)
      end do
      return
      end


      function average2(y,i,isegm,n)
      integer n, isegm
      real y(n)

      i1 = max(1,i-isegm)
      i2 = min(n,i+isegm)
      ic = 0
      av = 0.0
      do 10  j = i1, i2
        av = av + y(j)
        ic = ic + 1
10    continue
      average = av / ic

      return
      end
