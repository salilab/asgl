c --- hist2d
      subroutine act31
#include "top_appl.cmn"

c ----- generate trivial x's if they are not present:
        call colmn(tab,iprm(1,37),iprm(1,61),ncolmn,maxpnt,maxclm,ilast,
     -             .true.)
        call colmn(tab,iprm(2,37),iprm(1,61),ncolmn,maxpnt,maxclm,ilast,
     -             .true.)

c ----- x-column:
        if (iprm(1,46).ne.1) then
          iprm(1,46)=1
          iprm(1,47)=iprm(1,37)
        end if
        if (iprm(1,47) .ne. iprm(1,37)) then
          write(iolog,'(a,2i4/a)') 
     -    'act31___w> X_COLUMN <> X_SCOLUMN: ', iprm(1,37), iprm(1,47),
     -    '           if correction needed: SET NO_XY_SCOLUMNS = 0 n'
        end if

c ----- y-columns:
        if ((iprm(2,46).lt.1).or.(iprm(2,46).gt.ncolmn-1))then
          iprm(2,46)=1
          iprm(2,47)=iprm(2,37)
        end if
        if (iprm(2,46) .gt. 1) then
          write(iolog,'(a)') 'act31____> stacked bars in a histogram'
        else
          if (iprm(2,47) .ne. iprm(2,37)) then
            write(iolog,'(a,2i4/a)') 
     -      'act31___w> Y_COLUMN <> Y_SCOLUMN: ',iprm(2,37), iprm(2,47),
     -      '           if correction needed: SET NO_XY_SCOLUMNS = 0 0'
          end if
        end if

c ----- check:
        if ((iprm(1,47).lt.1).or.(iprm(1,47).gt.ncolmn)) then
          write(iolog,'(a)') 'act31__E> illegal x-column'
          stop
        end if
        do  iy = 2, 1+iprm(2,46)
          if ((iprm(iy,47).lt.1).or.(iprm(iy,47).gt.ncolmn)) then
            write(iolog,'(a,3i5)') 
     -      'act31__E> illegal y-column: ',iy,iprm(iy,47),ncolmn
            stop
          end if
        end do

c ----- (tab,y,n,line_type,grayness,relative_bar_width)
        call topgset('hist2')
        call hist2(tab(1,iprm(1,37)),tab,iprm(1,61),
     -             iprm(iprm(1,46)+1,47),iprm(2,46),
     -             iprm(1,38),rprm(1,36),rprm(1,37),
     -             rprm(1,32),rprm(3,32),rprm(1,59),iprm(1,72))
        call topgrestore('hist2')

        called(31) = .true.

        return
      end



c --- plot2d
      subroutine act32
#include "top_appl.cmn"
        real r, prob, z, c(maxpnt), hue1, hue2, cscale, ctrans
        real rminv, rmaxv
        external rminv, rmaxv

c ----- generate trivial x's, y's, colors, if they are not present:
        call colmn(tab,iprm(1,37),iprm(1,61),ncolmn,maxpnt,maxclm,ilast,
     -             .true.)
        call colmn(tab,iprm(2,37),iprm(1,61),ncolmn,maxpnt,maxclm,ilast,
     -             .true.)

        if (iprm(1,76).ge.1 .and. iprm(1,76).le.ncolmn) then
c ------- calculate the color first:
          if (rprm(1,81).eq.-999.)
     &      rprm(1,81)=rminv(tab(1,iprm(1,76)),iprm(1,61))
          if (rprm(2,81).eq.-999.)
     &      rprm(2,81)=rmaxv(tab(1,iprm(1,76)),iprm(1,61))
          write(iolog,'(a,2g14.4)')
     -    'plot1___> Cmin, Cmax: ', rprm(1,81),rprm(2,81)
c ------- get a scaling factor for the color:
c         from red to blue:
          hue1 = 0.
          hue2 = 0.5
          cscale = (hue2-hue1) / (rprm(2,81)-rprm(1,81)) 
          ctrans = -rprm(1,81)*cscale + hue1
          do  i = 1, iprm(1,61)
            c(i) = max(hue1, min(hue2, 
     -             grayness(sprm(1,63),tab(i,iprm(1,76)),rprm(1,81),
     -                      rprm(2,81),cscale,ctrans,hue1,hue2)))
          end do
        else
          do  i = 1, iprm(1,61)
            c(i) = -1.
          end do
        end if

c ----- (tab,y,color,n,line_type,symbol_type,point_font)
        call topgset('plot1')
        call plot1(tab(1,iprm(1,37)),tab(1,iprm(2,37)),c,iprm(1,61),
     -             iprm(1,36),iprm(1,35),iprm(1,49),iprm(1,72))

c ----- calculate Pearson correlation coefficient
        if (iprm(1,61) .gt. 2) then
          call pearsn(tab(1,iprm(1,37)),tab(1,iprm(2,37)),iprm(1,61),
     -                r,prob,z)
        else
          r = -999.9
          prob = -999.9
          z = -999.9
        end if
        write(iolog,'(a,2i5)')  'Correlation between columns       : ',
     -                      iprm(1,37), iprm(2,37)
        write(iolog,'(a, i5)')  'Number of points                  : ', 
     -                      iprm(1,61)
        write(iolog,'(a,f10.5)')'Pearson correlation coeff (R)     : ',r
        write(iolog,'(a,f10.5)')'Pearson correlation coeff^2 (R^2) : ',r*r
        write(iolog,'(a,f10.5)')'Significance (small is signif)    : ',prob
        write(iolog,'(a,f10.5)')'Fischer'' z                       : ',z


c ----- plot point symbols if so desired
        if (iprm(1,58).ge.1 .and. iprm(1,58).le.ncolmn) then
          if (istrcol(iprm(1,58))) then
            call pntlab(tab(1,iprm(1,37)),tab(1,iprm(2,37)),
     -                  iprm(1,61),tabstr(1,invstrs(iprm(1,58))),
     -                  iprm(1,56),iprm(1,57))
          end if
        end if

        call topgrestore('plot1')

        called(32) = .true.

        return
      end




c --- returns tab(i,j), iprm(1,61) (number of i's), ncolmn (number of j's) ;
      subroutine act33
        implicit none
#include "top_appl.cmn"
        integer iet,ival,i,j,nc,nbuffr,lenr,icol1,nline,ierr,ispec
        real rval
        character line*(lenact), buffr(maxclm)*(colwdt)
        character sval*(colwdt)
        logical lval,cmpr

        call gennam2(sprm(1,4),sprm(1,6),'D',iprm(1,2),
     -               iprm(1,3),sprm(1,12),sprm(1,17))
        call fullfn(sprm(1,5),sprm(1,12),sprm(1,17),sprm(1,12),ierr,
     &              cmpr,1,0)

c ----- add columns?
        if (lprm(1,32)) then
          icol1 = ncolmn + 1
        else
          icol1 = 1
          nstrs = 0
          ncolmn = 0
        end if
c ----- add rows?
        if (lprm(2,32)) iprm(1,61) = 0

c ----- no 'artificial vector'
        ilast = 0
        iprm(1,61) = 0

c ----- read the first nonempty line that is not a comment, to be used only to 
c       get the number and type of the items in a row (alternatively, types 
c       of columns specified in #COLUMNS line):
        call openf(ioin, sprm(1,12), 'old')
10      read(ioin, '(a)', end=100) line
          if (line(1:9) .eq. '#COLUMNS ') then
            call str_sn2(line(10:), buffr, maxclm, nbuffr)
            ispec = 1
            go to 15 
          end if
        if (line(1:1) .eq. '#') go to 10
        if (lenr(line) .lt. 1) go to 10
        ispec = 2
        call str_sn2(line, buffr, maxclm, nbuffr)

15      continue
        close(ioin)

c ----- the new number of columns in the table:
        ncolmn = icol1 + nbuffr  - 1
        if (ncolmn .gt. maxclm) then
          write(iolog,'(a,i6)')
     &          'act33__E> increase MAXCLM to at least: ', ncolmn
          stop
        end if

        j = 0
        do  i = icol1, ncolmn
          j = j + 1
          if (ispec .eq. 2) then
            call elmtyp(buffr(j),iet,ival,rval,sval,lval)
          else
            if (buffr(j) .eq. 'N') then
              iet = 1
            else
              iet = 3
            end if
          end if
          if (iet.eq.3.or.iet.eq.4) then
            istrcol(i) = .true.
            nstrs = nstrs + 1
            if (nstrs.gt.maxsclm) then
              write(iolog,'(a)') 'act33___E> Increase MAXSCLM.'
              stop
            end if
            istrs(nstrs) = i
            invstrs(i) = nstrs
          else
            istrcol(i) = .false.
          end if
        end do

        if (iprm(1,59) .lt. 1) then
          write(iolog,'(a)') 'act33__E> POINT_MODULUS < 1'
          stop
        end if

c ----- columns to be read this time:
        nc = ncolmn-icol1+1
        call openf(ioin, sprm(1,12), 'old')
        nline = 0
20      read(ioin, '(a)', end=100) line
          if (lenr(line) .lt. 1) go to 20
          if (line(1:13) .eq. '#CAPTION_TEXT') then
            sprm(1,40) = line(15:)
            write(iolog,*) 'CAPTION_TEXT assigned: ', sprm(1,40)
          end if
          if (line(1:11) .eq. '#PRINT_TEXT')   sprm(1,41) = line(13:)
          if (line(1:12) .eq. '#DESCRIPTION')  sprm(1,48) = line(14:)
          if (line(1:1)  .eq. '#') go to 20

          nline = nline + 1

          if (iprm(1,71).eq.0 .or. iprm(1,71).eq.-999 .or. 
     &        iprm(1,71).le.nline) then
            if (iprm(2,71).eq.0 .or. iprm(2,71).eq.-999 .or. 
     &          iprm(2,71).ge.nline) then
              if (mod(nline,iprm(1,59)) .eq. 0) then

                iprm(1,61) = iprm(1,61) + 1
                if (iprm(1,61) .gt. maxpnt) then
                  write(iolog,'(a)') 
     &            'act33____E> too many rows; increase MAXPNT'
                  stop
                end if

                call str_sn2(line, buffr, maxclm, nbuffr)
                if (nbuffr .ne. nc) then
                  write(iolog,'(a,2i5/a)')
     &            'act33___E> wrong format; items expected,items: ',
     &            nc, nbuffr,line(1:lenr(line))
                  stop
                end if

                j = 0
                do  i = icol1, ncolmn
                  j = j + 1
                  if (istrcol(i)) then
                    tabstr(iprm(1,61),invstrs(i)) = buffr(j)
                  else
                    call str_r(buffr(j), tab(iprm(1,61),i), ierr)
                    if (ierr .gt. 0) then
                      write(iolog,'(2a/a)')
     &                'act33___E> not a real: ', 
     &                buffr(j), line(1:lenr(line))
                      stop
                    end if
                  end if
                end do

              end if
            end if
          end if

          go to 20
100     continue
        close(ioin)
c        if (mod(nline,iprm(1,59)) .eq. 0) iprm(1,61) = iprm(1,61) - 1

        write(iolog,'(a,i3,2i8)') 'act33____> columns,points,lines: ',
     -             ncolmn,iprm(1,61),nline
        inp = 1
        called(33) = .true.

        return
      end


      subroutine act34
#include "top_appl.cmn"

c ----- xpos(i),ypos(i): starting positions of the first label of type i
        call initp(xpos,ypos,rprm(1,57),rprm(1,65),x0plt,y0plt,x1plt,
     -             y1plt,iprm(1,48))

        called(34) = .true.

        return
      end


c --- set the scale and draw the world:
      subroutine act35
#include "top_appl.cmn"
        real persp, rmaxv, rminv

c ----- was a molecule read in last:
        if (inp .eq. 3) then
          iprm(1,61) = numsel
          ncolmn = 2
          iprm(1,37) = 1
          iprm(2,37) = 2
          do  i = 1, numsel
            ia = isel(i)
            tab(i,3) = zpdb(ia)
          end do
          rprm(5,32) = rminv(tab(1,3), iprm(1,61))
          rprm(6,32) = rmaxv(tab(1,3), iprm(1,61))
          do  i = 1, numsel
            ia = isel(i)
            if (lprm(1,31)) then
              tab(i,1) = persp(xpdb(ia), zpdb(ia))
              tab(i,2) = persp(ypdb(ia), zpdb(ia))
            else
              tab(i,1) = xpdb(ia)
              tab(i,2) = ypdb(ia)
            end if
          end do
        end if

c ----- generate trivial x's and y's, if they are not present:
        call colmn(tab,iprm(1,37),iprm(1,61),ncolmn,maxpnt,maxclm,ilast,
     -             .true.)
        call colmn(tab,iprm(2,37),iprm(1,61),ncolmn,maxpnt,maxclm,ilast,
     -             .true.)

c ----- use a position specifier to find the paper window if so selected:
        if (iprm(1,44).ne.0) call position(iprm(1,44),iprm(2,44),
     -      rprm(1,31),rprm(2,31),rprm(3,31),rprm(4,31),rprm(5,31))

c ----- Plot window from the current PAPER_WINDOW (retain the aspect ratio):
        x0plt = 0.0
        y0plt = 0.0
        x1plt = 1.0
        y1plt = (rprm(4,31)-rprm(2,31)) / (rprm(3,31)-rprm(1,31))

c ----- create the position of the plot on the page
        ndim = max(2,iprm(1,46) + iprm(2,46))
        call world(iprm(1,37),iprm(1,46),iprm(1,47),ndim,iprm(1,61),
     -             rprm(1,32),rprm(2,32),rprm(3,32),rprm(4,32),
     -             rprm(1,31),rprm(2,31),rprm(3,31),rprm(4,31),
     -             rprm(5,31),lprm(1,37),rprm(1,80))

        called(35) = .true.

        return
      end


c --- returns the arr(i,j): nxdim by nydim
      subroutine act36
#include "top_appl.cmn"
        integer i, j, ierr
        real a(maxclm)
        character line*2048, buffr(maxclm)*(30)
        logical cmpr

        call gennam2(sprm(1,4),sprm(1,6),'D',iprm(1,2),
     &               iprm(1,3),sprm(1,12),sprm(1,17))
        call fullfn(sprm(1,5),sprm(1,12),':.dat:.out:.mat',sprm(1,12),
     &              ierr,cmpr,1,0)
        call openf(ioin, sprm(1,12), 'old')

        if (.not. lprm(1,35)) then

          write(iolog,'(a)') 'act36____> reading unformatted array file'

          if (sprm(1,58).eq.'XY') then
            read(ioin,*)nxdim,nydim
            if (nxdim.gt.maxxarr) then
              write(iolog,'(a,i8)') 'act36___E> increase MAXXARR: ',nxdim
              stop
            end if
            if (nydim.gt.maxyarr) then
              write(iolog,'(a,i8)') 'act36___E> increase MAXYARR: ',nydim
              stop
            end if
            do  i = 1, nxdim
              read(ioin,*) (arr(i,j),j=1,nydim)
            end do

          else

            read(ioin,*)nydim,nxdim
            if (nxdim.gt.maxxarr) then
              write(iolog,'(a,i8)') 'act36___E> increase MAXXARR: ',nxdim
              stop
            end if
            if (nydim.gt.maxyarr) then
              write(iolog,'(a,i8)') 'act36___E> increase MAXYARR: ',nydim
              stop
            end if
            do  j = 1, nydim
              read(ioin,*) (arr(i,j),i=1,nxdim)
            end do
          end if

        else

          write(iolog,'(a)') 'act36____> reading formatted array file'

          if (iprm(1,64).gt.maxclm) then
            write(iolog,'(a)') 'act36__E> increase MAXCLM'
            stop
          end if
          do  i = 1, maxxarr
            do  j = 1, maxyarr
              arr(i,j) = rprm(1,64)
            end do
          end do
          nxdim = 0
          nydim = 0
          il = 0
c 10        read(ioin, *, end=100) i,j,(a(k),k=1,iprm(1,64))
10        read(ioin, '(a)', end=100) line
            if (line(1:1) .eq. '#') go to 10
            il = il + 1

            call str_sn(line, buffr, iprm(1,64)+2)
            call str_i(buffr(1), i, ierr)
            call str_i(buffr(2), j, ierr)
            call str_r(buffr(iprm(1,64)+2), a(iprm(1,64)), ierr)

            if (min(i,j) .lt. 1) then
              write(iolog,'(a,3i4,g15.5)')
     -        'act36__E> line; indices less than 0: ', il, i, j,
     -        a(iprm(1,64))
              do  i = 1, iprm(1,64)
                write(iolog,'(a,1x,i5,1x,a)')
     -          'act36__E> i, buffr: ', i, buffr(i+2)
              end do
              stop
            end if
            if (i.gt.maxxarr) then
              write(iolog,'(a,i8)') 'act36___E> increase MAXXARR: ',i
              stop
            end if
            if (j.gt.maxyarr) then
              write(iolog,'(a,i8)') 'act36___E> increase MAXYARR: ',j
              stop
            end if

            nxdim = max(i,nxdim)
            nydim = max(j,nydim) 
            arr(i,j) = a(iprm(1,64))
            if (lprm(1,36)) then
              if (i.gt.maxyarr) then
                write(iolog,'(a,i8)')'act36___E> increase MAXYARR: ',i
                stop
              end if
              if (j.gt.maxxarr) then
                write(iolog,'(a,i8)')'act36___E> increase MAXXARR: ',j
                stop
              end if
              arr(j,i) = arr(i,j)
            end if
            go to 10
100       continue

          if (lprm(1,36)) then
            nxdim = max(nxdim, nydim)
            nydim = nxdim
          end if

        end if

        close(ioin)
        inp = 2

        called(36) = .true.

        return
      end 

      subroutine act37
#include "top_appl.cmn"

        called(37) = .true.

        return
      end

c --- plot the 2D line:
      subroutine act38
#include "top_appl.cmn"

c ----- x1,y1,x2,y2,width,gray
        call topgset('line2d')
c ----- CLIP the two points by the current WORLD_WINDOW:
        
        if (lprm(1,41)) then
c ------- clip everything outside the Plot area:
          d = 0.5*linwdt(iprm(1,72))
          call clip(x0plt+d, y0plt+d, x1plt-d, y1plt-d)
C          rprm(1,43)=max(rprm(1,32),min(rprm(1,43),rprm(3,32)))
C          rprm(2,43)=max(rprm(2,32),min(rprm(2,43),rprm(4,32)))
C          rprm(1,44)=max(rprm(1,32),min(rprm(1,44),rprm(3,32)))
C          rprm(2,44)=max(rprm(2,32),min(rprm(2,44),rprm(4,32)))
        end if

        if ((iprm(1,70).ge.1) .and. (iprm(1,70).le.nlintp)) then
          call line(wpx(rprm(1,43)),wpy(rprm(2,43)),
     -              wpx(rprm(1,44)),wpy(rprm(2,44)),
     -              iprm(1,70))
        else
          if (sprm(1,60).eq.'X' .or. sprm(1,60).eq.'x') then
            call line2(wpx(rprm(1,43)),wpy(rprm(2,43)),
     -                 wpx(rprm(1,44)),wpy(rprm(2,44)),
     -                 wpdx(rprm(1,46)),rprm(1,45))
          else
            call line2(wpx(rprm(1,43)),wpy(rprm(2,43)),
     -                 wpx(rprm(1,44)),wpy(rprm(2,44)),
     -                 wpdy(rprm(1,46)),rprm(1,45))
          end if
        end if
        call topgrestore('line2d')
        called(38) = .true.

        return
      end

c --- print text:
      subroutine act39
#include "top_appl.cmn"
        real x, y
        logical strgin
        external strgin

c        write(iolog,*) sprm(1,55)
c        write(iolog,*) iprm(1,69)

        if (indexw(sprm(1,55), 'POINT') .gt. 0) then
c ------- if point not defined, use the last one:
          if((iprm(1,69).lt.1).or.(iprm(1,69).gt.iprm(1,61)))
     -      iprm(1,69)=iprm(1,61)
          x = tab(iprm(1,69),iprm(1,37)) + rprm(1,78)
          y = tab(iprm(1,69),iprm(2,37)) + rprm(2,78)
c          write(iolog,*) iprm(1,69), iprm(1,61), iprm(1,37), iprm(2,37)
c          write(iolog,*) rprm(1,78), rprm(2,78), x, y
        else
          x = rprm(1,41) + rprm(1,78)
          y = rprm(2,41) + rprm(2,78)
        end if

        call topgset('text')
        call text(sprm(1,41),x,y,iprm(1,52),rprm(1,42),iprm(1,53),
     -            iprm(1,54),strgin(sprm(1,61),'WORLD'))
        call topgrestore('text')

        called(39) = .true.

        return
      end


c --- new page:
      subroutine act40
#include "top_appl.cmn"
c ----- eject the page
        call stamp
        call showpage(iprm(1,45))
c ----- start a new page
        call pspage
        called(40) = .true.
        return
      end

c --- arrow:
      subroutine act41
        implicit none
#include "top_appl.cmn"
        real wpx, wpy
        logical strgin
        external strgin

c ----- x1,y1,x2,y2,tail thickness,arrow width,arrow length
        call topgset('arrow')
        if (strgin(sprm(1,61),'PLOT')) then
          call arrow(rprm(1,66),rprm(2,66),
     -               rprm(3,66),rprm(4,66),
     -               rprm(1,35),rprm(2,35),rprm(3,35))
        else
          call arrow(wpx(rprm(1,66)),wpy(rprm(2,66)),
     -               wpx(rprm(3,66)),wpy(rprm(4,66)),
     -               rprm(1,35),rprm(2,35),rprm(3,35))
        end if
        call topgrestore('arrow')
        called(41) = .true.

        return
      end

c --- Prints PS command
      subroutine act42
#include "top_appl.cmn"

        call topgset('PS')
        write(iops, '(a)') sprm(1,49)(1:lenr(sprm(1,49)))
        call topgrestore('PS')

        called(42) = .true.

        return
      end



      subroutine act43
#include "top_appl.cmn"
        real tiny, sum
        parameter (tiny=1.0E-20)

        nvars = iprm(1,46) + iprm(2,46)
        if (nvars .gt. 0) then

c ------- transform the Table array:
          if (index(sprm(1,36), 'NORMALIZE') .gt. 0) then
            write(iolog,'(a)')
     -      'act43____> y = y/(Sum y)'
            do  i = 1, nvars
              ic = iprm(i,47)
              sum = 0.0
              do  j = 1, iprm(1,61)
                sum = sum + tab(j,ic)
              end do
              write(iolog,'(a,f10.5)') 'act43____> SUM = ', sum
              do  j = 1, iprm(1,61)
                tab(j,ic) = tab(j,ic)/sum
              end do
            end do 
          end if

          if (index(sprm(1,36), 'EXPONENTIAL') .gt. 0) then
            write(iolog,'(a,4f10.5/a)') 
     -      'act43____> parameters: ',rprm(1,40),rprm(2,40),rprm(3,40),
     -      rprm(4,40),
     -      '           y = a + exp[b + c*y^d]'
            do  i = 1, nvars
              ic = iprm(i,47)
              do  j = 1, iprm(1,61)
                tab(j,ic) = rprm(1,40) + exp(rprm(2,40) + 
     &                      rprm(3,40)*tab(j,ic)**rprm(4,40))
              end do
            end do 
          end if

          if (index(sprm(1,36), 'LOGARITHMIC1') .gt. 0) then
            xmin1 = rmina(tab,maxpnt,maxclm,iprm(1,61),nvars,iprm(1,47))
            write(iolog,'(a,3f10.5/a)') 
     -      'act43____> y_min, parameters: ',
     -      xmin1,rprm(1,40),rprm(2,40),
     -      '           y = Ln[a + b*(y-y_min)]'
            do  i = 1, nvars
              ic = iprm(i,47)
              do  j = 1, iprm(1,61)
                arg = rprm(1,40) + rprm(2,40)*(tab(j,ic)-xmin1)
                if (arg .lt. tiny) then
                  tab(j,ic) = rprm(1,77)
                else
                  tab(j,ic) = log(arg)
                end if
              end do
            end do 
          end if

          if (index(sprm(1,36), 'LOGARITHMIC2') .gt. 0) then
            write(iolog,'(a,2f10.5/a)') 
     -      'act43____> parameters: ', rprm(1,40),rprm(2,40),
     -      '           y = Ln[a + b*y]'
            do  i = 1, nvars
              ic = iprm(i,47)
              do  j = 1, iprm(1,61)
                arg = (rprm(1,40)+rprm(2,40)*tab(j,ic))
                if (arg .lt. tiny) then
                  tab(j,ic) = rprm(1,77)
                else
                  tab(j,ic) = log(arg)
                end if
              end do
            end do
          end if

          if (index(sprm(1,36), 'LOGARITHMIC3') .gt. 0) then
            xmin1 = rmina(tab,maxpnt,maxclm,iprm(1,61),nvars,iprm(1,47))
            write(iolog,'(a,3f10.5/a)') 
     -      'act43____> y_min, parameters: ',
     -      xmin1,rprm(1,40),rprm(2,40),
     -      '           y = Log10[a + b*(y-y_min)]'
            do  i = 1, nvars
              ic = iprm(i,47)
              do  j = 1, iprm(1,61)
                arg = rprm(1,40) + rprm(2,40)*(tab(j,ic)-xmin1)
                if (arg .lt. tiny) then
                  tab(j,ic) = rprm(1,77)
                else
                  tab(j,ic) = log10(arg)
                end if
              end do
            end do 
          end if

          if (index(sprm(1,36), 'LOGARITHMIC4') .gt. 0) then
            write(iolog,'(a,2f10.5/a)') 
     -      'act43____> parameters: ', rprm(1,40),rprm(2,40),
     -      '           y = Log10[a + b*y]'
            do  i = 1, nvars
              ic = iprm(i,47)
              do  j = 1, iprm(1,61)
                arg = (rprm(1,40)+rprm(2,40)*tab(j,ic))
                if (arg .lt. tiny) then
                  tab(j,ic) = rprm(1,77)
                else
                  tab(j,ic) = log10(arg)
                end if
              end do
            end do
          end if

          if (index(sprm(1,36), 'CUMULATIVE') .gt. 0) then
            write(iolog,'(a)') 
     -      'act43____> y_i = Sum_(k=1,i,1) y_k'
            do  i = 1, nvars
              ic = iprm(i,47)
              do  j = 2, iprm(1,61)
                tab(j,ic) = tab(j-1,ic)+tab(j,ic)
              end do
            end do
          end if

          if (index(sprm(1,36), 'LINEAR') .gt. 0) then
            write(iolog,'(a,2f10.5/a)') 
     -      'act43____> parameters: ', rprm(1,40),rprm(2,40),
     -      '           y = a + b*y'
            do  i = 1, nvars
              ic = iprm(i,47)
              do  j = 1, iprm(1,61)
                tab(j,ic) = rprm(1,40) + rprm(2,40)*tab(j,ic)
              end do
            end do
          end if

          if (index(sprm(1,36), 'INVERSE') .gt. 0) then
            write(iolog,'(a,2f10.5/a)') 
     -      'act43____> parameters: ', rprm(1,40),rprm(2,40),
     -      '           y = a + b/y'
            do  i = 1, nvars
              ic = iprm(i,47)
              do  j = 1, iprm(1,61)
                if (abs(tab(j,ic)) .lt. tiny) then
                  tab(j,ic) = rprm(1,77)
                else
                  tab(j,ic) = rprm(1,40) + rprm(2,40)/tab(j,ic)
                end if
              end do
            end do
          end if

        else

c ------- transform the DPLOT array:
          xmin1 = 1.0E34
          if (index(sprm(1,36), 'LOGARITHMIC1') .gt. 0) then
c --------- see above
            xmin1 = rminarr(arr,maxxarr,nxdim,nydim)
            write(iolog,'(a,3f10.5/a)') 
     -      'act43____> y_min, parameters: ',
     -      xmin1,rprm(1,40),rprm(2,40),
     -      '           y = Ln[a + b*(y-y_min)]'
            do  i = 1, nydim
              do  j = 1, nxdim
                arg = rprm(1,40) + rprm(2,40)*(arr(j,i)-xmin1)
                if (arg .lt. tiny) then
                  arr(j,i) = rprm(1,77)
                else
                  arr(j,i) = log(arg)
                end if
              end do
            end do 
          end if

          if (index(sprm(1,36), 'LOGARITHMIC2') .gt. 0) then
            write(iolog,'(a,2f10.5/a)') 
     -      'act43____> parameters: ', rprm(1,40),rprm(2,40),
     -      '           y = Ln[a + b*y]'
            do  i = 1, nydim
              do  j = 1, nxdim
                arg = rprm(1,40) + rprm(2,40)*arr(j,i)
                if (arg .lt. tiny) then
                  arr(j,i) = rprm(1,77)
                else
                  arr(j,i) = log(arg)
                end if
              end do
            end do
          end if

c ------- transform the DPLOT array:
          xmin1 = 1.0E34
          if (index(sprm(1,36), 'LOGARITHMIC3') .gt. 0) then
c --------- see above
            xmin1 = rminarr(arr,maxxarr,nxdim,nydim)
            write(iolog,'(a,3f10.5/a)') 
     -      'act43____> y_min, parameters: ',
     -      xmin1,rprm(1,40),rprm(2,40),
     -      '           y = Log10[a + b*(y-y_min)]'
            do  i = 1, nydim
              do  j = 1, nxdim
                arg = rprm(1,40) + rprm(2,40)*(arr(j,i)-xmin1)
                if (arg .lt. tiny) then
                  arr(j,i) = rprm(1,77)
                else
                  arr(j,i) = log10(arg)
                end if
              end do
            end do 
          end if

          if (index(sprm(1,36), 'LOGARITHMIC4') .gt. 0) then
            write(iolog,'(a,2f10.5/a)') 
     -      'act43____> parameters: ', rprm(1,40),rprm(2,40),
     -      '           y = Log10[a + b*y]'
            do  i = 1, nydim
              do  j = 1, nxdim
                arg = rprm(1,40) + rprm(2,40)*arr(j,i)
                if (arg .lt. tiny) then
                  arr(j,i) = rprm(1,77)
                else
                  arr(j,i) = log10(arg)
                end if
              end do
            end do
          end if

          if (index(sprm(1,36), 'LINEAR') .gt. 0) then
            write(iolog,'(a,2f10.5/a)') 
     -      'act43____> parameters: ', rprm(1,40),rprm(2,40), 
     -      '           y = a + b*y'
            do  i = 1, nydim
              do  j = 1, nxdim
                arr(j,i)=rprm(1,40)+arr(j,i)*rprm(2,40)
              end do
            end do 
          end if

          if (index(sprm(1,36), 'INVERSE') .gt. 0) then
            write(iolog,'(a,2f10.5/a)') 
     -      'act43____> parameters: ', rprm(1,40),rprm(2,40), 
     -      '           y = a + b/y'
            do  i = 1, nydim
              do  j = 1, nxdim
                if (abs(arr(j,i)) .lt. tiny) then
                  arr(j,i)=rprm(1,77)
                else
                  arr(j,i)=rprm(1,40)+rprm(2,40)/arr(j,i)
                end if
              end do
            end do 
          end if

        end if

        called(43) = .true.

        return
      end


c --- axes2d
      subroutine act44
#include "top_appl.cmn"

        call topgset('axes2d')
        call axes2d(x0plt,y0plt,x1plt,y1plt,rprm(1,32),rprm(2,32),
     -              rprm(3,32),rprm(4,32),
     -              rprm(1,33),rprm(1,34),rprm(2,33),rprm(2,34),
     -              rprm(3,33),rprm(3,34),
     -              sprm(1,34),sprm(1,37),iprm(1,31),iprm(1,32),
     -              iprm(1,33),iprm(1,34),mvsprm,
     -              iprm(1,41),iprm(2,41),iprm(1,42),iprm(2,42),
     -              iprm(1,48),rprm(1,57),rprm(1,65),sprm(1,50),
     -              rprm(1,67),rprm(1,68),rprm(1,71),rprm(1,72),
     -              lprm(1,39),iprm(1,72),iprm(1,73),iprm(1,74))
        call topgrestore('axes2d')
        called(44) = .true.

        return
      end

c --- draw a caption:
      subroutine act45
#include "top_appl.cmn"

        if (.not. called(34)) then
          write(iolog,'(a)') 
     -    'act45__E> WORLD, (AXES2D) and RESET_CAPTIONS first'
          stop
        end if
        call topgset('caption')
        call caption(xpos,ypos,sprm(1,40),iprm(1,50),iprm(1,51))
        call topgrestore('caption')

        called(45) = .true.

        return
      end


      subroutine colmn(tab,ix,npnts,ncolmn,maxpnt,maxclm,ilast,rising)
        real tab(maxpnt,maxclm)
        logical rising
        if (npnts .gt. maxpnt) then
          write(iolog,'(a)') 'colmn__E> increase MAXPNT'
          stop
        end if
        if (ncolmn .gt. maxclm) then
          write(iolog,'(a)') 'colmn__E> increase MAXCLM'
          stop
        end if
        if ((ix.lt.1).or.(ix.gt.ncolmn)) then
c ------- is there an assigned dummy vector that was used before:
          if (ilast .eq. 0) then
c           NO:
            ncolmn = ncolmn + 1
            ix = ncolmn
            ilast = ix
          else
c           YES:
            ix = ilast
          end if
          do i = 1, npnts
            if (rising) then
              tab(i,ix) = i
            else
              tab(i,ix) = 1.0
            end if
          end do
        end if

        return
      end



c --- plot the density plot:
      subroutine act46
#include "top_appl.cmn"
        call topgset('dplot1')
        call dplot1(arr,nxdim,nydim,maxxarr,rprm(1,38),rprm(2,38),
     -              iprm(1,39),sprm(1,32),rprm(1,39),rprm(2,39),
     -              sprm(1,33),lprm(1,33),iprm(1,55),iprm(2,55),
     -              lprm(1,34),iprm(1,4),iprm(2,4),iprm(1,49),
     -              iprm(1,52),x0plt,y0plt,x1plt,y1plt,rprm(1,78),
     -              rprm(2,78),iprm(1,72))
        call topgrestore('dplot1')
        called(46) = .true.

        return
      end



c --- get_bars (everything else but histogram is erased from the plotting 
c     array!)
      subroutine act47
#include "top_appl.cmn"
        integer nbars, iavrcol, istdcol

c ----- generate trivial x's and y's in their separate columns, 
c       if they are not present:
        ilast = 0
        call colmn(tab,iprm(1,37),iprm(1,61),ncolmn,maxpnt,maxclm,ilast,
     -             .true.)
        ilast = 0
        call colmn(tab,iprm(2,37),iprm(1,61),ncolmn,maxpnt,maxclm,ilast,
     -             .true.)

c ----- make space for average and standard deviation:
        ilast = 0
        iavrcol = 0
        call colmn(tab,iavrcol,maxpnt,ncolmn,maxpnt,maxclm,ilast,.true.)
        ilast = 0
        istdcol = 0
        call colmn(tab,istdcol,maxpnt,ncolmn,maxpnt,maxclm,ilast,.true.)

        write(iolog,*) 'X for bars in column                 : ',
     &                 iprm(1,37)
        write(iolog,*) 'Y for averaging and std dev in column: ',
     &                 iprm(2,37)
        write(iolog,*) 'Average in column                    : ',
     &                 iavrcol
        write(iolog,*) 'Standard deviation in column         : ',
     &                 istdcol

        if (ncolmn .le. (maxclm-2)) then 
c ------- create the histogram data in the first two free columns of the
c         plotting array:
          call gethist(tab(1,iprm(1,37)),iprm(1,61),rprm(1,58),
     -                 tab(1,ncolmn+1),tab(1,ncolmn+2),nbars,maxpnt,
     -                 rprm(1,32),rprm(3,32),tab(1,iprm(2,37)),
     -                 tab(1,iavrcol),tab(1,istdcol),lprm(1,42))
c ------- update the number of points in the plotting array
          iprm(1,61) = nbars
c ------- copy the bars over the original points:
          do  i = 1, iprm(1,61)
            tab(i,iprm(1,37)) = tab(i,ncolmn+1)
            tab(i,iprm(2,37)) = tab(i,ncolmn+2)
          end do
        else
          write(iolog,'(a)')
     -    'act47__E> not enough space; increase MAXCLM'
          stop
        end if

        called(47) = .true.

        return
      end


      subroutine act48
        implicit none
#include "top_appl.cmn"
        integer ierr
        logical cmpr

c ----- read the BRK file
        call gennam2(sprm(1,4),sprm(1,6),'B',iprm(1,2),
     &               iprm(1,3),sprm(1,12),sprm(1,17))
        call fullfn(sprm(1,5),sprm(1,12),':.atm:.pdb:.crd',sprm(1,12),
     &              ierr,cmpr,1,0)

        if (index(sprm(1,56), 'PDB') .gt. 0) then
          call readbrk(ioinp,iolog,sprm(1,12),xpdb,ypdb,zpdb,maxatm,
     &         maxres,natm,nres,resnam,resnum,atmnam,iatmr1,iatmr2,
     &         iresatm)
        else
          call rdxyz(ioinp,iolog,sprm(1,12),xpdb,ypdb,zpdb,maxatm,
     &         maxres,natm,nres,resnam,resnum,atmnam,iatmr1,iatmr2,
     &         iresatm)
        end if

        inp = 3

c ----- associate an (VDW.LIB,GROMOS) atom type with every atom in the list
        call atmtype(natm,atmnam,vdwatm,nvdwtyp,iattyp,vdwcnt,
     -               brkatm,groatm,iresatm,resnam,natmdc,mapres,
     -               nresdc,nres)

c ----- no atoms selected for display, for bonds, or for labelling
        numsel = 0
        numbsel = 0
        numlsel = 0

        called(48) = .true.

        return
      end


c --- plot ball-and-stick plot of a molecule
      subroutine act49
        implicit none
#include "top_appl.cmn"

        call topgset('draw_ball_stick')
        call draw_ball_stick(lprm(1,31),xpdb,ypdb,zpdb,isel,numsel,
     -       natm,atrad,acolor,ialine,ibond,bcolor,ibline,numbond,
     -       bond_fact,taper)
        call topgrestore('draw_ball_stick')

        called(49) = .true.

        return
      end


c --- label the selected atoms:
      subroutine act50
        implicit none
#include "top_appl.cmn"

c ----- create the desired labels for selected atoms
        call labatm(natm,isell,numlsel,atmnam,resnam,resnum,nres,
     -              iresatm,sprm(1,42),nvsprm(42),labls)

c ----- plot the labels
        call topgset('pltlab')
        call pltlab(lprm(1,31),xpdb,ypdb,zpdb,natm,isell,numlsel,
     -       labls,atrad,iprm(1,56),iprm(1,57))
        call topgrestore('pltlab')

        called(50) = .true.

        return
      end

      subroutine act51
        implicit none
        integer nbond2
#include "top_appl.cmn"
	 
c ----- calculate all bonds between all the selected atoms (add them
c       to the existing ones, or create the list from scratch)
	if (lprm(1,38)) then
	  nbond2 = numbond
	else
	  nbond2 = 0
	end if
	call calc_conn(xpdb,ypdb,zpdb,natm,iselb,numbsel,ibond,
     -       numbond,maxbond,lprm(1,38),sprm(1,46),iattyp,
     -       vdwcnt,nvdwtyp,rprm(1,52))

c ----- use the current BOND_COLOR and BOND_LINE
	call setrfeat2(bcolor(nbond2+1),numbond-nbond2,rprm(1,73))
	call setrfeat2(bond_fact(nbond2+1),numbond-nbond2,rprm(1,48))
	call setrfeat2(taper(nbond2+1),numbond-nbond2,rprm(1,70))
	call setifeat2(ibline(nbond2+1),numbond-nbond2,iprm(1,66))

	called(51) = .true.

	return
      end


      subroutine act52
	implicit none
#include "top_appl.cmn"

        call setdcol(acolor,ialine,natm,iattyp,nvdwtyp,atypc,iatypl,0)

        called(52) = .true.

        return
      end


      subroutine act53
        implicit none
#include "top_appl.cmn"
        integer n
        logical mstdev

c ----- determine whether I specified the error column (weights for fitting)
        if ((iprm(1,40).lt.1).or.(iprm(1,40).gt.ncolmn)) then
          mstdev=.false.
          iprm(1,40) = ncolmn + 1
          if (iprm(1,40) .gt. maxclm) then
            write(iolog,'(a,i4)') 'act53___E> increase maxclm: ', maxclm
            stop
          end if
        else
          mstdev=.true.
        end if

        if (lprm(1,40)) then
          n = max(iprm(1,61), iprm(1,43))
          if (n .gt. maxpnt) stop 'act53___E> Increase MAXPNT'
          ncolmn = max(ncolmn, iprm(1,37), iprm(2,37))
        else
          n = iprm(1,61)
        end if
        call fitmod(tab(1,iprm(1,37)), tab(1,iprm(2,37)), 
     -              tab(1,iprm(1,40)), iprm(1,61), sprm(1,54),
     -              rprm(1,75),nvrprm(75),nviprm(68),iprm(1,68),
     -              rprm(1,76),rprm(2,76),mstdev,rprm(1,32),
     -              rprm(3,32),n,iprm(1,43),lprm(1,40))

        called(53) = .true.

        return
      end


      subroutine act54
        implicit none
#include "top_appl.cmn"
        real aver(3)

        call centr3(xpdb,ypdb,zpdb,natm,aver)
        called(54) = .true.
 
        return
      end


      subroutine act55
        implicit none
#include "top_appl.cmn"
        integer i,j,idum,i1,j1,nxdim2,nydim2
        real dum

        call upper(sprm(1,57))

        if (index(sprm(1,57), 'TRANSPOSE').gt.0) then
          do  i = 1, nxdim-1
            do  j = i+1, nydim
              dum = arr(i,j)
              arr(i,j) = arr(j,i)
              arr(j,i) = dum
            end do
          end do
          idum = nydim
          nydim = nxdim
          nxdim = idum
        end if

        if (index(sprm(1,57), 'REVERT_Y').gt.0) then
          nydim2 = nydim/2
          do  i = 1, nxdim
            j1 = nydim
            do  j = 1, nydim2
              dum = arr(i,j1)
              arr(i,j1) = arr(i,j)
              arr(i,j) = dum
              j1 = j1 - 1
            end do
          end do
        end if

        if (index(sprm(1,57), 'REVERT_X').gt.0) then
          nxdim2 = nxdim/2
          do  j = 1, nydim
            i1 = nxdim
            do  i = 1, nxdim2
              dum = arr(i1,j)
              arr(i1,j) = arr(i,j)
              arr(i,j) = dum
              i1 = i1 - 1
            end do
          end do
        end if

        called(55) = .true.

        return
      end


      subroutine act56
        implicit none
#include "top_appl.cmn"
        integer i, j, n
        real rot(3,3)
      
        n = 0 
        do  i = 1, 3
          do  j = 1, 3 
            n = n + 1
            rot(i,j) = rprm(n,53)
          end do
        end do

c ----- check and re-normalize the matrix
        call orthog(rot)

        call rotat1(xpdb,ypdb,zpdb,natm,rot)

        called(56) = .true.

        return
      end


      subroutine act57
        implicit none
#include "top_appl.cmn"
        real radians

        call scrot1(xpdb,ypdb,zpdb,natm,rprm(1,54),rprm(2,54),
     -              rprm(3,54),radians(rprm(1,55)))
        called(57) = .true.

        return
      end


      subroutine act58
        implicit none
#include "top_appl.cmn"

        call trans1(xpdb,ypdb,zpdb,natm,rprm(1,56),rprm(2,56),
     -              rprm(3,56))
        called(58) = .true.

        return
      end


c --- write the table
      subroutine act59
#include "top_appl.cmn"

        call gennam2(sprm(1,4),sprm(1,6),'dat',iprm(1,2),
     -               iprm(1,3),sprm(1,12),sprm(1,17))
        call prefix(sprm(1,18), sprm(1,12))

        ndim = max(2,iprm(1,46) + iprm(2,46))
        call wrtabl(ioin,sprm(1,12),tab,iprm(1,46),iprm(1,47),ndim,
     -              maxpnt,maxclm,iprm(1,61))

        called(59) = .true.

        return
      end



      subroutine act60
        implicit none
#include "top_appl.cmn"

c ----- xleg(i),yleg(i): starting positions of the first label of type i
        call initp(xleg,yleg,rprm(1,57),rprm(1,65),x0plt,y0plt,x1plt,
     -             y1plt,iprm(1,48))
        called(60) = .true.

        return
      end

      subroutine act61
        implicit none
#include "top_appl.cmn"

        if (.not. called(60)) then
          write(iolog,'(a)') 
     -    'act61___E> WORLD, (AXES2D) and RESET_LEGEND first'
          stop
        end if
        call topgset('legend')
        call legend(xleg,yleg,sprm(1,47),sprm(1,48),rprm(1,36),
     -       iprm(1,35),iprm(1,36),iprm(1,38),iprm(1,63),iprm(1,75))
        call topgrestore('legend')
        
        called(61) = .true.

        return
      end

      subroutine act62
        implicit none
#include "top_appl.cmn"

        if ((iprm(1,40).lt.1) .or. (iprm(1,40).gt.ncolmn)) then
          write(iolog,'(a,2i4)') 
     -    'act62___E> error-bar column out of range: ',
     -    iprm(1,40), ncolmn
          stop
        end if

c ----- (tab,y,n,line_type,symbol_type,point_font)
        call topgset('errbars')
        call errbars(tab(1,iprm(1,37)),tab(1,iprm(2,37)),
     -               tab(1,iprm(1,40)),iprm(1,61),iprm(1,36),
     -               iprm(1,35),iprm(1,49),sprm(1,62),iprm(1,72))
        call topgrestore('errbars')
        called(62) = .true.

        return
      end


      subroutine act63
        implicit none
#include "top_appl.cmn"
        integer ic,i,n

        if (iprm(1,60).lt.1) then
          write(iolog,'(a)') 'act63___E> SELECT_COLUMN < 1'
          stop
        end if
        n = 0
        do  i = 1, iprm(1,61)
          if ((tab(i,iprm(1,60))+tsmall.ge.rprm(1,61)) .and. 
     -        (tab(i,iprm(1,60))-tsmall.le.rprm(2,61))) then
            n = n + 1
            do  ic = 1, ncolmn
              tab(n,ic) = tab(i,ic)
            end do
            do  ic = 1, maxsclm
              tabstr(n,ic) = tabstr(i,ic)
            end do
          end if
        end do
        iprm(1,61) = n
        write(iolog,'(a,i5)') 'act63____> new number of points: ',
     &                        iprm(1,61)

        called(63) = .true.

        return
      end


      subroutine act64
        implicit none
#include "top_appl.cmn"

        call gennam2(sprm(1,4),sprm(1,6),'B',iprm(1,2),
     -               iprm(1,3),sprm(1,12),sprm(1,17))
        call prefix(sprm(1,18), sprm(1,12))

        call writbrk(ioou,sprm(1,12),xpdb,ypdb,zpdb,natm,nres,resnam,
     -               resnum,atmnam,iresatm)

        called(64) = .true.

        return
      end


      subroutine act65
        implicit none
#include "top_appl.cmn"

        call topgset('spectr')
        call spectr(rprm(1,59),rprm(1,37),tab(1,iprm(2,37)),iprm(1,61),
     -              iprm(1,36),iprm(1,72))
        call topgrestore('spectr')
        called(65) = .true.

        return
      end


      subroutine act66
        implicit none
#include "top_appl.cmn"

        call gennam2(sprm(1,4),sprm(1,6),'ps',iprm(1,2),
     -               iprm(1,3),sprm(1,12),sprm(1,17))
        call prefix(sprm(1,18), sprm(1,12))

        call stamp
        call closeps(iprm(1,45), psfil, bbox0, bboy0, bbox1, bboy1)
        psfil=sprm(1,12)
        call headps(psfil, datetime)
        if (epsf) call initbb
        called(66) = .true.

        return
      end


      subroutine act67
        implicit none
#include "top_appl.cmn"
        integer ndim

        ndim = max(2,iprm(1,46) + iprm(2,46))
        call smooth(iprm(1,62),tab,iprm(1,61),maxpnt,ncolmn,iprm(1,46),
     -              iprm(1,47),ndim,iprm(1,37),iprm(2,37),iprm(1,43),
     -              sprm(1,31))
        called(67) = .true.

        return
      end


      subroutine act68
        implicit none
#include "top_appl.cmn"

        called(68) = .true.

        return
      end


      subroutine act69
        implicit none
#include "top_appl.cmn"

        call getdens(tab(1,iprm(1,37)),tab(1,iprm(2,37)),iprm(1,61),
     -       arr,maxxarr,maxyarr,rprm(1,58),rprm(1,63),nxdim,nydim,
     -       rprm(1,32),rprm(2,32),rprm(3,32),rprm(4,32))
        inp = 2
        called(69) = .true.

        return
      end


      subroutine act70
        implicit none
#include "top_appl.cmn"
        integer i, j

        call gennam2(sprm(1,4),sprm(1,6),'.out',iprm(1,2),
     -               iprm(1,3),sprm(1,12),sprm(1,17))
        call prefix(sprm(1,18), sprm(1,12))

        call openf(ioou, sprm(1,12), 'unknown')

        if (.not. lprm(1,35)) then

          write(iolog,'(a)') 'act70____> writing unformatted array file'

          if (sprm(1,58).eq.'XY') then

            write(ioou,'(2i7)')nxdim,nydim
            do  i = 1, nxdim
              write(ioou,'(999f10.5)') (arr(i,j),j=1,nydim)
            end do

          else

            write(ioin,'(2i7)')nydim,nxdim
            do  j = 1, nydim
              write(ioou,'(f10.5)') (arr(i,j),i=1,nxdim)
            end do

          end if

        else

          write(iolog,'(a)') 'act70____> writing formatted array file'

          do  i = 1, nxdim
            do  j = 1, nydim
              write(ioou, '(2i7,f10.5)') i,j,arr(i,j)
            end do
          end do

        end if
        close(ioou)

        called(70) = .true.

        return
      end


      subroutine act71
        implicit none
#include "top_appl.cmn"

c ----- select atoms, set atom radii, atom color, and atom lines
        call selatm(rprm(1,60),xpdb,ypdb,zpdb,sprm(1,39),sprm(2,39),
     -       sprm(1,44),sprm(1,45),atmnam,natm,
     -       iatmr1,iatmr2,iresatm,resnam,resnum,nres,isel,numsel,
     -       iselb,numbsel,isell,numlsel,sprm(1,38),
     -       iprm(1,65),sprm(1,35),rprm(1,69),rprm(2,69),rprm(3,69),
     -       rprm(4,69),rprm(5,69),sprm(1,52),sprm(1,51),sprm(1,53),
     -       sprm(1,43),sprm(2,43),ialine,acolor,iprm(1,67),rprm(1,74),
     -       vdwcnt,nvdwtyp,rprm(1,51),rprm(1,47),atrad,iattyp)

        return
      end


      subroutine act72
        implicit none
#include "top_appl.cmn"
        integer nxmax
        parameter (nxmax=3)
        real x(nxmax,maxpnt), sum
        integer n,i,j,iy,nx
        logical mstdev

c ----- determine whether I specified the error column (weights for fitting)
        if ((iprm(1,40).lt.1).or.(iprm(1,40).gt.ncolmn)) then
          mstdev=.false.
          iprm(1,40) = ncolmn + 1
          if (iprm(1,40) .gt. maxclm) then
            write(iolog,'(a,i4)') 'act53___E> increase maxclm: ', maxclm
            stop
          end if
        else
          mstdev=.true.
        end if

        if (iprm(1,46) .eq. 0) then
          iprm(1,46) = 1
          iprm(1,47) = iprm(1,37)
        end if
        nx = iprm(1,46)
        if (iprm(2,46) .eq. 0) then
          iprm(2,46) = 1
          iprm(nx+1,47) = iprm(2,37)
        end if
        iy  = iprm(nx+1,47)

        if (lprm(1,40)) then
          n = max(iprm(1,61), iprm(1,43))
          if (n .gt. maxpnt) stop 'act53___E> Increase MAXPNT'
          ncolmn = max(ncolmn, iprm(1,37), iprm(2,37))
        else
          n = iprm(1,61)
        end if

        write(iolog,'(a,99i3)') 'act72____> X columns : ', 
     &             (iprm(i,47), i = 1, nx)
        write(iolog,'(a,99i3)') 'act72____> Y column  : ', iy
        write(iolog,'(a,99i3)') 'act72____> STD column: ', iprm(1,40)
        write(iolog,'(a,99l7)') 'act72____> STD used  : ', mstdev
        write(iolog,'(a,99l7)') 'act72____> Interpolat: ', lprm(1,40)

c ----- make the X array for the lsqlib() routine:
        do  i = 1, nx
          do  j = 1, iprm(1,61)
            x(i,j) = tab(j,iprm(i,47))
          end do
        end do

        sum = 0
        do  i = 1, iprm(1,61)
          sum = sum + tab(i,iy)
        end do
        write(iolog,'(a,g14.4)') 'act72____> The sum of Y_i: ', sum

        call fitmod2(x,nx,nxmax,tab(1,iy),tab(1,iprm(1,40)),iprm(1,61),
     &       sprm(1,54),rprm(1,75),nvrprm(75),nviprm(68),iprm(1,68),
     &       rprm(1,76),rprm(2,76),mstdev,rprm(1,32),rprm(2,32),
     &       rprm(3,32),rprm(4,32),n,iprm(1,43),lprm(1,40))

c ----- copy the X array back to TAB:
        do  i = 1, nx
          do  j = 1, iprm(1,61)
            tab(j,iprm(i,47)) = x(i,j)
          end do
        end do

        called(72) = .true.

        return
      end


      subroutine act73
        implicit none
#include "top_appl.cmn"

        call tab2xy
        called(73) = .true.

        return
      end


      subroutine act74
        implicit none
#include "top_appl.cmn"

        call xy2tab
        called(74) = .true.

        return
      end


      subroutine act75
        implicit none
#include "top_appl.cmn"
        integer lenr, ic, i, l, lenx
        character strx*(40)
        external lenr

c        if (iprm(1,46) .eq. 0) then
c          iprm(1,46) = 1
c          iprm(1,47) = iprm(1,37)
c        end if
c        if (iprm(2,46) .eq. 0) then
c          iprm(2,46) = 1
c          iprm(iprm(1,46)+1,47) = iprm(2,37)
c        end if

        sprm(1,13) = ' '

        do i = 1, iprm(1,46)+iprm(2,46)
          ic = iprm(i, 47)

          l = lenr(sprm(1,13))
          sprm(1,13)(l+1:) = ' '

          if (istrcol(ic)) then
            if(l+1+lenr(tabstr(1,invstrs(ic))).gt.lensprm)then
              write(iolog,*)'act75____E> increase LENSPRM'
              stop
            end if
            sprm(1,13)(l+2:) = tabstr(1,invstrs(ic))
          else
            call condns(tab(1,ic),strx,lenx)
            if (l+1+lenx .gt. lensprm) then
              write(iolog,*)'act75____E> increase LENSPRM'
              stop
            end if
            sprm(1,13)(l+2:) = strx(1:lenx)
          end if
        end do

        called(75) = .true.

        return
      end


      subroutine act76
        implicit none
#include "top_appl.cmn"

        called(76) = .true.

        return
      end


      subroutine act77
        implicit none
#include "top_appl.cmn"

        called(77) = .true.

        return
      end


      subroutine act78
        implicit none
#include "top_appl.cmn"

        called(78) = .true.

        return
      end


      subroutine act79
        implicit none
#include "top_appl.cmn"

        called(79) = .true.

        return
      end


      subroutine act80
        implicit none
#include "top_appl.cmn"

        called(80) = .true.

        return
      end


      subroutine act81
        implicit none
#include "top_appl.cmn"

        called(81) = .true.

        return
      end


      subroutine act82
        implicit none
#include "top_appl.cmn"

        called(82) = .true.

        return
      end



      subroutine act83
        implicit none
#include "top_appl.cmn"

        called(83) = .true.

        return
      end


      subroutine act84
        implicit none
#include "top_appl.cmn"

        called(84) = .true.

        return
      end

      subroutine act85
        implicit none
#include "top_appl.cmn"

        called(85) = .true.

        return
      end



      subroutine elmtyp(elm,iet,ival,rval,sval,lval)
        implicit none
        integer ival, ierr, iet, il, ir, lenl, lenr
        real rval,fp
        character elm*(*),sval*(*)
        logical lval, equal
        external equal

c ----- Checks:
        il = lenl(elm)
        ir = lenr(elm)

        if (ir.eq.0) then
c ------- a string, by definition:
          sval = elm
          iet = 3
          return
        end if

        call str_i(elm, ival, ierr)
        if (ierr .eq. 0) then
          call str_r(elm, rval, ierr)
          fp = ival
          if (equal(fp, rval) .and. (index(elm, '.').lt.1)) then
c --------- Integer:
            iet = 2
          else
c --------- Real:
            iet = 1
          end if
          return
        else
c ------- only for [-].nnnnn situations:
          call str_r(elm, rval, ierr)
          iet = 1
          if (ierr .eq. 0) return
        end if

        call str_l(elm, lval, 'O', ierr)
        iet = 4
        if (ierr .eq. 0) return

c ----- If it is not any of the above, it must be a string:
        sval = elm
        iet = 3

        return
      end


      subroutine act86
        implicit none
c#include "top_appl.cmn"

c        called(86) = .true.

        return
      end


      subroutine act87
        implicit none
c#include "top_appl.cmn"

c        called(87) = .true.

        return
      end


      subroutine act88
        implicit none
c#include "top_appl.cmn"

c        called(88) = .true.

        return
      end



      subroutine act89
        implicit none
c#include "top_appl.cmn"

c        called(89) = .true.

        return
      end


      subroutine act90
        implicit none
c#include "top_appl.cmn"

c        called(90) = .true.

        return
      end


      subroutine act91
        implicit none
c#include "top_appl.cmn"

c        called(91) = .true.

        return
      end



      subroutine act92
        implicit none
c#include "top_appl.cmn"

c        called(92) = .true.

        return
      end


      subroutine act93
        implicit none
c#include "top_appl.cmn"

c        called(93) = .true.

        return
      end


      subroutine act94
        implicit none
c#include "top_appl.cmn"

c        called(94) = .true.

        return
      end



      subroutine act95
        implicit none
c#include "top_appl.cmn"

c        called(95) = .true.

        return
      end


      subroutine act96
        implicit none
c#include "top_appl.cmn"

c        called(96) = .true.

        return
      end


      subroutine act97
        implicit none
c#include "top_appl.cmn"

c        called(97) = .true.

        return
      end



      subroutine act98
        implicit none
c#include "top_appl.cmn"

c        called(98) = .true.

        return
      end


      subroutine act99
        implicit none
c#include "top_appl.cmn"

c        called(99) = .true.

        return
      end


      subroutine act100
        implicit none
c#include "top_appl.cmn"

c        called(100) = .true.

        return
      end
