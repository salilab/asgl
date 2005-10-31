c --- Plot: the whole area with with axes, labels and curves/points;
c     Graph: the smaller area bounded by the axes;
c     World coordinates: coordinates of the points to be plotted.
c     PS or plot coordinates: main PS coordinate system;

c --- Input:
c       x0plt,y0plt   ... lower left corner of the Graph in PS coords. (0,0)
c       x1plt,y1plt   ... uper right corner of the Graph in PS coords. (1.3,1)
c       x0wrl,y0wrl,x1wrl,y1wrl ... World window corresponding to the Graph;
c       x0tckw    ... x of the first tick on x-axis in World coordinates
c       dxtckw    ... distance between x-ticks in World coordinates
c       x2tckw    ... maximal x-value above which there are no ticks/labels
c       y0tckw, dytckw, y2tckw ... the same for y-axis
c       xlabl(i)  ... labels for the x-axis ticks
c       ylabl(i)  ... labels for the y-axis ticks
c       lablx     ... 1, labels supplied by the user;
c                     2, labels to be calculated by the program;
c                     3, do not plot any labels;
c       lably     ... the same for y-axis
c       ndecx     ... number of decimal places for x-axis labels (-1) for
c                     integer
c       ndecy     ... the same for the y-axis
c       maxtck    ... maximal number of tick labels
c       i0xtck    ... the index of the first labelled tick on x-axis
c       nxnumb    ... every which x-tick is labeled
c       i0ytck,nynumb ... the same for the y-axis
c       ifntxy    ... font for labelling the axes (tick numbers)
c       yside     ... string, ( RIGHT | LEFT ) for positioning Y-scale
c                     with ticks and numbers.
c       xtckshft  ... shift of x-axis labels in PS coordinates.
c       ytckshft  ... shift of y-axis labels in PS coordinates.
c
c       xfactaxs  ... factor for X-tick labels
c       yfactaxs  ... factor for Y-tick labels
c       expon     ... if ON, then E(log(x/yfactaxs)) is added to tick labels
c
c 
c --- Output is PS file with: 
c       1. window box; 
c       2. coordinate axes with ticks;
c       3. ticks, title and axes labels.
c       
c       xleft     ... left x-coordinate of the widest y-axis tick label
c                     (if yside  = 'LEFT')
c       xright    ... riht x-coordinate of the widest y-axis tick label
c                     (if yside  = 'RIGHT')

      subroutine axes2d(x0plt,y0plt,x1plt,y1plt,x0wrl,y0wrl,x1wrl,y1wrl,
     -           x0tckw,y0tckw,dxtckw,dytckw,x2tckw,y2tckw,
     -           xlabl,ylabl,lablx,lably,ndecx,ndecy,maxtck,
     -           i0xtck,nxnumb,i0ytck,nynumb,ifntxy,xleft,xright,
     -           yside,xtckshft,ytckshft,xfactaxs,yfactaxs,expon,
     -           iaxtyp,itktyp,itkstyp)
c ----- constants & scale in wpx&y
#include "psgl.cmn"
        integer iaxtyp,itktyp,itkstyp
        character xlabl(maxtck)*(*), ylabl(maxtck)*(*)
        character yside2*(5), yside*(*)
        real wpdx, wpdy
        logical expon

        yside2 = yside
        call upper(yside2)

c ----- no clipping:
c        call initclip


c ----- setup the axis ticks (not necessary to call if all the tick
c       parameters are setup by the user)
        call ticks(x0wrl,x1wrl,dxtckw,x0tckw,x2tckw,i0xtck,nxnumb,ndecx)
        call ticks(y0wrl,y1wrl,dytckw,y0tckw,y2tckw,i0ytck,nynumb,ndecy)

c ----- numbers of ticks on the x- and y-axes:
        nxtick = numtck(x0tckw, dxtckw, x2tckw)
        nytick = numtck(y0tckw, dytckw, y2tckw)

c ----- axes:
        call box(x0plt, y0plt, x1plt, y1plt, iaxtyp, 1.0)

c ----- setup the axis labels (if automatic labelling required):
c       (labels are stored consecutively for all labeled ticks with no 
c        missing labels even if not every tick is labeled ---
c        this is to have them as for manually labeled
c        string xlabl where you would not want to specify
c        all the empty labels)
        if(lablx .eq. 2) call axslbl(x0tckw,dxtckw,nxtick,ndecx,
     -                               i0xtck,nxnumb,xlabl,maxtck,
     -                               xfactaxs,expon)
        if(lably .eq. 2) call axslbl(y0tckw,dytckw,nytick,ndecy,
     -                               i0ytck,nynumb,ylabl,maxtck,
     -                               yfactaxs,expon)


c ----- x-ticks:
c ----- space between the end of tick and tick label:
        xtcksp = dtick + xtckshft
c        xtcksp = 0.5*fnthgt(ifntxy)*(spclin-1.0)
c ----- start of the first tick and interval between the ticks
        xtck = wpx(x0tckw)
        dxtick = wpdx(dxtckw)
        ilabl = 0
        do 10  i = 1, nxtick
c ------- write out a tick line:
          if (i.ge.i0xtck.and.mod((i-i0xtck), nxnumb) .eq. 0) then
            call line(xtck,y0plt,xtck,y0plt-dtick,itktyp)
c --------- write out a label for this tick if so requested:
            if (lablx .ne. 3) then
              xl = xtck 
              yl = y0plt-dtick-xtcksp
              ilabl = ilabl + 1
              call ps_text(xlabl(ilabl),xl,yl,ifntxy,horizo,cenjus,
     -                     topaln)
            end if
          else
c --------- write out a little tick with no label:
            call line(xtck,y0plt,xtck,y0plt-ftick*dtick,itkstyp)
          end if
          xtck = xtck + dxtick
10      continue

c ----- y-ticks:
c ----- space between the end of ticks and the right side of tick label:
        ytcksp = dtick + ytckshft
c        ytcksp = 0.5 * fnthgt(ifntxy) * (spclin-1.0)
        ytck = wpy(y0tckw)
        dytick = wpdy(dytckw)
c ----- number of characters in the longest y-label
        ndigy = 0
        ilabl = 0
        do 20  i = 1, nytick
          if (i.ge.i0ytck.and.mod((i-i0ytck), nynumb) .eq. 0) then
c --------- write out a tick line:
            if (yside2 .eq. 'RIGHT') then
              call line(x1plt,ytck,x1plt+dtick,ytck,itktyp)
            else
              call line(x0plt,ytck,x0plt-dtick,ytck,itktyp)
            end if
c --------- write out a label for this tick if so selected:
            if (lably .ne. 3) then
              if (yside2 .eq. 'RIGHT') then
                xl = x1plt+dtick+ytcksp
              else
                xl = x0plt-dtick-ytcksp
              end if
c ----------- center the label with the tick in vertical direction
              yl = ytck 
              ilabl = ilabl + 1
              if (yside2 .eq. 'RIGHT') then
c ------------- right justified:
                call ps_text(ylabl(ilabl),xl,yl,ifntxy,horizo,lftjus,
     -                       cenaln)
              else
c ------------- left justified:
                call ps_text(ylabl(ilabl),xl,yl,ifntxy,horizo,rgtjus,
     -                       cenaln)
              end if
              ndigy = max(ndigy, lenr(ylabl(ilabl)))
            end if
          else
c --------- write out little ticks:
            if (yside2 .eq. 'RIGHT') then
              call line(x1plt,ytck,x1plt+ftick*dtick,ytck,itkstyp)
            else
              call line(x0plt,ytck,x0plt-ftick*dtick,ytck,itkstyp)
            end if
          end if
          ytck = ytck + dytick
20      continue

        if (yside2 .eq. 'RIGHT') then
          xright = x1plt + dtick + ytcksp + ndigy*fnthgt(ifntxy)*0.65
          if (ndecy .gt. 0) xright = xright - 0.3*fnthgt(ifntxy)
        else
c ------- the approximate left side of the longest y-label
c        (assumming character width = 0.65 character height)
          xleft = x0plt - dtick - ytcksp - ndigy*fnthgt(ifntxy)*0.65
c ------- decimal point is narrower
          if (ndecy .gt. 0) xleft = xleft + 0.3*fnthgt(ifntxy)
        end if

c ----- if no explicit no-clip required elsewhere, clip everything
c       outside the Plot area in the future actions:
c        d = 0.5*linwdt(iaxtyp)
c        call clip(x0plt+d, y0plt+d, x1plt-d, y1plt-d)

        return
      end 


c --- similar to fitmod, except that X can be multidimensional. FITMOD()
c     is kept only for compatibility with the old TOP files. Do not
c     develop it anymore.

      subroutine fitmod2(x,nx2,nxmax,y,sig,npnts,type,ainit,nainit,
     &    nprmfit,lista,relcut,abscut,mstdev,x1,y1,x2,y2,n,npnts2,wr)
        implicit none
#include "io.cst"
        integer nprmmax, nmodels
        parameter (nprmmax = 50, nmodels = 17)
        integer nx2,npnts,maxcyc,nainit,ifind2word,npnts2,n,imod
        integer nprmfit,lista(nainit),nprmall,nx,niter,nxmax
        real x(nxmax,npnts), y(npnts), a(nprmmax),sig(npnts)
        real covar(nprmmax,nprmmax),alpha(nprmmax,nprmmax)
        real ainit(nainit),alamda,relcut,abscut,errmult,alammax
        real degfreed,chisq,q,x1,x2,y1,y2
        real fitnormal,fitpoly,fitpower,fitlognormal,fitexponential
        real fitlogarithmic,fitexponential2,fitpolygauss
        real fitexponential3,fitexponential4,fitlj1
        real fitpolygauss2,fitdisf,fitexponential5,fitexponential6
        real fitpolygauss3,fitpolygauss4
        character type2*(30), type*(*), comment1*50,comment2*50
        character smodels(nmodels)*(20)
        logical mstdev,wr
        external fitnormal,fitpoly,fitlognormal,fitexponential,
     -           fitlogarithmic,fitexponential2,fitpolygauss,
     -           fitpolygauss3,fitpolygauss4,
     -           fitexponential3,fitpolygauss2,fitpower,fitdisf,
     -           fitexponential4,fitexponential5,fitexponential6,
     -           fitlj1
        data smodels /'LJ1',          'DISSOCIATION', 'POLYNOMIAL', 
     &                'POWER',        'NORMAL',       'POLYGAUSS',    
     &                'POLYGAUSS360', 'BINORMAL360',  'LOG-NORMAL',
     &                'EXPONENTIAL',  'EXPONENTIAL2', 'EXPONENTIAL3', 
     &                'EXPONENTIAL4', 'EXPONENTIAL5', 'EXPONENTIAL6', 
     &                'LOGARITHMIC',  'BIPERIODIC'/

        maxcyc = 100
        alammax = 1.0E20 

        type2 = type
        call upper(type2)
        imod = ifind2word(type2,smodels,nmodels)
        if (imod .lt. 1) then
          write(iolog,'(2a)') 'fitmod2__E> model not found: ', type2
          stop
        end if

        go to (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17) imod

1       continue
          nx = 1
          nprmall = 6
          comment1 = 'Fit to a LJ1 expression: '
          comment2 = '  y = (a-c)/x^e - (b-d)/x^f + Ln Z(a,b)/Z(c,d)'
          call chkprm(nainit,nprmall,nprmfit,nx,nx2)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitlj1,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitlj1,y,n,npnts,x1,x2,npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        go to 100

2       continue
          nx = 1
          nprmall = 4
          comment1 = 'Fit to a Kd expression: '
          comment2 = '  y = Fo + (Fm-Fo) * ... '
          call chkprm(nainit,nprmall,nprmfit,nx,nx2)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitdisf,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitdisf,y,n,npnts,x1,x2,npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        go to 100

3       continue
          nx = 1
          nprmall = 7
          comment1 = 'Fit to a polynomial: '
          comment2 = '  y = a + bx + cx^2 + dx^3 + ex^4 + fx^5 + gx^6'
          call chkprm(nainit,nprmall,nprmfit,nx,nx2)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitpoly,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitpoly,y,n,npnts,x1,x2,npnts2,
     -              wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        go to 100


4       continue
          nx = 1
          nprmall = 3
          comment1 = 'Fit to a power law: '
          comment2 = '  y = a + bx^c'
          call chkprm(nainit,nprmall,nprmfit,nx,nx2)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitpower,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitpower,y,n,npnts,x1,x2,npnts2,
     -              wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        go to 100

5       continue
          nx = 1
          nprmall = 3
          comment1='Fit to a Gaussian: '
          comment2='y = exp(c) 1/(Sqrt[2 Pi] b) exp{-0.5*[(x-a)/b)^2]}'
          comment2=' '
          call chkprm(nainit,nprmall,nprmfit,nx,nx2)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq, fitnormal, abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitnormal,y,n,npnts,x1,x2,npnts2,
     -              wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        go to 100


6       continue
c ----- the parameters are: a, w_1, m_1, s_1, w_2, m_2, s_2, ..., m_n, s_n
          nx = 1
          nprmall = nainit
          comment1='Fit to a multiple Gaussian (Sum_i w_i=1): '
          comment2='y = a * {Sum_i w_i * Gauss[mean_i, stdev_i]}'
c          call chkprm(nainit,nprmall,nprmfit,nx,nx2)
          if (mod(nainit, 3) .ne. 0)
     -    write(iolog,'(a,i4)') 'fit_____E> wrong number of params: ',
     -                          nainit
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq, fitpolygauss, abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitpolygauss,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        go to 100

7       continue
c ----- the parameters are: a, w_1, m_1, s_1, w_2, m_2, s_2, ..., m_n, s_n
          nx = 1
          nprmall = nainit
          comment1='Fit to a multiple 360 Gaussian (Sum_i w_i=1):'
          comment2='y = a * {Sum_i w_i * Gauss[mean_i, stdev_i]}'
c          call chkprm(nainit,nprmall,nprmfit,nx,nx2)
          if (mod(nainit, 3) .ne. 0)
     -    write(iolog,'(a,i4)') 'fit_____E> wrong number of params: ',
     -                          nainit
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq, fitpolygauss2, abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitpolygauss2,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        go to 100

8       continue
          nx = 2
          nprmall = nainit
          comment1='Fit to a multiple 360 binormal (Sum_i w_i=1):'
          comment2=' '
          if (mod(nainit, 6) .ne. 0)
     -    write(iolog,'(a,i4)') 'fit_____E> wrong number of params: ',
     -                          nainit
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq, fitpolygauss3, abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety2(x,nxmax,a,nprmall,fitpolygauss3,y,n,npnts,x1,y1,
     -               x2,y2,npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        go to 100

9       continue
          nx = 1
          nprmall = 3
          comment1 = 'Fit to a logarithm of Gaussian:'
          comment2 = 'y = c+Ln{1/(Sqrt[2 Pi] b) - 0.5*[(x-a)/b)^2]}'
          call chkprm(nainit,nprmall,nprmfit,nx,nx2)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq, fitlognormal, abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitlognormal,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        go to 100

10      continue
          nx = 1
          nprmall = 4
          comment1 = 'Fit to an exponential:'
          comment2 = 'y = a + exp[b + c x^d]'
          call chkprm(nainit,nprmall,nprmfit,nx,nx2)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitexponential,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitexponential,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        go to 100

11      continue
          nx = 1
          nprmall = 3
          comment1 = 'Fit to an exponential:'
          comment2 = 'y = (1-exp[a]) + exp[a + b x^c]'
          call chkprm(nainit,nprmall,nprmfit,nx,nx2)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitexponential2,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitexponential2,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        go to 100

12      continue
          nx = 1
          nprmall = 1
          comment1 = 'Fit to an exponential:'
          comment2 = 'y = a [1 - exp(-x/a)]'
          call chkprm(nainit,nprmall,nprmfit,nx,nx2)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitexponential3,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitexponential3,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        go to 100

13      continue
          nx = 1
          nprmall = 1
          comment1 = 'Fit to an exponential:'
          comment2 = 'y = 1/a * exp[-x/a]'
          call chkprm(nainit,nprmall,nprmfit,nx,nx2)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitexponential4,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitexponential4,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        go to 100

14      continue
          nx = 1
          nprmall = 3
          comment1 = 'Fit to an exponential:'
          comment2 = 'y = c/b exp(-(x-a)/b)]'
          call chkprm(nainit,nprmall,nprmfit,nx,nx2)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitexponential5,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitexponential5,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        go to 100

15      continue
          nx = 1
          nprmall = 2
          comment1 = 'Fit to an exponential:'
          comment2 = 'y = 1/b  exp(-(x-a)/b)]'
          call chkprm(nainit,nprmall,nprmfit,nx,nx2)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitexponential6,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitexponential6,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        go to 100

16      continue
          nx = 1
          nprmall = 4
          comment1 = 'Fit to a logarithm: '
          comment2 = '  y = a + b ln(d + c x)'
          call chkprm(nainit,nprmall,nprmfit,nx,nx2)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitlogarithmic,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitlogarithmic,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        go to 100

17      continue
          nx = 2
          nprmall = nainit
          comment1='Fit to a true multiple 360 binormal (Sum_i w_i=1):'
          comment2=' '
          if (mod(nainit, 6) .ne. 0)
     -    write(iolog,'(a,i4)') 'fit_____E> wrong number of params: ',
     -                          nainit
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq, fitpolygauss4, abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety2(x,nxmax,a,nprmall,fitpolygauss4,y,n,npnts,x1,y1,
     -               x2,y2,npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        go to 100

100     continue
        return
      end


c --- Input: x0,y0  : plot window
c            x0plt,y0plt: graph window
c --- Output: xpos(i),ypos(i): starting positions of the first labels of type i
      subroutine initp(xpos,ypos,xleft,xright,
     -                 x0plt,y0plt,x1plt,y1plt,ifntxy)
        implicit none
#include "psgl.cmn"
        integer ifntxy
        real xpos(nposit), ypos(nposit), xleft, xright
        real x0plt,y0plt,x1plt,y1plt,xtcksp,ytcksp,tckhgt

c ----- height of the tick label characters
        tckhgt = fnthgt(ifntxy)
c ----- space between the end of tick and tick label:
        xtcksp = 0.5*tckhgt*(spclin-1.0)
        ytcksp = 0.5*tckhgt*(spclin-1.0)

c ----- 1: Top of plot, center justified, top aligned, horizontal:
        xpos(1) = 0.5 * (x0plt+x1plt) 
        ypos(1) = y1plt + dyt0 

c ----- 2: Bottom of plot, center justified, top aligned, horizontal:
        xpos(2) = 0.5 * (x0plt+x1plt) 
        ypos(2) = y0plt - dtick - xtcksp - tckhgt - 2*xtcksp

c ----- 3: Left of plot: center justified, bottom aligned, vertical:
        xpos(3) = xleft - 0.4*tckhgt
        ypos(3) = 0.5 * (y0plt+y1plt)

c ----- 4: Label of x-axis: right justified, top aligned, horizontal:
        xpos(4) = x1plt
        ypos(4) = ypos(2)

c ----- 5: Label of y-axis: left justified, top aligned, horizontal:
        xpos(5) = xleft - 0.4*tckhgt
        ypos(5) = y1plt

c ----- 6: Right of plot: center justified, top aligned, vertical:
        xpos(6) = xright + 0.4*tckhgt
        ypos(6) = 0.5 * (y0plt+y1plt)

c ----- 7: Label of y-axis: right justified, top aligned, horizontal:
        xpos(7) = xright + 0.4*tckhgt
        ypos(7) = y1plt

c ----- 8: upper left corner: left justified, top aligned, horizontal:
        xpos(8) = x0plt + 0.3*tckhgt
        ypos(8) = y1plt - 0.5*tckhgt

c ----- 9: upper right corner: right justified, top aligned, horizontal:
        xpos(9) = x1plt - 0.3*tckhgt
        ypos(9) = y1plt - 0.5*tckhgt

c ----- 10: lower right corner: right justified, bottom aligned, horizontal:
        xpos(10) = x1plt - 0.3*tckhgt
        ypos(10) = y0plt + 0.45*tckhgt

c ----- 11: lower left corner: left justified, bottom aligned, horizontal:
        xpos(11) = x0plt + 0.3*tckhgt
        ypos(11) = y0plt + 0.45*tckhgt

c ----- 12: top center, in the graph: center justified, top aligned, horizontal:
        xpos(12) = 0.5*(x0plt + x1plt)
        ypos(12) = y1plt - 0.3*tckhgt

        return
      end

      real function extndm(x0wrl, x1wrl, hngovr)
        implicit none
        real x0wrl, x1wrl, hngovr, fract
c ----- if in vicinity of 0.0, pick 0.0
        fract = 0.05*(x1wrl-x0wrl)
        if ((x0wrl.lt.fract) .and. (x0wrl.ge.0.0)) then
          extndm = 0.0
        else
          extndm = x0wrl - (x1wrl-x0wrl) * hngovr      
        end if
        return
      end

      real function extndp(x0wrl, x1wrl, hngovr)
c ----- if in vicinity of 0.0, pick 0.0
        fract = 0.05*(x1wrl-x0wrl)
        if ((x1wrl.gt.-fract) .and. (x1wrl.le.0.0)) then
          extndp = 0.0
        else
          extndp = x1wrl + (x1wrl-x0wrl) * hngovr      
        end if
        return
      end

c --- returns 0 for false and 1 for true (this evaluates as such in if 
c     statements):
      logical function deflt(x)
        if (abs(x+999.0) .lt. 1.0E-5) then
          deflt = .true.
        else
          deflt = .false.
        end if
        return
      end


      subroutine range1(tab,ixy,nwxy,iwxy,ndim,maxp,maxc,npnts,
     -                  x0wrl,y0wrl,x1wrl,y1wrl,center)
#include "psgl.cst"
#include "asgl.cst"
c        integer ixy(2), nwxy(2), iwxy(max(nwxy(1)+nwxy(2),2))
        integer ixy(2), nwxy(2), iwxy(ndim)
        real tab(maxp,maxc), center
        logical x0wrle, x1wrle, y0wrle, y1wrle, deflt

c ----- patch the world coordinates vectors xwrld and yworld so that 
c       thay include the span representative of the selected
c       x and y columns, maybe more than one:

c ----- if the columns for world calculations are undefined, then use
c       the current columns for plotting;

        if (nwxy(1) .lt. 1) then
           nwxy(1) = 1
           call insvec(iwxy,1,nwxy(1)+nwxy(2),ixy(1))
        end if
        if (nwxy(2) .lt. 1) then
           nwxy(2) = 1
           call insvec(iwxy,nwxy(1)+1,nwxy(1)+nwxy(2),ixy(2))
        end if

c ----- determine the real world range of the scatter graph or histogram:
        x0wrle = .false.
        x1wrle = .false.
        y0wrle = .false.
        y1wrle = .false.

        if (.not. deflt(center)) then
          call rminmaxa(tab,maxp,maxc,npnts,nwxy(1),iwxy(1),center,
     &                  rx1,rx2)
          call rminmaxa(tab,maxp,maxc,npnts,nwxy(2),iwxy(2),center,
     &                  ry1,ry2)
        end if

        if (deflt(x0wrl)) then
          if (deflt(center)) then
            x0wrl = rmina(tab,maxp,maxc,npnts,nwxy(1),iwxy(1))
          else
            x0wrl = rx1
          end if
          x0wrle = .true.
        end if
        if (deflt(x1wrl)) then 
          if (deflt(center)) then
            x1wrl = rmaxa(tab,maxp,maxc,npnts,nwxy(1),iwxy(1))
          else
            x1wrl = rx2
          end if
          x1wrle = .true.
        end if
        if (deflt(y0wrl)) then
          if (deflt(center)) then
            y0wrl = rmina(tab,maxp,maxc,npnts,nwxy(2),iwxy(nwxy(1)+1))
          else
            y0wrl = ry1
          end if
          y0wrle = .true.
        end if
        if (deflt(y1wrl)) then
          if (deflt(center)) then
            y1wrl = rmaxa(tab,maxp,maxc,npnts,nwxy(2),iwxy(nwxy(1)+1))
          else
            y1wrl = ry2
          end if
          y1wrle = .true.
        end if

c ----- if automatic bound was set, extend it a bit for a fraction HNGOVR:
        if (x0wrle) x0wrl = extndm(x0wrl,x1wrl,hngovr)
        if (x1wrle) x1wrl = extndp(x0wrl,x1wrl,hngovr)
        if (y0wrle) y0wrl = extndm(y0wrl,y1wrl,hngovr)
        if (y1wrle) y1wrl = extndp(y0wrl,y1wrl,hngovr)

        return
      end

c --- insert an element insval into vector ivec, at position
c     ins and new dimension newvec.
      subroutine insvec(ivec,ins,newvec,insval)
        integer ivec(newvec)
        do  i = newvec, ins+1, -1
          ivec(i) = ivec(i-1)
        end do
        ivec(ins) = insval
        return
      end

      subroutine range2(nx,ny,x0wrl,y0wrl,x1wrl,y1wrl)
        logical deflt
c ----- determine the real world range of the 'array' graph:
        if (deflt(x0wrl)) x0wrl = 0
        if (deflt(x1wrl)) x1wrl = nx + 1
        if (deflt(y0wrl)) y0wrl = 0
        if (deflt(y1wrl)) y1wrl = ny + 1
        return
      end

      real function rmax(y,npnts,ncurve,maxpnt)
        real y(maxpnt,ncurve)
        y1wrl = -9.9E20
        do 10  ic = 1, ncurve
          ym = rmaxv(y(1,ic),npnts)
          if(ym .gt. y1wrl) y1wrl = ym
10      continue
        rmax = y1wrl
        return
      end

      subroutine rminmaxa(y,maxpnt,maxclm,npnts,ny,iy,center,r1,r2)
        integer iy(ny)
        real y(maxpnt,maxclm)
        r1 =  9.9E20
        r2 = -9.9E20
        do 10  ic = 1, ny
          call rminmaxv(y(1,iy(ic)),npnts,center,r1a,r2a)
          if(r1a .lt. r1) r1 = r1a
          if(r2a .gt. r2) r2 = r2a
10      continue
        return
      end

      real function rmaxa(y,maxpnt,maxclm,npnts,ny,iy)
        integer iy(ny)
        real y(maxpnt,maxclm)
        y1wrl = -9.9E20
        do 10  ic = 1, ny
          ym = rmaxv(y(1,iy(ic)),npnts)
          if(ym .gt. y1wrl) y1wrl = ym
10      continue
        rmaxa = y1wrl
        return
      end

      real function rmina(y,maxpnt,maxclm,npnts,ny,iy)
        integer iy(ny)
        real y(maxpnt,maxclm)
        y0wrl = 9.9E20
        do 10  ic = 1, ny
          ym = rminv(y(1,iy(ic)),npnts)
          if(ym .lt. y0wrl) y0wrl = ym
10      continue
        rmina = y0wrl
        return
      end

      real function rmin(y,npnts,ncurve,maxpnt)
        real y(maxpnt,ncurve)
        y0wrl = 9.9E20
        do 10  ic = 1, ncurve
          ym = rminv(y(1,ic),npnts)
          if(ym .lt. y0wrl) y0wrl = ym
10      continue
        rmin = y0wrl
        return
      end

      real function rminv(y,npnts)
        real y(npnts)
        y0wrl = 9.9E20
        do  ip = 1, npnts
          if(y(ip) .lt. y0wrl) y0wrl = y(ip)
        end do
        rminv = y0wrl
        return
      end

      real function rmaxv(y,npnts)
        real y(npnts)
        y1wrl = -9.9E20
        do 5  ip = 1, npnts
          if(y(ip) .gt. y1wrl) y1wrl = y(ip)
5       continue
        rmaxv = y1wrl
        return
      end


      subroutine rminmaxv(y,npnts,center,rminv2,rmaxv2)
#include "asgl.cst"
        real y(npnts), ysrt(maxpnt)

        if (center .lt. 0 .or. center .gt. 1.0) then
          write(iolog,*)'rmaxv2___E> bad WORLD_FRACTION value: ', 
     &      center
          stop
        end if

        do  i = 1, npnts
          ysrt(i) = y(i)
        end do

        do  i = 1, npnts
          do  j = i+1, npnts
            if (ysrt(j) .lt. ysrt(i)) then
              ydummy  = ysrt(i)
              ysrt(i) = ysrt(j)
              ysrt(j) = ydummy
            end if
          end do
        end do

        imin = min(npnts, max(1,nint(npnts*(1.0-center))))
        imax = npnts - imin
        rminv2 = ysrt(imin)
        rmaxv2 = ysrt(imax)

        return
      end

      integer function numtck(x0tick, dxtick, x1wrl)
        if (x1wrl .lt. x0tick) then
          numtck = 0
        else
c ------- slightly larger to allow for rounding error
          span = x1wrl - x0tick + dxtick * 0.001
          numtck = ifix(span / dxtick) + 1
        end if
        return
      end


c --- prepare the nxtick labels for the x-axis ticks:
      subroutine axslbl(x0tckw,dxtckw,nxtick,ndec,i0xtck,nxnumb,xlabl,
     -                  maxtck,factaxs,expon)
        integer intexp
        real factaxs
        character xlabl(nxtick)*(*), expons*(10)
        logical expon, addexpon

        addexpon = .false.
        if (expon) then
          intexp = nint(log10(factaxs))
          addexpon = intexp .ne. 0
          if (addexpon) then
            call i_str(-intexp, expons, 9)
            call add3str('E', expons)
          end if
          if (abs(factaxs - 10.0**intexp) .gt. 0.001)
     -      write(iolog,'(a/a,2f15.5)') 
     -      'axslbl__W> E* in tick labels not quite correct ',
     -      '           (factor not an integer power of 10): ',
     -      factaxs, 10.0**intexp
        end if

        x = x0tckw
        l = len(xlabl(1))
        ilabl = 0
        do 10  i = 1, nxtick
c         write(iolog,*) i, nxtick, i0xtck, x, nxnumb
         if (i.ge.i0xtck.and.mod((i-i0xtck), nxnumb) .eq. 0) then
          ilabl = ilabl + 1
          if (ilabl .gt. maxtck) then
            write(iolog,'(a)') 'axslbl__E> increase MAXTCK'
            stop
          end if
c ------- get a string with plenty of pre-dot positions and specified 
c         number of post-dot positions (if ndec=-1, the integer is obtained,
c         if ndec=0, the real with dot at the end is obtained):
          call r_str(factaxs*x, xlabl(ilabl), 10, ndec)
c ------- left justify the string
          call ljust(xlabl(ilabl))
          if (addexpon) call addstr(xlabl(ilabl), expons)
         end if
         x = x + dxtckw
10      continue
        return
      end

c --- Plots a spectrum using linestyle ilintp (all in World coordinates)
      subroutine spectr(x,dx,y,npnts,ilintp,iaxtyp)
#include "asgl.cmn"
        integer iaxtyp
        integer ilintp,npnts,i
        real x,y(npnts),dx,wpx,wpy,x1,x2,yy,d

c ----- clip everything outside the Plot area:
        d = 0.5*linwdt(iaxtyp)
        call clip(x0plt+d, y0plt+d, x1plt-d, y1plt-d)

        x1 = wpx(x)
        x2 = wpx(x+dx)

        do  i = 1, npnts
          yy = wpy(y(i))
          call line(x1,yy,x2,yy,ilintp)
        end do

c ----- back to no clip
        call initclip

        return
      end

c --- Plots (x,y) using linestyle ilintp and symbols isybtp (if not defined,
c     nothing will show):
      subroutine plot1(x,y,c,npnts,ilintp,isybtp,itfont,iaxtyp)
#include "asgl.cmn"
        integer iaxtyp
        integer npnts, ilintp, isybtp, itfont
        real x(npnts), y(npnts), c(npnts), d

c ----- clip everything outside the Plot area:
        d = 0.5*linwdt(iaxtyp)
        call clip(x0plt+d, y0plt+d, x1plt-d, y1plt-d)

        call pline(x,y,npnts,ilintp)
        call psymbolc(x,y,c,npnts,isybtp,itfont)
c        call psymbol(x,y,npnts,isybtp,itfont)

c ----- back to no clip
        call initclip

        return
      end



c --- Plots (x,y) in a histogram form: 
c --- barspc: actual width of the bar measured in units of delta-x 
c             belonging to one bar
      subroutine hist2(x,y,npnts,iyc,nyc,ilintp,gray,
     -                 barspc,x0wrl,x1wrl,xshift,iaxtyp)
        implicit none
#include "asgl.cmn"
        integer iaxtyp
        integer npnts,ilintp,nyc,iyc(nyc),i,iy
        real x(npnts),xshift,gray(nyc),barspc,x0wrl,x1wrl,ybase,ycumul
        real y(maxpnt,maxclm),yzero,wp2,d,deltax,x1,x2,y1,y2
        real wpx,wpy,xw,yw

c ----- clip everything outside the Plot area:
        d = 0.5*linwdt(iaxtyp)
        call clip(x0plt+d, y0plt+d, x1plt-d, y1plt-d)

c ----- base of the histogram bars in the world coordinates
        ybase = 0.0
c ----- base of the histogram bars in the PS coordinates
        yzero = wpy(ybase)

c ----- x-range 'belonging' to one point (equal spacing assumed):
        deltax = (x(npnts)-x(1)) / max((npnts-1), 1)
        wp2 = 0.5 * deltax * barspc
        do  i = 1, npnts
c ------- stack the nyc bars on top of each other
          ycumul = ybase
          do  iy = 1, nyc
            xw = x(i)
            yw = y(i,iyc(iy))
            x1 = wpx(xw - wp2 + xshift)
            x2 = wpx(xw + wp2 + xshift)
            y1 = wpy(ycumul)
            y2 = wpy(ycumul+yw)
            call pbox(x1, y1, x2, y2, ilintp, gray(iy))
            ycumul = ycumul + yw
          end do
        end do
c ----- a line at y=0, spanning the window, very thin
        call line(wpx(x0wrl),yzero,wpx(x1wrl),yzero,3)
c ----- a line at y(world) = 0 from x1(left) to xn(right)
c         call line(wpx(x(1))-wp2,yzero,x2,yzero,iaxtyp)

        call initclip

        return
      end



c --- it will do a density plot of an array x(i,j):
      subroutine dplot1(arr,nxdim,nydim,maxa,gray1,gray2,lintyp,where,
     -                  small,big,style,barleg,nbpre,nbpost,
     -                  numb,npre,npost,ifntpnt,iprnfnt,
     -                  x0plt,y0plt,x1plt,y1plt,dxtxt,dytxt,iaxtyp)
        implicit none
#include "asgl.cst"
#include "io.cst"
#include "psgl.cmn"
        integer iaxtyp
        integer nleg, nnumb
        parameter (nleg = 100, nnumb=10)
        real xbar, dbar
        parameter (xbar = 1.07, dbar = 0.08)
        integer nbpre, nbpost, maxa, nydim, nxdim,lintyp
        integer lin,ix,iy,i
        integer npre,npost,ifntpnt,iprnfnt
        real gray1,gray2,small,big,x1,y1,x2,y2,gray,grayness
        real xl,wpx,xu,yl,wpy,yu,da,a,x3,x4,yval,y,x5
        real x0plt,y0plt,x1plt,y1plt,d,rminarr,rmaxarr,gscale,gtrans
        real arr(maxa,nydim),xx,yy,wd2,dxtxt,dytxt
        character where*(*), style*(*), labl*9, val*20
        logical barleg,numb

c ----- clip everything outside the Plot area:
        d = 0.5*linwdt(iaxtyp)
        call clip(x0plt+d, y0plt+d, x1plt-d, y1plt-d)

c ----- find the smallest and the largest elements of an array
        if (small .eq. -999.) small = rminarr(arr,maxa,nxdim,nydim)
        if (big   .eq. -999.) big   = rmaxarr(arr,maxa,nxdim,nydim)
        write(iolog,'(a,2g14.4)')
     -  'dplot1___> Zmin, Zmax: ', small, big

c ----- get a scaling factor for the grayness
c ----- workout the scales in x and y directions (gray=sc*x + tr)
        gscale = (gray2-gray1) / (big-small) 
        gtrans = -small*gscale + gray1

c ----- half-width of the small gray elements:
        wd2 = 0.5
        lin = 0

        if (where .eq. 'UPPER') then
          do 20  ix = 1, nxdim
            do 10  iy = ix, nydim
              call pos(ix,iy,wd2,x1,y1,x2,y2)
              if (numb) then
               if ((arr(ix,iy).ge.small).and.(arr(ix,iy).le.big)) then
                xx = 0.5*(x1+x2)+dxtxt
                yy = 0.5*(y1+y2)+dytxt
                call r_str(arr(ix,iy), val, npre, npost)
                call ljust(val)
                call ps_text(val,xx,yy,ifntpnt,0.0,2,2)
               end if
              else
                gray = grayness(style,arr(ix,iy),small,big,gscale,
     -                          gtrans,gray1,gray2)
                call box(x1,y1,x2,y2,lin,gray)
              end if
10          continue
20        continue
        else
          if (where .eq. 'LOWER') then
            do 21  iy = 1, nydim
              do 11  ix = iy, nydim
                call pos(ix,iy,wd2,x1,y1,x2,y2)
                if (numb) then
                 if((arr(ix,iy).ge.small).and.(arr(ix,iy).le.big))then
                  xx = 0.5*(x1+x2)+dxtxt
                  yy = 0.5*(y1+y2)+dytxt
                  call r_str(arr(ix,iy), val, npre, npost)
                  call ljust(val)
                  call ps_text(val,xx,yy,ifntpnt,0.0,2,2)
                 end if
                else
                  gray = grayness(style,arr(ix,iy),small,big,gscale,
     -                            gtrans,gray1,gray2)
                  call box(x1,y1,x2,y2,lin,gray)
                end if
11            continue
21          continue
          else
            do 22  ix = 1, nxdim
              do 12  iy = 1, nydim
                call pos(ix,iy,wd2,x1,y1,x2,y2)
                if (numb) then
                 if((arr(ix,iy).ge.small).and.(arr(ix,iy).le.big))then
                   xx = 0.5*(x1+x2)+dxtxt
                   yy = 0.5*(y1+y2)+dytxt
                   call r_str(arr(ix,iy), val, npre, npost)
                   call ljust(val)
                   call ps_text(val,xx,yy,ifntpnt,0.0,2,2)
                  end if
                 else
                   gray = grayness(style,arr(ix,iy),small,big,gscale,
     -                             gtrans,gray1,gray2)
                   call box(x1,y1,x2,y2,lin,gray)
                 end if
12            continue
22          continue
          end if
        end if


        xl = wpx(wd2)
        xu = wpx(nxdim + wd2)
        yl = wpy(wd2)
        yu = wpy(nydim + wd2)

        if (lintyp .gt. 0) then
          if (where .eq. 'UPPER') then
            do 30  iy = 1, nydim+1
              y1 = wpy(iy - wd2)
              call line(xl,y1,min(xu,wpx(iy+wd2)),y1,lintyp)
30          continue
            do 40  ix = 1, nxdim+1
              x1 = wpx(ix - wd2)
              call line(x1,max(yl,wpy(ix-1-wd2)),x1,yu,lintyp)
40          continue
          else
            if (where .eq. 'LOWER') then
              do 31  iy = 1, nydim+1
                y1 = wpy(iy - wd2)
                call line(max(xl,wpx(iy-1-wd2)),y1,xu,y1,lintyp)
31            continue
              do 41  ix = 1, nxdim+1
                x1 = wpx(ix - wd2)
                call line(x1,yl,x1,min(yu,wpy(ix+wd2)),lintyp)
41            continue
            else
              do 32  iy = 1, nydim+1
                y1 = wpy(iy - wd2)
                call line(xl,y1,xu,y1,lintyp)
32            continue
              do 42  ix = 1, nxdim+1
                x1 = wpx(ix - wd2)
                call line(x1,yl,x1,yu,lintyp)
42            continue
            end if
          end if
        end if

c ----- back to no clip
        call initclip

c ----- plot the gray-bar legend, if so selected:
        if (barleg) then
          da = (big-small) / (nleg-1)
          a = small
c ------- left of the bar
          x1 = xbar
c ------- right of the bar
          x2 = x1 + dbar
c ------- right of the ticks
          x3 = x2 + dbar*0.1
c ------- left of the tick labels
          x4 = x3 + dbar*0.04
          do  i = 1, nleg
            gray = grayness(style,a,small,big,gscale,gtrans,gray1,gray2)
            y1 = (i-1.0)/(nleg)
            y2 = (1.0*i)/(nleg)
            call box(x1,y1,x2,y2,0,gray)
            a = a + da
          end do

          x5 = x4 + 0.65*fnthgt(iprnfnt)*(nbpre+nbpost)
c          x5 = x4 + 0.65*fnthgt(2)*(nbpre+nbpost)
          do  i = 1, nnumb+1, 1
            y = (i-1.0)/(nnumb)
            yval = small + (big-small)*y
            call line(x2,y,x3,y,3)
            if (mod(i-1,2).eq.0) then
              call r_str(yval, labl, nbpre, nbpost)
              call ps_text(labl,x5,y,iprnfnt,0.0,3,2)
c             call ps_text(txt,x,y,ifont,angle,ihstyl,ivstyl)
            end if
            call box2(x1,0.0,x2,0.0,x2,1.0,x1,1.0,2,1.0)
          end do
        end if

        return
      end


      real function grayness(style, arr, small, big, gscale, gtrans,
     -                       gray1, gray2)
        character style*(*)
        if (index(style, 'GRAY') .gt. 0) then
          g = gscale * arr + gtrans
          grayness = min(max(0.0, g), 1.0)
        else
          if ((arr.ge.small).and.(arr.le.big)) then
            grayness = gray1
          else
            grayness = gray2
          end if
        end if
        return
      end


      subroutine pos(ix,iy,wd2,x1,y1,x2,y2)
        x1 = wpx(ix - wd2)
        y1 = wpy(iy - wd2)
        x2 = wpx(ix + wd2)
        y2 = wpy(iy + wd2)
        return
      end


c --- Smoothes the Table array (x,y) using cubic splines
      subroutine spltab(x,y,mpnt,npnts,nsmth)
#include "asgl.cmn"
        real x(mpnt), y(mpnt), xp(maxpnt), yp(maxpnt)
        real yd2(maxpnt)

c ----- fix the first derivatives on both sides and get the splines for y:
        yd1 = (y(2)-y(1)) / (x(2)-x(1))
        ydn = (y(npnts)-y(npnts-1)) / (x(npnts)-x(npnts-1))
        call spline(x,y,npnts,yd1,ydn,yd2)

c ----- get the spline points
        dx = (x(npnts)-x(1)) / (nsmth-1)
        xspl = x(1) 
        do 31  i = 1, nsmth
          call splint(x,y,yd2,npnts,xspl,yp(i))
          xp(i) = xspl
          xspl = xspl + dx
31      continue

        if (nsmth .le. mpnt) then
          do  i = 1, nsmth
            x(i) = xp(i)
            y(i) = yp(i)
          end do
          npnts = nsmth
        else
          write(iolog,'(a)') 
     -    'spltab__E> not enough space in Table array for a spline'
          stop
        end if

        return
      end



c --- Plots smoothed (x,y) using linestyle ilintp and symbols 
c     isybtp (if not defined, nothing will show):
c --- returns the splined curve if retspl=.true.
      subroutine plot2(x,y,mpnt,npnts,nsmth,ilintp,isybtp,itfont,
     -                 retspl,iaxtyp)
#include "asgl.cmn"
        integer iaxtyp
        real x(npnts), y(npnts), xp(maxpnt), yp(maxpnt)
        real yd2(maxpnt)
        logical retspl

c ----- clip everything outside the Plot area:
        d = 0.5*linwdt(iaxtyp)
        call clip(x0plt+d, y0plt+d, x1plt-d, y1plt-d)

c ----- fix the first derivatives on both sides and get the splines for y:
        yd1 = (y(2)-y(1)) / (x(2)-x(1))
        ydn = (y(npnts)-y(npnts-1)) / (x(npnts)-x(npnts-1))
        call spline(x,y,npnts,yd1,ydn,yd2)

c ----- get the spline points
        dx = (x(npnts)-x(1)) / (nsmth-1)
        xspl = x(1) 
        do 31  i = 1, nsmth
          call splint(x,y,yd2,npnts,xspl,yp(i))
          xp(i) = xspl
          xspl = xspl + dx
31      continue

        call pline(xp,yp,nsmth,ilintp)
        call psymbol(xp,yp,nsmth,isybtp,itfont)

        if (retspl) then
          if (nsmth .le. mpnt) then
            do  i = 1, nsmth
              x(i) = xp(i)
              y(i) = yp(i)
            end do
            npnts = nsmth
          else
            write(iolog,'(a)') 
     -      'plot2___E> not enough space in Table array for a spline'
            stop
          end if
        end if

        call initclip

        return
      end


      subroutine spline(x,y,n,yp1,ypn,y2)
      parameter (nmax=3000)
      real x(n),y(n),y2(n),u(nmax)
      if (yp1.gt..99e30) then
        y2(1)=0.
        u(1)=0.
      else
        y2(1)=-0.5
        u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      endif
      do 11 i=2,n-1
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))
     *      /(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
11    continue
      if (ypn.gt..99e30) then
        qn=0.
        un=0.
      else
        qn=0.5
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      endif
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do 12 k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
12    continue
      return
      end


      subroutine splint(xa,ya,y2a,n,x,y)
      real xa(n),ya(n),y2a(n)
      klo=1
      khi=n
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k).gt.x)then
          khi=k
        else
          klo=k
        endif
      goto 1
      endif
      h=xa(khi)-xa(klo)
      if (h.eq.0.) pause 'Bad XA input.'
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+
     *      ((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
      return
      end

      subroutine tparam(t,tmin,tmax,nextr)
      real t(nextr)
      dt = (tmax-tmin)/(nextr-1)
      do 101  i = 1, nextr
        t(i) = tmin + dt*(i-1)
101   continue
      return
      end


c --- 1 -  8  ... small 1-4, left column; 5-8, right column  (8/page)
c     9 - 11  ... medium small (3/page)
c    12 - 13  ... medium (2/page)
c         14  ... largish (1/page)
c         15  ... large  (rotated, 1/page)
c    16 - 47  ... very small (32/page; 4 columns of eight)
      subroutine position(ipos,iaspect,x0plt,y0plt,x1plt,y1plt,angle)
c ----- device dependent translation for all standard positions:
        parameter (xtrans=1.0,ytrans=0.5)

c ----- number of positions, ratio between x and y spans
        parameter (maxpos=47)
        real x0(maxpos), y0(maxpos), ang(maxpos)
        real side(maxpos)
        data  x0  /  3,  3,  3,  3, 13, 13, 13, 13,  7.2,  7.2,  
     -               7.2, 4.5, 4.5, 3.2,16.5,
     -               8*1.5, 8*6.5, 8*11.5, 8*16.5/
        data  y0  / 21, 14.7,  8.4,  2.1, 21, 14.7,  8.4,  2.1, 20, 11,
     -               2,16, 3,13,   5, 
     -              24.0, 20.8, 17.6, 14.4, 11.2, 8.0, 4.8, 1.6,
     -              24.0, 20.8, 17.6, 14.4, 11.2, 8.0, 4.8, 1.6,
     -              24.0, 20.8, 17.6, 14.4, 11.2, 8.0, 4.8, 1.6,
     -              24.0, 20.8, 17.6, 14.4, 11.2, 8.0, 4.8, 1.6/
        data side /5.5,5.5,5.5,5.5,5.5,5.5,5.5,5.5,7.5,7.5,
     -             7.5,11, 11, 14, 19,32*2.5/
        data ang  /  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
     -               0,  0,  0,  0, 90,  32*0/

        if ((iaspect .lt. 0).or.(iaspect.gt.2)) then
          write(iolog,'(a,i4)')
     -    'positio_E>, iaspect out of range: ', iaspect
          stop
        end if

        go to (1,2,3) iaspect+1
1         ratiox = 1.00000
          ratioy = 1.33333
          go to 100
2         ratiox = 1.3 
          ratioy = 1.3 
          go to 100
3         ratiox = 0.35000
          ratioy = 1.30000
100     continue

        x0plt = x0(ipos) + xtrans
        y0plt = y0(ipos) + ytrans
        x1plt = x0plt + side(ipos) / ratiox
        y1plt = y0plt + side(ipos) / ratioy
        angle = ang(ipos)

        return
      end


c --- given x0wrl,x1wrl in real World coordinates, return the best
c     tick structure, if not specified yet
      subroutine ticks(x0wrl,x1wrl,dxtckw,x0tckw,x2tckw,i0xtck,
     -                 nxnumb,ndecx)
        parameter (maxspc = 8, nspac = 6)
        real dtcks(nspac)
        data (dtcks(i),i=1,nspac) /2.0,1.0,0.5,0.4,0.2,0.1/

        if (x2tckw .eq. -999) x2tckw = x1wrl

c ----- determine the position of the starting tick, if required
        if (dxtckw .eq. -999) then
c ------- calculate the order of the magnitude of the x-axis span:
          deltax = (x1wrl-x0wrl)
          norder = ifix(log10(deltax))
          if (deltax .lt. 1.0) norder = norder-1
          order = 10.0**norder
c ------- 1 <= delta <= 10
          delta = deltax / order

c ------- find spacing between the ticks:
c ------- pick the first standard spacing that has more than
c         maxspc ticks per axis:
          do i = 1, nspac
            ii = i
            ntcks = ifix(delta / dtcks(i)) + 1
            if (ntcks .gt. maxspc) go to 10
          end do
10        continue
          dxtckw = dtcks(ii) * order
        end if

c ----- find the position of the first tick;
        if (x0tckw .eq. -999) then
         if (x0wrl .ne. 0.0) then
c ------- calculate the order of the magnitude of the x0wrl:
          norder = ifix(log10(abs(x0wrl)))
          if (abs(x0wrl) .lt. 1.0) norder = norder-1
          order = 10.0**norder
c ------- 1 <= xm <= 10
          xm = abs(x0wrl)/order

c ------- starting position is the first standard position larger than
c         or equal to xm; standard positions are 0, standard dxtckw, 
c         2 * standard dxtckw, ...
c          sdtckw = 10.0**ifix(log10(dxtckw))
          sdtckw = dxtckw/order
          dtck = 0.0
15        dtck = dtck + sdtckw
          if (dtck .lt. xm) go to 15
          if (x0wrl .lt. 0.0) dtck = dtck - sdtckw
          x0tckw = sign(1.0,x0wrl) * dtck * order
         else
          x0tckw = 0.0
         end if
        end if

c ----- the first tick that is labeled:
        if (i0xtck .eq. -999) i0xtck = 1
      
c ----- every which tick is labeled:
        if (nxnumb .eq. -999) nxnumb = 2

c ----- the number of decimal places for the labels:
        if (ndecx .eq. -999) then
          if (dxtckw .ge. 1.0) then
            ndecx = -1
          else if (dxtckw .lt. 0.0) then
            ndecx = -20000
          else
            ndecx = -ifix(log10(dxtckw))+1
          end if
        end if

        return
      end

c --- x0wrl and x1wrl are the centers of the first and last bar; there are
c     nhist such bars;
      integer function indhst(x,nhist,x0wrl,x1wrl,dx)
        implicit none
        integer nhist,i
        real x,x1wrl,x0wrl,dx
        i = ifix((x-(x0wrl-0.5*dx))/dx) + 1
        indhst = min(max(i,1),nhist)
        return
      end

c --- x0wrl+dx and x1wrl-dx are the centers of the first and last bar; there are
c     nhist such bars in-between;
      integer function indhst2(x,nhist,x0wrl,x1wrl,dx)
        implicit none
        integer nhist,i
        real x,x1wrl,x0wrl,dx
        i = ifix((x-(x0wrl+0.5*dx))/dx) + 1
        indhst2 = min(max(i,1),nhist)
        return
      end

c --- prepares a density plot from ordinary data


      subroutine getdens(x,y,npnts,arr,maxxarr,maxyarr,dx,dy,
     -                   nbarx,nbary,x0wrl,y0wrl,x1wrl,y1wrl)
        implicit none
#include "io.cst"
        integer i,npnts,nbarx,ix,indhst2,j,iy,nbary,maxxarr,maxyarr
        real x(npnts),arr(maxxarr,maxyarr),x0wrl,x1wrl,y0wrl,y1wrl
        real rminv,rmaxv,dx,dy,y(npnts)
        logical deflt

c ----- DX and DY have to be specified:

c ----- determine the real world range of the x-axis for the histogram:
        if (deflt(x0wrl)) then 
          x0wrl = rminv(x,npnts)
          x0wrl = x0wrl - 1.00001*dx
        end if
        if (deflt(x1wrl)) then
          x1wrl = rmaxv(x,npnts)
          x1wrl = x1wrl + 1.00001*dx
        end if
        if (deflt(y0wrl)) then 
          y0wrl = rminv(y,npnts)
          y0wrl = y0wrl - 1.00001*dy
        end if
        if (deflt(y1wrl)) then
          y1wrl = rmaxv(y,npnts)
          y1wrl = y1wrl + 1.00001*dy
        end if

c ----- get number of bars from x0wrl and x1wrl and dx
        if ((x1wrl-x0wrl) .lt. 1.0E-10) then
          write(iolog,'(a)') 
     -    'getdens_E> x-range too small (no data,bad XMIN/XMAX)'
          stop
        else
         if (dx .lt. 1.0E-5) then
           write(iolog,'(a)') 'getdens_E> BAR_DX too small'
           stop
         end if
c ------ there is half of a bar empty margin on each side:
         nbarx = ifix((x1wrl-x0wrl) / dx) - 1
        end if
c ----- if x1wrl is more than x0wrl+(nbarx+1)*dx, increase nbarx
        if (abs(x0wrl+(nbarx+1)*dx-x1wrl) .gt. 1.0E-5) nbarx=nbarx+1
c ----- make sure that x1wrl is the center of the last bar + dx
        x1wrl = x0wrl + (nbarx+1)*dx

        if ((y1wrl-y0wrl) .lt. 1.0E-10) then
          write(iolog,'(a)') 
     -    'getdens_E> y-range too small (no data,bad YMIN/YMAX)'
          stop
        else
         if (dy .lt. 1.0E-5) then
           write(iolog,'(a)') 'getdens_E> BAR_DY too small'
           stop
         end if
         nbary = ifix((y1wrl-y0wrl) / dy) - 1
        end if
        if (abs(y0wrl+(nbary+1)*dy-y1wrl) .gt. 1.0E-5) nbary=nbary+1
        y1wrl = y0wrl + (nbary+1)*dy

        if(nbarx.gt.maxxarr) then
          write(iolog,'(a)') 'getdens_E> increase MAXXARR'
          stop
        end if
        if(nbary.gt.maxyarr) then
          write(iolog,'(a)') 'getdens_E> increase MAXYARR'
          stop
        end if

        do  i = 1, nbarx
          do  j = 1, nbary
            arr(i,j) = 0
          end do
        end do

c ----- accumulate the arr values
        do  i = 1, npnts
          ix = indhst2(x(i),nbarx,x0wrl,x1wrl,dx)
          iy = indhst2(y(i),nbary,y0wrl,y1wrl,dy)
          arr(ix,iy) = arr(ix,iy) + 1
        end do

        write(iolog,'(a,2i4)') 'getdens__> NBARX, DX   : ', nbarx, dx
        write(iolog,'(a,2i4)') 'getdens__> NBARY, DY   : ', nbary, dy
        write(iolog,'(a,2i4)') 'getdens__> X0WRL, Y0WRL: ', x0wrl, y0wrl
        write(iolog,'(a,2i4)') 'getdens__> X1WRL, Y1WRL: ', x1wrl, y1wrl

        return
      end


c --- prepares histogram data from ordinary data, averages and std dev for
c     bars

      subroutine gethist(x,npnts,dx,xh,yh,nbars,maxpnt,x0wrl,x1wrl,
     -                   y,yavr,ystd,stderr)
        implicit none
#include "io.cst"
        integer i,npnts,nbars,ix,maxpnt,indhst2
        real x(npnts),xh(maxpnt),yh(maxpnt),x0wrl,x1wrl
        real y(npnts), yavr(maxpnt), ystd(maxpnt)
        real rminv,rmaxv,dx
        logical deflt,stderr

c ----- dx has to be defined.

c ----- determine the real world range of the x-axis for the histogram:
        if (deflt(x0wrl)) then 
          x0wrl = rminv(x,npnts)
          x0wrl = x0wrl - 1.000*dx
        end if
        if (deflt(x1wrl)) then
          x1wrl = rmaxv(x,npnts)
          x1wrl = x1wrl + 1.000*dx
        end if

c ----- get number of bars from x0wrl and x1wrl and dx
        if ((x1wrl-x0wrl) .lt. 1.0E-10) then
          write(iolog,'(a)') 
     -    'gethist_E> x-range too small (no data,bad XMIN/XMAX)'
        else
         if (dx .lt. 1.0E-5) then
           write(iolog,'(a)') 'gethist_E> BAR_DX too small'
           stop
         end if
         nbars = ifix((x1wrl-x0wrl) / dx) - 1
        end if
c ----- if x1wrl is more than x0wrl+(nbars+1)*dx, increase nbars
        if (abs(x0wrl+(nbars+1)*dx-x1wrl) .gt. 1.0E-5) nbars=nbars+1
c ----- make sure that x1wrl is the center of the last bar
        x1wrl = x0wrl + (nbars+1)*dx
        write(iolog,'(a,3g12.4,i4)') 
     -  'gethist__> x0wrl,x1wrl,dx,nbars: ',x0wrl,x1wrl,dx,nbars

c ----- initialize yh and assign the xh values
        do  i = 1, nbars
          yh(i) = 0.0
c ------- the center of the bar interval
          xh(i) = x0wrl + i*dx
          yavr(i) = 0.0
          ystd(i) = 0.0
        end do

c ----- accumulate the histogram bars and calculate avr and std of the 
c       corresponding Y values
        do  i = 1, npnts
          ix = indhst2(x(i),nbars,x0wrl,x1wrl,dx)
          yh(ix) = yh(ix) + 1.0
          yavr(ix) = yavr(ix) + y(i)
        end do
        do  ix = 1, nbars
          yavr(ix) = yavr(ix)/max(1.0,yh(ix))
        end do
        do  i = 1, npnts
          ix = indhst2(x(i),nbars,x0wrl,x1wrl,dx)
          ystd(ix) = ystd(ix) + (y(i)-yavr(ix))**2
        end do
        do  ix = 1, nbars
c ------- standard deviation:
          ystd(ix) = sqrt(ystd(ix)/max(1.0,yh(ix)))
c ------- standard error of the mean:
          if (stderr) ystd(ix) = ystd(ix) / sqrt(max(1.0,yh(ix)))
        end do

        return
      end
      

      subroutine caption(xpos,ypos,lbl,lblfnt,lblpos)
        implicit none
#include "psgl.cmn"
#include "cs.cmn"
        integer lblpos,justif(nposit),ialgn(nposit),lblfnt,i,j
        real orient(nposit),xylnfd(2,nposit)
        real xpos(nposit),ypos(nposit)
        character lbl*(*)

c ----- styles for the text positioned around the Graph:
        data (justif(i),i=1,nposit) /2, 2, 2, 3, 3, 2, 1, 1, 3, 3, 1,2/
        data (ialgn(i),i=1,nposit)  /1, 3, 1, 3, 3, 1, 3, 3, 3, 1, 1,3/
c        data (orient(i),i=1,nposit) /0.0,0.0,90.0,0.0,0.0,-90.0,0.0,
        data (orient(i),i=1,nposit) /0.0,0.0,90.0,0.0,0.0,90.0,0.0,
     -                               0.0,0.0,0.0,0.0,0.0/
c ----- x and y changes for the next label of the same style:
        data ((xylnfd(i,j),i=1,2),j=1,nposit) 
     -        /0,1, 0,-1, -1,0, 0,-1, 0,-1, 1,0, 0,-1, 0,-1, 0,-1, 
     -         0,1, 0,1, 0,-1/
   
c ----- print the caption:
c        write(iolog,*) 'lenl,lbl: ', lenl(lbl), lbl
        call ps_text(lbl,xpos(lblpos),ypos(lblpos),lblfnt,
     -               orient(lblpos),justif(lblpos),ialgn(lblpos))
c ------change the position for the next label of this type
        xpos(lblpos)=xpos(lblpos)+spclin*xylnfd(1,lblpos)*fnthgt(lblfnt)
        ypos(lblpos)=ypos(lblpos)+spclin*xylnfd(2,lblpos)*fnthgt(lblfnt)

        return 

      end



      subroutine wrtabl(ioout,outf,x,nwxy,iwxy,ndim,maxpnt,maxclm,npnts)
        implicit none
        integer npnts,ioout,ndim,maxpnt,maxclm,nwxy(2),iwxy(ndim),i,j
        real x(maxpnt,maxclm)
        character outf*(*)

        call openf(ioout, outf, 'unknown')
  
        do  i = 1, npnts
          write(ioout, '(99g15.4)') (x(i,iwxy(j)),j=1,nwxy(1)+nwxy(2))
        end do

        close(ioout)

        return
      end


c       subroutine resleg
c #include "asgl.cmn"
c         yleg = y1plt * 0.95
c         return
c       end


c --- plots a legend consisting of a line/point/bar and a text description
      subroutine legend(xleg2,yleg2,symb,descr,gray,ipntt,ilint,ilintb,
     &                  lblfnt,lblpos)
#include "asgl.cmn"
        character symb2*(20), symb*(*), descr*(*)

        integer lblpos,justif(nposit),ialgn(nposit),i,j
        real orient(nposit),xylnfd(2,nposit)
        real xleg2(nposit),yleg2(nposit), y3

c ----- styles for the text positioned around the Graph:
        data (justif(i),i=1,nposit) /2, 2, 2, 3, 3, 2, 1, 1, 3, 3, 1,2/
        data (ialgn(i),i=1,nposit)  /1, 3, 1, 3, 3, 1, 3, 3, 3, 1, 1,3/
        data (orient(i),i=1,nposit) /0.0,0.0,90.0,0.0,0.0,-90.0,0.0,
     -                               0.0,0.0,0.0,0.0,0.0/
c ----- x and y changes for the next label of the same style:
        data ((xylnfd(i,j),i=1,2),j=1,nposit) 
     -        /0,1.3, 0,-1.3, -1.3,0, 0,-1.3, 0,-1.3, 1.3,0, 0,-1.3, 
     -         0,-1.3, 0,-1.3, 0,1.3, 0,1.3, 0,-1.3/

c ----- is it a line or a symbol:
        symb2 = symb
        call upper(symb2)

c ----- left of the legend symbol
        x1 = xleg2(lblpos)
c        x1 = xlegend
c ----- right of the legend symbol
        if (index(symb2,'LINE').gt.0.or.index(symb2,'BAR').gt.0)then
          x2 = x1 + dlegend
        else
c         symb2 = POINT:
          x2 = x1 + 0.33*dlegend
        end if
c ----- right of the description
        x3 = x2 + dlegend*0.2

        y3 = yleg2(lblpos)
        if (orient(lblpos).eq.0. .and. ialgn(lblpos).eq.1)
     &    y3 = yleg2(lblpos) + 0.45*fnthgt(lblfnt)
        if (orient(lblpos).eq.0. .and. ialgn(lblpos).eq.3)
     &    y3 = yleg2(lblpos) - 0.45*fnthgt(lblfnt)

        if (index(symb2, 'LINE') .gt. 0) then
          call line(x1,y3,x2,y3,ilint)
        else
          if (index(symb2, 'BAR') .gt. 0) then
            d2 = 0.45 * fnthgt(lblfnt)
            call box(x1,y3-d2,x2,y3+d2,ilintb,gray)
          else
            if (index(symb2, 'POINT') .gt. 0) then
              call ps_symbol(x1+0.1*dlegend,y3,-1.,ipntt,lblfnt,1)
            end if
          end if
        end if

        call ps_text(descr,x3,yleg2(lblpos),lblfnt,orient(lblpos),
     -               justif(lblpos),ialgn(lblpos))

c ----- new x,y position
c        xleg2(lblpos) = xleg2(lblpos)
c        yleg2(lblpos) = yleg2(lblpos) - 2.0*fnthgt(lblfnt)

        xleg2(lblpos)=xleg2(lblpos)+spclin*xylnfd(1,lblpos)*
     &                      fnthgt(lblfnt)
        yleg2(lblpos)=yleg2(lblpos)+spclin*xylnfd(2,lblpos)*
     &                      fnthgt(lblfnt)

c        x4 = x3 + 0.70*fnthgt(lblfnt)*lenr(descr)
c        bbox2 = max(bbox2, x2papr+(x4-x1plt)*dxpapr)
   
        return
      end


c --- Plots error bars using linestyle ilintp and symbols isybtp (if not 
c     defined, nothing will show):
      subroutine errbars(x,y,error,npnts,ilintp,isybtp,itfont,sgn,
     &                   iaxtyp)
#include "asgl.cmn"
        integer iaxtyp
        real x(npnts), y(npnts), error(npnts)
        character sgn*(*)

c ----- clip everything outside the Plot area:
        d = 0.5*linwdt(iaxtyp)
        call clip(x0plt+d, y0plt+d, x1plt-d, y1plt-d)

        do  i = 1, npnts
          call errbar(x(i),y(i),error(i),i,ilintp,isybtp,itfont,sgn)
        end do

c ----- back to no clip
        call initclip

        return
      end


      subroutine errbar(x,y,error,i,ilintp,isybtp,itfont,sgn)
        implicit none
#include "psgl.cst"
        integer ilintp,i,isybtp,itfont
        real wpx, wpy, x, y, error, xps, xps1, xps2
        real yps1, yps2
        character sgn*(*)
        logical strgin
        external strgin

        xps  = wpx(x)
        xps1 = xps-derror
        xps2 = xps+derror
        if (strgin(sgn, 'DOWN')) then
          yps1 = wpy(y-error)
        else
          yps1 = wpy(y)
        end if
        if (strgin(sgn, 'UP')) then
          yps2 = wpy(y+error)
        else
          yps2 = wpy(y)
        end if

        call line(xps,yps1,xps,yps2,ilintp)
        call line(xps1,yps1,xps2,yps1,ilintp)
        call line(xps1,yps2,xps2,yps2,ilintp)

        call symbol(x,y,isybtp,itfont,i)

        return
      end


      subroutine fitmod(x,y,sig,npnts,type,ainit,nainit,nprmfit,lista,
     -                  relcut,abscut,mstdev,x1,x2,n,npnts2,wr)
        implicit none
#include "io.cst"
        integer nprmmax, nxmax
        parameter (nprmmax = 50, nxmax=1)
        integer npnts,maxcyc,nainit,indexw,npnts2,n
        integer nprmfit,lista(nainit),nprmall,nx,niter
        real a(nprmmax)
        real covar(nprmmax,nprmmax),alpha(nprmmax,nprmmax)
        real ainit(nainit),alamda,relcut,abscut,errmult,alammax
        real degfreed,chisq,q,x1,x2
        real x(n), y(n), sig(n)
        real fitnormal,fitpoly,fitpower,fitlognormal,fitexponential
        real fitlogarithmic,fitexponential2,fitpolygauss
        real fitexponential3,fitexponential4,fitlj1
        real fitpolygauss2,fitdisf,fitexponential5,fitexponential6
        real fitpolygauss4
        character type2*(30), type*(*), comment1*50,comment2*50
        logical mstdev,wr
        external fitnormal,fitpoly,fitlognormal,fitexponential,
     -           fitlogarithmic,fitexponential2,fitpolygauss,
     -           fitexponential3,fitpolygauss2,fitpower,fitdisf,
     -           fitexponential4,fitexponential5,fitexponential6,
     -           fitlj1,fitpolygauss4


        maxcyc = 100
        alammax = 1.0E20 

        type2 = type
        call upper(type2)

        if (indexw(type2, 'LJ1') .gt. 0) then
          nx = 1
          nprmall = 6
          comment1 = 'Fit to a LJ1 expression: '
          comment2 = '  y = (a-c)/x^e - (b-d)/x^f + Ln Z(a,b)/Z(c,d)'
          call chkprm(nainit,nprmall,nprmfit,nx,1)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitlj1,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitlj1,y,n,npnts,x1,x2,npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        end if

        if (indexw(type2, 'DISSOCIATION') .gt. 0) then
          nx = 1
          nprmall = 4
          comment1 = 'Fit to a Kd expression: '
          comment2 = '  y = Fo + (Fm-Fo) * ... '
          call chkprm(nainit,nprmall,nprmfit,nx,1)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitdisf,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitdisf,y,n,npnts,x1,x2,npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        end if

        if (indexw(type2, 'POLYNOMIAL') .gt. 0) then
          nx = 1
          nprmall = 7
          comment1 = 'Fit to a polynomial: '
          comment2 = '  y = a + bx + cx^2 + dx^3 + ex^4 + fx^5 + gx^6'
          call chkprm(nainit,nprmall,nprmfit,nx,1)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitpoly,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitpoly,y,n,npnts,x1,x2,npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        end if

        if (indexw(type2, 'POWER') .gt. 0) then
          nx = 1
          nprmall = 3
          comment1 = 'Fit to a power law: '
          comment2 = '  y = a + bx^c'
          call chkprm(nainit,nprmall,nprmfit,nx,1)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitpower,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitpower,y,n,npnts,x1,x2,npnts2,
     -              wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        end if

        if (indexw(type2, 'NORMAL') .gt. 0) then
          nx = 1
          nprmall = 3
          comment1='Fit to a Gaussian: '
          comment2='y = exp(c) 1/(Sqrt[2 Pi] b) exp{-0.5*[(x-a)/b)^2]}'
          comment2=' '
          call chkprm(nainit,nprmall,nprmfit,nx,1)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq, fitnormal, abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitnormal,y,n,npnts,x1,x2,npnts2,
     -              wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        end if


c ----- the parameters are: a, w_1, m_1, s_1, w_2, m_2, s_2, ..., m_n, s_n
        if (indexw(type2, 'POLYGAUSS') .gt. 0) then
          nx = 1
          nprmall = nainit
          comment1='Fit to a multiple Gaussian (Sum_i w_i=1): '
          comment2='y = a * {Sum_i w_i * Gauss[mean_i, stdev_i]}'
c          call chkprm(nainit,nprmall,nprmfit,nx,1)
          if (mod(nainit, 3) .ne. 0)
     -    write(iolog,'(a,i4)') 'fit_____E> wrong number of params: ',
     -                          nainit
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq, fitpolygauss, abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitpolygauss,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        end if


c ----- the parameters are: a, w_1, m_1, s_1, w_2, m_2, s_2, ..., m_n, s_n
        if (indexw(type2, 'POLYGAUSS360') .gt. 0) then
          nx = 1
          nprmall = nainit
          comment1='Fit to a multiple 360 Gaussian (Sum_i w_i=1):'
          comment2='y = a * {Sum_i w_i * Gauss[mean_i, stdev_i]}'
c          call chkprm(nainit,nprmall,nprmfit,nx,1)
          if (mod(nainit, 3) .ne. 0)
     -    write(iolog,'(a,i4)') 'fit_____E> wrong number of params: ',
     -                          nainit
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq, fitpolygauss2, abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitpolygauss2,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        end if


        if (indexw(type2, 'LOG-NORMAL') .gt. 0) then
          nx = 1
          nprmall = 3
          comment1 = 'Fit to a logarithm of Gaussian:'
          comment2 = 'y = c+Ln{1/(Sqrt[2 Pi] b) - 0.5*[(x-a)/b)^2]}'
          call chkprm(nainit,nprmall,nprmfit,nx,1)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq, fitlognormal, abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitlognormal,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        end if

        if (indexw(type2, 'EXPONENTIAL') .gt. 0) then
          nx = 1
          nprmall = 4
          comment1 = 'Fit to an exponential:'
          comment2 = 'y = a + exp[b + c x^d]'
          call chkprm(nainit,nprmall,nprmfit,nx,1)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitexponential,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitexponential,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        end if

        if (indexw(type2, 'EXPONENTIAL2') .gt. 0) then
          nx = 1
          nprmall = 3
          comment1 = 'Fit to an exponential:'
          comment2 = 'y = (1-exp[a]) + exp[a + b x^c]'
          call chkprm(nainit,nprmall,nprmfit,nx,1)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitexponential2,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitexponential2,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        end if

        if (indexw(type2, 'EXPONENTIAL3') .gt. 0) then
          nx = 1
          nprmall = 1
          comment1 = 'Fit to an exponential:'
          comment2 = 'y = a [1 - exp(-x/a)]'
          call chkprm(nainit,nprmall,nprmfit,nx,1)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitexponential3,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitexponential3,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        end if

        if (indexw(type2, 'EXPONENTIAL4') .gt. 0) then
          nx = 1
          nprmall = 1
          comment1 = 'Fit to an exponential:'
          comment2 = 'y = 1/a * exp[-x/a]'
          call chkprm(nainit,nprmall,nprmfit,nx,1)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitexponential4,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitexponential4,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        end if

        if (indexw(type2, 'EXPONENTIAL5') .gt. 0) then
          nx = 1
          nprmall = 3
          comment1 = 'Fit to an exponential:'
          comment2 = 'y = c/b exp(-(x-a)/b)]'
          call chkprm(nainit,nprmall,nprmfit,nx,1)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitexponential5,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitexponential5,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        end if

        if (indexw(type2, 'EXPONENTIAL6') .gt. 0) then
          nx = 1
          nprmall = 2
          comment1 = 'Fit to an exponential:'
          comment2 = 'y = 1/b  exp(-(x-a)/b)]'
          call chkprm(nainit,nprmall,nprmfit,nx,1)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitexponential6,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitexponential6,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        end if

        if (indexw(type2, 'LOGARITHMIC') .gt. 0) then
          nx = 1
          nprmall = 4
          comment1 = 'Fit to a logarithm: '
          comment2 = '  y = a + b ln(d + c x)'
          call chkprm(nainit,nprmall,nprmfit,nx,1)
          call lsqfit(x,nx,nxmax,y,sig,npnts,ainit,a,nprmfit,nprmall,
     -         nprmmax,lista,covar,alpha, chisq,fitlogarithmic,abscut, 
     -         relcut, mstdev, niter, alamda, q, errmult, degfreed,  
     -         alammax, maxcyc)
          call gety(x,nxmax,a,nprmall,fitlogarithmic,y,n,npnts,x1,x2,
     -              npnts2,wr)
          call wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -         ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -         q,errmult,alammax,nx,mstdev)
        end if

        return
      end

  
      subroutine chkprm(nainit,nprmall,nprmfit,nx,nwx)
        implicit none
#include "io.cst"
        integer nainit,nprmall,nprmfit,nx,nwx
        if (nx .ne. nwx) then
          write(iolog,'(a,2i3)') 'chkprm__E> specified and expected ',
     &          'numbers of X columns differ: ', nwx, nx
          stop
        end if
        if (nainit .ne. nprmall) then
          write(iolog,'(a,2i5)') 
     -    'chkprm__E> # of params initlzd <> all params: ',
     -    nainit,nprmall
          stop
        end if
        if (nprmfit .gt. nprmall) then
          write(iolog,'(a,2i5)') 
     -    'chkprm__E> # of param fitted > all params: ',
     -    nprmfit, nprmall
          write(iolog,'(a)') '      no param will be fitted'
          nprmfit = 0
        end if
        return
      end



      subroutine gety2(x,nxmax,a,nprmall,rmodel,y,n,npnts,x1,y1,
     &                 x2,y2,npnts2,wr)
        implicit none
        integer n, i, npnts, nprmall, npnts2, nxmax
        real x(nxmax,n), a(nprmall), rmodel, y(n), x1, x2, dx
        real y1, y2, dy
        logical wr
        external rmodel

        if (wr) then
          npnts = npnts2
          dx = (x2 - x1) / (npnts - 1)
          dy = (y2 - y1) / (npnts - 1)
          do  i = 1, npnts
            x(1,i)= x1 + (i-1)*dx
            x(2,i)= y1 + (i-1)*dy
          end do
          do  i = 1, npnts
            y(i)=rmodel(x(1,i),2,a,nprmall)
          end do
        else
          do  i = 1, npnts
            y(i)=rmodel(x(1,i),2,a,nprmall)
          end do
        end if

        return
      end



      subroutine gety(x,nxmax,a,nprmall,rmodel,y,n,npnts,x1,x2,npnts2,
     &                wr)
        implicit none
        integer n, i, npnts, nprmall, npnts2, nxmax
        real x(nxmax,n), a(nprmall), rmodel, y(n), x1, x2, dx
        logical wr
        external rmodel

        if (wr) then
          npnts = npnts2
          dx = (x2 - x1) / (npnts - 1)
          do  i = 1, npnts
            x(1,i)= x1 + (i-1)*dx
          end do
          do  i = 1, npnts
            y(i)=rmodel(x(1,i),1,a,nprmall)
          end do
        else
          do  i = 1, npnts
            y(i)=rmodel(x(1,i),1,a,nprmall)
          end do
        end if

        return
      end



      subroutine smooth(isegm,tab,npnts,maxpnt,ncolmn,nwxy,iwxy,
     -                  ndim,ixcol,iycol,nsmth,type)
        implicit none
        integer isegm,ncolmn,ndim,npnts,maxpnt,nsmth
        integer nwxy(2),iwxy(ndim),ixcol,iycol
        real tab(maxpnt,ncolmn)
        character type2*(30), type*(*)

        type2 = type
        call upper(type2)

        if (index(type2, 'AVERAGE') .gt. 0) then
          call avrtab(isegm,tab,npnts,maxpnt,ncolmn,nwxy,iwxy,ndim)
        end if

        if (index(type2, 'SPLINE') .gt. 0) then
          call spltab(tab(1,ixcol),tab(1,iycol),maxpnt,npnts,nsmth)
        end if

        return
      end


c --- Model function y(X,A), specified by the USER:
c     (x ... independent variables, i.e. 'independent' part of data points)
c     (a ... parameters to be estimated by the least-squares fit)

      real function fitpoly(x, nx, a, na)
        implicit none
        integer nx,na
        real x(nx), a(na), t1, t2, t3, t4, t5, t6, t7

c        if (abs(x(1)) .gt. 1.0E5)
c     -    write(iolog,'(a)') 'fitpoly_W> Numerical instability; x: ', x(1) 

c ----- this is to allow large x and higher a(i) = 0:

        t1 =  a(1)
        t2 = (a(2)*x(1))
        t3 = (a(3)*x(1))*x(1)
        t4 = (a(4)*x(1))*x(1)*x(1)
        t5 = (a(5)*x(1))*x(1)*x(1)*x(1)
        t6 = (a(6)*x(1))*x(1)*x(1)*x(1)*x(1)
        t7 = (a(7)*x(1))*x(1)*x(1)*x(1)*x(1)*x(1)

        fitpoly = t1+t2+t3+t4+t5+t6+t7

        return
      end


      real function getmex(i)
        implicit none
#include "top_appl.cmn"
        integer i
        getmex = tab(i,iprm(1,37))
        return
      end

      integer function ngetmenpnts()
        implicit none
#include "top_appl.cmn"
        ngetmenpnts = iprm(1,61)
        return
      end


c     y = (a-c)/x^e - (b-d)/x^f + Ln Z(a,b)/Z(c,d)
c
      real function fitlj1(x, nx, a, na)
        implicit none
        integer nx,na,i,npnts,ngetmenpnts
        real x(nx), a(na), zab, zcd, e1, e2, p1, p2, a1, b1, a2, b2
        real xx,getmex
        data a1,b1,a2,b2,p1,p2/-888.8,-888.8,-888.8,-888.8,-888.8,
     &                         -888.8/
 
        if (a(1).ne.a1.or.a(2).ne.b1.or.a(5).ne.p1.or.a(6).ne.p2) then
          npnts = ngetmenpnts()
          zab = 0.0
          do  i = 1, npnts
            xx = getmex(i)
            if (xx .gt. 0.0) then
              e1 = a(1)/xx**a(5) - a(2)/xx**a(6)
              zab = zab + exp(-e1)
            end if
          end do
          a1 = a(1)
          b1 = a(2)
          p1 = a(5)
          p2 = a(6)
        end if
        if (a(3).ne.a2.or.a(4).ne.b2.or.a(5).ne.p1.or.a(6).ne.p2) then
          npnts = ngetmenpnts()
          zcd = 0.0
          do  i = 1, npnts
            if (xx .gt. 0.0) then
              xx = getmex(i)
              e2 = a(3)/xx**a(5) - a(4)/xx**a(6)
              zcd = zcd + exp(-e2)
            end if
          end do
          a2 = a(3)
          b2 = a(4)
          p1 = a(5)
          p2 = a(6)
        end if

c        write(iolog,*) zab,zcd,npnts,x(1),a(1),a(2),a(3),a(4),a(5),a(6)

        if (x(1) .gt. 0.0) then
          fitlj1 = (a(1)-a(3))/x(1)**a(5) - (a(2)-a(4))/x(1)**a(6) + 
     &             log(zab/zcd)
        else
          fitlj1 = 9.9E32
        end if

        return
      end

      real function fitpower(x, nx, a, na)
        implicit none
        integer nx,na
        real x(nx), a(na)

        fitpower = a(1) + a(2)*x(1)**a(3)

        return
      end


      real function fitdisf(x, nx, a, na)
        implicit none
#include "io.cst"
        integer nx,na
        real x(nx), a(na), tmp1, tmp2, tmp2sq

        tmp1 = 4.0*a(3)*x(1)
        tmp2 = a(3)+x(1)+a(4)
        tmp2sq = tmp2*tmp2
        if (tmp2sq .le. tmp1) then
          write(iolog,'(a,2g15.5)')
     &    'fitdisf__E> tmp2sq .le. tmp1: ',tmp2sq,tmp1
          write(iolog,'(99g15.5)')
     &    x(1),a(1),a(2),a(3),a(4)
          stop
        end if
        fitdisf = a(1) + (a(2)-a(1)) *
     &            (tmp2 - sqrt(tmp2sq - tmp1))/(2.0*a(3))

        return
      end

      real function fitlogarithmic(x, nx, a, na)
        implicit none
        integer nx,na
        real x(nx), a(na)
        fitlogarithmic = a(1) + a(2)*log(a(3) + a(4)*x(1))
        return
      end

      real function fitexponential(x, nx, a, na)
        implicit none
        integer nx, na
        real x(nx), a(na)
        
        fitexponential = a(1) + exp(a(2) + a(3)*(x(1)**a(4)))

        return
      end

      real function fitexponential2(x, nx, a, na)
        implicit none
        integer nx, na
        real x(nx), a(na)
        
        fitexponential2 = 1.0-exp(a(1)) + exp(a(1)+a(2)*(x(1)**a(3)))

        return
      end

      real function fitexponential3(x, nx, a, na)
        implicit none
        integer nx, na
        real x(nx), a(na)
        
        fitexponential3 = a(1) * (1.0 - exp(-x(1)/a(1)))

        return
      end

      real function fitnormal(x, nx, a, na)
        real pi
        parameter (pi = 3.1415927)
        real x(nx), a(na)
        fitnormal = exp(a(3))/(sqrt(2.0*pi)*a(2)) * 
     -              exp(-0.5*((x(1)-a(1))/a(2))**2)
        return
      end


      real function fitexponential4(x, nx, a, na)
        implicit none
        integer nx, na
        real x(nx), a(na)
        
        fitexponential4 = 1.0/a(1) * exp(-x(1)/a(1))

        return
      end

      real function fitexponential5(x, nx, a, na)
        implicit none
        integer nx, na
        real x(nx), a(na), fitexponential6
        
        fitexponential5 = a(3)*fitexponential6(x, nx, a, na)

        return
      end

      real function fitexponential6(x, nx, a, na)
        implicit none
        integer nx, na
        real x(nx), a(na)
        
        fitexponential6 = 1.0/a(2) * exp(-(x(1)-a(1))/a(2))

        return
      end


c --- polymodal binormal with degrees (REAL 360 periodicity): BIPERIODIC
c
c N_tot  w_1 w_2 ... w_(i-1)   m_11 m_21 ... m_2n     s_11 s_21 ... s_2n   rho_1
c
c number of parameters: 6 * gaussians (last weight is not specified because
c the sum of weights is 1)

      real function fitpolygauss4(x, nx, a, na)
        implicit none
#include "numbers.cst"
c        real pi2
c        parameter (pi2 = 6.28318530717959)
        integer nx, na, ig, ng, iomega,imeans,istdvs,irhos
        integer im,is
        real x(nx), a(na), f, w, a2(200), rho, rho1, omega
        real mean1, mean2, stdv1, stdv2, wlast
        real d1, d2, sd1, sd2, cd1, cd2
	real diffdeg
	external diffdeg

        ng = na / 6

        w = 0.0
        do  ig = 2, ng
          w = w + a(ig)
        end do
        wlast = 1.0 - w

        do  ig = 2, ng
          a2(ig-1) = a(ig)
        end do
        a2(ng) = wlast
        do  ig = ng+1, na
          a2(ig) = a(ig)
        end do

c ----- the same order as in MODELLER:
        iomega = 0
        imeans = ng+1
        istdvs = 3*ng+1
        irhos  = 5*ng
        f      = 0.0
        do  ig = 1, ng
          im    = imeans + (ig-1)*2
          is    = istdvs + (ig-1)*2
          omega = a2(iomega+ig)
          mean1 = a2(im)
          mean2 = a2(im+1)
          stdv1 = a2(is)*deg2rad
          stdv2 = a2(is+1)*deg2rad
          rho   = a2(irhos+ig)
          rho1  = 1. - rho*rho
          d1    = diffdeg(x(1),mean1)*deg2rad
          d2    = diffdeg(x(2),mean1)*deg2rad
          sd1   = sin(d1)
          sd2   = sin(d2)
          cd1   = 1. - cos(d1)
          cd2   = 1. - cos(d2)
          f     = f+omega/(pi2*stdv1*stdv2*sqrt(rho1)) *
     &            exp(-1./rho1 * (cd1/(stdv1*stdv1) - 
     &                            rho*(sd1/stdv1)*(sd2/stdv2) + 
     &                            cd2/(stdv2*stdv2)))
        end do

        fitpolygauss4 = a(1) * f

        return
      end



c --- polymodal binormal with degrees (360 periodicity): BINORMAL360
c
c N_tot  w_1 w_2 ... w_(i-1)   m_11 m_21 ... m_2n     s_11 s_21 ... s_2n   rho_1
c
c number of parameters: 6 * gaussians (last weight is not specified because
c the sum of weights is 1)

      real function fitpolygauss3(x, nx, a, na)
        implicit none
        real pi2
        parameter (pi2 = 6.28318530717959)
        integer nx, na, ig, ng, iomega,imeans,istdvs,irhos
        integer im,is
        real x(nx), a(na), f, w, diffdeg, a2(200), rho, rho1, omega
        real mean1, mean2, stdv1, stdv2, rv1, rv2, wlast

        ng = na / 6

        w = 0.0
        do  ig = 2, ng
          w = w + a(ig)
        end do
        wlast = 1.0 - w

        do  ig = 2, ng
          a2(ig-1) = a(ig)
        end do
        a2(ng) = wlast
        do  ig = ng+1, na
          a2(ig) = a(ig)
        end do

c ----- the same order as in MODELLER:
        iomega = 0
        imeans = ng+1
        istdvs = 3*ng+1
        irhos  = 5*ng
        f      = 0.0
        do  ig = 1, ng
          im    = imeans + (ig-1)*2
          is    = istdvs + (ig-1)*2
          omega = a2(iomega+ig)
          mean1 = a2(im)
          mean2 = a2(im+1)
          stdv1 = a2(is)
          stdv2 = a2(is+1)
          rho   = a2(irhos+ig)
          rho1  = 1.0 - rho*rho
          rv1   = diffdeg(x(1),mean1)/stdv1
          rv2   = diffdeg(x(2),mean2)/stdv2
          f     = f+omega/(pi2*stdv1*stdv2*sqrt(rho1)) *
     &            exp(-0.5/rho1 * (rv1*rv1 - 2.0*rho*rv1*rv2 + rv2*rv2))
        end do

        fitpolygauss3 = a(1) * f

        return
      end

      real function fitpolygauss2(x, nx, a, na)
        implicit none
        real pi
        parameter (pi = 3.1415927)
        integer nx, na, ig, ng, iw, im, is
        real x(nx), a(na), f, w, wlast, anew(200), diffdeg

        ng = na / 3

        w = 0.0
        iw = 2
        do  ig = 1, ng-1
          w = w + a(iw)
          iw = iw + 3
        end do
        wlast = 1.0 - w

        do  is = 2, na-2
          anew(is) = a(is)
        end do
        anew(na-1) = wlast
        anew(na)   = a(na-1)
        anew(na+1) = a(na)

        f = 0.0
        iw = 2
        im = 3
        is = 4
        do  ig = 1, ng
          f = f + anew(iw) / (sqrt(2.0*pi)*anew(is)) * 
     -            exp(-0.5*(diffdeg(x(1),anew(im))/anew(is))**2)
          iw = iw + 3
          im = im + 3
          is = is + 3
        end do
c        write(iolog,'(99f9.3)') x(1), a(1)*f, (anew(is),is=1,na+1)

        fitpolygauss2 = a(1) * f

        return
      end


      real function fitpolygauss(x, nx, a, na)
        implicit none
        real pi
        parameter (pi = 3.1415927)
        integer nx, na, ig, ng, iw, im, is
        real x(nx), a(na), f, w, wlast

        w = 0.0
        do  iw = 2, na-3, 3
          w = w + a(iw)
        end do
        wlast = 1.0 - w

        ng = na / 3
        f = 0.0
        iw = 2
        im = 3
        is = 4
        do  ig = 1, ng-1
          f = f + a(iw) / (sqrt(2.0*pi)*a(is)) * 
     -            exp(-0.5*((x(1)-a(im))/a(is))**2)
          iw = iw + 3
          im = im + 3
          is = is + 3
        end do
        im = im - 1
        is = is - 1
        f = f + wlast / (sqrt(2.0*pi)*a(is)) * 
     -          exp(-0.5*((x(1)-a(im))/a(is))**2)

        fitpolygauss = a(1) * f

        return
      end


      real function fitlognormal(x, nx, a, na)
        implicit none
        integer nx, na
        real x(nx), a(na), fitnormal
        fitlognormal = log(fitnormal(x,nx,a,na))
        return
      end

c --- average the values in neighbouring elements of y-columns
      subroutine avrtab(isegm,x,npnts,maxpnt,ncolmn,nwxy,iwxy,ndim)
      implicit none
#include "io.cst"
      integer isegm,ncolmn, ndim,i,npnts,maxpnt
      integer nwxy(2), iwxy(ndim),icol
      real x(maxpnt,ncolmn)

      do  i = 1, nwxy(2)
        icol = iwxy(nwxy(1)+i)
        if ((icol.lt.1).or.(icol.gt.ncolmn)) then
          write(iolog,'(a)') 
     -    'avrtab__E> column index out of range (XY_SCOLUMNS)'
          stop
        end if
        call smth(x(1,icol),npnts,isegm)
      end do

      return
      end


      subroutine  smth(x,npnts,isegm)
      implicit none
#include "io.cst"
      integer maxn
      parameter (maxn=10000)
      integer isegm, i, npnts
      real x(npnts), xs(maxn), average
      if (npnts.gt.maxn) then
        write(iolog,'(a)') 'smth___E> increase maxn'
        stop
      end if
      do i = 1, npnts
        xs(i) = average(x,i,isegm,npnts)
      end do
      do i = 1, npnts
        x(i) = xs(i)
      end do
      return
      end


      real function average(x,i,isegm,n)
      implicit none
#include "io.cst"
      integer n, isegm, i1, i2, i, j
      real x(n),av,w,wi

      i1 = max(1,i-isegm)
      i2 = min(n,i+isegm)
      av = 0.0
      w = 0.0
      do  j = i1, i2
        wi = 0.1*(isegm - abs(j-i) + 1)
        av = av + wi*x(j)
        w = w + wi
      end do
      if (wi .lt. 1.0E-6) then
        write(iolog,'(a)') 'average_E> HALF_WINDOW too small'
        stop
      end if
      average = av / w

      return
      end



      subroutine trstab(x,npnts,maxpnt,ncolmn,ncol,icols,ndim,trans)
        implicit none
#include "io.cst"
        integer npnts,maxpnt,ncolmn,ncol,ndim,ic,i,j,icols(ndim)
        real trans,x(maxpnt,ncolmn)

        do  i = 1, ncol
          ic = icols(i)
          if ((ic.lt.1).or.(ic.gt.ncolmn)) then
            write(iolog,'(a)') 
     -      'trstab__E> column index out of range (XY_SCOLUMNS)'
            stop
          end if
          do  j = 1, npnts
            x(j,ic) = x(j,ic) + trans
          end do
        end do

        return
      end



      subroutine initbb
        implicit none
#include "asgl.cmn"
        bbox0 =  9E9
        bboy0 =  9E9
        bbox1 = -9E9
        bboy1 = -9E9
        return
      end


      real function rminarr(arr,maxarr,nxdim,nydim)
        implicit none
        integer nxdim,nydim,maxarr,i,j
        real arr(maxarr,nydim),zmin

        zmin = 9.9E20
        do  j = 1, nydim
          do  i = 1, nxdim
            if(arr(i,j) .lt. zmin) zmin = arr(i,j)
          end do
        end do
        rminarr = zmin
 
        return
      end



      real function rmaxarr(arr,maxarr,nxdim,nydim)
        implicit none
        integer nxdim,nydim,maxarr,i,j
        real arr(maxarr,nydim),zmax

        zmax = -9.9E20
        do  j = 1, nydim
          do  i = 1, nxdim
            if(arr(i,j) .gt. zmax) zmax = arr(i,j)
          end do
        end do
        rmaxarr = zmax
 
        return
      end



      subroutine stamp
#ifdef WIN
        use ifport
#endif
        implicit none
#include "top_appl.cmn"
        integer lenr
        real dx, dy
        character mark*255, page*5, fn*255
        character pwd*128, val*(20)
#ifdef WIN
        integer(4) istat
#endif

        val = sprm(1,59)
        call upper(val)

        if (val.ne.'NONE') then
          if (epsf) then
            write(iolog,'(a)')
     &            'stamp___W> not sensible in encapsulated PS'
            write(iolog,'(a)') '           PAGE_STAMP reset to OFF'
            sprm(1, 59) = 'NONE'
            return
          end if

c ------- mark the lower right corner of the page with ASGL version, 
c         date, time, and the TOP file name

          if (val.eq.'DEFAULT') then

            write(page,'(i5)') ipage
            call ljust(page)

c --------- get the current directory:
#ifdef WIN
            istat = getcwd(pwd)
            if (istat /= 0) pwd = ''
#else
            call sys('pwd', pwd)
#endif

c --------- get the filename:
            fn = commfile

            mark = version // '; ' // datetime(1:lenr(datetime)) // 
     -           '; File: ' // pwd(1:lenr(pwd)) // '/' //
     -           fn(1:lenr(fn)) // ', Pg: ' // page
            call subs2(mark, '/./', '/')

          else

            mark = sprm(1,59)

          end if

c ------- assume that the current PS is the original PS device CS system
          dx = (bbx1-bbx0)*0.067
          dy = (bby1-bby0)*0.055
          call gsave
          write(iops, '(a,f8.2,a)') '/Helvetica findfont ',rprm(1,79), 
     &         ' scalefont setfont'
          call moveto(bbx1-dx, bby0+dy)
          write(iops, '(a)')'(', mark(1:lenr(mark)), ') dup' 
          write(iops, '(a)')'stringwidth neg exch neg exch rmoveto show'
          call grestore

        end if

        return
      end


c c --- Example for the logical constraints function that returns .false. when
c c     the parameters assume a forbidden set of values and true when they
c c     are acceptable.
c       logical function constr(x,nx,nxmax,ndata,a,ma)
c         integer i
c         real x(nxmax,ndata),a(ma) 
c         real tmp1, tmp2, tmp2sq
c 
c         do  i = 1, ndata
c           tmp1 = 4.0*a(3)*x(1,i)
c           tmp2 = a(3)+x(1,i)+a(4)
c           tmp2sq = tmp2*tmp2
c           if (tmp2sq .le. tmp1) then
c c            write(iolog,'(a,2g15.5)')
c c     &      'fitdisf__E> tmp2sq .le. tmp1: ',tmp2sq,tmp1
c             constr = .false.
c             return
c           end if
c         end do
c 
c         constr = .true.
c 
c         return
c       end




      subroutine pntlab(x,y,n,labls,iloc,lblfnt)
        implicit none 
        integer n, i, iloc, ihstyl, ivstyl, lblfnt
        real x(n), y(n), xtxt, ytxt, wpx, wpy
        character labls(n)*(*)

        do  i = 1, n

          go to (1,2) iloc

c --------- label centered in the point center
1           continue
            xtxt = wpx(x(i))
            ytxt = wpy(y(i))
            ihstyl = 2
            ivstyl = 2
            go to 10

c --------- label leftjustified next to the point on its right side
2           continue
            xtxt = wpx(x(i)) + 0.014
            ytxt = wpy(y(i))
            ihstyl = 1
            ivstyl = 2
            go to 10

10        continue

          call ps_text(labls(i),xtxt,ytxt,lblfnt,0.0,ihstyl,ivstyl)
        end do

        return
      end 



c --- copy the density/table array to XY array
      subroutine tab2xy
        implicit none
#include "top_appl.cmn"
        integer i, j, ix, iy, npnts, iz

        ix = iprm(1,37)
        iy = iprm(2,37)
        iz = iprm(1,40)

        npnts = 0
        do  i = 1, nxdim
          do  j = 1, nydim
            npnts = npnts + 1
            if (npnts .gt. maxpnt) then
              write(iolog,'(a,i6)') 'tab2xy___E> increase MAXPNT: ',
     &                              npnts
              stop
            end if
            tab(npnts,ix) = i
            tab(npnts,iy) = j
            tab(npnts,iz) = arr(i,j)
          end do
        end do
        iprm(1,61)  = npnts
        ncolmn      = max(ncolmn, ix, iy, iz)
        istrcol(ix) = .false.
        istrcol(iy) = .false.
        istrcol(iz) = .false.
        nstrs       = 0

        inp = 1

        return
      end



c --- copy the XY array to density/table array
      subroutine xy2tab
        implicit none
#include "top_appl.cmn"
        integer i, j, ix, iy, iz

        ix = nint(tab(i,iprm(1,37)))
        iy = nint(tab(i,iprm(2,37)))

        do  i = 1, maxxarr
          do  j = 1, maxxarr
            arr(i,j) = 0.0
          end do
        end do

c ----- X,Y = XY_COLUMNS; Z = ERROR_COLUMN
        nxdim = 0
        nydim = 0
        iz = iprm(1,40)
        do  i = 1, iprm(1,61)

          ix = nint(tab(i,iprm(1,37)))
          iy = nint(tab(i,iprm(2,37)))

          if (ix.lt.1 .or. ix.gt.maxxarr) then
            write(iolog,'(a,i6)') 'xy2tab___E> IX out of bounds: ', ix
            stop
          end if
          if (iy.lt.1 .or. iy.gt.maxyarr) then
            write(iolog,'(a,i6)') 'xy2tab___E> IY out of bounds: ', iy
            stop
          end if
          nxdim = max(nxdim, ix)
          nydim = max(nydim, iy)
   
          arr(ix,iy) = tab(i,iz)
        end do

        inp = 2

        return
      end
