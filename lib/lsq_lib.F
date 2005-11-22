c --- General purpose non-linear least squares routine;
c --- Methods: Marquardt optimization from Numerical recipes;

c --- Input: x(i,j)    ... independent variables i, for each data point j
c            nx        ... number of independent variables
c            nxmax     ... maximal number of independent variables
c            y(i)      ... dependent variable at each data point j
c            sig(i)    ... stand.dev. of measurement of y(i) 
c            ndata     ... number of data points
c            ainit(i)  ... initial estimate of parameter i; nprmall of them
c            nprmfit   ... number of parameters to be fitted
c            nprmall   ... number of all parameters
c            nprmmax   ... maximal number of parameters
c            lista(i)  ... indices of the nprmfit parameters to be fitted 
c                          (nprmfit <= nprmall <= nprmmax)
c            funcs     ... the model function; func(x,nx,a,na)
c            abscut    ... absolute decrease in chi^2 convergence criteria
c            relcut    ... relative decrease in chi^2 convergence criteria
c            maxcyc    ... maximal number of cycles
c            mstdev    ... .F. if no sig supplied, .T. if sig supplied
c            alammax   ... maximal allowed lambda (usually 10^20)
c
c --- Output: a(i)     ... final values for nprmall parameters
c             covar(i,j) . co-variance matrix for all nprmall*nprmall 
c                          parameter pairs (0 if not optimized)
c             alpha(i,j) . curvature matrix for parameter pairs
c             chisq    ... sum of [(y-y') / s^2]^2 --- optimized for;
c             niter    ... number of calls of the Marquardt routine 
c             alamda   ... final value of the LAMBDA parameter
c             q        ... significance of the goodnes of fit
c             errmult  ... multiplier for covariance matrix when no sig given
c             degfreed ... degrees of freedom (NDATA - NPRMFIT).

c             maxcyc, abcut, relcut, sig, alammax ... 
c                                         these are reset if not supplied


c --- abscut and relcut can be 0.0 or less - they are reset to sensible defaults

c --- sig(i) also reset to 1.0 if mstdev.F.

c --- if alamda large, required convergence not achieved - but results may
c     still be good (for example, when the last step is large and chi^2
c     larger than abscut in the last two steps);

      subroutine lsqfit(x, nx, nxmax, y, sig, ndata, ainit, a, nprmfit, 
     -           nprmall, nprmmax, lista, covar, alpha, chisq, funcs, 
     -           abscut, relcut, mstdev, niter, alamda, q, errmult, 
     -           degfreed, alammax, maxcyc)

#include "io.cst"
c ----- to prevent division by 0.0:
        parameter (eps = 1.0e-18, nprmmax2 =100)

        integer ndata, nprmfit, nprmall, lista(nprmall), niter
        integer maxcyc, ia(nprmmax2)
        real x(nxmax, ndata), y(ndata), sig(ndata), a(nprmall)
        real covar(nprmmax,nprmmax), alpha(nprmmax,nprmmax)
        real chisq, abscut, relcut, ainit(nprmall)
        logical mstdev
        real funcs
        external funcs

        if (nprmall .gt. nprmmax2) stop 'lsqfit__E> increase NPRMMAX2'

c ----- some default values:

c ----- if the convergence criteria less or equal 0.0 they are properly reset:
        if (abscut.le.0.0) abscut = 0.1
        if (relcut.le.0.0) relcut = 0.001
        if (alammax.le.0.0) alammax = 1.0E20
        if (maxcyc .le. 0) maxcyc = 9999

c ----- if no standard deviations are supplied they are set to 1.0:
        if (.not.mstdev) then
          do  i = 1, ndata
            sig(i) = 1.0
          end do
        end if

c ----- if no list of parameters to be fitted is supplied, all are fitted:
        if (nprmfit.le.0) then
          nprmfit = nprmall
          do  i = 1, nprmall
            lista(i) = i
          end do
        end if
        do  i = 1, nprmall
          ia(i) = 0
        end do
        do  i = 1, nprmfit
          if (lista(i) .gt. 0) ia(lista(i)) = 1
        end do

c ----- copy the initial estimates to the working array:
        do  i = 1, nprmall
          a(i) = ainit(i)
        end do

c ----- any parameters to fit?
        do  i = 1, nprmall
          if (ia(i) .eq. 1) go to 3 
        end do
        return
3       continue

c ----- initialize the Marquardt routine with negative ALAMDA:
        alamda = -1.0
        call mrqmin(x,nx,nxmax,y,sig,ndata,a,ia,nprmall,
     -              covar,alpha,nprmmax,chisq,funcs,alamda)

c ----- iterate until satisfied
        niter = 0
        iabs  = 0
        irel  = 0
10      continue

          niter = niter + 1
          write(iolog,11) niter, chisq, (a(i),i=1,min(nprmall,6))
11        format('chi^2,P: ', i3, 99g13.4)

          chi2 = chisq
          call mrqmin(x,nx,nxmax,y,sig,ndata,a,ia,nprmall,
     -                covar,alpha,nprmmax,chisq,funcs,alamda)

c ------- Convergence criteria:
c
c         Normal: Twice in a row, either an absolute or a relative
c                 decrease in chi^2 has to be smaller than the cutoffs,
c                 but it has to be for better;

c ------- Abnormal: you stop when ALAMDA > 1.0E20, indicating a possible
c                   problem with convergence at specified precision;
c                   however, this will also occur when chi^2 jumps in a
c                   single cycle into a minimum from a very high position 
c                   so that convergence counters were not yet activated and 
c                   no improvement in chi^2 is possible since it is 
c                   already optimal (this will typically occur with good
c                   initial estimates).
c                   you also stop if niter .ge. maxcyc

          dchi2a = chisq - chi2
          dchi2r = dchi2a / max(chisq,eps)
          if ((abs(dchi2a) .lt. abscut) .and. (dchi2a .lt. 0.0)) then
            iabs = iabs + 1
          else
            iabs = 0
          end if
          if ((abs(dchi2r) .lt. relcut) .and. (dchi2r .lt. 0.0)) then
            irel = irel + 1
          else
            irel = 0
          end if
       
c          write(iolog,*) 'Iabs, irel: ', iabs, irel
c          write(iolog,*) 'dabs, drel, alamda: ', dchi2a, dchi2r, alamda
          if (((iabs.lt.2).and.(irel.lt.2).and.(alamda.lt.alammax)).and.
     -        (niter.lt.maxcyc)) go to 10

        continue

c ----- one last call to get curvatures and covariances
        alamd0 = 0.0
        call mrqmin(x,nx,nxmax,y,sig,ndata,a,ia,nprmall,
     -              covar,alpha,nprmmax,chisq,funcs,alamd0)

c ----- degrees of freedom:
        degfreed = ndata - nprmfit

        if (.not. mstdev) then
c ------- no standard deviations were supplied; increase the standard
c         errors in parameter estimates:

c ------- in other words: assuming a good fit, rescale the covariance matrix
          errmult = chisq / (ndata-nprmfit)
          do 30  i = 1, nprmall
            do 20  j = 1, nprmall
              covar(i,j) = errmult * covar(i,j)
20          continue
30        continue

c ------- since a good fit was assumed, no goodnes-of-fit criteria available:
          q = 1.0
        else
c ------- stnd deviations for data points were supplied:

c ------- no co-variance matrix rescaling here:
          errmult = 1.0

c ------- goodnes of fit criteria:
          q = gammq(0.5*degfreed, 0.5*chisq)
        end if

        return
      end



      SUBROUTINE mrqmin(x,nx,nxmax,y,sig,ndata,a,ia,ma,covar,alpha,
     *nca,chisq,funcs,alamda)
      INTEGER ma,nca,ndata,ia(ma),MMAX
      REAL alamda,chisq,funcs,a(ma),alpha(nca,nca),covar(nca,nca),
     *sig(ndata),x(nxmax,ndata),y(ndata)
      PARAMETER (MMAX=100)
CU    USES covsrt,gaussj,mrqcof
      INTEGER j,k,l,m,mfit
      REAL ochisq,atry(MMAX),beta(MMAX),da(MMAX)
      EXTERNAL funcs
      SAVE ochisq,atry,beta,da,mfit
      if(alamda.lt.0.)then
        mfit=0
        do 11 j=1,ma
          if (ia(j).ne.0) mfit=mfit+1
11      continue
        alamda=0.001
        call mrqcof(x,nx,nxmax,y,sig,ndata,a,ia,ma,alpha,beta,nca,
     *              chisq,funcs)
        ochisq=chisq
        do 12 j=1,ma
          atry(j)=a(j)
12      continue
      endif
      j=0
      do 14 l=1,ma
        if(ia(l).ne.0) then
          j=j+1
          k=0
          do 13 m=1,ma
            if(ia(m).ne.0) then
              k=k+1
              covar(j,k)=alpha(j,k)
            endif
13        continue
          covar(j,j)=alpha(j,j)*(1.+alamda)
          da(j)=beta(j)
        endif
14    continue
      call gaussj(covar,mfit,nca,da,1,1)
      if(alamda.eq.0.)then
        call covsrt(covar,nca,ma,ia,mfit)
        return
      endif
      j=0
      do 15 l=1,ma
        if(ia(l).ne.0) then
          j=j+1
          atry(l)=a(l)+da(j)
        endif
15    continue
      call mrqcof(x,nx,nxmax,y,sig,ndata,atry,ia,ma,covar,da,nca,
     *            chisq,funcs)
      if(chisq.lt.ochisq)then
        alamda=0.1*alamda
        ochisq=chisq
        j=0
        do 17 l=1,ma
          if(ia(l).ne.0) then
            j=j+1
            k=0
            do 16 m=1,ma
              if(ia(m).ne.0) then
                k=k+1
                alpha(j,k)=covar(j,k)
              endif
16          continue
            beta(j)=da(j)
            a(l)=atry(l)
          endif
17      continue
      else
        alamda=10.*alamda
        chisq=ochisq
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software =$j*m,).



      SUBROUTINE mrqcof(x,nx,nxmax,y,sig,ndata,a,ia,ma,alpha,beta,
     *                  nalp,chisq,funcs)
      INTEGER ma,nalp,ndata,ia(ma),MMAX
      REAL chisq,a(ma),alpha(nalp,nalp),beta(ma),sig(ndata),
     *x(nxmax,ndata),y(ndata)
      EXTERNAL funcs
      PARAMETER (MMAX=100)
      INTEGER mfit,i,j,k,l,m
      REAL dy,sig2i,wt,ymod,dyda(MMAX)
      mfit=0
      do 11 j=1,ma
        if (ia(j).ne.0) mfit=mfit+1
11    continue
      do 13 j=1,mfit
        do 12 k=1,j
          alpha(j,k)=0.
12      continue
        beta(j)=0.
13    continue
      chisq=0.
      do 16 i=1,ndata
        call modder(x(1,i),nx,a,ma,ymod,dyda,funcs,ia)
c        call funcs(x(i),a,ymod,dyda,ma)
        sig2i=1./(sig(i)*sig(i))
        dy=y(i)-ymod
        j=0
        do 15 l=1,ma
          if(ia(l).ne.0) then
            j=j+1
            wt=dyda(l)*sig2i
            k=0
            do 14 m=1,l
              if(ia(m).ne.0) then
                k=k+1
                alpha(j,k)=alpha(j,k)+wt*dyda(m)
              endif
14          continue
            beta(j)=beta(j)+dy*wt
          endif
15      continue
        chisq=chisq+dy*dy*sig2i
16    continue
      do 18 j=2,mfit
        do 17 k=1,j-1
          alpha(k,j)=alpha(j,k)
17      continue
18    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software =$j*m,).



      SUBROUTINE covsrt(covar,npc,ma,ia,mfit)
      INTEGER ma,mfit,npc,ia(ma)
      REAL covar(npc,npc)
      INTEGER i,j,k
      REAL swap
      do 12 i=mfit+1,ma
        do 11 j=1,i
          covar(i,j)=0.
          covar(j,i)=0.
11      continue
12    continue
      k=mfit
      do 15 j=ma,1,-1
        if(ia(j).ne.0)then
          do 13 i=1,ma
            swap=covar(i,k)
            covar(i,k)=covar(i,j)
            covar(i,j)=swap
13        continue
          do 14 i=1,ma
            swap=covar(k,i)
            covar(k,i)=covar(j,i)
            covar(j,i)=swap
14        continue
          k=k-1
        endif
15    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software =$j*m,).



      SUBROUTINE gaussj(a,n,np,b,m,mp)
      INTEGER m,mp,n,np,NMAX
      REAL a(np,np),b(np,mp)
      PARAMETER (NMAX=100)
      INTEGER i,icol,irow,j,k,l,ll,indxc(NMAX),indxr(NMAX),ipiv(NMAX)
      REAL big,dum,pivinv
      do 11 j=1,n
        ipiv(j)=0
11    continue
      do 22 i=1,n
        big=0.
        do 13 j=1,n
          if(ipiv(j).ne.1)then
            do 12 k=1,n
              if (ipiv(k).eq.0) then
                if (abs(a(j,k)).ge.big)then
                  big=abs(a(j,k))
                  irow=j
                  icol=k
                endif
              else if (ipiv(k).gt.1) then
                pause 'singular matrix in gaussj'
              endif
12          continue
          endif
13      continue
        ipiv(icol)=ipiv(icol)+1
        if (irow.ne.icol) then
          do 14 l=1,n
            dum=a(irow,l)
            a(irow,l)=a(icol,l)
            a(icol,l)=dum
14        continue
          do 15 l=1,m
            dum=b(irow,l)
            b(irow,l)=b(icol,l)
            b(icol,l)=dum
15        continue
        endif
        indxr(i)=irow
        indxc(i)=icol
        if (a(icol,icol).eq.0.) pause 'singular matrix in gaussj'
        pivinv=1./a(icol,icol)
        a(icol,icol)=1.
        do 16 l=1,n
          a(icol,l)=a(icol,l)*pivinv
16      continue
        do 17 l=1,m
          b(icol,l)=b(icol,l)*pivinv
17      continue
        do 21 ll=1,n
          if(ll.ne.icol)then
            dum=a(ll,icol)
            a(ll,icol)=0.
            do 18 l=1,n
              a(ll,l)=a(ll,l)-a(icol,l)*dum
18          continue
            do 19 l=1,m
              b(ll,l)=b(ll,l)-b(icol,l)*dum
19          continue
          endif
21      continue
22    continue
      do 24 l=n,1,-1
        if(indxr(l).ne.indxc(l))then
          do 23 k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
23        continue
        endif
24    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software =$j*m,).


      subroutine dfuncm(x, nx, a, na, dyda, func, ia)
      integer na, ia(na)
      real x(nx), a(na), dyda(na)
      real dfunc, func
      external dfunc, func

      do  i = 1, na
        if (ia(i) .ne. 0) dyda(i) = dfunc(x, nx, a, na, i, func)
      end do

c      do  i = 1, na
c        dyda(i) = dfunc(x, nx, a, na, i, func)
c      end do

      return
      end


c --- derivative of FUNC with respect to  A(I)
      real function dfunc(x, nx, a, na, i, func)
      parameter (rnear0 = 1.0e-5, toler=1.0E-4)
      real x(nx), a(na)
      external func

      if (abs(a(i)) .lt. rnear0) then
        da = sqrt(toler)
      else
        da = abs(a(i)*sqrt(toler))
      end if

      da2  = da + da
      a(i) = a(i) - da
c --- get value of func at point x, x vector of length nx;
c     parameters vector a, length na;
      yl   = func(x,nx,a,na)
      a(i) = a(i) + da2
      yr   = func(x,nx,a,na)
      a(i) = a(i) - da
      dfunc = (yr-yl)/da2

      return
      end



      subroutine wrres(comment1,comment2,npnts,nprmall,nprmfit,lista,
     -           ainit,a,covar,nprmmax,niter,alamda,chisq,degfreed,
     -           q,errmult,alammax,nx,mstdev)
        implicit none
#include "io.cst"
        real eps
        integer npnts,nx,i,j,nprmall,nprmfit,lista(nprmfit),nprmmax
        integer niter,lenr
        real ainit(nprmall),a(nprmall),covar(nprmmax,nprmmax)
        real alamda,chisq,rms,degfreed,q,errmult,alammax
        character comment1*(*), comment2*(*)
        logical mstdev
        parameter (eps = 1.0E-5)

        character stat*3
        logical allvar

        write(iolog, 10) 
     -  comment1(1:max(1,lenr(comment1))), 
     -  comment2(1:max(1,lenr(comment2))),
     -  npnts, mstdev, nx
10      format(///
     -        'Summary of non-linear least-squares fitting:'//
     -        'Comment                          : ', a //
     -        'Comment                          : ', a //
     -        'Number of data points            : ', i7 /
     -        'Standard deviations user supplied: ', l7 /
     -        'Number of independent variables  : ', i7 //)

        write(iolog, 20)
20      format('From the following output, extract this information:'//
     - '      1) Parameter estimates;' /
     - '      2) Errors in parameters;' /
     - '      3) Goodnes-of-fit.' //
     - 'Note: If the fit to the model is bad (3), both 1 and 2 are'/
     - '      meaningless. The fit is bad if chi^2 significance'/
     - '      is less than 0.001.' //
     - '      If the errors in the data are not normal, parameter'/
     - '      error estimates are not meaningfull; parameter estimates'/
     - '      are no longer maximum-likelihood estimates but may'/
     - '      still be useful.'//
     - '      If you do not supply stnd.deviations for the data'/
     - '      points the errors in parameters are tentative and'/
     - '      the goodnes-of-fit figure is meaningless.'//
     - '      Try several different starting positions. The function'/
     - '      minimized is chi^2, so the best solution is the one with'/
     - '      the minimal chi^2.'////)

        write(iolog, 30) '#', 'INIT', 'FINAL', 'STD.ERR.', 'FIXED'
30      format(a3,3a15,a9/
     -    '--------------------------------------------------------')
        allvar = .true.
        do 50  i = 1, nprmall
          call varied(lista, nprmfit, i, stat)
          if (stat .eq. 'YES') allvar = .false.
          write(iolog, 40) i, ainit(i), a(i), sqrt(covar(i,i)), stat
40        format(i3, 3g15.5, 6x, a3)
50      continue

        rms = sqrt(chisq / (degfreed-1.0))
        write(iolog, 60) niter, alamda, chisq, rms, degfreed, q, errmult
60      format(//'Number of calls of Marquardt routine   : ', i15   //
     -           'Final value of LAMBDA convergence param: ', e15.5 //
     -           'Chi^2 = SUMi {[y(i)-y(i)''] / s(i)}^2   : ',e15.5 //
     -           'RMS = sqrt(Chi^2 / (deg.freed.-1))     : ', e15.5 //
     -           'Degrees of freedom                     : ', e15.5 //
     -           'Significance of goodnes-of-fit (Q)     : ', e15.5 //
     -           'Multiplier for the co-variance matrix  : ', e15.5 ///)

        if (.not. mstdev) then
          write(iolog, 62)
62        format('Note: Goodness-of-fit estimates not meaningful.' /
     -           '      Error estimates tentative.'/)
        else
          write(iolog, 64)
64        format(
     - 'Note: Multiplier for the co-variance matrix not meaningful.'/)
        end if

c ----- see lsq_lib for more comments on convergence
        write(iolog, 63)
63      format('Note on convergence from the final value of lambda:')
        if (alamda .ge. alammax) then
          write(iolog, 65) 
65        format('      A potential problem with convergence or '/
     -           '      very good initial estimates or ' /
     -           '      a very deep chi^2 minimum.'/)
        else
          write(iolog, 66) 
66        format('      No convergence problem.'/)
        endif

        write(iolog, 70) (i,i=1,nprmall)
70      format(// 'Co-variance matrix:' // 2x, 999i8)
        do 90  i = 1, nprmall
          write(iolog, 80) i, (covar(i,j),j=1,nprmall)
80        format(i2, 999f8.4)
90      continue

        if (allvar) then
        write(iolog, 100) (i,i=1,nprmall)
100     format(// 'Cross-correlation matrix: ' // 2x, 999i8)
        do 120  i = 1, nprmall
          write(iolog, 110) i, 
     -    (covar(i,j)/max(eps,sqrt(covar(i,i)*covar(j,j))),j=1,nprmall)
110       format(i2, 999f8.4)
120     continue
        else
        write(iolog, 130)
130     format(// 'Cross-correlation matrix not written out since some'/
     -            'parameters were not varied.'//)
        end if
        write(iolog, '(///)')

        close(11)

        return
      end

      subroutine varied(lista, nprmfit, iprm, stat)
        integer lista(nprmfit)
        character stat*3
        do 10  i = 1, nprmfit
          if(lista(i).eq.iprm) then
            stat = 'NO'
            return
          end if
10      continue
        stat = 'YES'
        return
      end



c --- Example for the logical constraints function that returns .false. when
c     the parameters assume a forbidden set of values and true when they
c     are acceptable.
c      logical function constr(x,nx,nxmax,ndata,a,ma)
c        real x(nxmax,ndata),a(ma) 
c        constr = .true.
c        return
c      end

c --- returns the model's value as well as the derivatives:
      subroutine modder(x, nx, a, na, y, dyda, func, ia)
        integer ia(na)
        real x(nx), y, a(na), dyda(na)
        external func

c ----- value of the function, supplied by the user:
        y = func(x, nx, a, na)

c ----- Numerical or analytical derivatives:

c ----- for numerical derivatives of Y with respect to A use this routine:
        call dfuncm(x, nx, a, na, dyda, func, ia)

c ----- analytical derivatives, specified here by the USER, optional:
c       (NA derivatives of Y with respect to A have to be specified)
c       dyda(2) = x
c       dyda(1) = 1.0

        return
      end
