c --- Numerical Recipes routines:

      real function gammq(a,x)
      implicit none
      real a, x, gamser,gammcf,gln

      if(x.lt.0..or.a.le.0.)stop 'wrong argument'
      if(x.lt.a+1.)then
        call gser(gamser,a,x,gln)
        gammq=1.-gamser
      else
        call gcf(gammcf,a,x,gln)
        gammq=gammcf
      endif

      return
      end


      subroutine gser(gamser,a,x,gln)
      implicit none
#include "io.cst"
      integer itmax
      real eps
      parameter (itmax=100,eps=3.e-7)
      integer n
      real a,x,gln,gamser,del,sum,ap,gammln
      external gammln

      gln=gammln(a)
      if(x.le.0.)then
        if(x.lt.0.) then 
          write(iolog,'(a)') 'gser___E> x < 0'
          stop
        end if
        gamser=0.
        return
      endif
      ap=a
      sum=1./a
      del=sum
      do 11 n=1,itmax
        ap=ap+1.
        del=del*x/ap
        sum=sum+del
        if(abs(del).lt.abs(sum)*eps)go to 1
11    continue
      write(iolog,*) 'Warning[gser]; A too large, ITMAX too small'
1     gamser=sum*exp(-x+a*log(x)-gln)
      return
      end



      subroutine gcf(gammcf,a,x,gln)
      implicit none
#include "io.cst"
      integer itmax
      real eps
      parameter (itmax=100,eps=3.e-7)
      integer n
      real gammln,gln,gold,a1,a0,b0,b1,fac,an,ana,a
      real gammcf,anf,x,g
      external gammln

      gln=gammln(a)
      gold=0.
      a0=1.
      a1=x
      b0=0.
      b1=1.
      fac=1.
      do 11 n=1,itmax
        an=n
        ana=an-a
        a0=(a1+a0*ana)*fac
        b0=(b1+b0*ana)*fac
        anf=an*fac
        a1=x*a0+anf*a1
        b1=x*b0+anf*b1
        if(a1.ne.0.)then
          fac=1./a1
          g=b1*fac
          if(abs((g-gold)/g).lt.eps)go to 1
          gold=g
        endif
11    continue
      write(iolog,*) 'Warning[gcf]; A too large, ITMAX too small'
1     gammcf=exp(-x+a*log(x)-gln)*g

      return
      end



      real function gammln(xx)
      implicit none
      integer j
c --- let's leave this real*8 in the code:
      real*8 cof(6),stp,half,one,fpf,x,tmp,ser
      real xx
      data cof,stp/76.18009173d0,-86.50532033d0,24.01409822d0,
     *    -1.231739516d0,.120858003d-2,-.536382d-5,2.50662827465d0/
      data half,one,fpf/0.5d0,1.0d0,5.5d0/

      x=xx-one
      tmp=x+fpf
      tmp=(x+half)*log(tmp)-tmp
      ser=one
      do 11 j=1,6
        x=x+one
        ser=ser+cof(j)/x
11    continue
      gammln=tmp+log(stp*ser)

      return
      end




      subroutine linfit(x,y,ndata,sig,mwt,a,b,siga,sigb,chi2,q)
      implicit none
      integer ndata
CU    USES gammq
      integer i
      real sigdat,ss,st2,sx,sxoss,sy,t,wt,gammq
      real a,b,chi2,q,siga,sigb,sig(ndata),x(ndata),y(ndata)
      logical mwt
      external gammq

      sx=0.0
      sy=0.0
      st2=0.0
      b=0.0
      if (mwt) then
        ss=0.0
        do 11 i=1,ndata
          wt=1.0/(sig(i)**2)
          ss=ss+wt
          sx=sx+x(i)*wt
          sy=sy+y(i)*wt
11      continue
      else
        do 12 i=1,ndata
          sx=sx+x(i)
          sy=sy+y(i)
12      continue
        ss=ndata
      endif
      sxoss=sx/ss
      if(mwt) then
        do 13 i=1,ndata
          t=(x(i)-sxoss)/sig(i)
          st2=st2+t*t
          b=b+t*y(i)/sig(i)
13      continue
      else
        do 14 i=1,ndata
          t=x(i)-sxoss
          st2=st2+t*t
          b=b+t*y(i)
14      continue
      endif
      b=b/st2
      a=(sy-sx*b)/ss
      siga=sqrt((1.0+sx*sx/(ss*st2))/ss)
      sigb=sqrt(1.0/st2)
      chi2=0.0
      if(mwt) then
        do 15 i=1,ndata
          chi2=chi2+(y(i)-a-b*x(i))**2
15      continue
        q=1.0
        sigdat=sqrt(chi2/(ndata-2))
        siga=siga*sigdat
        sigb=sigb*sigdat
      else
        do 16 i=1,ndata
          chi2=chi2+((y(i)-a-b*x(i))/sig(i))**2
16      continue
        q=gammq(0.50*(ndata-2),0.50*chi2)
      endif
      return
      end
C  (C) Copr. 1986-92 Numerical Recipes Software =$j*m,).



      subroutine pearsn(x,y,n,r,prob,z)
      integer n
      real prob,r,z,x(n),y(n),TINY
      parameter (TINY=1.e-20)
CU    USES betai
      integer j
      real ax,ay,df,sxx,sxy,syy,t,xt,yt,betai
      ax=0.
      ay=0.
      do 11 j=1,n
        ax=ax+x(j)
        ay=ay+y(j)
11    continue
      ax=ax/n
      ay=ay/n
      sxx=0.
      syy=0.
      sxy=0.
      do 12 j=1,n
        xt=x(j)-ax
        yt=y(j)-ay
        sxx=sxx+xt**2
        syy=syy+yt**2
        sxy=sxy+xt*yt
12    continue
      r=sxy/sqrt(sxx*syy)
      if (r .lt. -1.0) r = -1.0
      z=0.5*log(((1.+r)+TINY)/((1.-r)+TINY))
      df=n-2
      t=r*sqrt(df/(((1.-r)+TINY)*((1.+r)+TINY)))
      prob=betai(0.5*df,0.5,df/(df+t**2))
C     prob=erfcc(abs(z*sqrt(n-1.))/1.4142136)
      return
      end


      function betai(a,b,x)
      real betai,a,b,x
CU    USES betacf,gammln
      real bt,betacf,gammln
      if(x.lt.0..or.x.gt.1.)pause 'bad argument x in betai'
      if(x.eq.0..or.x.eq.1.)then
        bt=0.
      else
        bt=exp(gammln(a+b)-gammln(a)-gammln(b)+a*log(x)+b*log(1.-x))
      endif
      if(x.lt.(a+1.)/(a+b+2.))then
        betai=bt*betacf(a,b,x)/a
        return
      else
        betai=1.-bt*betacf(b,a,1.-x)/b
        return
      endif
      end

      function betacf(a,b,x)
      integer maxit
      REAL betacf,a,b,x,EPS,FPMIN
      PARAMETER (maxit=200,EPS=3.e-7,FPMIN=1.e-30)
      INTEGER m,m2
      REAL aa,c,d,del,h,qab,qam,qap
      qab=a+b
      qap=a+1.
      qam=a-1.
      c=1.
      d=1.-qab*x/qap
      if(abs(d).lt.FPMIN)d=FPMIN
      d=1./d
      h=d
      do 11 m=1,MAXIT
        m2=2*m
        aa=m*(b-m)*x/((qam+m2)*(a+m2))
        d=1.+aa*d
        if(abs(d).lt.FPMIN)d=FPMIN
        c=1.+aa/c
        if(abs(c).lt.FPMIN)c=FPMIN
        d=1./d
        h=h*d*c
        aa=-(a+m)*(qab+m)*x/((a+m2)*(qap+m2))
        d=1.+aa*d
        if(abs(d).lt.FPMIN)d=FPMIN
        c=1.+aa/c
        if(abs(c).lt.FPMIN)c=FPMIN
        d=1./d
        del=d*c
        h=h*del
        if(abs(del-1.).lt.EPS)goto 1
11    continue
c      pause 'a or b too big, or MAXIT too small in betacf'
      write(iolog,'(a)')
     &      'betacf__W> a or b too big, or MAXIT too small in betacf'
1     betacf=h
      return
      END
