cf ----------------------------------------------------------------------------
cf
cf    IUNDEFD() returns .T. if an integer argument X has a value indicating
cf    that it is undefined (-999).
cf
cf    logical function iundefd(x)
cf
cf ----------------------------------------------------------------------------

      logical function iundefd(x)
        implicit none
#include "numbers.cst"
        integer x
        iundefd = x .eq. nundf
        return
      end


cf ----------------------------------------------------------------------------
cf
cf    IDEFD() returns .T. if an integer argument X has a value indicating
cf    that it is defined (<> -999).
cf
cf    logical function idefd(x)
cf
cf ----------------------------------------------------------------------------

      logical function idefd(x)
        implicit none
        integer  x
        logical iundefd
        idefd = .not. iundefd(x)
        return
      end


cf ----------------------------------------------------------------------------
cf
cf    ZERO() returns .T. if a real argument X is close to 0.
cf
cf    logical function zero(x)
cf
cf ----------------------------------------------------------------------------

      logical function zero(x)
        implicit none
#include "numbers.cst"
        real x
        zero = abs(x) .lt. fpequal
        return
      end


cf ----------------------------------------------------------------------------
cf
cf    NZERO() returns .T. if a real argument X is not close to 0.
cf
cf    logical function nzero(x)
cf
cf ----------------------------------------------------------------------------

      logical function nzero(x)
        implicit none
#include "numbers.cst"
        real x
        logical zero
        nzero = .not. zero(x)
        return
      end


cf ----------------------------------------------------------------------------
cf
cf    IZERO() returns .T. if an integer argument X is 0.
cf
cf    logical function izero(x)
cf
cf ----------------------------------------------------------------------------

      logical function izero(x)
        implicit none
        integer x
        izero = x .eq. 0
        return
      end


cf ----------------------------------------------------------------------------
cf
cf    INZERO() returns .T. if an integer argument X is not 0.
cf
cf    logical function inzero(x)
cf
cf ----------------------------------------------------------------------------

      logical function inzero(x)
        implicit none
        integer x
        inzero = x .ne. 0
        return
      end


cf ----------------------------------------------------------------------------
cf
cf    UNDEFD() returns .T. if a real argument X has a value that indicates
cf    it is not defined (-999 or 9999.9).
cf
cf    logical function undefd(x)
cf
cf ----------------------------------------------------------------------------

      logical function undefd(x)
        implicit none
#include "numbers.cst"
        real x,fp
        logical equal
        fp = nundf
        undefd = equal(x,fp) .or. equal(x,rundf2)
        return
      end

cf ----------------------------------------------------------------------------
cf
cf    DEFD() returns .T. if a real argument X has a value that indicates
cf    it is defined (<> -999).
cf
cf    logical function defd(x)
cf
cf ----------------------------------------------------------------------------

      logical function defd(x)
        implicit none
        real x
        logical undefd
        defd = .not. undefd(x)
        return
      end


cf ----------------------------------------------------------------------------
cf
cf    DEGREES2() returns an argument in radians to degrees, but it also takes
cf    into account that the argument may be undefined, and returns the
cf    undefined result itself in that case.
cf
cf    real function degrees2(rad)
cf
cf ----------------------------------------------------------------------------

      real function degrees2(rad)
      implicit none
#include "numbers.cst"
      real rad
      logical defd
      if (defd(rad)) then
        degrees2 = rad2deg*rad
      else
        degrees2 = nundf
      end if
      return
      end


cf ----------------------------------------------------------------------------
cf
cf    DEGREES() returns an argument in radians to degrees, but it also takes
cf    into account that the argument may be undefined, and returns the
cf    undefined result itself in that case. In addition, it makes sure that
cf    the result is in the range from -180 .. 180 degrees.
cf
cf    real function degrees(rad)
cf
cf ----------------------------------------------------------------------------

      real function degrees(rad)
      implicit none
#include "numbers.cst"
      real rad,angdeg
      logical defd
      if (defd(rad)) then
        degrees = angdeg(rad2deg*rad)
      else
        degrees = nundf
      end if
      return
      end


cf ----------------------------------------------------------------------------
cf
cf    RADIANS() transforms the argument in degrees to the radians. The argument
cf    may be undefined (the result is undefined) and the result is always in
cf    the range from -Pi .. Pi.
cf
cf    real function radians(deg)
cf
cf ----------------------------------------------------------------------------

      real function radians(deg)
      implicit none
#include "numbers.cst"
      real deg,angrad
      logical defd
      if (defd(deg)) then
        radians = angrad(deg2rad*deg)
      else
        radians = nundf
      end if
      return
      end


cf ----------------------------------------------------------------------------
cf
cf    RADIANS2() transforms the argument in degrees to the radians. The argument
cf    may be undefined, in which case the result is undefined, too.
cf
cf    real function radians2(deg)
cf
cf ----------------------------------------------------------------------------

      real function radians2(deg)
      implicit none
#include "numbers.cst"
      real deg
      logical defd
      if (defd(deg)) then
        radians2 = deg2rad*deg
      else
        radians2 = nundf
      end if
      return
      end

cf ----------------------------------------------------------------------------
cf
cf    ANGDEG() takes an angle in degrees and returns angle in degrees in
cf    the range from -180 .. 180 degrees.
cf
cf    real function angdeg(ang)
cf
cf ----------------------------------------------------------------------------

      real function angdeg(ang)
      implicit none
#include "numbers.cst"
      real a, ang
      logical defd
      if (defd(ang)) then
        a = mod(ang,pi2degr)
        if (a .lt. (-pidegr)) a = a + pi2degr
        if (a .gt.  pidegr) a = a - pi2degr
        angdeg = a
      else
        angdeg = nundf
      end if
      return
      end


cf ----------------------------------------------------------------------------
cf
cf    ANGRAD() takes an angle in radians and returns an angle in radians
cf    in the range from -Pi .. Pi radians.
cf
cf    real function angrad(ang)
cf
cf ----------------------------------------------------------------------------

      real function angrad(ang)
      implicit none
#include "numbers.cst"
      real a, ang
      logical defd
      if (defd(ang)) then
        a = mod(ang,pi2)
        if (a .lt. (-pi)) a = a + pi2
        if (a .gt.  pi) a = a - pi2
        angrad = a
      else
        angrad = nundf
      end if
      return
      end


cf ----------------------------------------------------------------------------
cf
cf    DIFFDEG() returns the difference (in degrees) of two angles
cf    (in degrees): ANG2-ANG1.  The difference is in the interval
cf    from -180 -- 180 degrees. It is defined as the shortest path
cf    around the circle from ANG1 to ANG2 (clockwise is +).
cf
cf    real function diffdeg(ang2,ang1)
cf
cf ----------------------------------------------------------------------------

      real function diffdeg(ang2,ang1)
      implicit none
#include "numbers.cst"
      real ang1,ang2,a1,a2,d
      logical defd
      if (defd(ang1) .and. defd(ang2)) then
        a1 = mod(ang1,pi2degr)
        a2 = mod(ang2,pi2degr)
        d = a2 - a1
        if (d .lt. (-pidegr)) d = d + pi2degr
        if (d .gt.  pidegr) d = d - pi2degr
        diffdeg = d
      else
        diffdeg = nundf
      end if
      return
      end


cf ----------------------------------------------------------------------------
cf
cf    DIFFRAD() returns the difference (in radians) of two angles
cf    (in radians): ANG2-ANG1.  The difference is in the interval
cf    from -Pi -- Pi radians. It is defined as the shortest path
cf    around the circle from ANG1 to ANG2 (clockwise is +).
cf
cf    real function diffrad(ang2,ang1)
cf
cf ----------------------------------------------------------------------------

      real function diffrad(ang2,ang1)
      implicit none
#include "numbers.cst"
      real ang2,ang1,a1,a2,d
      logical defd
      if (defd(ang1).and. defd(ang2)) then
        a1 = mod(ang1,pi2)
        a2 = mod(ang2,pi2)
        d = a2 - a1
        if (d .lt. (-pi)) d = d + pi2
        if (d .gt.  pi) d = d - pi2
        diffrad = d
      else
        diffrad = nundf
      end if
      return
      end


      real function diffrad2(ang2,ang1)
      implicit none
#include "numbers.cst"
      real ang2,ang1,a1,a2,d

      a1 = mod(ang1,pi2)
      a2 = mod(ang2,pi2)
      d = a2 - a1
      if (d .lt. -pi) d = d + pi2
      if (d .gt.  pi) d = d - pi2
      diffrad2 = d

      return
      end


cf ----------------------------------------------------------------------------
cf
cf    DIFF() returns a difference between two real arguments: X2-X1.
cf
cf    real function diff(x2,x1)
cf
cf ----------------------------------------------------------------------------

      real function diff(x2,x1)
      implicit none
#include "numbers.cst"
      real x2,x1
      logical defd
      if (defd(x1).and.defd(x2)) then
        diff = x2 - x1
      else
        diff = nundf
      end if
      return
      end


cf ----------------------------------------------------------------------------
cf
cf    JOULES() returns Joules from calories. The argument may be undefined,
cf    in which case the result is udnefined, too.
cf
cf    real function joules(cal)
cf
cf ----------------------------------------------------------------------------

      real function joules(cal)
        implicit none
#include "numbers.cst"
        real cal
        logical defd
        if (defd(cal)) then
          joules = cal2joul*cal
        else
          joules = nundf
        end if
        return
      end



cf ----------------------------------------------------------------------------
cf
cf    EQUAL() returns .T. if its two single precision floating-point
cf    arguments are not different for more than a small number.
cf
cf ----------------------------------------------------------------------------

      logical function equal(x1, x2)
        implicit none
        real x1, x2
c        logical zero

        equal = x2 .eq. x1

c        equal = zero(x2-x1)

        return
      end


cf ----------------------------------------------------------------------------
cf
cf    DIVIDE() returns the value that has the same sign as the argument
cf    but is at least DIVISOR large, so that division can be safely done.
cf
cf    real function divide(arg)
cf
cf ----------------------------------------------------------------------------

      real function divide(arg,ierr)
        implicit none
#include "numbers.cst"
        integer ierr
        real arg

        divide = sign(max(divisor,abs(arg)), arg)

        if (abs(arg) .lt. divisor) then
          write(*,'(a,2g20.6/a)')
     &    'divide__E> division by zero: ', arg, divisor,
     &    '           try changing RANDOM_SEED and complain to Andrej'
          ierr = 1
          return
        end if

        ierr = 0

        return
      end


cf ----------------------------------------------------------------------------
cf
cf    DIVIDE2() returns arg1/arg2 unless arg2 is so small that division
cf    cannot be done accurately. In that case, 0 is returned.
cf
cf    real function divide2(arg1,arg2)
cf
cf ----------------------------------------------------------------------------

      real function divide2(arg1,arg2)
        implicit none
#include "numbers.cst"
        real arg1, arg2

        if (abs(arg2) .lt. divisor) then
          divide2 = 0.0
        else
          divide2 = arg1 / arg2
        end if


        return
      end


      real function divide3(arg)
        implicit none
#include "numbers.cst"
        real arg

        divide3 = sign(max(divisor,abs(arg)), arg)

        return
      end
