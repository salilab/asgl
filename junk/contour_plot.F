      
      subroutine contour(jdim,kdim,nj,nk,x,y,f,
     $     ncont,acont)

      dimension x(jdim,kdim),y(jdim,kdim),f(jdim,kdim)
      dimension acont(ncont)

      call con2l(jdim,kdim,1,nj,1,nk,x,y,f,ncont,acont)
c
      return
      end
c
      subroutine cline2(cont,np,x,y)

      dimension x(np),y(np)
c
cccc      call concol(cont,funmin,funmax,indexlo,indexup,icolor)
ccc      call move2(x(1),y(1))
ccc      	do i = 1,np
ccc           write(3,*)x(i),y(i)
ccc        enddo
ccc 1       call draw2(x(i),y(i))


c
      return
      end
C
C-----------------------------------------------------------------------
      SUBROUTINE CON2L(IDIM,JDIM,IS,IE,JS,JE,X,Y,F,NCONT,ACONT)
C
C   Calculate contour lines for the function F in the region IS to IE, JS to
C   JE.  X,Y coordinates corresponding to the grid points are in arrays X and
C   Y.
C
C   Common block TEMP1 contains scratch arrays XT, YT, ZT, and IA, and may be
C   used elsewhere.  This subroutine is supposed to be able to recover from
C   filling either array.
C
      PARAMETER (MTEMP1=1000,MIA=3000)
      COMMON /TEMP1/ XT(MTEMP1),YT(MTEMP1),ZT(MTEMP1),IA(MIA)
      DIMENSION X(IDIM,JDIM),Y(IDIM,JDIM),F(IDIM,JDIM)
      DIMENSION ACONT(NCONT)
C
C   One little check.  If IS=IE or JS=JE, return with no contour lines.
C
      IF (IS.EQ.IE .OR. JS.EQ.JE) RETURN
C
C   Zero the marker array.
C
      DO 100 IIA= 1,MIA
         IA(IIA)= 0
  100    CONTINUE
C
C   LNSTRT=1 means we're starting a new line.
C
      LNSTRT= 1
      IGNEXT= 0
C
C   Loop through each contour level.
C
      DO 1000 ICONT= 1,NCONT
         CONT= ACONT(ICONT)
C
C   The IS-IE,JS-JE region may have to be subdivided because of IA space.  This
C   may have a detrimental effect on labelling of the contour lines.  Move 
C   IS,IE,JS,JE to new variables.
C
         ISS= IS
         IEE= IE
         JSS= JS
         JEE= JE
C
C   Flag points in IA where the the function increases through the contour
C   level, not including the boundaries.  This is so we have a list of at least
C   one point on each contour line that doesn't intersect a boundary.
C
  110    CONTINUE
         IAE= 0
         DO 200 J= JSS+1,JEE-1
            IMB  = 0
            IAEND= IAE
            DO 200 I= ISS,IEE
               IF (F(I,J).LE.CONT) THEN
                  IMB    = 1
               ELSE IF (IMB.EQ.1) THEN
                  IAE    = IAE+1
                  IA(IAE)= 1000*I+J
                  IMB    = 0
C
C   Check if the IA array is full.  If so, the subdividing algorithm goes like
C   this:  if we've marked at least one J row, drop back to the last completed
C   J and call that the region.  If we haven't even finished one J row, our 
C   region just extends to this I location.
C
                  IF (IAE.EQ.MIA) THEN
                     IF (J.GT.JSS+1) THEN
                        IAE= IAEND
                        JEE= J
                     ELSE
                        JEE= MIN0(J+1,JEE)
                        IEE= I
                     ENDIF
                     GOTO 210
                  ENDIF
C
               ENDIF
  200          CONTINUE 
C
C   Search along the boundaries for contour line starts.  IMA tells which 
C   boundary of the region we're on.
C
  210    CONTINUE
         IMA = 1
         IMB = 0
         IBEG= ISS-1
         JBEG= JSS
C
    1    IBEG= IBEG+1
         IF (IBEG.EQ.IEE)  IMA= 2 
         GOTO 5
    2    JBEG= JBEG+1
         IF (JBEG.EQ.JEE)  IMA= 3 
         GOTO 5
    3    IBEG= IBEG-1
         IF (IBEG.EQ.ISS)  IMA= 4
         GOTO 5
    4    JBEG= JBEG-1
         IF (JBEG.EQ.JSS)  IMA= 5
    5    IF (F(IBEG,JBEG).GT.CONT) GOTO 7
         IMB= 1
    6    GOTO (1,2,3,4,91),IMA 
    7    IF (IMB.NE.1) GOTO 6
C
C   Got a start point.
C
         IMB= 0
         I  = IBEG 
         J  = JBEG 
         FIJ= F(IBEG,JBEG) 
C
C   Round the corner if necessary.
C
         GOTO (21,11,12,13,51),IMA 
   11    IF (J.NE.JSS) GOTO 31
         GOTO 21 
   12    IF (I.NE.IEE) GOTO 41 
         GOTO 31 
   13    IF (J.NE.JEE) GOTO 51 
         GOTO 41 
C
C   This is how we start in the middle of the region, using IA.
C
   10    I      = IA(IIA)/1000
         J      = IA(IIA)-1000*I  
         FIJ    = F(I,J) 
         IA(IIA)= 0
         GOTO 21 
C                                                                       4
C   Look different directions to see which way the contour line went: 1-|-3
C                                                                       2
   20    J   = J+1
   21    I   = I-1
         IF (I.LT.ISS) GOTO 90
         IDIR= 1
         IF (F(I,J).LE.CONT) GOTO 52 
         FIJ = F(I,J) 
         GOTO 31 
   30    I   = I-1
   31    J   = J-1
         IF (J.LT.JSS) GOTO 90
         IDIR= 2
         IF (F(I,J).LE.CONT) GOTO 60 
         FIJ = F(I,J) 
         GOTO 41 
   40    J   = J-1
   41    I   = I+1
         IF (I.GT.IEE) GOTO 90 
         IDIR= 3
         IF (F(I,J).LE.CONT) GOTO 60 
         FIJ = F(I,J) 
         GOTO 51 
   50    I   = I+1
   51    J   = J+1
         IDIR= 4
         IF (J.GT.JEE) GOTO 90 
         IF (F(I,J).LE.CONT) GOTO 60 
         FIJ = F(I,J) 
         GOTO 21 
C
C   Wipe this point out of IA if it's in the list.
C
   52    IF (IAE.EQ.0) GOTO 60 
         IJ= 1000*I+J+1000 
         DO 300 IIA= 1,IAE 
            IF (IA(IIA).EQ.IJ) THEN
               IA(IIA)= 0
               GOTO 60
            ENDIF
  300       CONTINUE
C
C   Do interpolation for X,Y coordinates.
C
   60    XYF= (CONT-F(I,J))/(FIJ-F(I,J))
C
C   This tests for a contour point coinciding with a grid point.  In this case
C   the contour routine comes up with the same physical coordinate twice.  If
C   If we don't trap it, it can (in some cases significantly) increase the 
C   number of points in a contour line.  Also, if this happens on the first 
C   point in a line, the second point could be misinterpreted as the end of a
C   (circling) contour line.
C
         IF (XYF.EQ.0.) IGNEXT= IGNEXT+1
         GOTO (61,62,63,64),IDIR
   61    WXX= X(I,J)+XYF*(X(I+1,J)-X(I,J))
         WYY= Y(I,J)+XYF*(Y(I+1,J)-Y(I,J))
         GOTO 65 
   62    WXX= X(I,J)+XYF*(X(I,J+1)-X(I,J))
         WYY= Y(I,J)+XYF*(Y(I,J+1)-Y(I,J))
         GOTO 65 
   63    WXX= X(I,J)+XYF*(X(I-1,J)-X(I,J))
         WYY= Y(I,J)+XYF*(Y(I-1,J)-Y(I,J))
         GOTO 65 
   64    WXX= X(I,J)+XYF*(X(I,J-1)-X(I,J))
         WYY= Y(I,J)+XYF*(Y(I,J-1)-Y(I,J))
C
C   Figure out what to do with this point.
C
   65    CONTINUE 
         IF (LNSTRT.NE.1) GOTO 66
C
C   This is the first point in a contour line.
C
         NP    = 1
         XT(NP)= WXX
         YT(NP)= WYY
         WX    = WXX 
         WY    = WYY 
         LNSTRT= 0 
         GOTO 67
C
C   Add a point to this line.  Check for duplicate point first.
C
   66    CONTINUE
         IF (IGNEXT.EQ.2) THEN
            IF (WXX.EQ.XT(NP) .AND. WYY.EQ.YT(NP)) THEN
               IGNEXT= 0
               GOTO 67
            ELSE
               IGNEXT= 1
            ENDIF
         ENDIF
C
         NP    = NP+1
         XT(NP)= WXX
         YT(NP)= WYY
C
C   See if the temporary array xT is full.
C
         IF (NP.EQ.MTEMP1) THEN
            CALL CLINE2(CONT,NP,XT,YT)
            XT(1)= XT(NP)
            YT(1)= YT(NP)
            NP   = 1
         ENDIF
C
C   Check to see if we're back to the initial point.
C
         IF (WXX.NE.WX) GOTO 67
         IF (WYY.EQ.WY) GOTO 90
C
C   Nope.  Search for the next point on this line.
C
   67    CONTINUE
         GOTO (50,20,30,40),IDIR
C
C   This is the end of a contour line.  After this we'll start a new line.
C
   90    LNSTRT= 1
         IGNEXT= 0
         CALL CLINE2(CONT,NP,XT,YT)
C
C   If we're not done looking along the boundaries, go look there some more.
C
         IF (IMA.NE.5) GOTO 6
C
C   Otherwise, get the next start out of IA.
C
   91    CONTINUE
         IF (IAE.NE.0) THEN
            DO 400 IIA= 1,IAE
               IF (IA(IIA).NE.0) GOTO 10
  400          CONTINUE
         ENDIF
C
C   And if there are no more of these, we're done with this region.  If we've
C   subdivided, update the region pointers and go back for more.
C
         IF (IEE.EQ.IE) THEN
            IF (JEE.NE.JE) THEN
               JSS= JEE
               JEE= JE
               GOTO 110
            ENDIF
         ELSE
            ISS= IEE
            IEE= IE
            GOTO 110
         ENDIF
C
C   Loop back for the next contour level.
C
 1000    CONTINUE
C
      RETURN 
      END
