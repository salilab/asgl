cf ----------------------------------------------------------------------------
cf
cf    HEAD() writes out the ASGL header.
cf
cf    subroutine head(io)
cf
cf ----------------------------------------------------------------------------

      subroutine head(io)
        implicit none
#include "asgl.cst"
        integer io

        write(io,10) version
10      format(/
     &  10x,'                          ASGL'//
     &  10x,'                    POSTSCRIPT GRAPHICS'/
     &  10x,25x,a//
     &  10x,'                                        v              '/
     &  10x,'          Copyright(c) 1989-1998 Andrej Sali           '/
     &  10x,'                  All Rights Reserved                  '//
     &  10x,'                                   v                   '/
     &  10x,'                 Written by Andrej Sali                '//)

        write(io,20) maxpnt,maxclm,maxxarr,maxyarr
20      format('ASGL is compiled for:' /
     &  '  Maximum number of points in a curve              : ', i10/
     &  '  Maximum number of curves                         : ', i10/
     &  '  Maximum dimensions of the DPLOT array            : ', 2i10//)

        return
      end 
