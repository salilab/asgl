C --- Reads Brookhaven type file ;
C
C --- Input:
C       BRKNAME   ... file name ;
C       X,Y,Z     ... coordinate vectors ;
C       MAXATM    ... maximal number of atoms ;
C       MAXRES    ... maximal number of residues ;
C
C --- Output:
C       NATM      ... number of atoms ;
C       NRES      ... number of residues ;
C       RESNAM()  ... vector of residue names (A3);
C       RESNUM()  ... vector of residue numbers (A5 - 5. ch. is insertion code)
C       ATMNAM()  ... vector of atom names (A4)
C       IATMR1(I) ... index of the 1. atom in the I-th resiude ;
C       IATMR2(I) ... index of the last atom in the I-th resiude ;
C       IRESATM(I)... residue index of the I-th atom ;
C
      subroutine readbrk(ioinp,iolog,brkname,x,y,z,maxatm,maxres,natm,
     -                   nres,resnam,resnum,atmnam,iatmr1,iatmr2,
     -                   iresatm)
        implicit none

        integer maxatm,ioinp,iolog
        integer maxres,natm,nres,iatmr1(maxres),iatmr2(maxres)
        integer iresatm(maxatm)
        real x(maxatm), y(maxatm), z(maxatm)
        character resnam(maxres)*3,resnum(maxres)*5
        character card*60, atmnam(maxatm)*4, atnumb*5, brkname*(*)
        character test1*6,oldnum*5,oldnam*3,newnum*5,newnam*3

        call openf(ioinp, brkname, 'old')
        natm = 0
        nres = 0
        oldnum = '@@@@@'
5       continue
          read(ioinp, '(a60)', end=20)card
          test1 = card(1:6)
c          if ((test1.eq.'ATOM  ') .or. (test1.eq.'HETATM')) then
          if (test1.eq.'ATOM  ') then
            natm = natm + 1
            read(card, 15) atnumb, atmnam(natm), newnam,
     *                     newnum, x(natm), y(natm), z(natm)
15          format(6x,a5,1x,a4,1x,a3,2x,a5,3x,3f8.3)
            if (newnum .ne. oldnum) then
              nres = nres + 1
              resnum(nres) = newnum
              resnam(nres) = newnam
              oldnum = newnum
              oldnam = newnam
              iatmr1(nres) = natm
              if(nres.gt.1) iatmr2(nres-1) = natm-1
            end if
            iresatm(natm) = nres
          end if
          go to 5
20      continue
        iatmr2(nres) = natm
        close(ioinp)

        write(iolog,'(''readbrk__> residues, atoms: '', 2i6)') nres,
     &        natm

        return
      end



      subroutine rdxyz(ioinp,iolog,brkname,x,y,z,maxatm,maxres,natm,
     -                 nres,resnam,resnum,atmnam,iatmr1,iatmr2,iresatm)
        implicit none

        integer maxatm,ioinp,iolog
        integer maxres,natm,nres,iatmr1(maxres),iatmr2(maxres)
        integer iresatm(maxatm)
        real x(maxatm), y(maxatm), z(maxatm)
        character resnam(maxres)*3,resnum(maxres)*5
        character atmnam(maxatm)*4,brkname*(*)

        call openf(ioinp, brkname, 'old')
        natm = 1
        nres = 1

5       read(ioinp, *, end=20) x(natm), y(natm), z(natm)
          atmnam(natm) = 'CA'
          resnam(nres) = 'GLY'
          write(resnum(nres), '(i4,1x)') nres
          iatmr1(nres) = natm
          iatmr2(nres) = natm
          iresatm(natm) = nres
          natm = natm + 1
          nres = nres + 1
          go to 5
20      continue
        natm = natm - 1
        nres = nres - 1
        close(ioinp)

        write(iolog,'(''rdxyz____> residues, atoms: '', 2i6)') nres,
     &        natm

        return
      end
