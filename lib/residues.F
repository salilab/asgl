c --- This file contains the routines for conversion between 3 letter,
c     one letter and integer residue codes, in scalar and vector versions.
c --- Integer code 0, character '#' and '###' are for unknown residue types.
c ------------------------------------------------------------------------
c --- SCALAR VERSIONS:
c      subroutine int2str(ires, res3)
c      subroutine int2chr(ires,res1)
c      subroutine chr2str(res1,res3)
c      subroutine str2chr(res3,res1)
c      integer function istr2int(res3)
c      integer function ichr2int(res1)
c --- VECTOR VERSIONS:
c      subroutine int2strn(ires, res3, nres)
c      subroutine int2chrn(ires, res1, nres)
c      subroutine chr2strn(res1, res3, nres)
c      subroutine str2chrn(res3, res1, nres)
c      subroutine str2intn(res3, ires, nres)
c      subroutine chr2intn(res1, ires, nres)
c -----------------------------------------------------------------------

c --- this function returns an integer type for a three letter
c     residue code; if a code can not be recognized, 0 is returned;
      integer function istr2int(res)
      implicit none
      integer ntypes
      parameter (ntypes = 25)
      integer i
      character res*3, reslst(ntypes)*3
c --- three letter amino acid residue types:
      data   reslst /'ALA', 'CYS', 'ASP', 'GLU', 'PHE', 'GLY',
     -               'HIS', 'ILE', 'LYS', 'LEU', 'MET', 'ASN',
     -               'PRO', 'GLN', 'ARG', 'SER', 'THR', 'VAL',
     -               'TRP', 'TYR', 'ASX', 'GLX', 'UNK', 'PCA', 'INI'/
      do 20  i = 1, ntypes
        if (reslst(i) .eq. res) then 
          istr2int = i
c ------- ASX => ASN
          if (i .eq. 21) istr2int = 12        
c ------- GLX => GLN
          if (i .eq. 22) istr2int = 14        
c ------- UNK => GLY
          if (i .eq. 23) istr2int =  6        
c ------- PCA,INI => unrecognized
          if (i .gt. 23) istr2int =  0        
          return
        end if
20    continue
c --- ??? => unrecognized
      istr2int = 0                            
      return
      end

c --- this routine returns the string*3 type for the integer
c     residue code; if a code can not be recognized, '###' is returned
      subroutine int2str(ires, res)
      implicit none
      integer ntypes
      parameter (ntypes = 25)
      integer ires
      character res*3, reslst(ntypes)*3
c --- three letter amino acid residue types:
      data   reslst /'ALA', 'CYS', 'ASP', 'GLU', 'PHE', 'GLY',
     -               'HIS', 'ILE', 'LYS', 'LEU', 'MET', 'ASN',
     -               'PRO', 'GLN', 'ARG', 'SER', 'THR', 'VAL',
     -               'TRP', 'TYR', 'ASX', 'GLX', 'UNK', 'PCA', 'INI'/
      if ((ires .ge. 1).and. (ires.le. 20)) then
        res = reslst(ires)
      else
        res = '###'
      end if
      return
      end

c --- an integer function that returns the residue type index of a 
c     one letter residue code 
      integer function ichr2int(res)
        implicit none
        integer ntypes
        parameter (ntypes = 20)
        integer i
        character reslst(ntypes)*1, res*1
        data reslst /'A','C','D','E','F','G',
     *               'H','I','K','L','M','N',
     *               'P','Q','R','S','T','V',
     *               'W','Y'/
        do 20  i = 1, ntypes
          ichr2int = i
          if(reslst(i) .eq. res) return
20      continue
        ichr2int = 0
        return
      end


c --- an integer function that returns one letter residue code given
c     an integer index of a residue type
      subroutine int2chr(ires,res)
        implicit none
        integer ntypes,ires
        parameter (ntypes = 20)
        character reslst(ntypes)*1, res*1
        data reslst /'A','C','D','E','F','G',
     *               'H','I','K','L','M','N',
     *               'P','Q','R','S','T','V',
     *               'W','Y'/
        if ((ires.ge.1).and.(ires.le.20)) then
          res = reslst(ires)
        else
          res = '#'
        end if
        return
      end

c --- this routine returns three letter code, given one letter code:
      subroutine chr2str(res1,res3)
        implicit none
        integer ichr2int
        character res1*1, res3*3
        call int2str(ichr2int(res1), res3)
        return
      end


c --- this routine returns one letter code, given three letter code:
      subroutine str2chr(res3,res1)
        implicit none
        integer istr2int
        character res1*1, res3*3
        call int2chr(istr2int(res3), res1)
        return
      end

c --- The following are the versions of the above routines, but for vectors
c     of residue codes

      subroutine int2strn(ires, res, nres)
        implicit none
        integer i,nres,ires(nres)
        character res(nres)*3
        do  i = 1, nres
          call int2str(ires(i), res(i))
        end do
        return
      end 

      subroutine int2chrn(ires, res, nres)
        implicit none
        integer i,nres,ires(nres)
        character res(nres)*1
        do  i = 1, nres
          call int2chr(ires(i), res(i))
        end do
        return
      end 


      subroutine chr2strn(res1, res3, nres)
        implicit none
        integer i, nres
        character res1(nres)*1, res3(nres)*3
        do  i = 1, nres
          call chr2str(res1(i), res3(i))
        end do
        return
      end 

      subroutine str2chrn(res3, res1, nres)
        implicit none
        integer i, nres
        character res1(nres)*1, res3(nres)*3
        do  i = 1, nres
          call str2chr(res3(i), res1(i))
        end do
        return
      end 

      subroutine str2intn(res, ires, nres)
        implicit none
        integer i,nres,ires(nres),istr2int
        character res(nres)*3
        do  i = 1, nres
          ires(i) = istr2int(res(i))
        end do
        return
      end 

      subroutine chr2intn(res, ires, nres)
        implicit none
        integer i,nres,ires(nres),ichr2int
        character res(nres)*1
        do  i = 1, nres
          ires(i) = ichr2int(res(i))
        end do
        return
      end 
