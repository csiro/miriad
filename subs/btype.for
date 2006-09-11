c************************************************************************
c  These routines read and write the image "btype" item which
c  describes the quantity represented by the pixel intensity.
c  For example, intensity, or rotation measure say.
c  This is a TAB free environment.
c
c  History:
c    nebk  25may92  Original version.
c    nebk  26nov92  Add new types
c    nebk  02dec92  If btype  ' ' in wrbtype do nothing, rather than
c                   defaulting to intensity
c    rjs   16sep93  Rename bsrch to binsrch.
c    nebk  14jun94  Make case insensitive as conversion to FITS
c                   brings value in with upper case
c************************************************************************
c* wrbtype -- Write the "btype" item
c& nebk
c: header-i/o
c+
      subroutine wrbtype (lun, value)
c-----------------------------------------------------------------------
c     WRBTYPE writes the "btype" item into the header.  The
c     default value is "intensity".
c
c  Input:
c   lun    i   Handle of file
c   value  a   The value of "btype"
c
c--
c-----------------------------------------------------------------------
      implicit none
c
      integer lun
      character*(*) value
cc
      integer binsrcha, i
c
      integer ntype
      parameter (ntype = 15)
      character*25 types(ntype), lvalue
      save types
      data types /'beam', 
     +            'column_density',
     +            'depolarization_ratio', 
     +            'fractional_polarization', 
     +            'kinetic_temperature', 
     +            'intensity',
     +            'magnetic_field', 
     +            'optical_depth', 
     +            'polarized_intensity',
     +            'position_angle', 
     +            'rotation_measure',
     +            'rotational_temperature',
     +            'spectral_index', 
     +            'velocity', 
     +            'velocity_dispersion'/
c-----------------------------------------------------------------------
      if (value.eq.' ') return
      lvalue = value
      call lcase (lvalue)
c
      i = binsrcha (lvalue, types, ntype)
      if (i.eq.0) call bug ('f', 
     +   'Incorrect value for "btype" in WRBTYPE')
c
      call wrhda (lun, 'btype', lvalue)
c
      end
c
c
c* rdbtype -- Read the "btype" item
c& nebk
c: header-i/o
c+
      subroutine rdbtype (lun, value, def)
c-----------------------------------------------------------------------
c     RDBTYPE reads  the "btype" item from the header.  The
c     default value is "intensity".
c
c  Input:
c   lun    i   Handle of file
c   def    a   Default value
c  Output:
c   value  a   The value of "btype"
c
c--
c-----------------------------------------------------------------------
      implicit none
c
      integer lun
      character*(*) value, def
cc
      integer binsrcha, i
c
      integer ntype
      parameter (ntype = 15)
      character*25 types(ntype), temp, lvalue
      save types
      data types /'beam', 
     +            'column_density',
     +            'depolarization_ratio', 
     +            'fractional_polarization',
     +            'kinetic_temperature', 
     +            'intensity',
     +            'magnetic_field', 
     +            'optical_depth', 
     +            'polarized_intensity',
     +            'position_angle', 
     +            'rotation_measure',
     +            'rotational_temperature',
     +            'spectral_index', 
     +            'velocity', 
     +            'velocity_dispersion'/
c-----------------------------------------------------------------------
      temp = def
      if (temp.eq.' ') temp = 'intensity'
      call rdhda (lun, 'btype', value, temp)
      lvalue = value
      call lcase (lvalue)
c
      i = binsrcha (lvalue, types, ntype)
      if (i.eq.0) call bug ('f', 
     +   'Incorrect value for "btype" in RDBTYPE')
c
      end

