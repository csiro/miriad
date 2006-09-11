c +++ SAD.H
c
c     sad.for include file, defines a common block used for fitting
c
c ---

      integer    MAXPIX, MAXCOMP
      parameter (MAXPIX=256, MAXCOMP=2)

      integer    ns,
     -           nd,
     -           nc

      real       data(MAXPIX*MAXPIX),
     -           model(MAXPIX*MAXPIX)

      real       xd(MAXPIX*MAXPIX),
     -           yd(MAXPIX*MAXPIX),
     -           xoff,
     -           yoff

      real       pf(6,MAXCOMP),
     -           sf(6,MAXCOMP)

      logical    vf(6),
     -           dogauss,
     -           dopoint,
     -           dofixed

      common / SADFIT / ns, nd, nc, data, model, xd, yd, xoff, yoff, 
     - pf, sf, vf, dogauss, dopoint, dofixed
