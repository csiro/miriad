c
c
c MAXCALS = Maximum number of allowed calibrators
c
        INTEGER MAXCALS
        PARAMETER (MAXCALS=16)
c
c NDOFS   = Number of dofs actually computed
c
        INTEGER NDOFS
        PARAMETER (NDOFS=3)
c
c Degree-of-freedom definitions
c g is \gamma, d is \delta
c M is -, P is +
c
        integer GMM, DMP, DMM
        integer GPP, GPM, DPP, DPM
        parameter (GMM=1)
        parameter (DMP=2)
        parameter (DMM=3)
        parameter (GPP=4)
        parameter (GPM=5)
        parameter (DPP=6)
        parameter (DPM=7)
c
