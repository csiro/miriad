      integer MAXLEN, MAXKEYS
      parameter(MAXLEN=32768, MAXKEYS=32)

      logical expanded(MAXKEYS), luc(MAXKEYS), qhelp
      integer k1(MAXKEYS), k2(MAXKEYS), lu(MAXKEYS), nkeys
      character keys(MAXKEYS)*8, pbuf*(MAXLEN)

      common /keycommc/ pbuf, keys
      common /keycomm/  lu, k1, k2, luc, expanded, nkeys, qhelp
