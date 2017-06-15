/* Roger script */

    address 'BPAINT2' /*NOT really need as you are calling this from BPAINT so it is the Default address */
    DO i = 150 to 0 by -10
     MYLINE 0 150 400 i
    END i

    DO x = 0 to 400 by 10
     MYLINE 400 0 x 150
    END x

    EXIT "Script Completed OK!"

