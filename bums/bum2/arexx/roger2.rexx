/* Roger script */

    address 'BPAINT2' /*NOT really need as you are calling this from BPAINT so it is the Default address */
    DO i = 1 To 5
     MYBOX 10*i 10*i (10*i)+10 (10*i)+10 20
     mycirc 50 50 (i*4)
    End i
    EXIT "Script Completed OK!"

