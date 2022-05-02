CMD                                                       
                                                                      
PARM KWD(IFSFILE) TYPE(*CHAR) MIN(1) LEN(128) PROMPT('IFS SQL File')                       
                                                                      
PARM KWD(QMQRY) TYPE(Q1) MIN(1) PROMPT('QMQRY File')                                       
                                                                      
Q1: QUAL TYPE(*NAME) MIN(1)                             
    QUAL TYPE(*NAME) DFT(QTEMP) PROMPT('Library')
