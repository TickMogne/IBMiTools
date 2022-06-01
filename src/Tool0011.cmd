                                                                       
             CMD                                                       
                                                                       
             PARM       KWD(MSGID) TYPE(*NAME) LEN(7) MIN(1) +         
                          PROMPT('Message ID')                         
                                                                       
             PARM       KWD(MSGF) TYPE(Q1) PROMPT('Message file')      
                                                                       
 Q1:         QUAL       TYPE(*GENERIC) LEN(10) DFT(*ALL) SPCVAL((*ALL))
             QUAL       TYPE(*NAME) DFT(*ALL) SPCVAL(('*ALL') +        
                          ('*LIBL')) PROMPT('Library')                 
                                                                       
