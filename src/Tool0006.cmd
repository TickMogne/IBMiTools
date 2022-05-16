             CMD        VLDCKR(TOOL0006)                               
                                                                       
             PARM       KWD(OUTPUT) TYPE(*CHAR) LEN(8) RSTD(*YES) +    
                          DFT(*PRINT) VALUES(*PRINT *OUTFILE) +        
                          PROMPT('Output')                             
                                                                       
             PARM       KWD(OUTFILE) TYPE(Q1) MIN(0) PMTCTL(OUTFILE) + 
                          PROMPT('File to receive output')             
                                                                       
 Q1:         QUAL       TYPE(*NAME) LEN(10)                     
             QUAL       TYPE(*NAME) LEN(10) PROMPT('Library')   
                                                                       
 OUTFILE:    PMTCTL     CTL(OUTPUT) COND((*EQ *OUTFILE))               
                                                                       
