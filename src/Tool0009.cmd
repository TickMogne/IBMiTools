              CMD                                                   
                                                                    
              PARM       KWD(FILE) TYPE(Q1) MIN(1) +                
                          PROMPT('File')                           
                                                                    
              PARM       KWD(TEXT) TYPE(*CHAR) LEN(32) MIN(1) +     
                          CASE(*MIXED) PROMPT('Text to search')    
                                                                    
              PARM       KWD(OUTPUT) TYPE(*CHAR) LEN(6) RSTD(*YES) +
                          DFT(*) VALUES(* *PRINT) +                
                          PROMPT('Output')                         
                                                                    
              PARM       KWD(ORDER) TYPE(*CHAR) LEN(11) RSTD(*YES) +
                          DFT(*ASCENDING) VALUES(*ASCENDING +      
                          *DESCENDING) PROMPT('Member date order') 
                                                                    
              PARM       KWD(CASE) TYPE(*CHAR) LEN(4) RSTD(*YES) +  
                          DFT(*YES) VALUES(*YES *NO) PROMPT('Case +
                          sensitive')                              
                                                                    
             PARM       KWD(MULTI) TYPE(*CHAR) LEN(5) RSTD(*YES) +  
                          DFT(*SHOW) VALUES(*SHOW *SKIP) +          
                          PROMPT('Multiple occurrences in member')  
                                                                    
  Q1:        QUAL       TYPE(*NAME) LEN(10) MIN(1)                  
             QUAL       TYPE(*NAME) LEN(10) MIN(1) PROMPT('Library')
                                                                    
