CMD

PARM KWD(LIB) TYPE(*NAME) LEN(10) MIN(1) SPCVAL((*ALL) (*LIBL)) PROMPT('Searching in Library')

PARM KWD(LIBS) TYPE(*NAME) LEN(10) MIN(1) MAX(5) PROMPT('Libraries to search')

PARM KWD(MATCH) TYPE(*CHAR) LEN(4) DFT(*ALL) SPCVAL((*ANY) (*ALL)) PROMPT('Libraries should match')
