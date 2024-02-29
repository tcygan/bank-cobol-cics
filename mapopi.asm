MAPOPI DFHMSD TYPE=&SYSPARM,LANG=COBOL,MODE=INOUT,TERM=ALL,            X
               STORAGE=AUTO,                                           X
               DSATTS=COLOR,MAPATTS=COLOR,TIOAPFX=YES,                 X
               CTRL=(FREEKB,FRSET)                                      
MAP1   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1                              
       DFHMDF POS=(1,21),ATTRB=ASKIP,LENGTH=31,                        X
               INITIAL='HOW WOULD YOU RATE OUR SYSTEM? '                
       DFHMDF POS=(2,10),ATTRB=ASKIP,INITIAL='RATE US IN 1/5 SCALE:',  X
               LENGTH=22                                                
RATE   DFHMDF POS=(2,33),ATTRB=(IC,NUM,UNPROT),INITIAL='_',LENGTH=1     
       DFHMDF POS=(2,35),ATTRB=ASKIP,INITIAL='<- GIVE NUMBER HERE',    X
               LENGTH=19                                                
       DFHMDF POS=(4,10),ATTRB=ASKIP,LENGTH=26,                        X
               INITIAL='WHAT IS YOUR BANK NUMBER: '                     
NUMFIL DFHMDF POS=(4,37),ATTRB=(NUM,UNPROT),LENGTH=26,                 X
               INITIAL='__________________________'                     
       DFHMDF POS=(4,64),ATTRB=ASKIP,INITIAL=' ',LENGTH=1               
DESC   DFHMDF POS=(5,1),ATTRB=ASKIP,LENGTH=31,                         X
               INITIAL='LEAVE A DESCRIPTION IF YOU LIKE'                
OPI    DFHMDF POS=(6,1),ATTRB=UNPROT,LENGTH=79,                        X
               INITIAL=' '                                              
MSG    DFHMDF POS=(8,1),ATTRB=PROT,LENGTH=79,INITIAL=' '                
       DFHMSD TYPE=FINAL                                                
       END                                                              