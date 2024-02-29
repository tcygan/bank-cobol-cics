MAPLOG DFHMSD TYPE=&SYSPARM,LANG=COBOL,MODE=INOUT,TERM=ALL,            X
               STORAGE=AUTO,DSATTS=COLOR,MAPATTS=COLOR,TIOAPFX=YES,    X
               CTRL=(FREEKB,FRSET)                                      
MAP1   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1                              
       DFHMDF POS=(1,1),ATTRB=ASKIP,INITIAL='SIGN IN TO OUR SYSTEM',   X
               LENGTH=21                                                
       DFHMDF POS=(2,1),ATTRB=ASKIP,INITIAL='BANK NUMBER: ',LENGTH=13   
BNUM   DFHMDF POS=(2,15),ATTRB=(IC,NUM,UNPROT),LENGTH=26,              X
               INITIAL='__________________________'                     
       DFHMDF POS=(2,42),ATTRB=ASKIP,INITIAL=' ',LENGTH=1               
       DFHMDF POS=(3,1),ATTRB=ASKIP,INITIAL='YOUR PASSWORD: ',LENGTH=15 
PASSF  DFHMDF POS=(3,17),ATTRB=(UNPROT,DRK),INITIAL=' ',LENGTH=30       
       DFHMDF POS=(3,48),ATTRB=PROT,INITIAL=' ',LENGTH=1                
       DFHMDF POS=(5,1),ATTRB=PROT,INITIAL='PRESS ENTER TO CONTINUE',  X
               LENGTH=23                                                
       DFHMDF POS=(6,1),ATTRB=PROT,INITIAL='PRESS F1 TO CLEAR SCREEN', X
               LENGTH=24                                                
       DFHMDF POS=(7,1),ATTRB=PROT,INITIAL='PRESS F3 TO EXIT PGM',     X
               LENGTH=20                                                
MSG    DFHMDF POS=(10,1),ATTRB=PROT,INITIAL=' ',LENGTH=79               
       DFHMSD TYPE=FINAL                                                
       END                                                              