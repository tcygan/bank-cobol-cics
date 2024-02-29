MAPCREA  DFHMSD TYPE=&SYSPARM,LANG=COBOL,MODE=INOUT,TERM=ALL,          X
               STORAGE=AUTO,DSATTS=COLOR,MAPATTS=COLOR,TIOAPFX=YES,    X
               CTRL=(FREEKB,FRSET)                                      
MAP1    DFHMDI SIZE=(24,80),LINE=1,COLUMN=1                             
        DFHMDF POS=(1,20),ATTRB=ASKIP,LENGTH=26,                       X
               INITIAL='CREATION OF A BANK ACCOUNT'                     
        DFHMDF POS=(3,1),ATTRB=ASKIP,LENGTH=41,                        X
               INITIAL='PRESS ENTER TO ADD USER, PRESS F3 TO EXIT'      
        DFHMDF POS=(5,1),ATTRB=PROT,LENGTH=6,                          X
               INITIAL='NAME: '                                         
NAME    DFHMDF POS=(5,13),ATTRB=(UNPROT,IC),LENGTH=20,                 X
               INITIAL='____________________'                           
        DFHMDF POS=(5,34),ATTRB=ASKIP,INITIAL=' ',LENGTH=1              
        DFHMDF POS=(6,1),ATTRB=PROT,INITIAL='SURNAME: ',LENGTH=9        
SURNAME DFHMDF POS=(6,13),ATTRB=UNPROT,LENGTH=20,                      X
               INITIAL='____________________'                           
        DFHMDF POS=(6,34),ATTRB=ASKIP,INITIAL=' ',LENGTH=1              
        DFHMDF POS=(7,1),ATTRB=PROT,INITIAL='ADRESS: ',LENGTH=8         
ADRESS  DFHMDF POS=(7,13),ATTRB=UNPROT,LENGTH=45,                      X
               INITIAL='_____________________________________________'  
        DFHMDF POS=(7,59),ATTRB=ASKIP,INITIAL=' ',LENGTH=1              
        DFHMDF POS=(8,1),ATTRB=PROT,INITIAL='PHONE NUM: ',             X
               LENGTH=11                                                
PHONE   DFHMDF POS=(8,13),ATTRB=UNPROT,INITIAL='____________',         X
               LENGTH=12                                                
        DFHMDF POS=(8,26),ATTRB=ASKIP,INITIAL=' ',LENGTH=1              
        DFHMDF POS=(9,1),ATTRB=PROT,INITIAL='ID NUMBER: ',LENGTH=11     
IDNUM   DFHMDF POS=(9,13),ATTRB=UNPROT,INITIAL='___________',LENGTH=11  
        DFHMDF POS=(9,25),ATTRB=ASKIP,INITIAL=' ',LENGTH=1              
MSG     DFHMDF POS=(11,1),ATTRB=PROT,INITIAL=' ',LENGTH=79              
BNUM    DFHMDF POS=(13,1),ATTRB=(DRK,PROT),INITIAL=' ',LENGTH=26        
        DFHMSD TYPE=FINAL                                               
        END                                                             