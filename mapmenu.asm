MAPMENU DFHMSD TYPE=&SYSPARM,LANG=COBOL,MODE=INOUT,TERM=ALL,           X
               STORAGE=AUTO,DSATTS=COLOR,MAPATTS=COLOR,TIOAPFX=YES,    X
               CTRL=(FREEKB,FRSET)                                      
MAP1    DFHMDI SIZE=(24,80),LINE=1,COLUMN=1                             
        DFHMDF POS=(1,18),ATTRB=ASKIP,LENGTH=30,                       X
               INITIAL='WELCOME TO BANK SAMPLE PROGRAM'                 
        DFHMDF POS=(3,20),ATTRB=ASKIP,LENGTH=24,                       X
               INITIAL='1. CREATE A BANK ACCOUNT'                       
        DFHMDF POS=(4,20),ATTRB=ASKIP,LENGTH=26,                       X
               INITIAL='2. SIGN IN TO BANK ACCOUNT'                     
        DFHMDF POS=(5,20),ATTRB=ASKIP,LENGTH=22,                       X
               INITIAL='3. LEAVE US AN OPINION'                         
        DFHMDF POS=(6,20),ATTRB=ASKIP,LENGTH=19,                       X
               INITIAL='4. EXIT THE PROGRAM'                            
        DFHMDF POS=(7,10),ATTRB=ASKIP,LENGTH=70,                       X
               INITIAL=' '                                              
        DFHMDF POS=(8,30),ATTRB=ASKIP,LENGTH=8,                        X
               INITIAL='OPTION: '                                       
OPFILD  DFHMDF POS=(8,39),ATTRB=(UNPROT,IC,NUM),LENGTH=1,              X
               INITIAL='_'                                              
        DFHMDF POS=(8,41),ATTRB=PROT,INITIAL=' ',LENGTH=1               
MSG     DFHMDF POS=(10,1),ATTRB=PROT,INITIAL=' ',LENGTH=79              
        DFHMSD TYPE=FINAL                                               
        END                                                             