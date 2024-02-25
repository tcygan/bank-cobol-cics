       IDENTIFICATION DIVISION.                                       
       PROGRAM-ID. MAPCREAP.                                          
      
      * this program takes input from the user and assign to that
      * data bank account number,
      * data of user are stored in DATAF file
      * free bank numbers are stored in FREENUM file
      * bank account will be later used as a login
      * to MAPBANKP program

       DATA DIVISION.                                                 
       WORKING-STORAGE SECTION.                                       
      
           COPY DFHAID.                                               
           COPY MAPCREA.                                              
           COPY DFHBMSCA.                                             
      
       01 RESPCODE PIC S9(8) COMP.                                    
      
       01 WS-COMMAREA PIC X VALUE 'A'.                                
      
       01 MESSAGES-TO-USER.                                           
           05 NORMAL-EXIT-MSG PIC X(11) VALUE 'NORMAL EXIT'.          
           05 ERROR-EXIT-MSG PIC X(15) VALUE 'NON NORMAL EXIT'.       
           05 INVALID-KEY-MSG PIC X(11) VALUE 'INVALID KEY'.          
           05 FILE-ERROR-MSG PIC X(15) VALUE 'ERROR WITH FILE'.       
           05 MAPFAIL-MSG PIC X(31)                                    
              VALUE 'YOU DID NOT PROVIDE PROPER DATA'.                 
           05 INVALID-DATA-MSG PIC X(20) VALUE 'GIVE US A VALID DATA'.
      
      * variables use to process given data      
       01 SCREEN-DATA.                                                 
           05 OPTION-FIELD PIC X.                                      
           05 NAME-FIELD PIC X(20).                                    
           05 SURNAME-FIELD PIC X(20).                                 
           05 ADRESS-FIELD PIC X(45).                                  
           05 PHONE-NUM-FIELD PIC X(12).                               
           05 ID-NUMBER-FIELD PIC X(11).                               
      * DATA IS USED IN AUDIT-DATA-PARA                                
       01 IF-CORRECT PIC X VALUE 'N'.                                  
           88 DATA-CORRECT VALUE 'Y'.                                  
      * FREENUM FILE FILE STRUCTURE                                    
       01 FS-FREENUM.                                                  
           05 FREENUM-NUMBER PIC X(26).                                
      * DATAF FILE STRUCTURE                                           
      * FILE IS USED TO STORE ALL DATA ABOUT USER                       
       01 FS-DATAF.                                                     
           05 DATAF-BANK-NUMBER     PIC X(26).                          
           05 DATAF-TIMESTAMP       PIC X(15).                          
           05 DATAF-NAME            PIC X(20).                          
           05 DATAF-SURNAME         PIC X(20).                          
           05 DATAF-ADRESS          PIC X(45).                          
           05 DATAF-PHONE-NUM       PIC X(12).                          
           05 DATAF-ID-NUM          PIC X(11).         

       01 TIMESTAMP-VARIABLES.                                          
           05 WS-TIMESTAMP PIC S9(14) COMP.                             
           05 WS-TIMESTAMP-FORMAT PIC X(15).                            
       
      * this flag will be used to determine if 
      * the previous operations were succesfully processed

       01 IF-CONTINUATION-ALLOWED PIC X VALUE 'N'.                      
           88 CONTINUATION-ALLOWED VALUE 'Y'.                           
       
       PROCEDURE DIVISION.                                              
       MAIN.                                                            
           IF EIBCALEN = 0 THEN                                         
               PERFORM FIRST-TIME-RUN-PARA                               
           ELSE              

      * handling user actions   
           
             EVALUATE EIBAID                                         
              WHEN DFHENTER                                          
                PERFORM GET-THE-DATA-PARA                              
              WHEN DFHPF3                                            
                PERFORM EXIT-PROGRAM-PARA                              
              WHEN OTHER                                             
                PERFORM INVALID-KEY-PARA                               
             END-EVALUATE                                            
           END-IF       
      * stopping the transaction

           EXEC CICS                                                 
           RETURN TRANSID('KREA') COMMAREA(WS-COMMAREA)              
           END-EXEC                                                  
           
           GOBACK.                                                   
      
       FIRST-TIME-RUN-PARA.                                          
      * clearing all data and sending fresh screen to the user

           MOVE LOW-VALUES TO MAP1O                                  
            PERFORM SEND-THE-MAP                                      
           EXIT.                                                     
      
       EXIT-PROGRAM-PARA.                                 
      * paragraph will terminate the transaction
      * in normal situation

           EXEC CICS                                                 
           SEND TEXT FROM(NORMAL-EXIT-MSG)                           
           ERASE                                                     
           END-EXEC                                                  

           EXEC CICS                                                 
           RETURN                                                    
           END-EXEC                                                  
           GOBACK.                                                   
       ERROR-EXIT-PROGRAM-PARA.                                      
      * paragraph will terminate the transaction in the case
      * of severe errors
           
           EXEC CICS                                                 
           SEND TEXT FROM(ERROR-EXIT-MSG)                            
           ERASE                                                     
           END-EXEC                                                  
           
           EXEC CICS                                                 
           RETURN                                                   
           END-EXEC                                                 
           GOBACK.            

       INVALID-KEY-PARA.                                            
      * paragraph send to the user information that pressed key
      * was invalid

           MOVE INVALID-KEY-MSG TO MSGO                             
           PERFORM SEND-THE-MAP                                     
           EXIT.                                                    
      
       SEND-THE-MAP.                                                
      * paragraph is responsible for sending screen to the user

           EXEC CICS                                                
      
           SEND MAP('MAP1') MAPSET('MAPCREA')                       
           FROM(MAP1O)                                              
           ERASE                                                    
           RESP(RESPCODE)                                           
      
           END-EXEC                                                 
      
           EVALUATE RESPCODE                                        
           WHEN DFHRESP(NORMAL)                                     
           
             CONTINUE                                               
           
           WHEN OTHER                                             

      * in case of an error in here transaction will be terminated
             PERFORM ERROR-EXIT-PROGRAM-PARA                            
      
           END-EVALUATE                                                 
      
           EXIT.                                                        
         
       GET-THE-DATA-PARA.                                               
      * THIS PARAGRAPFH HAS A LOT OF STEPS:                             
      * THE NEXT ONES ARE PERFORMED IF PREVIOS ONE HAS ENDED SUCCESFULLY  

      * all of the paragraphs performed in here will modify
      * if-continuation-allowed flag
      * in case anything went wrong the logic of this paragraph
      * will be stopped
      * below there is description what action are performed here

                                                                  
      * 1. RECIVING DATA FROM SCREEN AND CREATING A TIMESTAMP           
      * 2. IF STEP1 CORRECT: VALIDATION OF DATA                         
      * 3. IF STEP2 CORRECT: STARTBR OF FREENUM FILE                    
      * 4. IF STOP3 CORRECT: READNEXT OF FREENUM FILE                   
      * 5. IF STOP4 CORRECT: SAVING DATA TO DATAF FILE                  
      * 6. IF STOP5 CORRECT: ENDING THE BROWSE ON FREENUM FILE
      * 7. IF STOP6 CORRECT: DELETING RECORD FROM FREENUM FILE          
      * 8. IF STEP7 CORRECT: SENDING DATA TO A SCREEN        

      * WHEN ANY PARAGRAPH HAVE FAILED THE PROPER OUTPUT IS DISPLAYED   
      * IF THIS PARAGRAP ENDS WITHOUT ERRORS THEN: 
      * BANK NUMBER WILL BE DISPLAYED TO THE USER
      * USER'S DATA WILL BE STORED IN DATAF FILE
      *
      * LATER USING THIS DATA USER WILL BE ABLE TO 
      * LOG INTO HIS BANK ACCOUNT
                             
           PERFORM RECEIVE-MAP-DATA                                     
           IF CONTINUATION-ALLOWED THEN                                 
           PERFORM AUDIT-DATA-PARA                                      
                                                                        
           IF CONTINUATION-ALLOWED THEN                                 
                                                                    
              PERFORM STARTBR-PARA                                  
              IF CONTINUATION-ALLOWED THEN                          
      
                PERFORM READEXT-PARA                                
                IF CONTINUATION-ALLOWED THEN                        
      
                  PERFORM SAVE-DATA-TO-DATAF                        
                    IF CONTINUATION-ALLOWED THEN                    
                                             
                      PERFORM ENDBR-PARA                            
                        IF CONTINUATION-ALLOWED THEN                
                         PERFORM DELETE-FROM-FREENUM-PARA           
                           IF CONTINUATION-ALLOWED THEN             
      * CHANGING THE ATTRIBUTES OF BNUMA FILD
      * IN THAT FIELD ACCOUNT NUBMER OF USER WILL BE DISPLAYED

                              MOVE DFHBMBRY TO BNUMA                
                              MOVE FREENUM-NUMBER TO BNUMO          
                              MOVE 'YOUR BANK ACCOUNT: ' TO MSGO    
      
                           ELSE                                     
      * DELETION FAILED                                             
                              MOVE FILE-ERROR-MSG TO MSGO        
                           END-IF                                
                        ELSE                                     
      * ENDBR-PARA FAILED                                        
                       
                         MOVE FILE-ERROR-MSG TO MSGO             
                        END-IF                                   
                    ELSE                                         
      * SAVING DATA TO DATAF FAILED                              
                     
                     MOVE FILE-ERROR-MSG TO MSGO                 
                    END-IF                                       
                ELSE                                             
                                                                 
      * READNEXT FAILED                                          
                 
                 MOVE FILE-ERROR-MSG TO MSGO                     
                END-IF                                           
              ELSE                                               
      * STARTBR FAILED                                           
                 MOVE FILE-ERROR-MSG TO MSGO                       
              END-IF                                               
           ELSE                                                    
      *  BAD DATA INPUT                                            
           MOVE INVALID-DATA-MSG TO MSGO                           
           ELSE                                                    
      * RECIVING OF DATA WASN'T SUCCESFULL

           MOVE MAPFAIL-MSG TO MSGO                                
           END-IF                                                  
      
      * SENDING MODIFIED SCREEN TO THE USER
           PERFORM SEND-THE-MAP                                    
           EXIT.                           
                             
       AUDIT-DATA-PARA.                                            
      
      * PARAGRAPH WILL DO SIMPLE AUDITION OF PHONE NUMBER
      * AND IDENTIFICATION NUBMER GIVEN BY USER

           IF PHONE-NUM-FIELD IS NUMERIC AND                       
           ID-NUMBER-FIELD IS NUMERIC THEN                         
           MOVE 'Y' TO IF-CONTINUATION-ALLOWED                 
           ELSE                                                
           MOVE 'N' TO IF-CONTINUATION-ALLOWED                 
           END-IF                                              
           EXIT.                                               
      
       RECEIVE-MAP-DATA.                                       
      * PARAGRAPHS WILL RECEIVE USER PROVIDED DATA FORM A SCREEN

           MOVE LOW-VALUES TO MAP1I                            
      
           EXEC CICS                                           
           RECEIVE MAP('MAP1') MAPSET('MAPCREA')               
           INTO(MAP1I)                                         
           RESP(RESPCODE)                                      
           END-EXEC                                            
      
           EVALUATE RESPCODE                                   
           WHEN DFHRESP(NORMAL)                               

      * MOVING DATA FROM SCREEN VARIABLES TO PROGRAM VARIABLES
            MOVE NAMEI    TO    NAME-FIELD                     
            MOVE SURNAMEI TO    SURNAME-FIELD                  
            MOVE ADRESSI  TO    ADRESS-FIELD                   
            MOVE PHONEI   TO    PHONE-NUM-FIELD                  
            MOVE IDNUMI   TO    ID-NUMBER-FIELD         
                     
      * CODE BELOW WILL DELETE UNDERSORES FROM USER DATA 
            INSPECT NAME-FIELD REPLACING ALL '_' BY ' '       
            INSPECT SURNAME-FIELD REPLACING ALL '_' BY ' '    
            INSPECT ADRESS-FIELD REPLACING ALL '_' BY ' '     


      * EXEC BELOW WILL GET THE CURRENT TIME
      * THAT WILL BE STORED IN THE FILE

            EXEC CICS                                            
            ASKTIME                                              
            ABSTIME(WS-TIMESTAMP)                                
            END-EXEC                                           

      * MOVING TIMESTAMP FROM NUMBERIC TO ALPHANUMERIC VARIABLE    
            MOVE WS-TIMESTAMP TO WS-TIMESTAMP-FORMAT             
            MOVE 'Y' TO IF-CONTINUATION-ALLOWED                  
          
           WHEN DFHRESP(MAPFAIL)                                 
            MOVE 'N' TO IF-CONTINUATION-ALLOWED                  
          
           WHEN OTHER                                            
            MOVE 'N' TO IF-CONTINUATION-ALLOWED                  
          
           END-EVALUATE                                          
           EXIT.                                                 
       STARTBR-PARA.                                             
      * PARAGRAPH START BROWSE PROCESS TO FIND THE FIRST 
      * FREE (NOT USED BEFORE) ACCOUNT NUMBER
      * THIS NUMBER WILL BE LATER ASSINED TO THE USER

      * STARTBR WILL SET A POINTER ON THE EQUAL OR GRATER THAN
      * SO WE WILL MOVE ZEROS TO FREENUM-NUMBER
      * THANKS TO THAT POINTER WILL POINT TO FIRST NUMBER

           MOVE LOW-VALUES TO FREENUM-NUMBER                     
                  
           EXEC CICS                                           
   
           STARTBR                                             
           FILE('FREENUM')                                     
           RIDFLD(FREENUM-NUMBER)                              
           RESP(RESPCODE)                                      
   
           END-EXEC                                            
           EVALUATE RESPCODE                                   
           WHEN DFHRESP(NORMAL)                                
             MOVE 'Y' TO IF-CONTINUATION-ALLOWED                 
           WHEN OTHER                                          
             MOVE 'N' TO IF-CONTINUATION-ALLOWED                 
           END-EVALUATE                                        
           EXIT.                                               
       READEXT-PARA.                                           
      * PARAGRAPH READS THE NEXT (IN THAT CASE FIRST)
      * RECORD FROM FREENUM FILE

           EXEC CICS                                           
           READNEXT                                            
           FILE('FREENUM')                                          
           RIDFLD(FREENUM-NUMBER)                                   
           INTO(FS-FREENUM)                                         
           RESP(RESPCODE)                                           
           END-EXEC                                                 
      
           EVALUATE RESPCODE                                        
           WHEN DFHRESP(NORMAL)                                     
               MOVE 'Y' TO IF-CONTINUATION-ALLOWED            

      * HERE ALL VARIABLES ARE MOVED TO DATAF FILE STRUCTURE
      * IN THE NEXT PERFORMED PARAGRAPH THAT DATA WILL BE SAVE     
              
               MOVE FREENUM-NUMBER TO  DATAF-BANK-NUMBER            
               MOVE NAME-FIELD     TO DATAF-NAME                    
               MOVE SURNAME-FIELD  TO DATAF-SURNAME                 
               MOVE ADRESS-FIELD   TO DATAF-ADRESS                  
               MOVE PHONE-NUM-FIELD TO DATAF-PHONE-NUM              
               MOVE ID-NUMBER-FIELD TO DATAF-ID-NUM                 
                                                                    
               MOVE WS-TIMESTAMP-FORMAT TO DATAF-TIMESTAMP          
           WHEN OTHER                                               
              MOVE 'N' TO IF-CONTINUATION-ALLOWED                   
           END-EVALUATE                                             
           EXIT.                                                    
       SAVE-DATA-TO-DATAF.                                          
      * WRITING DATA PROVIDED BY USER TO A DATAF FILE               
      * WITH THOSE DATA USER CAN LATER LOG INTO HIS BANK ACCOUNT          
          
           EXEC CICS                                                
          
           WRITE FILE('DATAF')                                      
           FROM(FS-DATAF)                                           
           RESP(RESPCODE)                                           
           RIDFLD(DATAF-BANK-NUMBER)                                
          
           END-EXEC                                                 
            EVALUATE RESPCODE                                       
            WHEN  DFHRESP(NORMAL)                                   
               MOVE 'Y' TO IF-CONTINUATION-ALLOWED                  
            WHEN OTHER                                              
               MOVE 'N' TO IF-CONTINUATION-ALLOWED            
            END-EVALUATE                                      
          
            EXIT.                                             
       ENDBR-PARA.                                            
      * PARAGRAPH STOPS BROWSE PROCESS ON FREENUM FILE

           EXEC CICS                                          
           ENDBR                                              
      
           FILE('FREENUM')                                    
           RESP(RESPCODE)                                     
      
           END-EXEC                                           
           EVALUATE RESPCODE                                  
           WHEN DFHRESP(NORMAL)                               
             MOVE 'Y' TO IF-CONTINUATION-ALLOWED              
           WHEN OTHER                                         
             MOVE 'N' TO IF-CONTINUATION-ALLOWED              
           END-EVALUATE                                       
           EXIT.                                              
       DELETE-FROM-FREENUM-PARA.                              
      * PARAGRAPH WILL DELETE NUMBER WE ASSIGNED TO THE USER
      * THANKS TO THAT NO OTHER USER WILL BE ASIGNED WITH THE SAME 
      * NUMBER

           EXEC CICS                                                 
      
           DELETE                                                    
           FILE('FREENUM')                                           
           RIDFLD(FREENUM-NUMBER)                                    
           RESP(RESPCODE)                                            
           END-EXEC                                                  
      
           IF RESPCODE = DFHRESP(NORMAL) THEN                        
      * DELETION SUCCESFULL                                          
           MOVE 'Y' TO IF-CONTINUATION-ALLOWED                       
           ELSE                                                      
      * DELETION NOT SUCCESFULL                                      
            MOVE 'N' TO IF-CONTINUATION-ALLOWED                      
           END-IF                                                    
           EXIT.                                                     
