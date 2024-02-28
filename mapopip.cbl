       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. MAPOPIP.
      * program is used by user to leave an opinion
      * user opinion is saved to OPIF file
      * user have to provide his rate <1,5>
      * his bank account (program checks if this accoutn exists)
               
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
           COPY MAPOPI.                                                 
           COPY DFHAID.          

       01 RESPCODE PIC S9(8) COMP.                                      
       01 WS-COMMAREA PIC X VALUE 'A'.                                  
      
       01 USER-MESSAGES.                                                
           05 INVALID-KEY-MSG PIC X(11) VALUE 'INVALID KEY'.             
           05 EXIT-MSG PIC X(10) VALUE 'NORMAL END'.                     
           05 ABNORMAL-EXIT-MSG PIC X(12) VALUE 'ABNORMAL END'.          
           05 DUPREC-ERROR-MSG PIC X(29)                                 
              VALUE 'WE HAVE YOUR OPINION ALREADY.'.                    
           05 ERROR-MSG PIC X(20) VALUE 'SOMETHING WENT WRONG'.          
           05 CORRECT-RESPONSE-MSG PIC X(23)    
              VALUE 'YOUR OPINION WAS SAVED.'.                      
           05 NOTFND-ERROR-MSG PIC X(28)                             
              VALUE 'THERE IS NO SUCH BANK NUMBER'.                 
      
       01 IF-DATA-RECEIVED PIC X.                                   
           88 DATA-RECEIVED-SUCCESSFULLY VALUE 'Y'.                  

      * program data where screen data will be store 
       01 SCREEN-DATA.                                              
           05 USER-RATE PIC X.    
           05 USER-RATE-9 PIC 9.                                   
           05 USER-BANK-ACCOUNT PIC X(26).                           
           05 USER-OPINION PIC X(79).                                
      * dataf file, file structrue 
       01 FS-DATAF.                                                 
           05 DATAF-BANK-ACCOUNT PIC X(26).                          
           05 FILLER             PIC X(123).                         
      * opif file, file structure 
       01 FS-OPIF.                                                  
           05 OPIF-BANK-ACCOUNT      PIC X(26).                      
           05 OPIF-USER-RATE         PIC X.                          
           05 OPIF-USER-OPINION      PIC X(79).                      
       
       01 IF-VALIDATED-CORRECTLY PIC X.      
           88 VALIDATED-CORRECTLY VALUE 'Y'.                             
           88 NOTFND-ERROR        VALUE 'F'.                             
       
       01 IF-SAVED-CORRECTLY PIC X.                                     
           88 SAVED-CORRECTLY VALUE 'Y'.                                 
           88 DUPREC-ERROR    VALUE 'D'.                                 
      
      * value will be put in opinion field
      * thanks to that field will be visible for user

       01 START-OPI-VALUE PIC X(79) VALUE '_____________________________
      - '__________________________________________________'.           
       
       PROCEDURE DIVISION.                                              
       MAIN.                                                            
           IF EIBCALEN = 0 THEN  
      *     this part of code will be executed only 
      *    one time at begining of the transaction               
              PERFORM FIRST-TIME-RUN-PARA                               
           ELSE                                   
      * logic below will allow user action to be processed

              EVALUATE EIBAID                                           
              WHEN DFHENTER                                             
      * when enter will be pressed

                 PERFORM PROCESS-THE-DATA-PARA                          
              WHEN DFHPF3                                               
      
      * when f3 button will be pressed
                 PERFORM EXIT-PARA                                      
              WHEN DFHPF1                                         
      
      * when f1 button will be pressed
                 PERFORM CLEAR-THE-SCREEN-PARA                    
              WHEN OTHER                                          
      * when non of that will be pressed

                 PERFORM INVALID-KEY-PARA                         
              END-EVALUATE                                        
           END-IF       
      * code below will stop transaction

           EXEC CICS                                              
           RETURN                                                 
           TRANSID('OPIP') COMMAREA(WS-COMMAREA)                  
           END-EXEC                                               
           GOBACK.                                                
      
       INVALID-KEY-PARA.                                          
      * this paragraph will send to the user
      * information that he pressed strange key

           MOVE INVALID-KEY-MSG TO MSGO                           
           MOVE START-OPI-VALUE TO OPIO                           
           PERFORM SEND-THE-DATA                                  
           EXIT.                               

       CLEAR-THE-SCREEN-PARA.                                     
      * paragraph will delete all informations from the screen

           MOVE LOW-VALUES TO MAP1O                           
           MOVE START-OPI-VALUE TO OPIO                       
           PERFORM SEND-THE-DATA-ER                           
           EXIT.                                              

       FIRST-TIME-RUN-PARA.                                   
      * paragraph is executed only on the start of the transaction

           MOVE LOW-VALUES TO MAP1O                           
           MOVE START-OPI-VALUE TO OPIO                       
           PERFORM SEND-THE-MAP                               
           EXIT.        

       SEND-THE-MAP.                                          
      * PARAGRAPH WILL SEND WHOLE MAP TO THE USER             
      * should be executed only once 
      * is called by FIRST-TIME-RUN-PARA

           EXEC CICS                                           
           SEND MAP('MAP1') MAPSET('MAPOPI')                   
           FROM(MAP1O)                                         
           RESP(RESPCODE)                                      
           ERASE                                               
           END-EXEC                                            
      
           EVALUATE RESPCODE                                      
           WHEN DFHRESP(NORMAL)                                   
             CONTINUE                                             
           WHEN OTHER                                             
             PERFORM ABNORMAL-EXIT-PARA                           
           END-EVALUATE                                           
           EXIT.                                                  
      
       SEND-THE-DATA.                                             
      * paragraph send data to the user (not the whole map)

           EXEC CICS                                              
           SEND MAP('MAP1') MAPSET('MAPOPI')                      
           DATAONLY                                               
           FREEKB                                                 
           RESP(RESPCODE)                                         
           END-EXEC                                               
           EVALUATE RESPCODE                                      
           WHEN DFHRESP(NORMAL)                                   
             CONTINUE                                             
           WHEN OTHER                                            
             PERFORM ABNORMAL-EXIT-PARA                          
           END-EVALUATE                                          
           EXIT.                                                 
      
       SEND-THE-DATA-ER.                                         
      * PARAGRAPH WILL SEND THE DATA TO THE MAP AND ALSO WILL    
      * CLEAR THE UNPROTECTED FIELDS                             
           EXEC CICS                                             
           SEND MAP('MAP1') MAPSET('MAPOPI')                     
           DATAONLY                                              
           FREEKB                                                
           ERASEAUP                                              
           RESP(RESPCODE)                                        
           END-EXEC                                              
           EVALUATE RESPCODE                                     
           WHEN DFHRESP(NORMAL)                                  
             CONTINUE                                            
           WHEN OTHER                                             
             PERFORM ABNORMAL-EXIT-PARA                           
           END-EVALUATE                                           
           EXIT.                                                  
       PROCESS-THE-DATA-PARA.                                     
      * main logic of the program
      * this paragraph will call other to maintain readability

      
      * paragraph below will take data from screen to program

           PERFORM GET-THE-DATA                                   
      
             IF DATA-RECEIVED-SUCCESSFULLY THEN                   
      * paragraph checks if user rate is beetwen <1,5>
      * and if user bank account exists

                PERFORM VALIDATE-PARA                             
      
                 EVALUATE TRUE                                    
      
                 WHEN VALIDATED-CORRECTLY                         
      * HERE THE USER RATE WAS VALIDATED                          
      * AND BANK NUMBER PROVIDED BY USER IS CHECKED               
      * SO ONLY THING THAT LEFT IS TO SAVE THAT TO                
      * OPIF FILE                                                 
                   PERFORM SAVE-OPINION-TO-FILE                   
                   
                   EVALUATE TRUE                                  
                   WHEN SAVED-CORRECTLY                           
      * everything went good             
                      PERFORM SEND-CORRECT-RESPONSE               
                   WHEN DUPREC-ERROR                              
      
      * in the file we have already opinion from that user
                      PERFORM SEND-DUPREC-ERROR                   
                   WHEN OTHER                                     
      * somthing went wrong

                      PERFORM SEND-ERROR-MSG                      
                   END-EVALUATE                                   
                 WHEN NOTFND-ERROR                                
      
      * THAT BANK DOESN'T EXISTS WE WILL SEND PROPER MSG          
                      PERFORM SEND-NOTFND-ERROR                   
                 WHEN OTHER                                       
      * VALIDATION FAILED                                         
                  PERFORM SEND-ERROR-MSG                          
                 END-EVALUATE                                     
             ELSE                                                 
      * RECIVING FAILED                                           
              PERFORM SEND-ERROR-MSG                              
             END-IF                                               
           EXIT.                                                
       GET-THE-DATA.                                            
      
           MOVE LOW-VALUES TO MAP1I                             
           EXEC CICS                                            
           RECEIVE MAP('MAP1') MAPSET('MAPOPI')                 
           INTO(MAP1I)                                          
           RESP(RESPCODE)                                       
           END-EXEC                                             
           EVALUATE RESPCODE                                    
           WHEN DFHRESP(NORMAL)                                 
      * moving data from screen data to program data

             MOVE RATEI TO USER-RATE                            
             MOVE NUMFILI TO USER-BANK-ACCOUNT                  
             MOVE OPII TO USER-OPINION                          
             MOVE 'Y' TO IF-DATA-RECEIVED                       
           WHEN OTHER                                           
             MOVE 'N' TO IF-DATA-RECEIVED                       
           END-EVALUATE                                         
           EXIT.                                                   
       VALIDATE-PARA.                                              
      * PARAGRAPH WILL CHECK IF RATE IS IN <1,5>                   
      * AND IF BANK-ACCOUNT EXISTS                                 
           MOVE USER-RATE TO USER-RATE-9                     
           IF USER-RATE-9 > 0 AND USER-RATE-9 < 6 THEN       
      * IF USER-RATE-NUMBER IS GRATER BETWEEN <1,5> THEN               
              MOVE USER-BANK-ACCOUNT TO DATAF-BANK-ACCOUNT      
              EXEC CICS                                         
              READ FILE('DATAF')                                
              INTO(FS-DATAF)                                    
              RIDFLD(DATAF-BANK-ACCOUNT)                        
              RESP(RESPCODE)                                    
              END-EXEC                                          
              EVALUATE RESPCODE                                 
              WHEN DFHRESP(NORMAL)                              
              MOVE 'Y' TO IF-VALIDATED-CORRECTLY            
              WHEN DFHRESP(NOTFND)                              
              MOVE 'F' TO IF-VALIDATED-CORRECTLY            
              WHEN OTHER                                        
              MOVE 'N' TO IF-VALIDATED-CORRECTLY            
              END-EVALUATE                            
           ELSE                                    
      * USER-RATE IS INPROPER

              MOVE 'N' TO IF-VALIDATED-CORRECTLY      
                                                   
           END-IF                                  
         
           EXIT.                                                   
       SAVE-OPINION-TO-FILE.                                       
      * PARAGRAPH SAVES USER-PROVIDED DATA TO OPIF FILE

      * MOVING VARIABLES TO FILE STRUCTURE OF OPIF FILE

           MOVE USER-RATE         TO OPIF-USER-RATE                
           MOVE USER-BANK-ACCOUNT TO OPIF-BANK-ACCOUNT             
           MOVE USER-OPINION      TO OPIF-USER-OPINION             
           INSPECT OPIF-USER-OPINION REPLACING ALL '_' BY ' '      
           IF OPIF-USER-OPINION = ' ' THEN                         
               MOVE 'NONE' TO OPIF-USER-OPINION                    
           ELSE                                                    
               CONTINUE                                       
           END-IF                                             
      * WRITING TO OPIF FILE

            EXEC CICS                                         
            WRITE                                             
            FILE('OPIF')                                      
            FROM(FS-OPIF)                                     
            RESP(RESPCODE)                                    
            RIDFLD(OPIF-BANK-ACCOUNT)                         
            END-EXEC                                          
      
            EVALUATE RESPCODE                                 
            WHEN DFHRESP(NORMAL)                              
               MOVE 'Y' TO IF-SAVED-CORRECTLY                 
            WHEN DFHRESP(DUPREC)                              
               MOVE 'D' TO IF-SAVED-CORRECTLY                 
            WHEN OTHER                                        
               MOVE 'N' TO IF-SAVED-CORRECTLY                 
            END-EVALUATE                                      
            EXIT.                                             
      
       SEND-ERROR-MSG.                                        
      
           MOVE ERROR-MSG TO MSGO                            
           PERFORM SEND-THE-DATA                             
           EXIT.                                             
      
       SEND-DUPREC-ERROR.                                     
           MOVE DUPREC-ERROR-MSG TO MSGO                     
           PERFORM SEND-THE-DATA                             
           EXIT.                                             
      
       EXIT-PARA.                                             
      * paragraph will send control to mapmenup                   
           EXEC CICS                                         
         
           XCTL PROGRAM('MAPMENUP')                                            
         
           END-EXEC                                          
           GOBACK.                                              
       ABNORMAL-EXIT-PARA.     
      * termination in case of error                                   
           EXEC CICS                                            
           SEND TEXT FROM(ABNORMAL-EXIT-MSG)                    
           ERASE                                                
           END-EXEC                                             
           EXEC CICS                                            
           RETURN                                               
           END-EXEC                                             
           GOBACK.                  

       SEND-CORRECT-RESPONSE.                                    
           MOVE CORRECT-RESPONSE-MSG TO MSGO                    
           PERFORM SEND-THE-DATA                                
           EXIT.                                                
       
       SEND-NOTFND-ERROR.                                        
           MOVE NOTFND-ERROR-MSG TO MSGO                        
           PERFORM SEND-THE-DATA                                
	       EXIT.
                    