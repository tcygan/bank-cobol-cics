       IDENTIFICATION DIVISION.                                      
       PROGRAM-ID. MAPLOGP.                                          
       DATA DIVISION.                                                
       WORKING-STORAGE SECTION.                                      
           COPY MAPLOG.                                               
           COPY DFHAID.                            

       01 RESPCODE PIC S9(8) COMP.                                    
       01 WS-COMMAREA PIC X VALUE 'A'.                                
            
       
       01 USER-MESSAGES.                                              
           05 INVALID-KEY-MSG PIC X(11) VALUE 'INVALID KEY'.          
           05 FILE-ERROR-MSG  PIC X(33)                               
                VALUE 'THERE IS SOME ERROR WITH THE FILE'.            
           05 CORRECT-RESPONSE PIC X(18) VALUE 'YOUR DATA IS VALID'.  
           05 MAPFAIL-ERROR-MSG PIC X(26)                             
                VALUE 'YOU DIDNT PROVIDE ALL DATA'.                     
           05 EXIT-MSG PIC X(11) VALUE 'NORMAL EXIT'.                   
           05 ABNORMAL-EXIT-MSG PIC X(13) VALUE 'ABNORMAL EXIT'.        
           05 ERROR-MSG PIC X(20) VALUE 'SOMETHING WENT WRONG'.         
           05 INVALID-PASS-MSG PIC X(16) VALUE 'INVALID PASSWORD'.      
           05 NOTFND-ERROR-MSG PIC X(30)                                
                VALUE 'YOUR BANK ACCOUNT IS NOT EXIST'.                 
           05 PASS-CREATED-MSG PIC X(47)                                
                VALUE 'YOUR PASSWORD WAS CREATED. IT IS YOUR FIRST LOG'.
       
      * program variables where screen variables goes

       01 SCREEN-VARIABLES.                                             
           05 USER-BANK-ACCOUNT PIC X(26).                              
           05 USER-PASSWORD     PIC X(30).      

      * passf file structure (passwords are stored in that file)
       01 FS-PASSF.                                                     
           05 PASSF-BANK-ACCOUNT PIC X(26).                             
           05 PASSF-PASSWORD     PIC X(30).     
      * dataf file structure
      * whole file isn't needed so only bank account will be processed 
       01 FS-DATAF.                             
           05 DATAF-BANK-ACCOUNT PIC X(26).     
           05 FILLER             PIC X(123).    
      
      * flags used to determite if password was already created

       01 IF-PASSF-EXIST PIC X.                                         
           88 PASSF-EXIST       VALUE 'Y'.                              
           88 PASSF-NOT-EXIST   VALUE 'N'.                          
           88 PASSF-OTHER-ERROR VALUE 'O'.    
       
      * flag is user to determite if CHECK-DATAF-FOR-ACCOUNT 
      * ended correctly

       01 IF-DATAF-EXIST PIC X.                                     
           88 DATAF-EXIST       VALUE 'Y'.                          
           88 DATAF-NOT-EXIST   VALUE 'N'.                          
           88 DATAF-OTHER-ERROR VALUE 'O'.    

      * used to determite if GET-THE-DATA  ended successfully
       01 IF-RECEIVED PIC X.                       
           88 RECEIVED-SUCCESSFULLY VALUE 'Y'.     
           88 MAPFAIL-ERROR         VALUE 'M'.     
           88 OTHER-RECEIVE-ERROR   VALUE 'N'.       
      * flags used to determite if password was already created
       01 IF-PASSF-EXIST PIC X.                                       
           88 PASSF-EXIST       VALUE 'Y'.                            
           88 PASSF-NOT-EXIST   VALUE 'N'.                          
           88 PASSF-OTHER-ERROR VALUE 'O'.                          
                           
                           
      * used to check if SAVE-DATA-TO-FILE paragraph 
      * was processed correctly 
      
      * this paragraph will save data to passf file

       01 IF-SAVED-SUCCESSFULLY PIC X.                              
           88 SAVED-SUCCESSFULLY VALUE 'Y'.                         
           88 SAVED-WITH-ERROR   VALUE 'N'.                            
       PROCEDURE DIVISION.                                           
       MAIN.                                                         
           IF EIBCALEN = 0 THEN           
      * code will happen only once at begining of the transaction       
               PERFORM FIRST-TIME-RUN-PARA                           
           ELSE  
      * this code will handle user actions                     
               EVALUATE EIBAID                                 
               WHEN DFHENTER                                   

                   PERFORM PROCESS-DATA-PARA                   
               WHEN DFHPF1                                     
               
                   PERFORM CLEAR-THE-SCREEN-PARA               
               WHEN DFHPF3                                     
               
                   PERFORM EXIT-PARA                           
               WHEN OTHER                                      
               
                   PERFORM INVALID-KEY-PARA                    
               END-EVALUATE                                    
           END-IF               

      * stopping the transaction     
           EXEC CICS                                           
           RETURN                                              
           TRANSID('LOGG') COMMAREA(WS-COMMAREA)               
           END-EXEC                                            
           GOBACK.                                             
       FIRST-TIME-RUN-PARA.                                          
      * paragraph will be called only once at begining of transaction
      * it send whole screen to the user

           MOVE LOW-VALUES TO MAP1O                                  
           PERFORM SEND-THE-MAP                                      
           EXIT.                                                     
                                           
      
       CLEAR-THE-SCREEN-PARA.                                        
      * paragraph will clear screen from data provided by user

           MOVE LOW-VALUES TO MAP1O                                  
           MOVE ' ' TO MSGO                                          
           PERFORM SEND-THE-DATA                                     
           EXIT.                                                     
      
       EXIT-PARA.                                                    
      * paragraph is called when user wants to terminate transaction
      * proper message will be sended

           EXEC CICS                                                 
           SEND TEXT FROM(EXIT-MSG)                                  
           ERASE                                                     
           END-EXEC                                                  
      
           EXEC CICS                                                 
           RETURN                                                    
           END-EXEC                                                  
           GOBACK.                                                   
       ABNORMAL-EXIT-PARA.                                           
      * this paragraph should not happen
      * termiates the transaction in case of the error

           EXEC CICS                                                 
           SEND TEXT FROM(ABNORMAL-EXIT-MSG)                         
           ERASE                                                     
           END-EXEC                                                  
      
           EXEC CICS                                                 
           RETURN                                                    
           END-EXEC                                                  
           GOBACK.                                                   
      
       SEND-THE-MAP.                                                 
      * paragraph will send whole map to the user

           EXEC CICS                                                 
           SEND MAP('MAP1') MAPSET('MAPLOG')                         
           FROM(MAP1O)                                         
           ERASE                                               
           RESP(RESPCODE)                                      
           END-EXEC                                            
      * in case of error of that code 
      * transaction will be terminated

           EVALUATE RESPCODE                                   
           WHEN DFHRESP(NORMAL)                                
             CONTINUE                                          
           WHEN OTHER                                          
             PERFORM ABNORMAL-EXIT-PARA                        
           END-EVALUATE                                        
           EXIT.                                               
      
       SEND-THE-DATA.                                          
      * paragraph send to the user only data output
      * in most of the cases it send user messages

           EXEC CICS                                           
           SEND MAP('MAP1') MAPSET('MAPLOG')                   
           DATAONLY                                            
           FREEKB                                              
           ERASEAUP                                            
           RESP(RESPCODE)                                               
           END-EXEC                                                     
      * in case of error termination

           EVALUATE RESPCODE                                            
           WHEN DFHRESP(NORMAL)                                         
             CONTINUE                                                   
           WHEN OTHER                                                   
             PERFORM ABNORMAL-EXIT-PARA                                 
           END-EVALUATE                                                 
           EXIT.                                                        
      
       PROCESS-DATA-PARA.                                               
      * first we need to get data user provided

           PERFORM GET-THE-DATA                                         
           EVALUATE TRUE                                                
           WHEN RECEIVED-SUCCESSFULLY                                   
      * here we know that reciving of data was successfull           
                                                                        
      * WE WILL CHECK IF LOGING DATA ARE ALREADY IN PASSF FILE          
      * IF NOT DATAF FILE WILL BE CHECKED IF THAT BANK NUMBER EXISTS   
      * IF NOT PROPER OUTPUT WILL BE DISPLAYED                       
                                                                     
             MOVE USER-BANK-ACCOUNT TO PASSF-BANK-ACCOUNT     

                 PERFORM CHECK-PASSF-FOR-ACCOUNT                     
                  EVALUATE TRUE                                      
                  WHEN PASSF-EXIST                                   
      * here we know that bank account is correct  and sitting in passf
      * file, now we will check if passwords are the same   
                  
                   IF PASSF-PASSWORD = USER-PASSWORD                 
                   THEN                                              
      * EVERYTHING IS CORRECT USER CAN BE LOGGED IN                  
                       PERFORM SEND-CORRECT-RESPONSE-PARA            
                   ELSE                                              
      * INVALID PASSWORD                                             
                       PERFORM SEND-INVALID-PASS-MSG                 
                   END-IF                                            
                  WHEN PASSF-NOT-EXIST                                 
      * NOTFND ERROR                                                   
      * WE NEED TO CHECK DATAF FILE ALSO                               
      * BECAUSE ACCOUNT CAN EXIST WHILE PASSWORD WASN'T CREATED YET    
                    MOVE USER-BANK-ACCOUNT TO DATAF-BANK-ACCOUNT       
                   PERFORM CHECK-DATAF-FOR-ACCOUNT                     
                    EVALUATE TRUE                                      
                    WHEN DATAF-EXIST                                   
      * THIS IS THE FIRST TIME USER IS SIGNING IN TO SYSTEM            
      * PASSWORD HE GAVE WILL BE SAVED IN PASSF FILE

                     MOVE USER-BANK-ACCOUNT TO PASSF-BANK-ACCOUNT      
                     MOVE USER-PASSWORD     TO PASSF-PASSWORD          
                         PERFORM SAVE-DATA-TO-FILE                     
                           EVALUATE TRUE                               
                           WHEN SAVED-SUCCESSFULLY                     
      * USER HAVE HIS NEW PASSWORD TO BANK ACCOUNT     
      * WE ARE SENDING TO THE USER INFO ABOUT
      * SUCCESSFULL CREATION OF THE PASSWORD

                             PERFORM SEND-PASS-CREATED-MSG             
                           WHEN OTHER                                 
      
      *         ERROR WHILE TRYING TO SAVE DATA TO PASSF FILE         
      
                             PERFORM SEND-ERROR-MSG                   
                           END-EVALUATE                               

                                         
                    WHEN DATAF-NOT-EXIST                              
      * here we know that user provided bank number
      * is not exist, we will send proper output

                      PERFORM SEND-NOTFND-ERROR-MSG                   
                    WHEN OTHER                                        
      *     error while cheking if dataf data is corect with user data

                    PERFORM SEND-ERROR-MSG                            
                    END-EVALUATE                                      
      
                  WHEN OTHER  
      * error while checking if passf data is correct               
                   PERFORM SEND-ERROR-MSG                             
                  END-EVALUATE                                        
           WHEN MAPFAIL-ERROR        
      * user didn't provided all data

                PERFORM MAPFAIL-ERROR-PARA                            
      
           WHEN OTHER-RECEIVE-ERROR                                   
      * as the title -> other error in reciving

                PERFORM SEND-ERROR-msg                             
           END-EVALUATE                                          
           EXIT.                                                 
      
       GET-THE-DATA.                                             
      * paragraph will get data from screen
      * and save it to correct variables
      * below output of that paragraph:
      * modified flag ->  IF-RECEIVED              
      * Y IF ALL IS CORRECT                                      
      * M IN THE CASE OF MAPFAIL ERROR                           
      * N IN OTHER CASES                                         
           MOVE LOW-VALUES TO MAP1I     

           EXEC CICS                                             
           RECEIVE MAP('MAP1') MAPSET('MAPLOG')                  
           INTO(MAP1I)                                           
           RESP(RESPCODE)                                        
           END-EXEC                                              
      * handling of the errors 

           EVALUATE RESPCODE                                     
           WHEN DFHRESP(NORMAL)                                  
      
                MOVE BNUMI TO USER-BANK-ACCOUNT                  
                MOVE PASSFI TO USER-PASSWORD                     
                MOVE 'Y' TO IF-RECEIVED                      
      
           WHEN DFHRESP(MAPFAIL)                             
                MOVE 'M' TO IF-RECEIVED                      
      
           WHEN OTHER                                        
                MOVE 'N' TO IF-RECEIVED                      
      
           END-EVALUATE                                      
           EXIT.                                             
                                   
       CHECK-PASSF-FOR-ACCOUNT.   
      * paragraph checks if data provided by the user are
      * in passf file
      * paragraph will modify IF-PASSF-EXIST flag

      * Y IF EXISTS                                          
      * N IF NOT FOUND                                        
      * O IF OTHER ERROR   

           EXEC CICS                                          
           READ                                               
           FILE('PASSF')                                      
           INTO(FS-PASSF)                                     
           RIDFLD(PASSF-BANK-ACCOUNT)                         
           RESP(RESPCODE)                                     
           END-EXEC           

      * handling of the errors
           EVALUATE RESPCODE                                  
           
           WHEN DFHRESP(NORMAL)                               
              MOVE 'Y' TO IF-PASSF-EXIST                      
           
           WHEN DFHRESP(NOTFND)                               
              MOVE 'N' TO IF-PASSF-EXIST                      
           
           WHEN OTHER                                         
              MOVE 'O' TO IF-PASSF-EXIST                      
           
           END-EVALUATE                                       
           EXIT.                                            
                    
       CHECK-DATAF-FOR-ACCOUNT.                             
      * paragraph will check if data provided by user are in
      * dataf file
      * paragraph will modify IF-DATAF-EXIST  flag

      * Y -> NORMAL                                         
      * N -> NOTFND                                         
      * O -> OTHER                                          
      
           EXEC CICS                                        
           READ FILE('DATAF')                               
           INTO(FS-DATAF)                                   
           RIDFLD(DATAF-BANK-ACCOUNT)                       
           RESP(RESPCODE)                                   
           END-EXEC                                         
      * handling errors

           EVALUATE RESPCODE                                
           WHEN DFHRESP(NORMAL)                             
      
              MOVE 'Y' TO IF-DATAF-EXIST                           
           WHEN DFHRESP(NOTFND)                                    
      
              MOVE 'N' TO IF-DATAF-EXIST                           
           WHEN OTHER                                              
      
              MOVE 'O' TO IF-DATAF-EXIST                           
           END-EVALUATE                                            
           EXIT.                                                   
                                
       SAVE-DATA-TO-FILE.                                          
      * PARAGRAPH WILL SAVE DATA TO PASSF FILE                     
      * AND USER WILL NOW HAVE PASSWORD                            
      * paragraph will be called only when:
      * - in dataf file is bank number specified by user
      
           EXEC CICS                                               
           WRITE FILE('PASSF')                                     
           FROM(FS-PASSF)                                          
           RIDFLD(PASSF-BANK-ACCOUNT)                                  
           RESP(RESPCODE)                                              
           END-EXEC                        

           EVALUATE RESPCODE                                           
           WHEN DFHRESP(NORMAL)                                        
      * DATA SUCCESSFULLY SAVED TO PASSF FILE                          
             MOVE 'Y' TO IF-SAVED-SUCCESSFULLY                         
                                                                       
      *    WHEN DFHRESP(DUPREC)                                        
      *         DATA ALREADY EXIST                                     
      *         THIS SHOULD NOT HAPPEN                                 
           WHEN OTHER                                                  
                                                                       
             MOVE 'N' TO IF-SAVED-SUCCESSFULLY                         
           END-EVALUATE                                                
           EXIT.                                                       
       
       SEND-CORRECT-RESPONSE-PARA.                                     
      * paragraph will send proper output when 
      * user provided valid password and valid b number

           MOVE CORRECT-RESPONSE TO MSGO              
           PERFORM SEND-THE-DATA                      
           EXIT.                                      
       SEND-ERROR-MSG.                                
      * standard error message will be sended to ther user

           MOVE ERROR-MSG TO MSGO                     
           PERFORM SEND-THE-DATA                      
           EXIT.                                      
      
       SEND-PASS-CREATED-MSG.                         
      * paragraph will be called when 
      * 1. in dataf file is valid bank number (with user provided)
      * 2. in passf file this number isnt exist

           MOVE PASS-CREATED-MSG TO MSGO              
           PERFORM SEND-THE-DATA                      
           EXIT.                                      
       SEND-NOTFND-ERROR-MSG.                                      
      * paragraph will send proper output while
      * user data wasn't find in dataf file

           MOVE NOTFND-ERROR-MSG TO MSGO                           
           PERFORM SEND-THE-DATA                                   
           EXIT.                   
       SEND-INVALID-PASS-MSG.                               
      * paragraph will send proper output when user 
      * will press other key than supported ones

           MOVE INVALID-PASS-MSG TO MSGO                    
           PERFORM SEND-THE-DATA                            
           EXIT.                        
       MAPFAIL-ERROR-PARA.                                   
      * paragraph will send proper output when user didn't provide
      * all data

           MOVE MAPFAIL-ERROR-MSG TO MSGO                    
           PERFORM SEND-THE-MAP                              
           EXIT.          
       INVALID-KEY-PARA.                                             
      * send iformation to the user about pressing wrong key

           MOVE INVALID-KEY-MSG TO MSGO                              
           PERFORM SEND-THE-DATA                                     
           EXIT.          