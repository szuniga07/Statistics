* Encoding: UTF-8.
* Creates new variables by recoding text into 1 to 5 

RECODE q0002 q0006 q0008 q0013 q0015 q0026 q0027 q0028 
    ('Strongly Agree' = 5) ('Agree' = 4) ('Uncertain' = 3) ('Disagree' = 2) 
    ('Strongly Disagree' = 1) ('NA' = SYSMIS)  
    INTO q0002r q0006r q0008r q0013r q0015r q0026r q0027r q0028r.
EXECUTE.

