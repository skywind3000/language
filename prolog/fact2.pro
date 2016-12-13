factorial(0,F,F). 

factorial(N,A,F) :-  
   N > 0, 
   N1 is N-1, 
   A1 is N*A,
   factorial(N1, A1, F).

