;***************************************************************************************************** 
;* Program Name: Proj2
;* Programmer:   Seth Norton
;* Class:        CSCI 2160-942
;* Lab:          Project 2: Matrix Operations
;* Date:         16 November 2020
;* Purpose:      This is a matrix operation calculator that does addition and multiplication 
;*               (whichever the user specifies) and is menu driven.
;*****************************************************************************************************
BR main   
Matr1P1: .BLOCK 2            ;a pointer to matrix one in the heap
Matr2P1: .BLOCK 2            ;a pointer to matrix two on the heap
Matr1D1: .BLOCK 2            ;the first dimension of matrix 1
Matr1D2: .BLOCK 2            ;the second dimension of matrix 1
Matr2D1: .BLOCK 2            ;the first dimension of matrix 2
Matr2D2: .BLOCK 2            ;the second dimension of matrix 2
Matr3D1: .BLOCK 2            ;the first dimension of the matrix 3 (our resulting matrix)
Matr3D2: .BLOCK 2            ;the first dimension of the matrix 3 (our resulting matrix)
Matr1DL: .BLOCK 2            ;the length of how big matrix 1 is
Matr2DL: .BLOCK 2            ;the length of how big matrix 2 is
Matr3DL: .BLOCK 2            ;the dimension length of our resulting matrix
MatrAPoi: .BLOCK 2           ;pointer to the a matrix
MatrBPoi: .BLOCK 2           ;pointer to the b matrix
ASCOFF:  .EQUATE 97          ;offset for ascii to determine decimal value of input to the switch (constant)
temp:    .BLOCK 2            ;temporary storage for a number when inputting values into the matrix
count:   .BLOCK 2            ;a counter variable for multiplication by addition
Scount:  .BLOCK 2            ;a counter for how many characters in a string we have 
numElem: .BLOCK 2            ;how many elements in our matrix we should have 
sLength: .BLOCK 2            ;keeps track of how big our string is on the stack
;********void extractMatrix(char *1pInput, short *matrix)
;        This procedure accepts a pointer to a string of input characters and a pointer to an array to store the matrix
;        parsed from the string input in. Assume the user input valid data
;
a:       .EQUATE 0           ;used to store the number which we will multiply by ten
Sin:     .EQUATE 4           ;the string input which we are reading into our function
pMatr:   .EQUATE 2           ;the memory address where are matrix is in memory
negFlag: .BLOCK 2            ;a flag which will be 1 if the number we parse in is negative and 0 otherwise
MatrAddr:.BLOCK 2            ;the address of the matrix which we will pass back to use as our Matrix A or Matrix B
MatrInd: .BLOCK 2            ;used to store numbers in the correct position of our matrix
sIndex:  .BLOCK 2            ;used to store a string index when we are multiplying by ten
strInd:  .BLOCK 2            ;a counter we will use to index the string
tempTen: .BLOCK 2            ;a temporary variable to store the number we are multiplying by ten
tempIn:  .BLOCK 2            ;our input stored as a temporary variable
pTemp:   .Block 2            ;a pointer we will use to traverse the array
dFlag:   .BLOCK 2            ;signifies we are done with decimal input
spaceBe: .BLOCK 1            ;boolean to check and see if we hit a space or tab before
r:       .BLOCK 1            ;a random block to pad out memory so we dont read a stop instruction
exMatr:  LDWX 0, i           ;make sure the indexer is starting where the beginning of the array is 
         LDWA 0,i            ;clear the indexer to setup for the extract matrix method
         STWA spaceBe,d      ;clear spaceBe because we will check if there is a space prior to a space so we know we can skip it
         STWA MatrInd,d      ;used to index our matrix
         STWA tempTen,d      ;make sure our temp variable is zero
         STWA MatrAddr, d    ;initialize the matrix address to zero just to make sure we clear it from any previous inputs
         STWA dFlag, d       ;makes sure the done flag is set to zero
         STWA negFlag,d      ;makes sure the negative flag (determine if the string we are parsing in should be a negative number)
         LDWA pMatr,s        ;load the pointer to the matrix we passed in into the accumulator so we can store where we need to store the 
                             ;matrix
         STWA MatrAddr,d     ;store the starting point of where we are going to store the matrix
         LDWA 0,i            ;initialize the accumulator to zero because we will load a byte into the accumulator and we dont need to
                             ;grab whatever is in the left half of the accumulator
loop:    LDBA Sin, sfx       ;points to the beginner of the array and then uses (negative) indexing
         STBA pTemp, d       ;store our character that we just loaded in so we can reference it later
         LDWA 0, i           ;set the accumulator back to zero to prepare for the string input from the stack
                             ;Working with the data we grabbed from the string
         LDBA pTemp,d        ;use indirect addressing to actually grab the data from the string
         CPBA '-', i         ;if it is a negative sign then we need to set a flag
         BRNE Noneg          ;if it is not negative then we can skip over setting the sign flag
         LDWA 1,i            ;we set the negative flag to one because it is negative           
         STWA negFlag,d      ;store one to the variable which we will check if negative
         BR subInd           ;after we set the flag for the negative we can done to the bottom and go to the next number
Noneg:   CPBA ' ', i         ;check if it is a space and if not then get rid of the data
         BRNE tabC           ;branch if it is a space because we dont need to multiply by ten 
         ADDX 1,i            ;Add one to the indexer so we can check if there was a space before this space
         LDBA Sin,sfx        ;load the character into the accumulator after adding one to the index to check for a space
         CPBA ' ',i          ;see if the character is a space
         BRNE SpSkip         ;if the character is not a space than we can skip over our index
         SUBX 1,i            ;subtract one from the indexer because we need to go to our current character we are parsing in
         BR subInd           ;we can branch to set up for the next iteration of the loop
SpSkip:  SUBX 1,i            ;subtract one from the indexer to go to the current character we are dealing with
         LDBA Sin,sfx        ;load the current character we are dealing with into the accumulator
         BR DSkip            ;branch to store the variable in temp ten
tabC:    CPBA '\t', i        ;see if the byte we are comparing is a tab and if it is 
         BRNE newlC          ;if it is not a tab then we can skip over this section of the code because we do not need to store anything
         BR DSkip            ;branch to where we will store the number because it is a tab
newlC:   CPBA '\n', i        ;if we hit a new line then we can break out after storing our last input                        
         BRNE noLine         ;if it is not equal to a new line then we can skip over this section because this section sets a flag if it 
                             ;is a new line
         LDWA 1,i            ;load one into the accumulator so we can set our flag
         STWA dFlag,d        ;set a flag so we know that we hit a new line
         LDBA pTemp,d        ;load the original byte back into the accumulator
noLine:  CPBA '\n', i        ;have to do the original comparison again
         BREQ Done           ;if we hit a new line then we know we are done and we have to store the last input
         BR skip             ;if it passes all the previous checks then we know it must be a number
                             ;DSkip is where we will actually store the number because we know there has been a space (or tab)
DSkip:   LDWA 0,i            ;clear the accumulator so we can do addition
         STWX strInd, d      ;store our string indexer because we are going to load into the indexer 
         LDWA tempTen, d     ;load what we calculated for one 
         LDWX MatrInd, d     ;load our indexer for that matrice back into the indexer so we know where to store the number we calculated
         LDWA negFlag,d      ;load our negate flag into the accumulator and see if it was set or not
         CPWA 1,i            ;before we store in our matrix we have to check if the number we are storing should be negative
         BRNE NoNeg          ;branch if we dont need to invert our number
         LDWA tempTen,d      ;load the number we calculated into the accumulator so we can negate it
         NEGA                ;we now have our negative number in the accumulator which we then store
         STWA tempTen,d      ;store our now negated calculated number
NoNeg:   LDWA tempTen,d      ;load the number we want to store back to the accumulator
         STWA pMatr,sfx      ;store the byte in where our matrix is
         LDWA dFlag,d        ;load the flag to see if we are done into the accumulator
         CPWA 1,i            ;see if the flag is equal to one 
         BREQ Done           ;we can return from the method
         LDWA 0,i            ;clear the negative flag
         STWA negFlag, d     ;store our newly cleared negative flag for next time around when we store a number
         ADDX 2,i            ;add two to our matrice indexer
         STWX MatrInd,d      ;store our matrix indexer back to its variable
         LDWA 0, i           ;load zero into our indexer to clear our temporary ten variable
         STWA tempTen,d      ;we are done with our temporary input so set it back to zero
         LDWA 1,i            ;set a boolean to know that there was a space before so we know we can get rid of any other whitespace
         STBA spaceBe,d      ;we know there was a space prior so set our space before variable
         LDWX strInd, d      ;load back our string indexer
         BR subInd           ;we can go to the next iteration of the loop         
skip:    SUBA 48,i           ;subtract 48 from the accumulator to get the decimal number from the ascii value
         STWA tempIn, d      ;store the decimal number as a temporary variable
         LDWA tempTen,d      ;load what we have in temp ten to see if we need to add to it
         CPBA 0,i            ;see if nothing is stored in it
         BREQ sMult          ;skip multiplying by ten because nothing is in the temp variable
                             ;we have something in the accumulator which we need to multiply by ten
         STWA -2, s          ;store what we have in the accumulator so we can multiply by ten
         STWX sIndex, d      ;push the number which we will add to itself
         SUBSP 2, i          ;push onto the stack 
         LDWA 0,i            ;clear the accumulator
         LDWA a, s           ;load the number into the accumulator which we will multiply by ten
                             ;need to store the indexer back to strInd and load zero into the indexer
         STWX strInd,d       ;store our string indexer because we aregoing to use the index register
         LDWX 0,i            ;clear the index register to prepare for multiplying by ten
tenL:    CPWX 9, i           ;we need to add the number to itself ten times to multiply by ten
         BREQ DAdd           ;branch if we added to itself ten times
         ADDA a, s           ;add the number in the accumulator to itself (we stored it on the stack)
         ADDX 1, i           ;increment the index register which lets us know when we will stop
         BR tenL             ;branch back to continue adding to the accumulator ten times
DAdd:    LDWX strInd,d       ;load our indexer for the string back into the indexer
         ADDA tempIn,d       ;add our 'ones' digit to our tens
         ADDSP 2,i           ;pop our temporary variable which we were adding ten to off the stack
         STWA tempTen,d      ;store our variable which we just multiplied by ten
         LDWX strInd, d      ;put back the indexer into the accumulator
         BR subInd           ;branch to go to the next iteration of the loop
sMult:   LDWA tempIn, d      ;load our decimal integer we are currently on if nothing is in tempTen
         STWA tempTen,d      ;since nothing is in tempTen we can store our tempIn into tempTen
subInd:  ADDX -1,i           ;go to the next character on the stack
         STWX strInd,d       ;store the indexer
         BR loop             ;branch back to continue taking input from the stack
Done:    LDWA tempTen,d      ;load and see if we have anything else we need to store
         CPWA 0,i            ;see if the accumulator is equal to zero because we do not need to store that number
         BREQ NoTen          ;branch if there is nothing in temp ten that we need to store
         LDWA negFlag,d      ;load the flag that tells us if we need to negate our number
         CPWA 1,i            ;before we store in our matrix we have to check if the number we are storing should be negative
         BRNE NoNega         ;branch if we dont need to invert our number
         LDWA tempTen,d      ;load the last number we calculated
         NEGA                ;we now have our negative number in the accumulator which we then store
         BR NegaY            ;branch over because we want to keep in the accumulator the number we just negated
NoNega:  LDWA tempTen,d      ;load what we have in tempTen because we dont need to negate what is in tempTen
NegaY:   LDWX MatrInd,d      ;store what we calculated for tempTen in the appropriate place in the matrix
         STWA pMatr,sfx      ;store the byte in where our matrix is 
NoTen:   RET                 ;we have hit a new line so we can return from the method
;
;
;         
;********void displayMatrix(short *matrix, byte rows, byte cols)
;        This procedure accepts a pointer to an array of shorts that represents the matrix, the number of rows in the
;        matrix, and the number of columns in the matrix
;
;
matrix:  .EQUATE 4           ;a parameter passed into the function to point to the beginning of the matrix
rows:    .EQUATE 3           ;a parameter passed into the function to say how many rows are in the matrix            
cols:    .EQUATE 2           ;a parameter passed into the function to say how many columns are in the matrix we are displaying
rowCou:  .BLOCK 1            ;a counter for seeing how many rows we have done
colCou:  .BLOCK 1            ;a counter to determine what column we are on in the matrix
elem:    .BLOCK 2            ;a counter used to see how many elements we stored in the matrix
elemC:   .BLOCK 2            ;a variable which tells us how many elements we need to store in our matrix
dispSt:  LDWA 0,i            ;load zero into the accumulator because we will need to clear all our variables
         STWA elemC,d        ;clear elemC from previous iterations of displaying matrices
         STWA elem,d         ;clear elme from previous iterations of displaying matrices
         LDBA cols,s         ;load how many columns are in our matrix because we need to calculate how many elements we are storing
         STWA -2,s           ;put the columns onto the stack to multiply by the number of rows
         LDBA rows,s         ;load the rows into the accumulator to multiply with the number of columns in the matrix
         STWA -4,s           ;put the rows onto the stack so we can determine how many elements are in the matrix
         SUBSP 4,i           ;push the rows and columns onto the stack for our function
         CALL AddMul         ;call addMul which will tell us how many elements are in our matrix 
         ADDSP 4,i           ;pop the rows and columns off of the stack
         STWA elemC,d        ;store how many elements are in our matrix so we can use this element as a counter
         LDBA 0,i            ;initialize the row and column count to 0
         LDWX 0,i            ;clear the index register
         STBA rowCou,d       ;set row count to zero
         STBA colCou,d       ;set column count to zero 
                             ;PLAN   print out pipe on each row beginning and end. Then see how big the number is )
pipeL:   STRO pipe,d         ;print out a pipe to start the line            
nextN:   LDWA matrix,sfx     ;point to the matrix on the heap
         CPWA 0,i            ;see if the number we are displaying is greater than zero
         BRLT double         ;if it is less than zero than we can branch because we do not need an extra space
         CPWA 9, i           ;see if the number in our array is bigger than nine
         BRGT double         ;branch to double because we do not need an extra space
         STRO space, d       ;add another space if the number is less than nine
double:  CPWA -9,i           ;check to see if the number is less than nine and if so then skip over this code
         BRLT triple         ;check to see if the number is less than nine and if so then skip over this code
         CPWA 99,i           ;see if the number is in the triple digits 
         BRGT triple         ;if the number is in the triple digits then we dont need to add a space
         STRO space,d        ;if the number is in the double digits then we need to add a space
triple:  LDWA matrix,sfx     ;load the number we are comparing into the accumulator again
         CPWA -99,i          ;if the number is less than -99 then we dont need to add another space
         BRLT quad           ;we can skip this section of code which adds another space if its less than -99
         CPWA 999,i          ;if the number we are comparing is greater than 999 then we dont need to add another space
         BRGT quad           ;if the number we are comparing is greater than 999 then we dont need to add another space
         STRO space,d        ;add another space if the number we are comparing is less than 999
quad:    LDWA matrix,sfx     ;load the number we are comparing into the accumulator again
         CPWA -999,i         ;we can skip this section of code which adds another space if its less than -999
         BRLT quin           ;we can skip this section of code which adds another space if its less than -999
         CPWA 9999,i         ;if the number we are comparing is greater than 9999 then we dont need to add another space
         BRGT quin           ;if the number we are comparing is greater than 9999 then we dont need to add another space
         STRO space,d        ;add another space if the number we are comparing is less than 9999
quin:    LDWA matrix,sfx     ;load the number we are comparing into the accumulator again
         CPWA -9999,i        ;we can skip this section of code which adds another space if its less than -9999
         BRLT Sskip          ;we can skip this section of code which adds another space if its less than -999
         STRO space,d        ;add another space if the number we are comparing is less than 9999
Sskip:   ADDA 48,i           ;add 48 to the number in the accumulator to get the ascii value
         DECO matrix,sfx     ;dont need to do any more comparisons because you can only have a triple digit number
         ADDX 2,i            ;increment the index register by two to go to the next number
         STRO space,d        ;add another space for spacing
                             ;check to see if we are done
         LDBA colCou,d       ;load the column count into the accumulator to increase it   
         ADDA 1,i            ;add one to the accumulator to say that we have done one more element in the matrix
         STBA colCou,d       ;store the incremented column count
         CPBA cols,s         ;see if the column count is the columns in the matrix
         BRLT skipNL         ;we can skip over if the column count is less than the columns in the matrux
         LDWA 0,i            ;set cols back to zero
         STBA colCou,d       ;store zero back to the column count
         STRO space,d        ;output a space for spacing
         STRO pipe,d         ;print out a pipe and a new line at the end
         LDBA rowCou,d       ;load the row count into the accumulator so we can increase it
         ADDA 1,i            ;add one to the row count because we are going to a new line
         STBA rowCou,d       ;store the newly incremented row count for future reference
         CPBA rows,s         ;see if the rows is equal to how many rows there should be 
         BREQ dispEn         ;branch if the rows are equal because we know we are done
         STRO newline,d      ;print a newline for spacing
                             ;see if we are done outputting
         LDWA elem,d         ;load elements into the accumulator so we can increment it
         ADDA 1,i            ;we displayed one more element so we can increment the count of elem
         CPWA elemC,d        ;see if we stored the amount of elements we were supposed to store
         BREQ dispEn         ;branch if we stored the amount of elemnents we were supposed to store
         LDWA elem,d         ;load how many elements we stored
         ADDA 1,i            ;add one because we stored another
         STWA elem,d         ;actually store the number of elem we stored because we didnt do so before
         BR pipeL            ;branch back to where we begin another line
skipNL:  LDWA elemC,d        ;load how many elements we were supposed to store
         CPWA elem,d         ;see if we stored that many elements
         BREQ dispEn         ;if we stored that many elements than we can exit the method
         LDWA elem,d         ;load how many elements we currently have stored
         ADDA 1,i            ;add one to the count of how may elements we actually have stored
         STWA elem,d         ;store the newly incremented counter to how many elements we have stored 
         BR nextN            ;branch to the next number in the loop   
dispEn:  STRO newline,d      ;add a newline for spacing
         RET                 ;return from the method
;
;********short multMatrices(short *matrixA, byte rowsA, byte colsA, short *matrixA, byte rowsA, byte colsA)
;        This function accepts two pointers to arrays of shorts (represented as matrices), along with associated rows
;        and columns for each matrix. After verifying the addition may occur (display an error message and return a
;        NULL pointer if not), the function performs the matrix addition and returns a pointer (allocated from the
;        ?heap??memory after your program) to the resulting summed matrix. 
;
;
paMatrA:   .EQUATE 2         ;the variable relative to the stack which points to matrix A
mrowsA:    .EQUATE 4         ;the variable relative to the stack which is how many rows are in matrix A
mcolsA:    .EQUATE 5         ;the variable relative to the stack which is how many columns are in matrix A         
paMatrB:   .EQUATE 6         ;the variable relative to the stack which points to matrix B
mrowsB:    .EQUATE 8         ;the variable relative to the stack which is how many rows are in matrix B
mcolsB:    .EQUATE 9         ;the variable relative to the stack which is how many columns are in matrix B
MatrCPoi:  .EQUATE 11        ;the variable relative to the stack which points to matrix C
rowCou1:   .BLOCK 2          ;a variable which we will use as a counter for the rows of matrix A
colCou1:   .BLOCK 2          ;a variable which we will use as a counter for the columns of matrix A
rowCou2:   .BLOCK 2          ;a variable which we will use as a counter for the rows of matrix B
colCou2:   .BLOCK 2          ;a variable which we will use as a counter for the columns of matrix B         
MatrAIn:   .BLOCK 2          ;a variable which keeps track of the indexer for matrix A
MatrBIn:   .BLOCK 2          ;a variable which keeps track of the indexer for matrix B 
MatrCIn:   .BLOCK 2          ;a variable which keeps track of the indexer for matrix C
tempAdd:   .BLOCK 2          ;store our addition in a temporary variable
tmpIn:     .BLOCK 2          ;a block of memory to store our temporary index when we are calculating the correct row index
stvarLoc:  .BLOCK 2          ;the locations of our numbers we calculate on the stack
multMatr:LDWA 0,i            ;clear the accumulator to prepare for matrix multiplication
         LDWX 0,i            ;clear the indexer to prepare for matrix multiplication      
         STWA MatrCIn,d      ;clear all are variables to zero to prepare for the function
         STWA MatrAIn,d      ;clear all are variables to zero to prepare for the function
         STWA MatrBIn,d      ;clear all are variables to zero to prepare for the function
         STWA rowCou2,d      ;clear all are variables to zero to prepare for the function
         LDWA 1,i            ;load one into the accumulator to set the rows to 1
         STWA rowCou1,d      ;set the row count of the first matrix to 
         STWA colCou1,d      ;set the column count of matrix 1 to 1 because we are starting at the first column
         STWA colCou2,d      ;set the column count of matrix 2 to one because we are starting at the first column
         LDWA 0,i            ;load zero into te accumulator because we will load a byte afterwards
                             ;DIMENSION CHECKING
         LDBA mcolsA,s       ;load the columns of A into the accumulator to see if it equals the rows of B meaning matrix 
                             ;multiplication is possible
         CPBA mrowsB,s       ;check to see if the columnns of A is equal to the rows of B 
         BRNE NotP           ;branch to symbol meaning not possible
                             ;if it is possible allocate enough space for the resulting matrix
         LDBA mrowsA,s       ;load the rows of matrix A intor the accumulator so we can set aside space for matrix C
         STWA Matr3D1,d      ;set the row dimension of the third matrix 
         LDBA mcolsB,s       ;load the columns of matrix B into the accumulator so we can set aside space for matrix B
         STWA Matr3D2,d      ;set the column dimension of the third matrix 
         LDWA 0,i            ;clear the accumulator
         LDWA Matr3D1,d      ;load the rows of matrix C into the accumulator so we can set aside space
         STWA -2,s           ;store the rows of matrix C onto the stack because we are going to set aside space for the matrice
         LDWA Matr3D2,d      ;load the columns of matrix C into the accumulator so we can set aside space
         STWA -4,s           ;store the columns of matrix C onto the stack because we are going to set aside space for the matrice
         SUBSP 4,i           ;push the rows and columns of matrix C onto the stack so we can call a method to set aside space for the 
                             ;matrix
         CALL AddMul         ;call addMul to see how much storage we need to set aside 
         ADDSP 4,i           ;pop the rows and columns off the stack
         ASLA                ;shift left on the element count because we are using words
         CALL malloc         ;call malloc to allocate space for matrix C
         STWX MatrCPoi,s     ;store the pointer to something we can reference later
         STWX MatrOff,d      ;store the pointer to something we can reference later
                             ;START the actual algorithm for multiplication
                             ;matrix multiplication
         LDWX 0,i            ;clear the indexer from allocating memory on the heap
Mloop:   LDWX MatrAIn,d      ;we need to load the indexer for the A array to ensure we get the correct number
         LDWA paMatrA,sfx    ;load what we are pointing to in matrix A (traversing the row of matrix A)
         STWA -2,s           ;store the number onto the stack because we are going to multiply it
         LDWX MatrBIn,d      ;we need to load the indexer for the B array to ensure we get the correct number (determined by our indexer)
         LDWA paMatrB,sfx    ;load the number we are pointing to in the B matrix so we can multiply it
         STWA -4,s           ;store the number on the stack so we can multiply it
         SUBSP 4,i           ;push the two numbers we are multiplying on the stack
         CALL AddMul         ;call AddMul to multiply the two numbers we pushed onto the stack 
         ADDSP 4,i           ;pop the two numbers we called for AddMul
         LDWX 0,i            ;clear indexer from add multiply
         ADDA tempAdd,d      ;the result of the previous multiply
         STWA tempAdd,d      ;keep a tally of each element in the matrix
         LDWA rowCou2,d      ;load rowCou2 into the accumulator
         ADDA 1,i            ;add one to rowCou2 because we are moving down a row
         STWA rowCou2,d      ;store the newly incremented rowCou2 for future reference
         LDWX MatrAIn,d      ;we need to load the indexer for the A array to ensure we get the correct number
         ADDX 2,i            ;go to the next element in each array
         STWX MatrAIn,d      ;we need to load the indexer for the A array to ensure we get the correct number
         CPWA Matr2D1,d      ;see if the index of matrix A is equal to the first dimension of matrix 2 and if it is branch
         BREQ colRes         ;see if the index of matrix A is equal to the first dimension of matrix 2 and if it is branch
         STWA colCou1,d      ;store the new column count of matrix A
         LDBA mcolsB,s       ;go to the next element in each array
                             ;To go to the next element in the array we have to multiply the number of columns by two because
                             ;everything is stored as words
         STWA -2,s           ;pass the columns of matrix A to our multiply method
         LDWA 2,i            ;multiply the columns of matrix B by 2 to use it as an index
         STWA -4,s           ;store the columns on th stack because we are going to pass it as a parameter to AddMul
         SUBSP 4,i           ;push the columns and 2 to AddMult to get how much we should add to the indexer of Matrix B
         CALL AddMul         ;call AddMult to see what are new Matrix B index should be
         ADDSP 4,i           ;pop 2 and the columns of B off the stack to cleanup the stack
         ADDA MatrBIn,d      ;add to the accumulator our current position of the matrix B indexer to get the correct Matrix B index for next time around
         STWA MatrBIn,d      ;store the newly calculated index for Matrix B
         BR Mloop            ;branch back up to continue row and column multiplication
colRes:  LDWX MatrCIn,d      ;load the index to the C matrix
         LDWA tempAdd,d      ;load what we calculated for one entry into the matrix
         STWA MatrCPoi,sfx   ;store what we calculated into matrix C (with using MatrCIn as our indexer)
         LDWA 0,i            ;clear the accumalator
         LDWA Matr3D2,d      ;load the columns of matrix C into the accumulator
         STWA -2,s           ;pass the columns of matrix C onto the stack
         LDWA Matr3D1,d      ;load the rows of matrix C into the accumulator
         STWA -4,s           ;pass the rows of matrix C onto the stack
         SUBSP 4,i           ;push the rows and columns of matrix C onto the stack
         CALL AddMul         ;call AddMul to see if our indexer should say we are done    
         ADDSP 4,i           ;pop the rows and columns of matrix C off the stack
         ASLA                ;shift left because we are using words
         SUBA 2,i            ;subtract from the accumulator two because we are using indexes 
         CPWA MatrCIn,d      ;see if what we calculated is equal to our current index
         BREQ MFin           ;if it is equal then we know we are done
         LDWA MatrCIn,d      ;load the Matrix C indexer and add two to offset the other input
         ADDA 2,i            ;add one to offset for the next iteration
         STWA MatrCIn,d      ;store the newly calculated indexer
         LDWA 0,i            ;set temp add back to zero
         STWA tempAdd,d      ;set temp add back to zero
moveIn:  LDWA colCou2,d      ;load the counter for the column of the second matrice
         CPBA mcolsB, s      ;see if we are at the end of the columns on matrix B 
         BREQ else1          ;if we are at the end of the columns then we need to go to the next row
if1:     ADDA 1,i            ;add one to the column count because we are going over one column in the matrix
         STWA colCou2,d      ;store our newly calculated column count
         SUBA 1,i            ;we are going to use the column count as an index so subtract one
         STWA -2,s           ;we are going to multiply by two to use as an index
         LDWA 2,i            ;multiply by two because we are using words
         STWA -4,s           ;store what we are multiplying by onto the stack
         SUBSP 4,i           ;push elements onto the stack
         CALL AddMul         ;multiply our two numbers
         ADDSP 4,i           ;pop the column count for matrix 2 and 2 off the stack
         STWA MatrBIn,d      ;store our calculated index for matrix B
         LDBA mcolsA,s       ;load the columns of matrix A into the accumulator so we can set the appropriate index
         STWA -2,s           ;store the columns on the stack to pass to our multiply method
         LDWA -2,i           ;load -2 into our accumulator because we will multiply by -2
         STWA -4,s           ;store -2 onto the stack because we are passing it as a parameter
         SUBSP 4,i           ;push elements onto the stack
         CALL AddMul         ;call our multiply method to multiply the columns of matrix A by -2
         ADDSP 4,i           ;pop -2 and the columns of matrix A off the stack
         ADDA MatrAIn,d      ;add what we just calculated to the current index for matrix A
         STWA MatrAIn,d      ;store what we just calculated to be the new index for matrix A
         LDWA 0,i            ;load zero into the accumulator so we can clear rowCou2
         STWA rowCou2,d      ;clear rowCou2 because we are going to the first row 
         BR elseEn           ;branch to go back to the beginning of multiplying 
else1:   LDWA 0,i            ;clear the accumulator beecause we need to make the index of matrix B zero
         STWA MatrBIn,d      ;make the index of matrix B zero
         LDWA 0,i            ;clear accumulator because we are going to load a byte then store a word
         LDBA mcolsA,s       ;we need to multiply by the columns to get to the next row
         STWA -2,s           ;we are going to multiply by two to use as an index
         LDWA 2,i            ;clear accumulator because we are going to load a byte then store a word
         STWA -4,s           ;store 2 onto the stack because we are multiplying by two
         SUBSP 4,i           ;push elements onto the stack
         CALL AddMul         ;multiply the columns by two to get an index
         ADDSP 4,i           ;pop the columns of matrix A and two off the stack
         STWA -2,s           ;we need to multiply by the rowcount
         LDWA rowCou1,d      ;load the row count of matrix A into the accumulator 
         STWA -4,s           ;store the row count of matrix A onto the stack because we are multiplying by it
         SUBSP 4,i           ;push the row count of matrix A and our previous calculated index onto the stack
         CALL AddMul         ;returns our newly calculated A index
         ADDSP 4,i           ;pop both our parameters off the stack
         STWA MatrAIn,d      ;store our newly calculated A index
         LDWA rowCou1,d      ;We need to increment the row count of matrix 1 by one because we are going to a new row
         ADDA 1,i            ;add one to our row count because we are going to the next
         STWA rowCou1,d      ;we need to multiply the row count of matrix one by the column count 
         CPWA mrowsA,s       ;if we go past our number of rows then we know we are done
         BRGT MFin           ;if the rows of matrix A is greater then we know we are done multiplying 
         LDWA 1,i            ;load one into the accumulator because we need to set the second column count to 1
         STWA colCou2,d      ;load the counter for the column of the second matrice  
         LDWA 0,i            ;set rowCou2 back to zero because we are going to reset to pointing to the first row
         STWA rowCou2,d      ;set rowCou2 back to zero because we are going to reset to pointing to the first row
         BR elseEn           ;branch to elseEn to continue matrix multiplication
elseEn:  BR Mloop            ;branch back to Mloop to continue matrix multiplication
MFin:    LDWA MatrCPoi,s     ;load pointer into the accumulator to return from the method 
         RET                 ;return from the method so we can go back to main
NotP:    STRO NotPossM,d     ;display a message to the user saying it is not possible
         LDWA 0,i            ;return a null pointer if matrix multiplication is not possible
         RET                 ;return from the method to go back from main
;********short addMatrices(short *matrixA, byte rowsA, byte colsA, short *matrixB, byte rowsB, byte colsB)
;       This function accepts two pointers to arrays of shorts (represented as matrices), along with associated rows
;       and columns for each matrix. After verifying the multiplication may occur (display an appropriate error 
;       message and return a NULL pointer if not), the function performs the matrix multiplication and returns a
;       pointer (allocated from the ?heap??memory) to the resulting product matrix. 
;
;
pasMatrA: .EQUATE 2          ;the variable which refers to the pointer of matrix A relative on our stack
rowsA:    .EQUATE 4          ;the variable which refers to the rows of A relative on our stack
colsA:    .EQUATE 5          ;the variable which refers to the columns of A relative on our stack
pasMatrB: .EQUATE 6          ;the variable which refers to the pointer of matrix B relative on our stack
rowsB:    .EQUATE 8          ;the variable which refers to the rows of B relative on our stack
colsB:    .EQUATE 9          ;the variable which refers to the columns of B relative on our stack
element:  .BLOCK 2           ;used to determine what indexes we are adding
MatrOff:  .BLOCK 2           ;offset for the C matrix (actually a memory address just used for this function)
addMatr: LDWA 0,i            ;clear the accumulator to setup for addition
         LDWX 0,i            ;clear the indexer to setup for addition
         STWA MatrOff,d      ;clear our matrix offset to setup for this method
                             ;DIMENSION CHECKING
         LDBA rowsA,s        ;check if they are the same dimensions
         CPBA rowsB,s        ;if matrix A rows are not the same as matrix B then addition is not possible
         BRNE NP             ;branch to symbol meaning not possible
         LDBA colsA,s        ;check if they are the same dimensions
         CPBA colsB,s        ;if matrix A columns are not the same as matrix B then addition is not possible
         BRNE NP             ;branch to symbol meaning not possible
                             ;if it is possible allocate enough space for the resulting matrix
         LDWA 0,i            ;clear the accumulator because we are loading a byte
         LDBA rowsA,s        ;load the rows of A into the accumulator to see how much storage we should allocate  
         STWA -2,s           ;store the rows of A onto the stack as a word because our function takes a word
         LDBA colsA,s        ;load the columns of A into the accumulator to see how much storage we should allocate
         STWA -4,s           ;store the columns of A onto the stack as a word because our function takes a word
         SUBSP 4,i           ;push the rows and columns to our multiply method 
         CALL AddMul         ;call addMult which will calculate how many elements we need in our array 
         ASLA                ;shift left on the element count because we are using words
         STWA element,d      ;store how many elements are in our array
         CALL malloc         ;allocate on the heap memory that we will use for the first heap
         ADDSP 4,i           ;pop the rows and columns off our stack
         STWX MatrCPoi,d     ;store the pointer to something we can reference later
         STWX MatrOff,d      ;store the same thing in our indexer as an offset
                             ;store that in a pointer
         ;                   start the addition in a loop
         LDWX 0,i            ;clear the indexer because we had our pointer stored in the indexer
Floop:   CPWX element,d      ;see if our indexer is equal to our counter
         BREQ finis          ;we are finished if the indexer is equal to our element counter
         LDWA pasMatrA,sfx   ;load the element in matrix A that corresponds to our index register
         ADDA pasMatrB,sfx   ;Add to the accumulator the element in matrix B that corresponds to the same index
         STWA MatrOff,n      ;store the word in our accumulator as a matrix offset
         LDWA MatrOff,d      ;load our matrix offset so we can add to to it(go to the next number to add)
         ADDA 2,i            ;add two to the indexer (go to the next number to add)
         STWA MatrOff,d      ;store our matrix offset indexer for future use
         ADDX 2,i            ;increment index by two
         BR Floop            ;unconditional branch to continue the for loop
finis:   LDWA MatrCPoi,d     ;return the pointer to the C matrix
         RET                 ;return from our method because we are done
NP:      STRO NotPossA,d     ;if the dimensions do not match then we know it is not possible so we display a message saying it is not 
                             ;possible
         LDWA 0,i            ;return a null pointer if not possible
         RET                 ;return from our method because we did all we can do

;OLD METHOD OF MULTIPLICATION
;
;
;********int AddMult(int a, int b)
;;        temporary function to multiply by adding
;mpcnd: .EQUATE 6               ;our multiplicand which we will add to itself over and over again
;mpr:   .EQUATE 8               ;out multiplier which we will add our multiplicand n (mpcnd) times
;temps: .EQUATE 2               ;a variable which we will use for temporary storage
;                               ;Multiplication by adding
;AddMul:  SUBSP 4,i             ;push temp to make room for local variables
;         LDWA mpcnd,s          ;load our multiplicand into the accumulator to prepare for multiplication
;         LDWX 1,i              ;load 1 into our indexer to prepare for multiplication
;loop1:   CPWX mpr,s            ;see if we added the multiplicand mpr times
;         BREQ Sto              ;if we added our multiplicand mpr times then branch
;         ADDA mpcnd,s          ;add to the accumulator the multiplicand to continue our multiplication
;         ADDX 1,i              ;add one to our index register (which we are using as our count)
;         BR loop1              ;branch back up to continue adding the multiplicand mpr times
;Sto:     STWA temps,s          ;store our result into a temp variable
;         LDWA mpr,s            ;load our multiplier into the accumulator to see if it is one
;        CPWA 1,i               ;if the multiplier is one then we can just load the multiplicand
;         BRNE No               ;branch if the mpr is not one 
;         LDWA mpcnd,s          ;if the multiplier is just one then load mpcnd into the accumulator
;No:      LDWA temps,s          ;load our temp variable which is our result into the accumulator so we can return that number
;         ADDSP 4,i             ;pop temp to clean up the stack
;         RET                   ;return from our function with the number we multiplied in the accumulator

;NEW METHOD OF MULTIPLICATION
;
;********int AddMult(int a, int b)
;;        Multiplication by shift adding
prod:  .BLOCK 2                ;block of storage to store the product we calculate
cnt:   .BLOCK 2                ;block of storage that counts how many times we execute our multiplication loop
tempa: .BLOCK 2                ;block of storage to hold whatever we need to store in the accumulator
tempx: .BLOCK 2                ;block of storage to hold whatever we need to store in the indexer
BNeg:  .BLOCK 2                ;holds a flag to tell us if we need to negate the number
mpcnd: .EQUATE 2               ;where our multiplicand is relative to the stack which we are multiplying mpr times
mpr:   .EQUATE 4               ;where our mpr is relative to the stack which we are using to multiply the mpcnd
                               ;Multiplication
AddMul: LDWA 0,i               ;clear the accumulator to setup for the method
      LDWX 0,i                 ;clear the indexer to setup for the method
      STWA prod,d              ;clear prod to setup for the method
      STWA cnt,d               ;clear cnt to setup for the method
      STWA BNeg,d              ;clear BNeg to setup for the method
      LDWA mpcnd,s             ;load the mpcnd into the accumulator to setup for multiplication
      LDWX mpr,s               ;load the mpr into the indexer to setup for multiplication
      CPWX 0,i                 ;see if the indexer is less than zero because we cant have a negative indever
      BRGT top                 ;if the indexer is greater than zero then we dont hav eto flip the indexer
      NEGX                     ;if it is negative then make it positive
      LDWA 1,i                 ;now we need to make a note that we flipped the indexer
      STWA BNeg,d              ;store a flag that we flipped the indexer into BNeg
      LDWA mpcnd,s             ;load our mpcnd and mpr to setup for multiplication
      STWX mpr,s               ;load our mpcnd and mpr to setup for multiplication
top:  ASRX                     ;shift the accumulator right so we can check the carry bit
      BRC addL                 ;if there is a carry then we know we need to store whatever is in the accumulator and add it to our product
      BR cntL                  ;if there is no carry then skip over where we store our product
addL: STWA tempa,d             ;store the accumulator into a temp variable so we can reload it later
      ADDA prod,d              ;add to our final product when we have hit a bit with a one in it
      STWA prod,d              ;store our product for the end
      LDWA tempa,d             ;load back our temp variable so we can continue our loop
cntL: STWX tempx,d             ;store where we are in our index register so we dont lose track
      LDWX cnt,d               ;load how many times we have shifted
      CPWX 7,i                 ;see if we shifted 8 times (7 because it is an index) (we need to shift 8 times to go over every bit in our number)
      BREQ out                 ;if we shifted 8 times then we know we are done
      ADDX 1,i                 ;add one more to our count because we shifted one more time
      STWX cnt,d               ;store how many times we shifted
      LDWX tempx,d             ;load back our indexer which is our mpr we shifted left
      ASLA                     ;shift our multiplier left to get the right number we are trying to calculate
      BR top                   ;continue the loop until we shift the accumulator and index register 8 times
out:  LDWA BNeg,d              ;load our negative flag for our B input and see if it is set
      CPWA 0,i                 ;see if the negative flag is set
      BREQ RETU                ;if the negative flag is not set (not one) then skip over negating our product
      LDWA prod,d              ;load our product into the accumulator so we can negate it
      NEGA                     ;the negative flag is so negate our product and store it
      STWA prod,d              ;store our newly negated product
RETU: LDWA prod,d              ;load our product into the accumulator 
      LDWX 0,i                 ;clear the indexer to get ready to return from the method
       RET                     ;return to wherever this method was called from
;;********void main()
;        This is where the program starts and acts as the driver for our program
main:    STRO Divide,d         ;Displays a big line to divide the output for our program which makes it look nicer
         STRO MenuPr, d        ;gives the menu a displayable menu to choose an option to guide the user       
         BR mainSe             ;skip over instructions that clear out the input buffer because nothing is in the input buffer now
mainSe:  LDWX 0, i             ;clear the index register to prevent problems of reading same input
         LDBX charIn, d        ;loads our character into the indexer which we will switch on and select the correct option
         SUBX ASCOFF, i        ;subtract a constant 97 to be able to use the lowercase letters as indexing variables
         ASLX                  ;shift left because our addresses take up a word of space
         CPBX 0, i             ;if its less than zero then we know we have an invalid input
         BRLT mainSe           ;branch back up to main because of an invalid input 
         CPBX 12, i            ;if its greater than ten then we know we have an invalid input
         BRGT mainSe           ;branch back up to main because of an invalid input
         BR menSW, x           ;we have achieved a valid input so we can branch to menSW using indexed addressing which goes to the 
                               ;correct
                               ;labels which we have setup which correspond to the menu choices
menSW:   .ADDRSS dimen1        ;the label which sets the dimensions of the first matrix (the user is redirected to that label)
         .ADDRSS dimen2        ;the label which sets the dimensions of the second matrix (the user is redirected to that label)
         .ADDRSS add           ;the label which sets up addition for two matrices (the user is redirected to that label)
         .ADDRSS mult          ;the label which sets up multiplication for two matrices (the user is redirected to that label)
         .ADDRSS exOne         ;the label which begins extraction for the first matrix (make it usable for arithmetic operations)
         .ADDRSS exTwo         ;the label which begins extraction for the second matrix (make it usable for arithmetic operations)
         .ADDRSS exit          ;the label which exits the program
dimen1:  STRO Matr1Pr, d       ;prompts the user to input their first matrix dimensions to make the program user friendly
         DECI Matr1D1, d       ;gets the first dimension of the first matrix so we can calculate the dimension for multiplication
         DECI Matr1D2, d       ;gets the second dimension of the first matrix so we can calculate the dimension for multiplication
         STRO newline, d       ;adds a new line for spacing         
                               ;allocate storage for the new matrix
                               ;we know have in the accumulator the dimensions of the matrix that we should store the matrix in         
         LDWA Matr1D1,d        ;load the rows of matrix A into the accumulator and make sure it is not less than zero
         CPWA 0,i              ;see if the rows of matrix A is less than zero and display a message if it is less than zero
         BRLT DimMess          ;display a message saying the dimensions are invalid
         STWA -2,s             ;pass the rows of matrix A onto the stack so we can determine how much space to allocate for the matrix
         LDWA Matr1D2,d        ;load the columns of matrix A into the accumulator to check and see if it is less than zero
         BRLT DimMess          ;if the columns are less than zero then displayy an error message
         STWA -4,s             ;if the columns are greater than zero then store the columns on the stack
         SUBSP 4,i             ;push the rows and columns of matrix A onto the stack
         CALL AddMul           ;calculate how big the matrix is going to be 
         ADDSP 4,i             ;pop the rows and columns off the stack    
         ASLA                  ;shift the accumulator by left one because we are using words
         CALL malloc           ;allocate on the heap memory that we will use for the first matrix
         STWX MatrAPoi, d      ;store the pointer to the matrix in memory
         BR main               ;go to main after getting the first matrix pointer
dimen2:  LDWA 0,i              ;clear the accumulator
         STRO Matr2Pr, d       ;prompts the user to input their second matrix dimensions to make the program user friendly
         DECI Matr2D1, d       ;gets the first dimension of the second matrix so we can calculate the dimension for multiplication
         DECI Matr2D2, d       ;gets the second dimension of the second matrix so we can calculate the dimension for multiplication         
         LDWA Matr2D1,d        ;load the rows of the second matrix into the acculator
         STWA Matr2DL, d       ;store how big are matrix should be (dead code)
         LDWA Matr2D1,d        ;load the rows of matrix B into the accumulator to calculate how much storage we should set aside for our 
                               ;matrix
         STWA -2,s             ;store the rows of matrix B onto the stack so we can calculate how much storage to set aside
         LDWA Matr2D2,d        ;load the columns of matrix B into the accumulator so we push it to the stack
         STWA -4,s             ;store the columns of matrix B onto the stack so we can calculate how much storage to set aside
         SUBSP 4,i             ;push the rows and columns of matrix B onto the stack
         CALL AddMul           ;see how many elements should be in the matrix 
         ADDSP 4,i             ;pop the rows and columns of matrix B off the stack
         STRO newline, d       ;adds a new line for spacing         
                               ;allocate storage for the new matrix      
                               ;we know have in the accumulator the dimensions of the matrix that we should store the matrix in
         ASLA                  ;we need to shift left because we are using words
         CALL malloc           ;allocate on the heap memory that we will use for the second matrix
         STWX MatrBPoi, d      ;store the pointer to the matrix in memory
         BR main               ;branch to main because we allocated storage for matrix B
add:    LDWA Matr2D2,d         ;load the columns of matrix B into the accumulator to prepare for passing parameters
        LDWX 0,i               ;clear the index register
                               ;push from right to left         
         STBA -1,s             ;store the columns of matrix B onto the stack for addMatr
         LDWA Matr2D1,d        ;load the rows of matrix B into the accumulator to prepare for passing parameters
         STBA -2,s             ;store the rows of matrix B onto the stack for addMatr
         LDWA MatrBPoi,d       ;load the pointer to matrix B into the accumulator to prepare to store to the stack for addMatr
         STWA -4,s             ;store the pointer to the stack for addMatr
         LDWA Matr1D2,d        ;load the columns of Matrix A into the accumulator to pass to addMatr
         STBA -5,s             ;store the columns of Matrix A to the stack because addMatr use it
         LDWA Matr1D1,d        ;load the rows of matrix A into the accumulator to pass to addMatr
         STBA -6,s             ;store the rows of Matrix A to the stack because addMatr use it
         LDWA MatrAPoi,d       ;load the pointer to matrix A into the accumulator to pass to the stack to use in addMatr
         STWA -8,s             ;store the pointer to Matrix A to the stack because addMatr use it
         SUBSP 8,i             ;push all the parameters that addMatr will use  
         CALL addMatr          ;call addMatr which will add Matrix A and Matrix B together
         ADDSP 8,i             ;pop all the parameters off of the stack which we passed to addMatr
         CPWA 0,i              ;see if we returned a new pointer
         BREQ main             ;branch to main if we returned a null pointer
         STWA -2,s             ;push pointer to matrix on the stack 
         LDWA Matr1D1,d        ;load the rows of matrix 1 into the accumulator so we can push it to the stack
         STBA -3,s             ;store the rows onto our stack as a byte for a parameter
         LDWA Matr1D2,d        ;load the columns of matrix 1 into the accumulator so we can push it to the stack
         STBA -4,s             ;store the columns onto our stack as a byte for a parameter
         SUBSP 4,i             ;push the rows and columns onto the stack and the pointer to the matrix so we know what we need to display
         CALL dispSt           ;display our matrix we just calculated
         ADDSP 4,i             ;pop the rows columns and the pointer to the matrix off the stack to cleanup the stack
         BR main               ;branches to main after we are done adding the matrices
mult:    LDWA Matr2D2,d        ;load the columns of matrixA into the accumulator so we can store it on the stack
         STBA -1,s             ;store the columns of matrixA onto the stack so we can push it as a parameter to multMatr
         LDWA Matr2D1,d        ;load the rows of matrixB into the accumulator so we can store it on the stack
         STBA -2,s             ;store the rows of matrixB onto the stack so we can reference it in multMatr
         LDWA MatrBPoi,d       ;load the pointer to matrixB into the accumulator so we can pass it to multMatr
         STWA -4,s             ;store the pointer to matrixB so we can reference it in multMatr
         LDWA Matr1D2,d        ;load the columns of matrixA into the accumulator so we can push it onto the stack
         STBA -5,s             ;store the columns of matrixA onto the stack so we can reference it in multMatr
         LDWA Matr1D1,d        ;load the rows of matrixA into the accumulator so we can push it onto the stack
         STBA -6,s             ;store rows of matrixA onto the stack so we can use it as a parameter in multMatr
         LDWA MatrAPoi,d       ;load the pointer to matrixA into the accumulator so we can push it onto the stack
         STWA -8,s             ;store the pointer to matrixA onto the stack so we can reference it in multMatr
         SUBSP 8,i             ;push all parameters to the multiply method (pushed right to left)
         CALL multMatr         ;call multiply matrices which will multiply the matrices we passed to it
         ADDSP 8,i             ;pop all the parameters off the stack
         CPWA 0,i              ;see if the pointer is zero
         BREQ main             ;branch to main if it is equal to zero
         STWA -2,s             ;push pointer to matrix on the stack 
         LDWA Matr3D1,d        ;load the rows of matrix 3 into the accumulator 
         STBA -3,s             ;store the rows onto the stack to get ready to display the matrix
         LDWA Matr3D2,d        ;load the columns of matrix 3 into the accumulator so we can pass it to the display matrix method
         STBA -4,s             ;store the columns on the stack
         SUBSP 4,i             ;pop the pointer to matrix C, the rows and the columns off the stack
         CALL dispSt           ;temporary only to test the code
         ADDSP 4,i             ;pop the matrix pointer, the rows and the columns off the stack
         BR main               ;branch back to main because we did our multiply matrices function
                               ;Setup section to make sure dimensions for the matrices have been setup 
exOne:   LDBA charIn, d        ;clear the accumulator to prevent problems of reading same input
         CPBA newline, d       ;see if its the new line character
         BRNE exOne            ;keep loading characters (it is not a new line character)
         LDWA Matr1D1, d       ;check to see if the user inputs any dimensions for the first matrix
         CPBA 1,i              ;check to see if the user entered less than one
         BRLT addErr           ;check to see if the user entered less than one and branch otherwise because dimensions have to be at 
                               ;least one 
         LDWA Matr1D2, d       ;check to see if the user inputs any dimensions
         CPBA 1,i              ;check to see if the user entered less than one and branch otherwise because dimensions have to be at 
                               ;least one
         BRLT addErr           ;If the user entered less than one then it is a dimension error 
         STWA numElem, d       ;how many elements in our matrix we should have
         STRO UserInPr, d      ;extract matrix (prompt for extracting first matrix )
         LDWA 0, i             ;load zero into the accumulator because we need to initialize count
         STWA count, d         ;initialize count to zero because we will check how many characters we pushed onto the stack
         MOVSPA                ;move stack pointer to the accumulator so we can store where our matrix A is
         ADDA 1, i             ;make it point to the correct address when reading from the string
         STWA Matr1P1, d       ;Store our pointer to Matr1P1 so we can reference it later
                               ;Cont: Section that actually starts pushing the string input onto the stack
cont:    LDWX numElem, d       ;use numElem as a counter to see how many elements we should put on the stack 
         LDBA 0xFC15,d         ;load the next character from keyboard input
         STBA temp,d           ;store our input into a variable just in case we need to reference the input later
         LDBA temp,d           ;after incrementing count load back the byte that was inputted 
         CPBA space, d         ;see if we have encountered a space and if we have then increment count
         BRNE Nspace           ;it is not a space so we can skip the code that increments count only when we are done parsing one 
                               ;number 
         LDWA count, d         ;we have encountered a space so we need to load count into the accumulator to increment it
         ADDA 1, i             ;we have encountered a space so we know we have pushed one more element to the stack
         STWA count, d         ;after incrementing count store it back into the counter variable so we know how many elements we have 
                               ;pushed 
                               ;to the stack
         LDWA ' ', i           ;we need to load the space back into the accumulator to store it
Nspace:  CPWA '\n', i          ;see if it is the end of the line and if so stop pushing onto the stack
         BREQ M1break          ;if we hit a new line then we can stop with the input
         STWA 0,s              ;start storing the matrix on the stack at where the first matrix is 
         SUBSP 1, i            ;push the element onto the stack so we can extract from the string the correct corresponding decimal
                               ;start storing a count of how many elements we are pushing on the stack
         LDWA sLength,d        ;load our counter into the accumulator to add one to it to know how long our string is
         ADDA 1,i              ;add one to our string counter because we added one byte (added 1 to our length)
         STWA sLength, d       ;store back our counter so we can know how long our string is on the stack
         BR cont               ;branch back up to cont to continue our input of the first matrix 
                               ;M1 Break is used once the first matrix is done being pushed onto the stack and ready to be put into a 
                               ;matrix of just numbers
M1break: STWA 0,s              ;new line is in the accumulator and we need to push this onto the stack to determine a stopping 
                               ;condition
         SUBSP 1,i             ;push the element onto the stack
         LDWA Matr1P1,d        ;load the starting point of the string input 
         STWA  0,s             ;push the starting point of the string input onto the stack
         SUBSP 2,i             ;push the starting point of the string input onto the stack so we can know where the string begins
         LDWA MatrAPoi,d       ;move where we want to store the resulting matrix onto the stack
         STWA 0,s              ;store the pointer to our first matrix on the stack because exMatr requires a pointer to our matrix
         CALL exMatr           ;Call the method which will extract our first matrix into a block of memory that resembles our matrix in 
                               ;decimal
         ADDSP 4,i             ;pop parameters off the stack
         ADDSP sLength,d       ;pop our string off the stack
         BR main               ;branch to main after we are done storing our first matrix
exTwo:   LDBA charIn, d        ;clear the accumulator to prevent problems of reading same input
         CPBA newline, d       ;see if its the new line character
         BRNE exTwo            ;keep loading characters (it is not a new line character)
         LDWA Matr2D1, d       ;check to see if the user inputs any dimensions for the second matrix
         CPBA 1,i              ;check to see if the user entered less than one
         BRLT DimMess          ;if the user entered less than one then we ned to display an error
         LDWA Matr2D2, d       ;check to see if the user inputs any dimensions
         CPBA 1,i              ;check to see if the user entered less than one
         BRLT DimMess          ;if the user entered less than one then we know it was an invalid dimension
         STWA numElem, d       ;how many elements in our matrix we should have
         STRO UserInPr, d      ;extract matrix (prompt for extracting first matrix )
         LDWA 0, i             ;load zero into the accumulator to clear our count variable
         STWA count, d         ;clear our count variable for future use
         MOVSPA                ;move the stack pointer to the accumulator to store the pointer to the second matrix
         STWA Matr2P1, d       ;store where are second matrix is (we loaded the location from the stack pointer to the accumulator)
         LDWA 0,i              ;load 0 into the accumulator so we can clear sLength
         STWA sLength,d        ;clear sLength because we are going to use it for counting how many strings we put on the stack
                               ;start parsing the second matrix string  onto the stack
M2strPa: LDWX numElem, d       ;use as a counter to see how many elements we should put on the stack
         LDBA 0xFC15,d         ;load user input through the memory address 0xFC15
         STBA temp,d           ;store user input into a temp variable so we can increment our string counter
         LDWA Scount,d         ;load our counter which tells us how many strings we are placin gon the stack
         ADDA 1, i             ;increment the counter that tells us how many characters we pushed on the stack so we can then pop the 
                               ;strings off the stack
         STWA Scount, d        ;store our counter that we just incremented telling us how many strings there are 
         LDBA temp,d           ;after incrementing count load back the byte that was inputted   
         CPBA space, d         ;see if we have encountered a space and if we have then increment count
         BRNE Nospace          ;it is not a space so we can skip the code that increments count only when we are done parsing one 
                               ;number
                               ;we need to store the space
         LDWA count, d         ;we have encountered a space so we need to load count into the accumulator to increment it
         ADDA 1, i             ;we have encountered a space so we know we have pushed one more element to the stack
         STWA count, d         ;after incrementing count store it back into the counter variable so we know how many elements we have 
                               ;pushed 
                               ;to the stack
         LDWA ' ', i           ;we need to load the space back into the accumulator to store it
Nospace: CPWA '\n', i          ;see if it is the end of the line and if so stop pushing onto the stack
         BREQ M2break          ;if we hit a new line then we can branch because we are done with input
         STBA 0,s              ;start storing the matrix on the stack at where the first matrix is 
         SUBSP 1, i            ;push the element onto the stack 
                               ;start storing a count of how many elements we are pushing on the stack
         LDWA sLength,d        ;load our counter into the accumulator
         ADDA 1,i              ;add one to our string counter
         STWA sLength, d       ;store back our counter
         BR M2strPa            ;branch to where we continue parsing string input for matrix 2
                               ;
                               ;used once the first matrix is done being pushed onto the stack and ready to be put into a matrix of just 
                               ;numbers
                               ;
M2break: STBA 0,s              ;new line is in the accumulator and we need to push this onto the stack to determine a stopping condition
         SUBSP 2,i             ;push the element Matr2P1 onto the stack
         LDWA Matr2P1,d        ;load the starting point of the string input 
         STWA  0,s             ;push the starting point of the string input onto the stack
         SUBSP 2,i             ;push the pointer to matrix 2 on the stack
         LDWA MatrBPoi,d       ;move where we want to store the resulting matrix onto the stack
         STWA 0,s              ;push the pointer to the stack
         CALL exMatr           ;call a method to take the string we placed on the stack and turn it into decimal numbers
         ADDSP 4,i             ;pop parameters off the stack cleaning up the stack
         ADDSP sLength,d       ;pop our string off the stack cleaning up the stack
         BR main               ;branch back to main because we are done with extracting the B matrix
                               ;Section for if there is no dimensions setup 
addErr:  STRO newline, d       ;add a newline for user readability 
         STRO MatrAEr, d       ;used if there are dimension errors 
         STRO newline, d       ;add a newline for user readability
         BR main               ;go back to main if the dimensions of the matrices are not the same
DimErr:  STRO newline,d        ;add a newline for user readability 
         STRO DimMess,d        ;informs the user that the dimensions of the matrices cannot be less than zero
exit:    STOP                  ;stop the program because the user entered g
UserInPr:.ASCII "Please enter the elements that go into your matrix:\n\x00"
                               ;prompt to tell the user to input their matrix (makes the program user friendly)
MatrAEr: .ASCII "Make sure you have initialized both matrices dimensions before attempting to add\n\x00"
                               ;tell user to initialize matrices before adding
Matr1Pr: .ASCII "Please enter the size of your first matrix (one after another (in decimal). For example 4 3 will be a 4x3):\n\x00"   
                               ;prompts the user to input their matrix (make program user friendly)
Matr2Pr: .ASCII "Please enter the size of your second matrix (one after another (in decimal). For example 4 3 will be a 4x3):\n\x00"   
                               ;prompts the user to input their matrix (make program user friendly)
newline:  .ASCII "\n\x00"      ;allows the program to look nicer when we use newline because it adds space between statements
MenuPr:   .ASCII "Please select an option:\na:  Input matrix 1 dimension\nb:  Input matrix 2 dimension\nc:  Add matrices\nd:  Multiply matrices\ne:  Input matrix 1\nf:  Input Matrix 2\ng:  Exit\n\x00"
                               ;a prompt which gives the user a menu to select from multiple matrix manipulation operations
space:    .ASCII " \x00"       ;see if we have a space in our input 
pipe:     .ASCII "\|\x00"      ;a pipe character for displaying the matrices
Divide:   .ASCII "-----------------------------------------\n\x00" 
                               ;used for dividing output up 
NotPossA: .ASCII "The matrices you entered are not able to be added\n\x00"
                               ;used to tell the user if the matrices they entered cannot be added
NotPossM: .ASCII "The matrices you entered are not able to be multiplied\n\x00"
                               ;used to tell the user if the matrices they entered cannot be multiplied
DimMess:  .ASCII "The dimensions you have entered are invalid. Please enter dimensions that are greater than zero\n\x00"
                               ;used to tell the user if the dimensions they entered are invalid
;
;
;*********malloc()
;        Precondition: A contains number of bytes 
;        Postcondition: X contains pointer to bytes
ran: .BLOCK 2                ;a random block of memory to stop stop errors
malloc:  LDWX hpPtr,d        ;returned pointer
         ADDA hpPtr,d        ;allocate from the heap
         STWA hpPtr,d        ;in the first location of the heap put the pointer to that memory (it is also in the index register)
         RET                 ;Return from allocating storage on the heap
hpPtr:   .ADDRSS heap        ;address of next free byte
heap:    .BLOCK 1            ;first byte in the heap
END:     .END                ;label with code for .END for when we want to end the program when the user enters g