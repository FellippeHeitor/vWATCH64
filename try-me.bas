'Pointless code, made just for testing of vWATCH64
'After you compile vWATCH64, drag this .bas file into vwatch.exe
'to trace its execution, run it line by line and also monitor
'the values of the variables below in real time.
'
_DEFINE A-Z AS _UNSIGNED _INTEGER64
DIM SHARED a AS STRING, fe(0 TO 5) AS STRING, i AS _UNSIGNED LONG
DIM yname AS STRING
DIM SHARED tempo AS DOUBLE
DIM SHARED tiempo#
DIM SHARED j AS INTEGER
DIM SHARED k$
DIM used$6, used`8
DIM strayDog, temp

RANDOMIZE TIMER
DO: _LIMIT 60
    RESTORE Names
    tempo = TIMER
    FOR j = 1 TO 10: _LIMIT 10
        READ a
        yname$ = a
        used$6 = a
        PRINT a$;
        i = _CEIL(RND * 100000)
        k$ = INKEY$
        fe(temp) = a
        temp = temp + 1: IF temp > UBOUND(fe) THEN temp = 0
        IF k$ = CHR$(27) THEN EXIT DO
    NEXT j
    used`8 = tiempo#
    strayDog = strayDog + 1
    PRINT strayDog
    tiempo# = TIMER
    PRINT
    PRINT tiempo# - tempo
LOOP

Names:
DATA "qb64 ","is ","one "
DATA "cool ","project ","which "
DATA "i ","really ","love ",playing with