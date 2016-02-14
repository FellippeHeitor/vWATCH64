FUNCTION Revert$ (text$)
    DIM i AS LONG, temp$
    FOR i = LEN(text$) TO 1 STEP -1
        temp$ = temp$ + MID$(text$, i, 1)
    NEXT i
    Revert$ = temp$
END FUNCTION

SUB TakeABreak
    DIM r AS INTEGER
    STATIC NoOfBreaks AS LONG

    NoOfBreaks = NoOfBreaks + 1

    DO
        RANDOMIZE TIMER
        r = _CEIL(RND * 100)
        PRINT r
        IF r > 50 THEN EXIT DO
    LOOP
END SUB

