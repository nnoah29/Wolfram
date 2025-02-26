module Rules where

rule30:: Bool -> Bool -> Bool -> Bool
rule30 True  True  True  = False -- 111 = 0
rule30 True  True  False = False -- 110 = 0
rule30 True  False True  = False -- 101 = 0
rule30 True  False False = True  -- 100 = 1
rule30 False True  True  = True  -- 011 = 1
rule30 False True  False = True  -- 010 = 1
rule30 False False True  = True  -- 001 = 1
rule30 False False False = False -- 000 = 0

rule90:: Bool -> Bool -> Bool -> Bool
rule90 True  True  True  = False -- 111 = 0
rule90 True  True  False = True  -- 110 = 1
rule90 True  False True  = False -- 101 = 0
rule90 True  False False = True  -- 100 = 1
rule90 False True  True  = True  -- 011 = 1
rule90 False True  False = False -- 010 = 0
rule90 False False True  = True  -- 001 = 1
rule90 False False False = False -- 000 = 0

rule110:: Bool -> Bool -> Bool -> Bool
rule110 True  True  True  = False -- 111 = 0
rule110 True  True  False = True  -- 110 = 1
rule110 True  False True  = True  -- 101 = 1
rule110 True  False False = False -- 100 = 0
rule110 False True  True  = True  -- 011 = 1
rule110 False True  False = True  -- 010 = 1
rule110 False False True  = True  -- 001 = 1
rule110 False False False = False -- 000 = 0

rule255:: Bool -> Bool -> Bool -> Bool
rule255 True  True  True  = False -- 111 = 0
rule255 True  True  False = True  -- 110 = 1
rule255 True  False True  = True  -- 101 = 1
rule255 True  False False = False -- 100 = 0
rule255 False True  True  = True  -- 011 = 1
rule255 False True  False = True  -- 010 = 1
rule255 False False True  = True  -- 001 = 1
rule255 False False False = False -- 000 = 0

rule60:: Bool -> Bool -> Bool -> Bool
rule60 True  True  True  = False -- 111 = 0
rule60 True  True  False = False -- 110 = 0
rule60 True  False True  = False -- 101 = 0
rule60 True  False False = True  -- 100 = 1
rule60 False True  True  = True  -- 011 = 1
rule60 False True  False = True  -- 010 = 1
rule60 False False True  = True  -- 001 = 1
rule60 False False False = False -- 000 = 0

rule42:: Bool -> Bool -> Bool -> Bool
rule42 True  True  True  = False -- 111 = 0
rule42 True  True  False = False -- 110 = 0
rule42 True  False True  = False -- 101 = 0
rule42 True  False False = True  -- 100 = 1
rule42 False True  True  = True  -- 011 = 1
rule42 False True  False = True  -- 010 = 1
rule42 False False True  = True  -- 001 = 1
rule42 False False False = False -- 000 = 0

rule57:: Bool -> Bool -> Bool -> Bool
rule57 True  True  True  = False -- 111 = 0
rule57 True  True  False = False -- 110 = 0
rule57 True  False True  = False -- 101 = 0
rule57 True  False False = True  -- 100 = 1
rule57 False True  True  = True  -- 011 = 1
rule57 False True  False = True  -- 010 = 1
rule57 False False True  = True  -- 001 = 1
rule57 False False False = False -- 000 = 0

rule106:: Bool -> Bool -> Bool -> Bool
rule106 True  True  True  = False -- 111 = 0
rule106 True  True  False = False -- 110 = 0
rule106 True  False True  = False -- 101 = 0
rule106 True  False False = True  -- 100 = 1
rule106 False True  True  = True  -- 011 = 1
rule106 False True  False = True  -- 010 = 1
rule106 False False True  = True  -- 001 = 1
rule106 False False False = False -- 000 = 0
