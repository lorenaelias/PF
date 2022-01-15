-- (||) :: Bool->Bool->Bool
-- True || True=True
-- True || False=True
-- False || True=True
-- False || False=True

-- ou a b | (a || b) = True
--        | otherwise = False

-- case (a,b) of
--     (a,_) | a==True -> "True"
--     (_,b) | b==True -> "True"
--           otherwise = "False"

or a b =
    case a || b of
        a -> True
        b -> True
        otherwise -> False