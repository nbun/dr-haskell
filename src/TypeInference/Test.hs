-- Test zur Extraktion der Funktionsnamen

{- f x = x

h x y = 5

i =  \x -> 4 + x

j = 4 + 5

z a b | a == b = True
      | otherwise = false

w c d = y c
  where
    y c = 6

v x = let g e = e + x
        in x + g 5

hallo x =
  do
    o <- gets
    let fn =  "bu"
    return ()
-}
--f :: [a] -> [a]
--f y = y ++ y

--f :: Int -> Int -> Int
--f x y = x

-- Abhängigkeit nach oben funktioniner hier bei f und g noch nicht
--g :: Int -> Int -> Int
--g z y = f z y
--------------------------------------------------------------------------------

-- Wenn lokale Funktionen keinen Typen haben gibt es bei der
-- Inferenz einen Fehler


--f :: Int -> a -> Int
--f x y = h 5
--  where
--     h z = x

-------------------------------------------------------------------------------

-- Noch keine Typeangaben für Funktionen aus der Prelude
-- f :: Int -> Int -> Int
-- f x y = x + y

-------------------------------------------------------------------------------
-- Leeres Program funktioniert
-------------------------------------------------------------------------------

--Funktioniert



-------------------------------------------------------------------------------
--h :: a -> a
--h x  = x
-------------------------------------------------------------------------------
