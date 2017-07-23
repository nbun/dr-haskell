f :: Int -> Int -> Int
f x y = x

-- Abhängigkeit nach oben funktioniner hier bei f und g noch nicht
g :: Int -> Int -> Int
g z y = f z y
--------------------------------------------------------------------------------

-- Wenn lokale Funktionen keinen Typen haben gibt es bei der
-- Inferenz einen Fehler


--f :: Int -> a -> Int
--f x y = h 5
--  where -
--     h z = x

-------------------------------------------------------------------------------

-- Noch keine Typeangaben für Funktionen aus der Prelude
-- f :: Int -> Int -> Int
-- f x y = x + y

-------------------------------------------------------------------------------
-- Leeres Program funktioniert
-------------------------------------------------------------------------------

--Funktioniert

--h :: a -> a -> a
--h x y = x

-------------------------------------------------------------------------------
--h :: a -> a
--h x  = x
-------------------------------------------------------------------------------
