
class Mappable f where
    myMap :: (a -> b) -> f a -> f b


-- This means to implement MultipleMappabe you must implement both myMap and myMap2
class Mappable g => Mappable2 g where
    myMap2 :: (a -> b -> c) -> g a -> g b -> g c

instance Mappable Maybe where
    myMap f (Just x) = Just (f x)
    myMap _ Nothing = Nothing

myFunction' :: Int -> Int
myFunction' x = x + 1

usingMyMap = myMap myFunction' (Just 0) -- Just 1

instance Mappable2 Maybe where
    myMap2 f (Just x) (Just y) = Just (f x y)
    myMap2 _ Nothing _ = Nothing
    myMap2 _ _ Nothing = Nothing

class Wrappable f where
    wrap :: a -> f a

instance Wrappable Maybe where
    wrap = Just

class (Wrappable f, Mappable2 f) => WrapAndMappable2 f where

class AltWrapAndMappable2 f where
    wrap' :: a -> f a
    myMap' :: (a -> b) -> f a -> f b
    moveOutFromTuple :: (f a, f b) -> f (a, b)

instance AltWrapAndMappable2 Maybe where
    wrap' = Just
    myMap' = fmap
    moveOutFromTuple (Just x, Just y) = Just (x, y)
    moveOutFromTuple (Nothing, _) = Nothing
    moveOutFromTuple (_, Nothing) = Nothing

myMap2' :: AltWrapAndMappable2 f => (a -> b -> c) -> f a -> f b -> f c
myMap2' = undefined

myMap3' :: AltWrapAndMappable2 f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
myMap3' f x y z =
    let
        something = myMap2' f x y
        -- applyAFunction not True <=> not True
        applyAFunction g a = g a
    in
        myMap2' applyAFunction something z

class (AltWrapAndMappable2 f) => MyFlattable f where
    flatten :: f (f a) -> f a
    -- unwrap :: f a -> a

class MyMonad f where
    wrap'' :: a -> f a
    map'' :: (a -> b) -> f a -> f b
    flatten'' :: f (f a) -> f a

flatMap :: MyMonad m => (a -> m b) -> m a -> m b
flatMap f a = flatten'' (map'' f a)

myMap2'' :: MyMonad m => forall a b c. (a -> b -> c) -> m a -> m b -> m c
myMap2'' f x y =
    let
        -- intermediateF :: m (b -> c)
        intermediateF = map'' f x
        -- mapFAgain :: m (m c)
        mapFAgain = map'' (`map''` y) intermediateF
    in
        flatten'' mapFAgain


ifElse :: Bool -> a -> a -> a
ifElse condition thenAction elseAction = if condition then thenAction else elseAction

ifElseMaybe0 :: Maybe Bool -> Maybe a -> Maybe a -> Maybe a
ifElseMaybe0 Nothing _ _ = Nothing
ifElseMaybe0 (Just _) Nothing _ = Nothing
ifElseMaybe0 (Just _) _ Nothing = Nothing
ifElseMaybe0 (Just condition) (Just thenAction) (Just elseAction) = Just (ifElse condition thenAction elseAction)

ifElseMaybe0' :: Maybe Bool -> Maybe a -> Maybe a -> Maybe a
ifElseMaybe0' = myMap3' ifElse

ifElseMaybe1 :: Maybe Bool -> Maybe a -> Maybe a -> Maybe a
ifElseMaybe1 Nothing _ _ = Nothing
ifElseMaybe1 (Just True) Nothing _ = Nothing
ifElseMaybe1 (Just False) _ (Just elseValue) = Just elseValue
ifElseMaybe1 (Just False) _ Nothing = Nothing
ifElseMaybe1 (Just x) (Just thenValue) (Just elseValue) = Just (ifElse x thenValue elseValue)


-- Defining intermediate values
myArithmeticFunction :: Int -> Int
myArithmeticFunction x =
    let
        added2 = x + 2
    in
        added2 * 3 -- Exact same as (x + 2) * 3

myArithmeticFunction' :: Int -> Int
myArithmeticFunction' x =
    added2 * 3 -- Exact same as (x + 2) * 3
        where
            added2 = x + 2

myAddition :: Int -> Int -> Int
myAddition x y = x + y

usingMyMap2' :: Maybe Int
usingMyMap2' = myMap2' myAddition (Just 1) (Just 2) -- Just 3
