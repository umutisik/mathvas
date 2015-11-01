-- turns functions R^2->R^3 into images
{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS -fno-warn-tabs  #-}

import Data.List
import Control.Monad
import Control.Monad.State
import System.Environment
import Data.Word
import Data.Array.Repa.IO.BMP
import Data.Array.Repa.IO.Timing
import Data.Array.Repa                                 as A
import qualified Data.Array.Repa.Repr.Unboxed   as U
import Prelude                                        as P
import Data.Time
import System.Random
import System.IO.Unsafe

import qualified Data.Vector.Unboxed                as VU
import qualified Data.Vector.Generic                as G
import qualified Data.Vector.Unboxed.Mutable        as MV

-- global settings
defaultWidth :: Int
defaultWidth = 300
antiAlias = False
randomGenerator = mkStdGen 42


-- Write a function
-- this is the function that will be drawn 
theFunctionToDraw = myFunction01

myFunction01 :: R -> R -> (R,R,R)
myFunction01 x y = (x^2, y^2, sqrt (x^2+y^2)/2)


--
usage         = putStr $ unlines
        [ "umutArt <algorithm::string> <sizeX::Int> <sizeY::Int>" ]
--

main :: IO ()
main = do args <- getArgs
          case args of
            [filename] -> run' "gradient01" defaultWidth defaultWidth filename
          _            -> usage
          putStr "Done!\n"

-- run with selected algorithm
run' :: String -> Int -> Int -> String -> IO ()
run' algorithm sizeX sizeY filename = do   let umutsImage = imageFromRealF' (theFunctionToDraw) sizeX sizeY
                                              timeStr <- liftM show (getCurrentTime >>= return)
                                           writeImageToBMP (filename P.++ ".bmp") umutsImage




-- selects the algorithm, possible options imageFromRealF, imageFromRandomlyFViaVec, newImageFromOldViaMap ... 
selectAlgorithm :: String -> Int -> Int -> Array U DIM2 UIColor8 
selectAlgorithm nm = do    let preGeneratedRandomRs :: Array U DIM1 R
                               preGeneratedRandomRs = (A.fromListUnboxed (Z :. (1000::Int)) (take 1000 $ randomList rg) ) :: Array U DIM1 R
                               rg = mkStdGen 42
                           case nm of 
                                  "blank"       -> blank  
                                  "gradient01"  -> imageFromRealF' (\x -> \y -> (x^2, y^2, sqrt (x^2+y^2)/2))
                                  "gradient02"  -> imageFromRealF' (\x -> \y -> (x^2, y^2, sqrt (x^2+y^2)/4))
                                  "gradient03"  -> imageFromRealF' (\x -> \y -> (r1 x y,r1 x y,r1 x y)) 
                                                                        where r1 x y = sin(1000 * (x^3 + y^3))
                                  "gradient04"  -> imageFromRealF' (\x -> \y -> (((r1 x y) `cScProd` (-2,0,2)) `cadd` ((r2 x y) `cScProd` (1,0,-1))) ) 
                                                                        where r1 x y = sqr $ (1+sin(50*(x - y)*(x + y)))/2
                                                                              r2 x y = 1 
                                  "gradient05"  -> imageFromRealF' (\x -> \y -> (((r1 x y) `cScProd` (-2,0,2)) `cadd` ((r2 x y) `cScProd` (1,0,-1))) ) 
                                                                        where r1 x y = abs $ sqr $ (1+sin(100*(x - y)*(cos x)*(x+y)*(sin y)))/2
                                                                              r2 x y = 1 
                                  "gradient06"  -> imageFromRealF' (\x -> \y -> (((r1 x y) `cScProd` (1,1,1)) `cadd` ((r2 x y) `cScProd` (0,0,0))) ) 
                                                                        where r1 x y = (/ 10) . fromIntegral . floor . (*10) . sin $ 100 * (distance2D x y + max (abs x) (abs y))  
                                                                              r2 x y = 1
                                  "gradient07"  -> imageFromRealF' (\x -> \y -> (((r1 x y) `cScProd` (1,1,1)) `cadd` ((r2 x y) `cScProd` (0,0,0))) ) 
                                                                        where r1 x y = sin $ 100 * ((1-dis) + dis * (max (abs x) (abs y))) where dis = distance2D x y
                                                                              r2 x y = 1
                                  "gradient08"  -> imageFromRealF' $ flip (flip (\x -> \y -> (((r1 x y) `cScProd` (1,1,1)) `cadd` ((r2 x y) `cScProd` (0,0,0))) ) . (2 *)) . (2 *)
                                                                        where r1 x y = sin $ 50 * ((1-dis) * dis + dis * (max (abs x) (abs y))) where dis = distance2D x y
                                                                              r2 x y = 1
                                  "gradient09"  -> imageFromRealF' $ flip (flip (\x -> \y -> (((r1 x y) `cScProd` (1,1,1)) `cadd` ((r2 x y) `cScProd` (0,0,0)))) . (1 *)) . (1 *)
                                                                        where r1 x y = sin $ 200 * ((1-mm) * dis + mm * mm) where dis = distance2D x y
                                                                                                                                  mm = max (abs x) (abs y)
                                                                              r2 x y = 1
                                  "gradient10"  -> imageFromRealF' $ flip (flip (\x -> \y -> (((r1 x y) `cScProd` (1,1,1)) `cadd` ((r2 x y) `cScProd` (0,0,0))) ) . (1 *)) . (1 *)
                                                                        where r1 x y = 0.3 + (sin $ 200 * ((1-mm) * dis + mm * mm)) where dis = distance2D x y
                                                                                                                                          mm = max (abs x) (abs y)
                                                                              r2 x y = 1
                                  "boxgrad"     -> imageFromRealF' $ (\x -> \y -> mixColors (m x y) (1 - (m x y)) (0.04,0.04,0.8) (0,0,0)) 
                                                                        where m x y = (max (abs x^2) (abs y))
                                  "boxes"       -> imageFromRealF' $ (\x -> \y -> ( ((0.4 * stepFunction 10 (m x y)) + 0.6 * (m x y))) `cScProd` (0.2,0.2,0.9) )
                                                                        where m x y = (max (abs x^2) (abs y))
                                  "colors01"      -> imageFromRealF $ (\x -> \y -> (rrr x y, rrr x y, rrr x y )) -- warning! generates randomlist each time
                                                                        where rrr x y = (!!) (randomList rg) (plc (s x) (s y))
                                                                              steps = 10::Int
                                                                              s xx = stepFunction steps (xx+0.5)
                                                                              plc :: R -> R -> Int
                                                                              plc x y = 3*(floor ((s x)*((fromIntegral steps)^2)) + (floor ((s y)* (fromIntegral steps))))
                                  "colors02"    -> imageFromRealF $ (\x -> \y -> (rrr x y 0, rrr x y 1,rrr x y 2))
                                                                        where rrr x y aa = (!) preGeneratedRandomRs $ (Z :. (+ aa) (plc (s x) (s y)))
                                                                              steps = 10::Int
                                                                              s xx = stepFunction steps (xx+0.5)
                                                                              plc :: R -> R -> Int
                                                                              plc x y = 3*(floor ((s x)*((fromIntegral steps)^2)) + (floor ((s y)* (fromIntegral steps))))
                                  "colors03"    -> imageFromRealF $ (\x -> \y -> (rrr x y 0 0.1, rrr x y 1 0.1,rrr x y 2 0.8))
                                                                        where rrr x y aa dd = ((!) preGeneratedRandomRs $ (Z :. (+ aa) (plc (s x) (s y)))) * dd
                                                                              steps = 10::Int
                                                                              s xx = stepFunction steps (xx+0.5)
                                                                              plc :: R -> R -> Int
                                                                              plc x y = 3*(floor ((s x)*((fromIntegral steps)^2)) + (floor ((s y)* (fromIntegral steps))))
                                  "colors04"    -> imageFromRealF $ (\x -> \y -> (stepFunction steps (0.8 - (max (abs x) (abs y)))) `cScProd` ((rrr x y 0 0.1), rrr x y 1 0.1,rrr x y 2 0.8))
                                                                        where rrr x y aa dd = ((!) preGeneratedRandomRs $ (Z :. (+ aa) (plc (x) (y)))) * dd
                                                                              steps = 100::Int
                                                                              s xx = stepFunction steps (xx+0.5)
                                                                              plc :: R -> R -> Int
                                                                              plc x y = 3*(floor ((s x)*((fromIntegral steps)^2)) + (floor ((s y)* (fromIntegral steps))))
                                  "colors05"    -> imageFromRealF $ (\x -> \y -> ((rrr x y 0 0.1), rrr x y 1 0.1,rrr x y 2 0.8))
                                                                        where rrr x y aa dd = ((!) preGeneratedRandomRs $ (Z :. ((+ aa) (plc (x) (y))))) * dd
                                                                              steps = 100::Int
                                                                              s xx = stepFunction (steps) (xx+0.5)
                                                                              plc :: R -> R -> Int
                                                                              plc x y = 3*(floor ((s x)*((fromIntegral steps + 1)^2)) + (floor ((s y)* (fromIntegral steps))))
                                                                                -- careful about the total number of randoms generated above
                                  "colors06"    -> imageFromRealF $ (\x -> \y -> (rrr x y 0 0.1, rrr x y 1 0.1, rrr x y 2 0.8) `cAdd` ((peak2d (sss x) (sss y)) `cScProd` (1,1,1)))
                                                                        where rrr x y aa dd = ((!) preGeneratedRandomRs $ (Z :. ((+ aa) (plc (x) (y))))) * dd
                                                                              steps = 100::Int
                                                                              sf = fromIntegral steps :: R
                                                                              s xx = stepFunction (steps) (xx+0.5)
                                                                              sss xx = 1 --((xx+0.5) - (05/sf + stepFloor steps (xx+0.5+(0.5/sf)))) * (2*(fromIntegral steps))
                                                                              plc :: R -> R -> Int
                                                                              plc x y = 3*(floor ((s x)*((fromIntegral steps + 1)^2)) + (floor ((s y) * sf)))
                                  "noise01" -> imageFromRandomlyFViaVec (\x -> \y -> liftM2 cScProd (return (abs x)) graySnow)
                                  "noise02" -> imageFromRandomlyFViaVec (\x -> \y -> liftM2 cScProd (return (x^2 + y^2)) graySnow)
                                  "noise03" -> imageFromRandomlyFViaVec (\x -> \y -> liftM2 cScProd (return (2*(x^2 + y^2))) graySnow)
                                  "noise04" -> imageFromRandomlyFViaVec (\x -> \y -> (liftM2) cAdd ((liftM2 cScProd (return (2*(x^2 + y^2))) noise)) (liftM2 cScProd (return (2*(x^2 + y^2))) graySnow))
                                  "noise05" -> imageFromRandomlyFViaVec (\x -> \y -> (liftM2) cAdd ((liftM2 cScProd (return (2*(x^2 + y^2))) graySnow)) (liftM2 cScProd (return (1-2*(x^2 + y^2))) noise))
                                  "noise06test" -> imageFromRandomlyFViaVec (\x -> \y -> foldr (liftM2 cAdd) (return (0,0,0)) [graySnow, graySnow, graySnow, graySnow, graySnow])
                                        -- this one is five snows added whereas the one below is one snow 5 times. proves that the randomness really is working sequentially as expctd.
                                  "noise07test" -> imageFromRandomlyFViaVec (\x -> \y -> (liftM2 cScProd) (return 5) graySnow)
                                  "noise08" -> imageFromRandomlyFViaVec (\x -> \y -> liftM2 cScProd (return (abs (x-(1/7)*y))) graySnow)
                                  "noise09" -> imageFromRandomlyFViaVec (\x -> \y -> noiseWithF (\a -> sqrt a))
                                  "noise10" -> imageFromRandomlyFViaVec (\x -> \y -> noiseWithF (stepFunction 2))
                                  "noise11" -> imageFromRandomlyFViaVec (\x -> \y -> grayNoiseWithF (\a -> if a<0.99 then 0 else 1))
                                  "noise12" -> imageFromRandomlyFViaVec (\x -> \y -> noiseWithF (\a -> if a<0.99 then 0 else 1))
                                  "noise13" -> imageFromRandomlyFViaVec (\x -> \y -> liftM2 cScProd (return ((sin x) + (cos y))) noise )
                                  "noise14" -> imageFromRandomlyFViaVec (snowFromFunction $ colorsf) where colorsf = (\x -> \y -> (rrr x y 0))
                                                                                                           rrr x y aa = (!) preGeneratedRandomRs $ (Z :. (+ aa) (plc (s x) (s y)))
                                                                                                           steps = 10::Int
                                                                                                           s xx = stepFunction steps (xx+0.5)
                                                                                                           plc :: R -> R -> Int
                                                                                                           plc x y = 3*(floor ((s x)*((fromIntegral steps)^2)) + (floor ((s y)* (fromIntegral steps))))
                                  "noise15" -> imageFromRandomlyFViaVec (normalishSnowFromFunction $ colorsf) where colorsf = (\x -> \y -> (rrr x y 0))
                                                                                                                    rrr x y aa = (!) preGeneratedRandomRs $ (Z :. (+ aa) (plc (s x) (s y)))
                                                                                                                    steps = 10::Int
                                                                                                                    s xx = stepFunction steps (xx+0.5)
                                                                                                                    plc :: R -> R -> Int
                                                                                                                    plc x y = 3*(floor ((s x)*((fromIntegral steps)^2)) + (floor ((s y)* (fromIntegral steps))))

                                  "noise16" -> imageFromRandomlyFViaVec (snowFromFunction $ scaleNormal colorsf) where colorsf = (\x -> \y -> ( ((0.4 * stepFunction 10 (m x y)) + 0.6 * (m x y)))) where m x y = (max (abs x) (abs y^2))
                                  "noise17" -> imageFromRandomlyFViaVec (snowFromFunction $ scaleNormal colorsf) where colorsf = (\x -> \y -> ( ((0.4 * (m x y)) + 0.6 * (m x y)))) where m x y = (max (abs x) (abs y^2))
                                  "noise18" -> imageFromRandomlyFViaVec (snowFromFunction $ scaleNormal colorsf) where colorsf = (\x -> \y -> ( ((0.4 * stepFunction 100 (m x y)) + 0.6 * (m x y)))) where m x y = (max (abs x) (abs y^2))
                                  "noise19" -> imageFromRandomlyFViaVec (snowFromFunction $ scaleNormal colorsf) where colorsf = (\x -> \y -> sin (50*(x^2 + y^2)) ^ 2 ) 
 
                                  "noise20" -> imageFromRandomlyFViaVec (snowFromFunction $ scaleNormal colorsf) where colorsf = (\x -> \y -> (((r1 x y)*1 + (r2 x y)*0)))
                                                                                                                        where r1 x y = nsin $ 220 * ((1-mm) * dis + mm * mm) 
                                                                                                                                        where dis = distance2D x y
                                                                                                                                                mm = max (abs x) (abs y)
                                                                                                                              r2 x y = 1
                                  "noise21" -> imageFromRandomlyFViaVec (snowFromFunction $ scaleNormal (funnyTwirl 12 colorsf)) where colorsf = (\x -> \y -> abs x  ) 
                                  "noise22" -> imageFromRandomlyFViaVec (snowFromFunction $ scaleNormal (funnyTwirl 12 colorsf)) where colorsf = (\x -> \y -> (ncos $ 50*y) * (abs x)) 
                                  "noise23" -> imageFromRandomlyFViaVec (noiseFromFunction $ scaleNormal (funnyTwirl 4 colorsf)) where colorsf = (\x -> \y -> (ncos $ 10*y) * (abs x)) 
                                  "noise24" -> imageFromRandomlyFViaVec (noiseFromFunction $ scaleNormal (funnyTwirl 100 colorsf)) where colorsf = (\x -> \y -> (abs x)) 
                                  "noise25" -> imageFromRandomlyFViaVec (snowFromFunction $ scaleNormal (funnyTwirl 100 colorsf)) where colorsf = (\x -> \y -> (abs x)) 
                                  "noise26" -> imageFromRandomlyFViaVec (noiseFromFunction2 $ scaleNormal (funnyTwirl 1 colorsf)) where colorsf = (\x -> \y -> stepFunction 3 (abs x)) 
                                  "noise27" -> imageFromRandomlyFViaVec (noiseFromFunction2 $ scaleNormal (funnyTwirl 200 colorsf)) where colorsf = (\x -> \y -> (0.8 - abs y + abs x) ) 
                                  "noise28" -> imageFromRandomlyFViaVec (snowFromFunction $ scale 5 $ funnyTwirl 0 pattern) where pattern = (\x -> \y -> nsin $ 70*y + 10*(1 - x^2 - y^2)*(nsin $ 2*x)) 
                                  "noise29" -> imageFromRandomlyFViaVec (snowFromFunction $ scale 100 $ pattern) where pattern = (\x -> \y -> stepFloor 2 $ abs x + abs y )  
                                  "noise30" -> imageFromRealF (\x -> \y -> tripleDiagonal $ (stepFloor 2 $ nsin ((50*x) + (abs (100*y)))))
                                  "noise31" -> imageFromRandomlyFViaVec (\xx -> \yy -> ((liftM2) cAdd ((snowFromFunction $ scale 100 $  pattern) xx yy) (liftM2 cScProd (return $ (scale 100) pattern2 xx yy) noise))) 
                                                                                                                                where pattern = (\x -> \y -> stepFloor 2  $ nsin (y*0.5 + (abs x)) )
                                                                                                                                      pattern2 = (\x -> \y -> 1 - (stepFunction 2  $ nsin (y*0.5 + (abs x) )))  
                                    "noise32" -> imageFromRandomlyFViaVec (snowFromFunction $ scale 40 $ pattern) where pattern = (\x -> \y -> 0.5*((stepFloor 2 $ nsin (x + y)) + (stepFloor 2 $ nsin (x - y))))  
                                  "minnoise01" -> imageFromRandomlyFViaVec (snowFromFunction $ scale 3 (colorsf)) where colorsf = (\x -> \y -> (1-x^2-y^2)  ) 
                                  "minnoise02" -> imageFromRandomlyFViaVec (snowFromFunction $ scale 3 (colorsf)) where colorsf = (\x -> \y -> (x^2+y^2)  ) 
                                  "minnoise03" -> imageFromRandomlyFViaVec (snowFromFunction $ scale 3 $ (colorsf)) where colorsf = (\x -> \y -> 0.1 + 0.9*(x^2+y^2)^14  ) 
                                  "minnoise04" -> imageFromRandomlyFViaVec (snowFromFunction $ scale 3 $ (colorsf)) where colorsf = (\x -> \y -> 0.5 + 0.5*(x^2+y^2)^14  ) 
                                  "minnoise05" -> imageFromRandomlyFViaVec $ (scale 3) $ combineNoises noise snow (\x -> \y -> (x^2 + y^2) ) 
                                  "minnoise06" -> imageFromRandomlyFViaVec $ (scale 3) $ combineNoises noise snow (\x -> \y -> 1-(x^2 + y^2) ) 
                                  "minnoise07" -> imageFromRandomlyFViaVec $ (scale 1) $ combineNoises noise snow $ funnyTwirl 12 (\x -> \y -> abs x ) 
                                  "minnoise08" -> imageFromRandomlyFViaVec $ (scale 1) $ combineNoises (return (0,0,0)) snow $ funnyTwirl 12 (\x -> \y -> abs x ) 
                                  "minnoise09" -> imageFromRandomlyFViaVec $ (scale 1) $ combineNoises (return (0,0,0)) noise $ funnyTwirl 12 (\x -> \y -> abs x ) 
                                  "minnoise10" -> imageFromRandomlyFViaVec $ (scale 0.7) $ combineNoises (return black) (return white)$ funnyTwirlWithFunction (sqrt . safeInvert) (\x -> \y -> sqrt (abs x) ) 
                                  "minnoise11" -> imageFromRandomlyFViaVec $ (scale 0.7) $ combineNoises  (return white) (return black)$ funnyTwirlWithFunction (sqrt . safeInvert) (\x -> \y -> sqrt (abs x) ) 
                                  "minnoise12" -> imageFromRandomlyFViaVec $ (scale 1) $ combineNoises  (noise) (snow)$ (\x -> \y -> x+0.5 ) 
                                  "minnoise13" -> imageFromRandomlyFViaVec $ (scale 1) $ combineNoises  (noise) (snow)$ funnyTwirl 4 (\x -> \y -> x+0.5 ) 
                                  "minnoise14" -> imageFromRandomlyFViaVec $ (scale 1) $ combineNoises  (return white) (chooseFrom [red, green])$ funnyTwirl 4 (\x -> \y -> x+0.5 ) 
                                  "minnoise15" -> imageFromRandomlyFViaVec $ (scale 1) $ constant (chooseFrom [red, green, blue]) 
                                  "minnoise16" -> imageFromRandomlyFViaVec $ (scale 1) $ constant (chooseFrom [(0,0,0), (0.3,0,0), (0.5,0,0), (0.7,0,0)]) 
                                  "minnoise17" -> imageFromRandomlyFViaVec $ (scale 1) $ combineNoises (chooseFrom [(0,0,0), (0.3,0,0), (0.5,0,0), (0.7,0,0)]) (graySnow) $ (\x -> \y -> x+0.5) 
                                  "minnoise18" -> imageFromRandomlyFViaVec $ (scale 1) $ combineNoises (chooseFrom [(0,0,0), (0.3,0,0), (0.5,0,0), (0.7,0,0)]) (snow) $ (\x -> \y -> (stepFunction 5 (x+0.5))) 
                                  "minnoise19" -> imageFromRandomlyFViaVec $ (scale 1) $ combineNoises (chooseFrom [(0,0,0), (0.3,0,0), (0.5,0,0), (0.7,0,0)]) (graySnow) $ (\x -> \y -> 0.5) 
                                  "mapped00"   -> newImageFromOldViaMap fp oldImage where
                                                        oldImage = (imageFromRandomlyFViaVec $ (scale 1) $ combineNoises (chooseFrom [(0,0,0), (0.3,0,0), (0.5,0,0), (0.7,0,0)]) (graySnow) $ (\x -> \y -> 0.5))  200 200
                                                        fp x y = (floor $ 20*(1-(sin (2*x))) , floor $ 20*(1-(sin (2*y))))
                                  "mapped01"   -> newImageFromOldViaMap fp oldImage where
                                                        oldImage = (imageFromRandomlyFViaVec $ (scale 1) $ combineNoises (chooseFrom [(0,0,0), (0.3,0,0), (0.5,0,0), (0.7,0,0)]) (graySnow) $ (\x -> \y -> 0.5))  200 200
                                                        fp x y = (floor $ 10*(2-(sin (2*x)+cos(2*y))) , floor $ 10*(1-(sin (2*y+x))))
                                  "mapped02"   -> newImageFromOldViaMap fp oldImage where
                                                        oldSizeX = 100
                                                        oldSizeY = 100
                                                        oldImage = (imageFromRandomlyFViaVec $ (scale 1) $ (\x -> \y -> noise)) oldSizeX oldSizeY  
                                                        fp x y = safeDoubleFloor oldSizeX oldSizeY (-1) (100*(0.5+y))
                                  
                                  "lines01"    -> imageFromRealF $ (\x -> \y -> tripleDiagonal (pulse (pw x y) (40*((y+0.5) + (factor x y)*(wave x))))) where
                                                        pw x y = 0.1 
                                                        wave x = 0.03 * (sin (30*x))
                                                        factor x y = (cosularDist (4*(x^2 + y^2))) 

                                  "lines02"    -> imageFromRealF $ crop (-0.51125) 0.51125 (-0.51125) (0.51125) (\x -> \y -> (1,1,1) `cadd` (((-1)*(pulse (pw x y) ((40*((y) + (factor x y)*(wave x)))-0.45 ))) `cScProd` (1,1,1) )) where
                                                        pw x y = 0.1 
                                                        wave x = 0.03 * (sin (30*x))
                                                        factor x y = (cosularDist (4*(x^2 + y^2))) 
                                  "lines03"    -> imageFromRealF $ crop (-0.51125) 0.51125 (-0.51125) (0.51125) (\x -> \y -> (1,1,1) `cadd` (((-1)*(pulse (pw x y) ((5*((y) + (factor x y)*(wave x)))-0.45 ))) `cScProd` (0,1,0) )) where
                                                        pw x y = 0.5 
                                                        wave x = 0.03 * (sin (30*x))
                                                        factor x y = (cosularDist (4*(x^2 + y^2))) 
                                  "testnoise" -> imageFromRandomlyF ((scale 1) $ combineNoises (chooseFrom [(0,0,0), (0.3,0,0), (0.5,0,0), (0.7,0,0)]) (graySnow) $ (\x -> \y -> 0.5)) 

                                         _             -> imageFromRealF' (\x -> \y -> (0,0,0))

-- basic function tools ------------------------------------------------------------------------------------------------
--
-- square wave with pulse width function. pw=0.5 gives square wave (min=0 max=1)  
pulse :: R -> R -> R
pulse pw x = aaa $ x - ((fromIntegral (floor x)))
                                        where aaa xx = if xx <= pw then 1 else 0


-- 'normalized' sin and cos in radians, range is 0 to 1
ncos x = (1 + (cos x))/2
nsin x = (1 + (sin x))/2

-- triangle function min=0 max=1 centered at 0
triangularDist x = if x>=0 then (if x>0.5 then 0 else 1-(2*x)) else (if x<(-0.5) then 0 else 2*x+1) 
-- cos bump. wrong name
cosularDist x = if x>=0 then (if x>0.5 then 0 else ncos (2*pi*x)) else (if x<(-0.5) then 0 else ncos (2*pi*x)) 

-- floors two numbers at the same time, doesn't leave the range 0 to size-1
safeDoubleFloor sizeX sizeY x y = (ppp x sizeX , ppp y sizeY) where
                                        ppp :: R -> Int -> Int        
                                        ppp xx ss = if (floor xx) >= ss then ss-1 else if xx<0 then 0 else floor xx

safeInvert x = if x<0.000000001  then 1000000000 else 1/x

peak2d :: R -> R -> R
peak2d x y = 1 - (max (abs x) (abs y))


stepFunction :: Int -> R -> R
stepFunction n x =if x>=0 then (nn/(nn-1))*((fromIntegral $ floor (nn * x)) / nn)
                                        else (nn/(nn-1))*(fromIntegral $ ceiling ((fromIntegral n) * x)) / (fromIntegral n) where nn = fromIntegral n

stepFloor :: Int -> R -> R
stepFloor n x =if x>=0 then ((fromIntegral $ floor (nn * x)) / nn)
                                        else (fromIntegral $ ceiling ((fromIntegral n) * x)) / (fromIntegral n) where nn = fromIntegral n



distance2D x y = sqrt (x*x + y*y)

angle2D x y = if distance2D x y <0.00000001 then 0 else acos(x/(distance2D x y))

sqr x = x^2
-- basic image ---------------------------------------------------------------------------------------------------
--

constant cc = \x -> \y -> cc


-- 2D transformations ---------------------------------------------------------------------------------------------------------

funnyTwirl :: R -> (R->R->R) -> R->R->R
funnyTwirl kk f x y = let r = x^2 + y^2
                      in  f ((cos $ r*kk)*x + (sin $ r*kk)*y ) ((cos $ r*kk)* y - (sin $ r*kk)*x) 

funnyTwirl2 :: R -> (R->R->R) -> R->R->R
funnyTwirl2 kk f x y = let r = safeInvert $ x^2 + y^2
                       in  f ((cos $ r*kk)*x + (sin $ r*kk)*y ) ((cos $ r*kk)* y - (sin $ r*kk)*x) 

funnyTwirlWithFunction :: (R -> R) -> (R->R->R) -> R->R->R
funnyTwirlWithFunction kk f x y = let r = kk (x^2 + y^2)
                      in  f ((cos $ r)*x + (sin $ r)*y ) ((cos $ r)* y - (sin $ r)*x) 

scaleNormal f x y = f (2*x) (2*y)
scale n f x y = f (n*x) (n*y)

--crops an image, xs first then ys, x is 0 to 1 top to bottom, y is 0 to 1 left to right
crop' :: R -> R -> R -> R -> (R->R-> a) -> R -> R -> a
crop' x1 x2 y1 y2 f x y  = let sx = x2-x1
                               sy = y2-y1        
                           in f (x1 + sx*x) (y1 + sy*y)

-- as usual, no '  means right side = 0.5
--crops an image, xs first then ys, x is -0.5 to 0.5 top to bottom, y is -0.5 to 0.5 left to right
crop :: R -> R -> R -> R -> (R->R-> a) -> R -> R -> a
crop x1 x2 y1 y2 f x y  = let sx = x2-x1
                              sy = y2-y1        
                           in f (x1 + sx*(x+0.5)) (y1 + sy*(y+0.5))
-- color --------------------------------------------------------------------------------------------------------
--

mixColors :: R -> R -> RealColor -> RealColor -> RealColor
mixColors c1 c2 v1 v2 = (((c1/(c1+c2)) `cScProd` v1) `cadd` ((c2/(c1+c2)) `cScProd` v2))

-- randomness and noise ------------------------------------------------------------------------------------------
--
type GeneratorState = State StdGen 

noise :: GeneratorState (R,R,R)
noise = randomColor 

-- infinite list of random floats
randomList :: RandomGen g => g -> [R] 
randomList g = randomRs (0.0,1.0) g

-- makes noise but applies a function to the color every time
noiseWithF :: (R->R) -> GeneratorState (R,R,R)
noiseWithF ff = liftM3 (,,) aa aa aa where aa = randomWithF ff  

grayNoiseWithF :: (R->R) -> GeneratorState (R,R,R)
grayNoiseWithF ff = liftM tripleDiagonal (randomWithF ff)

snow = grayNoiseWithF (stepFunction 2)

graySnow :: GeneratorState (R,R,R)
graySnow = liftM tripleDiagonal $ getRandomR

chooseFrom :: [a] -> GeneratorState a
chooseFrom xs = do r<-getRandomR
                   if r==1 then return (last xs) else return (xs !! (floor $ (fromIntegral (length xs)) * r)) 

randomWithF :: (R->R) -> GeneratorState R
randomWithF ff = liftM ff $ getRandomR
 
randomColor :: GeneratorState (R, R, R)
randomColor = liftM3 (,,) getRandomR getRandomR getRandomR

binaryRandomFromFunction :: (R->R->R) -> R -> R -> GeneratorState R
binaryRandomFromFunction f x y = do r<-getRandomR
                                    return $ if r<(f x y) then 1 else 0

normalishDistFromFunction :: (R->R->R) -> R -> R -> GeneratorState R
normalishDistFromFunction f x y = do 
                                   r<-getRandomR
                                   return $ let mu = f x y 
                                           in if r < mu then ((r^3)/(mu^2) -(3*(r^2)/mu) + 3*r) else (((r-mu)^3) / ((1-mu)^2) + mu)
                                   
snowFromFunction :: (R->R->R) -> R -> R -> GeneratorState (R,R,R)
snowFromFunction f x y = liftM tripleDiagonal $ (binaryRandomFromFunction f) x y

normalishSnowFromFunction :: (R->R->R) -> R -> R -> GeneratorState (R,R,R)
normalishSnowFromFunction f x y = liftM tripleDiagonal $ (normalishDistFromFunction f) x y

noiseFromFunction :: (R->R->R) -> R -> R -> GeneratorState (R,R,R)
noiseFromFunction f x y = do r1<-getRandomR
                             r2<-getRandomR
                             r3<-getRandomR
                             let norm = (f x y)/(r1 + r2 + r3)
                             return (r1*norm, r2*norm, r3*norm)  

noiseFromFunction2 :: (R->R->R) -> R -> R -> GeneratorState (R,R,R)
noiseFromFunction2 f x y = do r1<-getRandomR
                              r2<-getRandomR
                              r3<-getRandomR
                              let norm = (f x y)
                              return (r1*norm, r2*norm, r3*norm)  

-- combines two noise functions. if random<f chooses the first one, otherwise chooses the second. there has to be a better way of doing this
combineNoisesFromFunctions :: (R -> R -> GeneratorState (R,R,R)) -> (R -> R -> GeneratorState (R,R,R)) -> (R->R->R) -> R -> R -> GeneratorState (R,R,R) 
combineNoisesFromFunctions noise1 noise2 f x y = do r<-getRandomR
                                                           if r< (f x y) then (noise1 x y) else (noise2 x y)

combineNoises :: (GeneratorState (R,R,R)) -> (GeneratorState (R,R,R)) -> (R->R->R) -> R -> R -> GeneratorState (R,R,R) 
combineNoises noise1 noise2 f x y = do r<-getRandomR
                                       if r < (f x y) then noise1 else noise2

getRandomR :: GeneratorState R 
getRandomR =
  get >>= \gen ->
  let (val, gen') = randomR (0,1) gen in
  put gen' >>
  return val

-- infrastructure :) ------------------------------------------------------------------------------------------------------------------------------
--

type UImage = Array U DIM2 UIColor8 

type DImage = Array D DIM2 UIColor8

type UIColor8 = (Word8,Word8,Word8) 

type RealColor = (R,R,R) 

constantColor :: UIColor8 -> DIM2 -> UIColor8
constantColor col = \x -> col                 

blank :: Int -> Int -> Array U DIM2 UIColor8 
blank sizeX sizeY = t where
                      [t] = computeP 
                             (A.fromFunction (Z :. (sizeX::Int) :. (sizeY::Int)) (constantColor (255,255,255)) ) :: [Array U DIM2 UIColor8] 

-- these ' functions are for when the right side is +1
imageFromRealF' :: (R -> R-> (R,R,R)) -> Int -> Int -> Array U DIM2 UIColor8 
imageFromRealF' f sizeX sizeY = t where
                      [t] = computeP
                             (A.fromFunction (Z :. (sizeX::Int) :. (sizeY::Int)) (convertRealToPixels' f sizeX sizeY) ) :: [Array U DIM2 UIColor8] 

-- non ' functions are for right side = 0.5
imageFromRealF :: (R -> R -> (R,R,R)) -> Int -> Int -> Array U DIM2 UIColor8 
imageFromRealF f sizeX sizeY = t where
                      [t] = computeP
                             (A.fromFunction (Z :. (sizeX::Int) :. (sizeY::Int)) (convertRealToPixels f sizeX sizeY) ) :: [Array U DIM2 UIColor8] 

randomlyImageFromRandomlyF :: (R -> R -> GeneratorState (R,R,R)) -> Int -> Int -> GeneratorState (Array U DIM2 UIColor8)
randomlyImageFromRandomlyF f sizeX sizeY = undefined

imageFromRandomlyF :: (R -> R -> GeneratorState (R,R,R)) -> Int -> Int -> (Array U DIM2 UIColor8)
imageFromRandomlyF f sizeX sizeY = evalState (randomlyImageFromRandomlyF f sizeX sizeY) randomGenerator   

imageFromRandomlyFViaVec :: (R -> R -> GeneratorState (R,R,R)) -> Int -> Int -> Array U DIM2 UIColor8 
imageFromRandomlyFViaVec f sizeX sizeY = fromUnboxed (Z :. (sizeX::Int) :. (sizeY::Int)) $ tempEvaluateAsVector (convertRandomlyToPixels f sizeX sizeY) sizeX sizeY where
                                            tempEvaluateAsVector :: (Int -> Int -> GeneratorState UIColor8) -> Int -> Int -> VU.Vector UIColor8
                                            tempEvaluateAsVector ff sizeX sizeY = G.create 
                                                                                        $ do
                                                                                           let len = sizeX * sizeY
                                                                                           vec        <- MV.new len
                                                                                           let go !ix !iy !s
                                                                                                | iy == sizeY         = do return ()
                                                                                                | otherwise
                                                                                                = do        let (xx,s') = (runState (ff ix iy) s)
                                                                                                        MV.write vec (ix*sizeY + iy) $ xx 
                                                                                                        if (ix + 1 /= sizeX) then go (ix + 1) (iy) s' else go 0 (iy + 1) s'
                                                                                           go 0 0 randomGenerator 
                                                                                           return vec


        
-- fp is the pixelmap, expected to be a map to integers, telling where it should be. 
-- warning/todo: make sure that you don't get out of bounds when accessing the oldImage!
newImageFromOldViaMap :: (R->R->(Int,Int)) -> Array U DIM2 UIColor8 -> Int -> Int -> Array U DIM2 UIColor8 
newImageFromOldViaMap fp oldImage sizeX sizeY = fromUnboxed (Z :. (sizeX::Int) :. (sizeY::Int)) $ tempEvaluateAsVector (convertToPixels fp sizeX sizeY) sizeX sizeY where
                                                            tempEvaluateAsVector :: (Int->Int->(Int,Int)) -> Int -> Int -> VU.Vector UIColor8
                                                            tempEvaluateAsVector fpp sizeX sizeY = G.create 
                                                                                                        $ do
                                                                                                           let len = sizeX * sizeY
                                                                                                           vec        <- MV.new len
                                                                                                           let go !ix !iy 
                                                                                                                | iy == sizeY         = do return ()
                                                                                                                | otherwise
                                                                                                                = do        let (pX,pY) = fpp ix iy
                                                                                                                            xx = (!) oldImage (Z :. pX :. pY)
                                                                                                                        MV.write vec (ix*sizeY + iy) $ xx 
                                                                                                                        if (ix + 1 /= sizeX) then go (ix + 1) (iy) else go 0 (iy + 1) 
                                                                                                           go 0 0 
                                                                                                           return vec



--gradient = imageFromRealF' realGradient  

realGradient :: R -> R -> (R,R,R)
realGradient x y = (x^2, y^2, sqrt (x^2+y^2)/2)

invert :: RealColor -> RealColor
invert (x,y,z) = (1-x,1-y,1-z)

squeezeToRange :: RealColor -> RealColor -> RealColor -> RealColor
squeezeToRange lolo upo cc = lolo `cadd` (cc `dotProduct` (upo `cadd` lolo))

dotProduct :: RealColor -> RealColor -> RealColor  
dotProduct (x,y,z) (x',y',z') = (x*x' ,y*y',z*z')

cadd :: RealColor -> RealColor -> RealColor  
cadd (x,y,z) (x',y',z') = (x+x',y+y',z+z')

cAdd = cadd

cScProd :: (Num a) => a -> (a,a,a) -> (a,a,a)
cScProd x (v1,v2,v3) = (x*v1,x*v2,x*v3)

tripleDiagonal :: a -> (a,a,a)
tripleDiagonal x = (x,x,x)                                
        
tripleApply f (x,y,z) = (f x, f y, f z)
        
denemeGradient :: DIM2 -> UIColor8
denemeGradient (Z :. x :. y ) = tripleDiagonal $ trunkate (x+y)                

trunkate :: Int -> Word8
trunkate x = fromIntegral $ min 255 (max 0 x)

type R = Double

-- these ' functions are for when the right side is +1
-- these should not be defined separately! 
convertRealToPixels' :: (R -> R -> (R,R,R)) -> Int -> Int -> DIM2  -> UIColor8
convertRealToPixels' f sizeX sizeY (Z :. x :. y )  = colorScale ((if antiAlias then antiIlyas f (1/(fromIntegral sizeX)) (1/(fromIntegral sizeY)) else f) convX convY) where
                                                        convY = ((2 * fromIntegral y) - (fromIntegral sizeY)) / (fromIntegral sizeY)  
                                                        convX = ((2 * fromIntegral x) - (fromIntegral sizeX)) / (fromIntegral sizeX)  
                                                        rround :: R -> Int
                                                        rround xxx = round xxx
                                                        colorScale :: (R,R,R) -> UIColor8
                                                        colorScale (r,g,b) = (trunkate $ rround (255 * (bnd r)), trunkate $ rround (255 * g), trunkate $ rround (255 * b))
                                                        bnd :: R -> R
                                                        bnd xx = xx 

-- non ' functions are for right side = 0.5
convertRealToPixels :: (R -> R -> (R,R,R)) -> Int -> Int -> DIM2 -> UIColor8
convertRealToPixels f sizeX sizeY (Z :. x :. y )  = colorScale ((if antiAlias then antiIlyas f (1/(fromIntegral sizeX)) (1/(fromIntegral sizeY)) else f) convX convY) where
                                                        convY = ((2 * fromIntegral y) - (fromIntegral sizeY)) / (2 * fromIntegral sizeY)  
                                                        convX = ((2 * fromIntegral x) - (fromIntegral sizeX)) / (2 * fromIntegral sizeX)  
                                                        rround :: R -> Int
                                                        rround xxx = round xxx
                                                        colorScale :: (R,R,R) -> UIColor8
                                                        colorScale (r,g,b) = (trunkate $ rround (255 * (bnd r)), trunkate $ rround (255 * g), trunkate $ rround (255 * b))
                                                        bnd :: R -> R
                                                        bnd xx = xx 

-- right side = 0.5
antiIlyas :: (R -> R -> (R,R,R)) -> R -> R -> R -> R -> (R,R,R)
antiIlyas f pixelSizeX pixelSizeY x y = (cScProd) (0.25) $ (f x y) `cadd` (f (x+pixelSizeX*0.478845) y) `cadd` (f x (y+pixelSizeY*0.518234)) `cadd` (f (x+pixelSizeX*0.528382) (y+pixelSizeY*0.48734))   


-- non ' functions are for right side = 0.5
convertRandomlyToPixels :: (R -> R -> GeneratorState (R,R,R)) -> Int -> Int -> Int -> Int -> GeneratorState UIColor8
convertRandomlyToPixels f sizeX sizeY x y  = liftM colorScale (f convX convY) where
                                                        convY = ((2 * fromIntegral y) - (fromIntegral sizeY)) / (2 * fromIntegral sizeY)  
                                                        convX = ((2 * fromIntegral x) - (fromIntegral sizeX)) / (2 * fromIntegral sizeX)  
                                                        rround :: R -> Int
                                                        rround xxx = round xxx
                                                        colorScale :: (R,R,R) -> UIColor8
                                                        colorScale (r,g,b) = (trunkate $ rround (255 * (bnd r)), trunkate $ rround (255 * g), trunkate $ rround (255 * b))
                                                        bnd :: R -> R
                                                        bnd xx = xx 


-- non ' functions are for right side = 0.5
convertToPixels :: (R -> R -> a) -> Int -> Int -> Int -> Int -> a
convertToPixels f sizeX sizeY x y =  (f convX convY) where
                                                        convY = ((2 * fromIntegral y) - (fromIntegral sizeY)) / (2 * fromIntegral sizeY)  
                                                        convX = ((2 * fromIntegral x) - (fromIntegral sizeX)) / (2 * fromIntegral sizeX)  


black = (0,0,0)
white = (1,1,1)
red = (1,0,0)
green = (0,1,0)
blue = (0,0,1)
        



