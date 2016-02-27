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
import Data.Array.Repa 			        as A
import qualified Data.Array.Repa.Repr.Unboxed   as U
import Prelude				        as P
import Data.Time
import System.Random
import System.IO.Unsafe

import qualified Data.Vector.Unboxed		as VU
import qualified Data.Vector.Generic		as G
import qualified Data.Vector.Unboxed.Mutable	as MV

-- this is the function that will be drawn 
theFunctionToDraw = f

f :: R -> R -> R
f x y = x^2+y^2 



-- global settings
defaultWidth :: Int
defaultWidth = 300
antiAlias = False
randomGenerator = mkStdGen 42

--
usage 	= putStr $ unlines
	[ "umutArt <algorithm::string> <sizeX::Int> <sizeY::Int>" ]
--
--


main :: IO ()
main = do args <- getArgs       
          case args of
             (filename:swidth:[]) -> let width = read swidth::Int 
                                     in writeBMPFromFunction (\x -> \y -> (tripleDiagonal $ theFunctionToDraw x y)) width width filename
             _                    -> putStr "Internal problem. Arguments of the running program might be wrong."
          putStr "Done!\n"


-- run with selected algorithm
writeBMPFromFunction :: (R->R->(R,R,R)) -> Int -> Int -> String -> IO ()
writeBMPFromFunction func sizeX sizeY filename = do let umutsImage = imageFromRealF' func sizeX sizeY
                                   	            timeStr <- liftM show (getCurrentTime >>= return)
	                                            writeImageToBMP (filename P.++ ".bmp") umutsImage




-- basic function tools ------------------------------------------------------------------------------------------------
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
											   vec	<- MV.new len
											   let go !ix !iy !s
												| iy == sizeY 	= do return ()
												| otherwise
												= do	let (xx,s') = (runState (ff ix iy) s)
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
													   vec	<- MV.new len
													   let go !ix !iy 
														| iy == sizeY 	= do return ()
														| otherwise
														= do	let (pX,pY) = fpp ix iy
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
convertRealToPixels' f sizeX sizeY (Z :. x :. y )  = colorScale ((if antiAlias then antiIlyas g (1/(fromIntegral sizeX)) (1/(fromIntegral sizeY)) else g) convX convY) where
                                                        g xx yy = f yy xx
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





