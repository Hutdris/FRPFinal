{-# LANGUAGE Arrows, BangPatterns, NamedFieldPuns #-}
import FRP.Yampa
import Control.Concurrent
import FRP.Yampa.Vector3
import FRP.Yampa.Utilities
import Unsafe.Coerce
import Data.IORef
import FRP.Yampa.Integration

import Graphics.UI.GLUT hiding (Level,Vector3(..),normalize)
import qualified Graphics.UI.GLUT as G(Vector3(..))

type R = GLdouble

type Pos = Double
type Vel = Double



data GameState = Game { xpos     :: Pos, ypos :: Pos,
                        xvel      :: Vel, yvel :: Vel,
                        playerXPos :: Pos, 
                        playerXVel :: Vel}


					  
--mainSF = ((movingBall (-4.0) 2.0) &&& (bouncingBall 10.0 0.0)) >>^ (\ ((xpos,xvel),(ypos, yvel)) -> putStrLn ("pos: " ++ show xpos ++ "," ++ show ypos ++ "   vel: " ++ show xvel ++ "," ++ show yvel) >> draw (xpos, ypos))
mainSF = parseInput >>> update >>> (movingBall 0.0 3.0) >>> (bouncingBall 10.0 0.0)   >>^ (\ gs -> draw gs)
--mainSF = parseInput >>^ (\ParsedInput{aCount, dCount}-> putStrLn ("playerPos: " ++ show (aCount ) ++ "," ++ "   vel: " ++ show (dCount ) ))  
 

main :: IO ()
main = do
    newInput <- newIORef NoEvent
    oldTime <- newIORef (0 :: Int)
    rh <- reactInit (initGL >> return NoEvent) (\_ _ b -> b >> return False) 
                    mainSF
    displayCallback $= return ()
    keyboardMouseCallback $= Just 
        (\k ks m _ -> writeIORef newInput (Event $ Keyboard k ks m))
    idleCallback $= Just (idle newInput oldTime rh)
    oldTime' <- get elapsedTime
    writeIORef oldTime oldTime' 
    mainLoop


-- | Reactimation iteration, supplying the input
idle :: IORef (Event Input) -> IORef Int -> 
        ReactHandle (Event Input) (IO ()) -> IO ()
idle newInput oldTime rh = do
    newInput' <- readIORef newInput
    newTime'  <- get elapsedTime
    oldTime'  <- get oldTime
    let dt = let dt' = (fromIntegral $ newTime' - oldTime')/1000
             in if dt' < 0.8 then dt' else 0.8
    react rh (dt, Just newInput')
    writeIORef oldTime newTime'
    return ()
					
					
-- | Ignore the following Graphics GLUT part for now. 					
initGL :: IO ()
initGL = do
    getArgsAndInitialize
    initialDisplayMode $= [ WithDepthBuffer, DoubleBuffered ]
    createWindow "Bounce"
    depthFunc          $= Just Less
    clearColor         $= Color4 0 0 0 0
    light (Light 0)    $= Enabled
    lighting           $= Enabled
    lightModelAmbient  $= Color4 0.5 0.5 0.5 1
    diffuse (Light 0)  $= Color4 0 0 0 1
    blend              $= Enabled
    blendFunc          $= (SrcAlpha, OneMinusSrcAlpha)
    colorMaterial      $= Just (FrontAndBack, AmbientAndDiffuse)
    reshapeCallback    $= Just resizeScene
    return ()
	
resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1) -- prevent divide by zero
resizeScene s@(Size width height) = do
  -- putStrLn "resizeScene"
  viewport   $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45 (w2/h2) 1 1000
  matrixMode $= Modelview 0
 where
   w2 = half width
   h2 = half height
   half z = realToFrac z / 2


----

data Input = Keyboard { key       :: Key,
                        keyState  :: KeyState,
                        modifiers :: Modifiers }
						

data ParsedInput = 
    ParsedInput { aCount :: Double, dCount :: Double }


-- Event Definition:
filterKeyDowns :: SF (Event Input) (Event Input)
filterKeyDowns = arr $ filterE ((==Down) . keyState)

keyIntegral :: Double -> SF (Event a) Double
keyIntegral a = let eventToSpeed (Event _) = a
                    eventToSpeed NoEvent   = 0 
                in arr eventToSpeed >>> integral 
                       
-- Input
parseInput :: SF (Event Input) ParsedInput
parseInput = proc i -> do
    down     <- filterKeyDowns                  -< i
    aCount   <- countKey 'a'                    -< down
    dCount   <- countKey 'd'                    -< down
    returnA -< ParsedInput aCount dCount                            
    where countKey c  = filterE ((==(Char c)) . key) ^>> keyIntegral 10
          filterKey k = arr $ filterE ((==k) . key)



-------------------------------------
				

{- 
  switch :: SF in (out, Event t)
       -> (t -> SF in out)
       -> SF in out 
-}

flyingBall :: Pos -> Vel -> SF GameState GameState
flyingBall x0 v0 = proc gs@(Game{ xpos, ypos, xvel, yvel, playerXPos, playerXVel }) -> do
  v <- integral >>^ (+ v0) -< 0    -- v = v0 + \int_0^T a dt
  x <- integral >>^ (+ x0) -< v        -- y = y0 + \int_0^T v dt
  returnA -< Game x ypos v yvel playerXPos playerXVel

movingBall :: Pos -> Vel -> SF GameState GameState
movingBall  x0 v0 = switch (mb x0 v0) (\ (pos, vel) -> movingBall pos (-vel))
    where mb x0' v0' = proc input -> do 
                        gs <- flyingBall x0' v0' -< input
                        event <- edge -< ((xpos gs) <=(-5) || (xpos gs) > 5) --edge :: SF Bool (Event ())
                        
                        returnA -<( gs, event `tag` (xpos gs, xvel gs))      --
                        
fallingBall ::  Pos ->  Vel -> SF GameState GameState
fallingBall y0 v0 = proc gs@(Game{ xpos, ypos, xvel, yvel, playerXPos, playerXVel }) -> do
  v <- integral >>^ (+ v0) -< -9.81    -- v = v0 + \int_0^T a dt
  y <- integral >>^ (+ y0) -< v        -- y = y0 + \int_0^T v dt
  returnA -< Game xpos y xvel v playerXPos playerXVel

bouncingBall :: Pos -> Vel -> SF GameState GameState
bouncingBall y0 v0 = switch (bb y0 v0) (\ (pos, vel) -> bouncingBall pos (-vel))
    where bb y0' v0' = proc input -> do
                    gs <- fallingBall y0' v0' -< input 
                    event <- edge -< ((ypos gs) <=0 && (ypos gs)>(-0.3))  && (abs((xpos gs)-(playerXPos gs))<3.0)
                    returnA -<  (gs , event `tag` (ypos gs, yvel gs))


update :: SF ParsedInput GameState
update = proc pi@(ParsedInput{ aCount, dCount }) -> do
    returnA -< Game { xpos     = 0, ypos = 0, xvel = 0, yvel = 0,
                      playerXPos = realToFrac (dCount-aCount), playerXVel = 0}

----------------------------					  

draw :: GameState-> IO ()
draw gs@(Game{ xpos, ypos, xvel, yvel, playerXPos, playerXVel }) = do
    clear [ ColorBuffer, DepthBuffer ]
    loadIdentity
    renderBall $ vector3 (unsafeCoerce xpos) (unsafeCoerce ypos) (-20)	
    renderPlayer $ vector3 (unsafeCoerce playerXPos) (-3) (-20)
    swapBuffers
    where size2 :: R
          size2 = (fromInteger $ 6)/2
          green  = Color4 0.8 1.0 0.7 0.9 :: Color4 R
          greenG = Color4 0.8 1.0 0.7 1.0 :: Color4 R
          red    = Color4 1.0 0.7 0.8 1.0 :: Color4 R
          renderShapeAt s p = preservingMatrix $ do
            translate $ G.Vector3 (0.5 - size2 + vector3X p)
                                  (0.5 - size2 + vector3Y p)
                                  (0.5 - size2 + vector3Z p)
            renderObject Solid s
          renderBall   = (color red >>) . (renderShapeAt $ Sphere' 0.5 20 20)          
          renderPlayer   = (color green >>) . (renderShapeAt $ Cube 6)  		  

		  



    