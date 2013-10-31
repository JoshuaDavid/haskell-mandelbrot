import Graphics.UI.GLUT

ticks = [-1, -0.995 .. 1]
resolution = 30

myCoords :: [(GLfloat, GLfloat, GLfloat)]
myCoords = [ (a, b, 0) | a <- ticks, b <- ticks]

step :: Num a => (a, a) -> (a, a) -> (a, a)
step (a0, b0) (a, b) = (a * a - b * b + a0, 2 * a * b + b0)

applyNTimes :: Integral a => a -> (b -> b) -> b -> b
applyNTimes n f x
    | n == 0      = x
    | otherwise   = (applyNTimes (n - 1) f (f x))

locationAfterNIterations :: (Integral a, Num a1) => a -> (a1, a1) -> (a1, a1)
locationAfterNIterations n (a, b) = applyNTimes n (step (a, b)) (a, b)

distanceFromOrigin2 :: Num a => (a, a) -> a
distanceFromOrigin2 (a, b) = a * a + b * b

converges :: (Num a, Ord a) => (a, a) -> Bool
converges (a, b) = distanceFromOrigin2 (locationAfterNIterations resolution (a, b)) < 4

vertexify :: VertexComponent a => (a, a, a) -> IO ()
vertexify (x, y, z) = vertex (Vertex3 x y z)

firstTwoOfTrio :: (a, a, a) -> (a, a)
firstTwoOfTrio (a, b, c) = (a, b)

glPointIsVisible :: (Num a, Ord a) => (a, a, a) -> Bool
glPointIsVisible (a, b, c) = converges (firstTwoOfTrio (a, b, c))

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Mandelbrot Set"
    displayCallback $= display
    mainLoop

display :: DisplayCallback
display = do
    clear [ ColorBuffer ]
    renderPrimitive Points $ mapM_ vertexify (filter glPointIsVisible myCoords)
    flush
