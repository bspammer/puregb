module Lib
    ( run
    ) where

-- base
import Control.Monad (when)
import Control.Exception (bracket)
-- GLFW-b, qualified for clarity
import qualified Graphics.UI.GLFW as GLFW
-- gl, all types and funcs here will already start with "gl"
import Graphics.GL.Core33
import Graphics.GL.Types

winWidth = 800

winHeight = 600

winTitle = "Hello Window"


initGLFW :: IO Bool
initGLFW = do
    GLFW.setErrorCallback $ Just $ \e s -> putStrLn $ unwords ["###", show e, show s]
    GLFW.init

-- | Ensures that we only run GLFW code while it's initialized, and also that we
-- always terminate it when we're done. Also, this function should only be used
-- from the main thread.
bracketGLFW :: IO () -> IO ()
bracketGLFW act = bracket initGLFW (const GLFW.terminate) $ \initWorked ->
    when initWorked act

-- type KeyCallback = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
callback :: GLFW.KeyCallback
callback window key scanCode keyState modKeys = do
    print key
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)

run :: IO ()
run = bracketGLFW $ do
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
    GLFW.windowHint (GLFW.WindowHint'Resizable True)
    maybeWindow <- GLFW.createWindow winWidth winHeight winTitle Nothing Nothing
    case maybeWindow of
        Nothing -> putStrLn "Failed to create a GLFW window!"
        Just window -> do
            -- enable keys
            GLFW.setKeyCallback window (Just callback)
            -- calibrate the viewport
            GLFW.makeContextCurrent (Just window)
            (x,y) <- GLFW.getFramebufferSize window
            glViewport 0 0 (fromIntegral x) (fromIntegral y)
            -- enter our main loop
            let loop = do
                    shouldContinue <- not <$> GLFW.windowShouldClose window
                    when shouldContinue $ do
                        -- event poll
                        GLFW.pollEvents
                        -- drawing
                        glClearColor 0.2 0.3 0.3 1.0
                        glClear GL_COLOR_BUFFER_BIT
                        -- swap buffers and go again
                        GLFW.swapBuffers window
                        loop
            loop
--
