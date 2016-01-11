module Play where
import Control.Monad
import System.Environment
import Control.Concurrent
import Sound.ProteaAudio
playSound :: String -> IO ()
playSound file = do
    initAudio 8 44100 1024
    smp <- sampleFromFile file 1
    volume 5 5
    soundPlay smp 1 1 0 1
    let loop = do
            n <- soundActive
            when  (n > 0) $ threadDelay 1000000 >> loop
    loop
    finishAudio