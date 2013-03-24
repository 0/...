-- | Control VLC via a Unix socket.
module VLC
    ( pause
    , play
    , stop
    , prev
    , next
    , left
    , right
    , voldn
    , volup
    ) where

import qualified Network as N

import qualified System.IO as IO


pause = talkToVLC' "pause"
play  = talkToVLC' "play"
stop  = talkToVLC' "stop"
prev  = talkToVLC' "prev"
next  = talkToVLC' "next"
left  = talkToVLC' "key key-jump-short"
right = talkToVLC' "key key-jump+short"
voldn = talkToVLC' "voldown 1"
volup = talkToVLC' "volup 1"


talkToVLC :: String -> String -> IO ()
talkToVLC path msg = do h <- N.connectTo "" $ N.UnixSocket path
                        IO.hPutStrLn h msg
                        IO.hClose h

talkToVLC' = flip talkToVLC
