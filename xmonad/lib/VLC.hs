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

import qualified Data.ByteString.Char8 as B

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

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
talkToVLC path msg = do s <- socket AF_UNIX Stream 0
                        connect s $ SockAddrUnix path
                        send s $ B.pack msg
                        close s

talkToVLC' = flip talkToVLC
