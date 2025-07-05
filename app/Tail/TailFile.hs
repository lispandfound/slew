-- The following code is a copy of the tailfile-hinotify package on
-- hackage: https://hackage.haskell.org/package/tailfile-hinotify
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ YOU SHOULD
-- JUST USE THIS I am taking the simple monadic update function in
-- this package because it doesn't install for me with the bytestring
-- dependency restrictions placed on the repo. Also it appears to be
-- unmaintained. What follows is the original MIT License for the
-- package:

-- MIT License

-- Copyright (c) 2016 Daniel Díaz Carrete

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

{-# language NumDecimals #-}
{-# language BangPatterns #-}
module Tail.TailFile (tailFile) where

import Control.Concurrent (threadDelay)
import Control.Exception
import qualified Data.ByteString
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified Data.Text
import qualified Data.Text.Encoding
import System.INotify
import System.IO (
  hSeek
  ,SeekMode(AbsoluteSeek,SeekFromEnd)
  ,hFileSize)
import System.IO.Error (isDoesNotExistError)

{-| Tail a file, while keeping an internal state.

    If the file doesn't exist, `tailFile` will poll for it until it is found.

    If `tailFile` detects the file has been moved or renamed, it goes back to
    watching a file with the original name.

    `tailFile` also detects file truncations, in which case it starts reading
    again from the beginning.

    Data already existing in the file before `tailFile` is invoked is ignored.
 -}
tailFile :: FilePath
         -> (a -> Data.ByteString.ByteString -> IO a) -- ^ State update function.
         -> IO a -- ^ Monadic action for getting the initial state.
         -> IO void -- ^ The result action never returns!
tailFile filepath callback initial = withINotify (\i ->
    do state <- initial
       loop i state)
    where
    loop i =
        let go pristine a = do ea' <- tryJust (guard . isDoesNotExistError)
                                              (watchFile pristine i a)
                               case ea' of
                                  Left ()  -> do threadDelay 5e5
                                                 go False a -- reuse the state
                                  Right a' -> go False a'
        in  go True
    watchFile pristine i a =
        do sem <- newMVar mempty
           bracket (addWatch i
                             [Modify,MoveSelf,DeleteSelf]
                             (Data.Text.Encoding.encodeUtf8 (Data.Text.pack filepath))
                             (\event -> let stop = Any (case event of
                                                           MovedSelf {} -> True
                                                           Deleted {} -> True
                                                           _ -> False)
                                        in do old <- fold <$> tryTakeMVar sem
                                              new <- evaluate $ old <> stop
                                              putMVar sem new))
                   removeWatch
                   (\_ -> withFile filepath ReadMode (\h ->
                              do when pristine
                                      (hSeek h SeekFromEnd 0)
                                 sleeper sem h a))
    sleeper sem h =
        let go ms a = do event <- takeMVar sem
                         size' <- hFileSize h
                         for_ ms (\size -> when (size' < size) -- truncation
                                                (hSeek h AbsoluteSeek 0))
                         !a' <- drainBytes h a
                         if getAny event then return a'
                                         else go (Just size') a'
        in  go Nothing
    drainBytes h =
        let go a = do c <- Data.ByteString.hGetSome h defaultChunkSize
                      if Data.ByteString.null c
                         then do return a
                         else do !a' <- callback a c
                                 go a'
        in  go
