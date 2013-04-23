-- | Quick, hacky sendmail wrapper
module Sihemo.Sendmail
    ( sendmail
    ) where

import System.Process (readProcess)

sendmail :: String    -- ^ Recipient
         -> String    -- ^ Subject
         -> [String]  -- ^ Content (lines)
         -> IO ()     -- ^ Blocks until mail is sent
sendmail recipient subject body = do
    _ <- readProcess "/usr/sbin/sendmail" ["-t"] $ unlines $
        [ "To: " ++ recipient
        , "Subject: " ++ subject
        , ""
        ] ++ body
    return ()
