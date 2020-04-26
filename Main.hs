{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import qualified Data.String.Class as S
import Network.Xmpp
import Network.Xmpp.Internal hiding (priority, status)
import System.Console.GetOpt
import System.Environment


passWordEnvVar = "HSENDXMPP_PASSWORD"

data Options = Options
	{ oUserName :: String
	, oPassWord :: String
	, oServer :: String
	} deriving (Eq, Show)

defaultOptions = Options
	{ oUserName = ""
	, oPassWord = ""
	, oServer = ""
	}

options :: [OptDescr (Options -> Options)]
options =
	[ Option ['u']	["username"]	(ReqArg	(\str o -> o { oUserName = str }) "user")	"Use this username to authenticate to the server"
	, Option ['p']	["password"]	(ReqArg	(\str o -> o { oPassWord = str }) "password") $	"Use this password to authenticate to the server.\nThe password can also be provided via " ++ passWordEnvVar ++ " environment variable to avoid it leaking into process lists, and it will override the CLI option contents."
	, Option ['j']	["jserver"]	(ReqArg	(\str o -> o { oServer = str }) "server")	"Connect to this server"
	]

getOpts :: IO (Options, [String])
getOpts = do
	args <- getArgs
	pn <- getProgName
	case getOpt Permute options args of
		(o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
		(_,_,errs) -> ioError (userError (concat errs ++ usageInfo ("Usage: " ++ pn ++ " <recipient>") options))

main :: IO ()
main = do
	(opts, recipients) <- getOpts
	text <- getContents
	envPassWord <- lookupEnv passWordEnvVar
	let justEnvPassWord = fromMaybe "" envPassWord
	let passWord = if null justEnvPassWord then oPassWord opts else justEnvPassWord

	eSess <- session (oServer opts) (simpleAuth (S.toText $ oUserName opts) (S.toText passWord)) def
	let sess = either (error . show) id $ eSess
	sendPresence presenceOnline sess
	mapM_ (\tjid -> sendMessage ((simpleIM (parseJid tjid) $ S.toText text) { messageType = Chat }) sess >> pure ()) recipients
