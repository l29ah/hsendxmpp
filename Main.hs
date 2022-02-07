{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Data.Maybe
import qualified Data.String.Class as S
import qualified Data.Text.IO as T
import Network.TLS
import Network.Xmpp
import Network.Xmpp.Internal hiding (priority, status)
import Network.Xmpp.Extras.MUC
import System.Console.GetOpt
import System.Environment
import System.Log.Logger


passWordEnvVar = "HSENDXMPP_PASSWORD"

data Options = Options
	{ oUserName :: String
	, oPassWord :: String
	, oServer :: String
	, oResource :: String
	, oMessageType :: MessageType
	, oVerbose :: Bool
	, oNoTLSVerify :: Bool
	} deriving (Eq, Show)

defaultOptions = Options
	{ oUserName = ""
	, oPassWord = ""
	, oServer = error "no server specified"
	, oResource = "hsendxmpp"
	, oMessageType = Chat
	, oVerbose = False
	, oNoTLSVerify = False
	}

options :: [OptDescr (Options -> Options)]
options =
	[ Option ['u']	["username"]	(ReqArg	(\str o -> o { oUserName = str }) "user")	"Use this username to authenticate to the server"
	, Option ['p']	["password"]	(ReqArg	(\str o -> o { oPassWord = str }) "password") $	"Use this password to authenticate to the server.\nThe password can also be provided via " ++ passWordEnvVar ++ " environment variable to avoid it leaking into process lists, and it will override the CLI option contents."
	, Option ['j']	["jserver"]	(ReqArg	(\str o -> o { oServer = str }) "server")	"Connect to this server"
	, Option ['r']	["resource"]	(ReqArg	(\str o -> o { oResource = str }) "res")	"Use resource res for the sender [default: 'hsendxmpp']"
	, Option ['c']	["chatroom"]	(NoArg	(\o -> o { oMessageType = GroupChat }))		"Send the message to a chatroom (MUC)"
	, Option ['v']	["verbose"]	(NoArg	(\o -> o { oVerbose = True }))			"Be verbose on what's happening on the wire"
	, Option ['n']	["no-tls-verify"]	(NoArg	(\o -> o { oNoTLSVerify = True }))	"Accept TLS certificates without verification"
	]

getOpts :: IO (Options, [String])
getOpts = do
	args <- getArgs
	pn <- getProgName
	case getOpt Permute options args of
		(o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
		(_,_,errs) -> ioError (userError (concat errs ++ usageInfo ("Usage: " ++ pn ++ " [options] <recipient1> [<recipient2> ...]") options))

main :: IO ()
main = do
	(opts, recipients) <- getOpts
	when (oVerbose opts) $ updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
	text <- T.getContents
	envPassWord <- lookupEnv passWordEnvVar
	let justEnvPassWord = fromMaybe "" envPassWord
	let passWord = if null justEnvPassWord then oPassWord opts else justEnvPassWord

	let authData = Just (fst $ fromJust (simpleAuth (S.toText $ oUserName opts) (S.toText passWord)), if null $ oResource opts then Nothing else Just $ S.toText $ oResource opts) :: AuthData
	let sessionConfiguration = if oNoTLSVerify opts
		then def { sessionStreamConfiguration = def { tlsParams = xmppDefaultParams { clientHooks = def { onServerCertificate = \_ _ _ _ -> pure [] } } } }
		else def
	eSess <- session (oServer opts) authData sessionConfiguration
	let sess = either (error . show) id $ eSess
	sendPresence presenceOnline sess
	mapM_ (\tjid -> do
			let parsedJid = parseJid tjid
			when (oMessageType opts == GroupChat) $ do
				let (roomName, roomServer, _) = jidToTexts parsedJid
				let roomJid = fromJust $ jidFromTexts roomName roomServer $ Just $ S.toText $ oResource opts
				result <- joinMUCResult roomJid Nothing sess
				either (\err -> error $ show $ stanzaErrorText err) (const $ pure ()) result
			result <- sendMessage ((simpleIM parsedJid text) { messageType = oMessageType opts }) sess
			either (\err -> error $ show err) pure result
		) recipients
	sendPresence presenceOffline sess
	-- FIXME a workaround for https://github.com/l29ah/hsendxmpp/issues/1
	threadDelay 1000000
	endSession sess
