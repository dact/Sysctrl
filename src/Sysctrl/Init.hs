module Sysctrl.Init (initRead, initRead_v)where

import Data.Sysctrl.Types.Automata (Automata)
import Data.Sysctrl.Types.Util
import qualified Data.Sysctrl.Types.Internal.Automata as I
import Sysctrl.Init.Automata
import Sysctrl.Util
import Sysctrl.Control

import Data.Yaml (decodeEither, decode)
import Data.ByteString (ByteString)
import System.Posix.IO(createPipe)
import System.Posix.Types(Fd)
import Control.Applicative((<$>))
import Data.Map (fromList)
import System.Exit
import Control.Applicative

-- #############
-- # Read step #
-- #############

initRead_v :: ByteString -> IO ()
initRead_v conf = case (decodeEither conf :: Either String [Automata]) of
  Right a -> initCheck a
  Left a -> putStrLn a >> exitFailure


initRead :: ByteString -> IO ()
initRead conf = do
  let errorMsg = "Error on File, use -v for more detail."
  case (decode conf :: Maybe [Automata]) of
    Just a -> initCheck a
    Nothing -> putStrLn errorMsg >> exitFailure

-- ##############
-- # Check step #
-- ##############

initCheck :: [Automata] -> IO ()
initCheck autoList = do
  let internalAutoList = map _ExternalToInternal autoList
  let errorMsg = "Error on Automata description"
  if all correct internalAutoList
    then
    initProcces internalAutoList
    else
    putStrLn errorMsg >> exitFailure

-- ###############################
-- # Procces initialization step #
-- ###############################

initProcces :: [I.Automata] -> IO ()
initProcces autoList = do
  (r,w) <- createPipe
  let mkAutoPar auto = (,) (I.name auto)
                       <$> AutoPar auto
                       <$> (automataInit w auto)
  autoProcList <- sequence $ map (mkAutoPar) autoList
  initLoop autoProcList r

-- ############################
-- # Loop initialization step #
-- ############################

initLoop :: [(String,AutoPar)] -> Fd -> IO ()
initLoop autoMap control = loop control $ fromList autoMap
