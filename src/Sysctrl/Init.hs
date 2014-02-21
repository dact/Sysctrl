module Sysctrl.Init (initRead) where

import qualified Data.Sysctrl.Types.Internal.Automata as I (Automata(..))

import Data.Sysctrl.Types.Automata (Automata)
import Data.Sysctrl.Types.Util     (AutoPar(..))
import Sysctrl.Init.Automata       (automataInit)
import Sysctrl.Control             (loop)
import Sysctrl.Util                (correct,
				    _ExternalToInternal)

import Data.Yaml           (decodeEither)
import Data.ByteString     (ByteString)
import System.Posix.IO     (createPipe)
import System.Posix.Types  (Fd)
import Data.Map            (fromList)
import System.Exit         (exitFailure)
import Control.Applicative ((<$>))

-- #############
-- # Read step #
-- #############

initRead :: ByteString -> IO ()
initRead conf = case (decodeEither conf :: Either String [Automata]) of
  Right a -> initCheck a
  Left a -> putStrLn a >> exitFailure

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
