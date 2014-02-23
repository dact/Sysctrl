module Sysctrl.Init (initRead) where

import qualified Data.Sysctrl.Types.Internal.Automata as I (Automata(..))

import Data.Sysctrl.Types.Automata (Automata)
import Data.Sysctrl.Types.Util     (AutoPar(..))
import Data.Sysctrl.Types          (Response(..),
                                    DataType(Error),
                                    AutoError(..))
import Sysctrl.Init.Automata       (automataInit)
import Sysctrl.Control             (loop)
import Sysctrl.Util                (correct_v,
                                    _ExternalToInternal)

import Data.Yaml           (decodeEither,encode)
import Data.ByteString.Char8     (ByteString, unpack)
import System.Posix.IO     (createPipe)
import Data.Map            (fromList)
import System.Exit         (exitFailure)
import Control.Applicative ((<$>))

-- #############
-- # Read step #
-- #############

initRead :: Bool -> ByteString -> IO ()
initRead tty conf = case (decodeEither conf :: Either String [Automata]) of
  Right a -> initCheck a
  Left a  -> (putStrLn.unpack.encode)  [Response "error" (Error $ errors a)]
  where
    errors a = [AutoError "YAML Error" a]
    -- ##############
    -- # Check step #
    -- ##############
    initCheck :: [Automata] -> IO ()
    initCheck autoList = do
      let internalAutoList = map _ExternalToInternal autoList
      case concat $ map (correct_v) internalAutoList of
        []     -> initProcces internalAutoList
        _err -> (putStrLn.unpack.encode) [Response "error" (Error _err)]
                  >> exitFailure
    -- ###############################
    -- # Procces initialization step #
    -- ###############################
    initProcces :: [I.Automata] -> IO ()
    initProcces autoList = do
      let mkAutoPar auto = do
            (r,w) <- createPipe
            (,) (I.name auto) <$> AutoPar auto
                              <$> (automataInit w r auto)
      autoProcList <- sequence $ map (mkAutoPar) autoList
      initLoop autoProcList
    -- ############################
    -- # Loop initialization step #
    -- ############################
    initLoop :: [(String,AutoPar)] -> IO ()
    initLoop autoMap = loop tty $ fromList autoMap
