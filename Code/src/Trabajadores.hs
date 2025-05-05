{-# LANGUAGE OverloadedStrings #-} -- Permite usar literales de texto como tipos distintos de String (como Text, ByteString, etc.)

-- src/Trabajadores.hs
module Trabajadores (Trabajador(..), cargarTrabajadores) where
-- El (..) exporta el tipo y todos sus campos

import Data.Csv (FromNamedRecord(..), decodeByName, (.:))
import qualified Data.ByteString.Lazy as BL
import Data.Vector (toList)

data Trabajador = Trabajador {
    cedula :: String,
    nombre :: String,
    rol :: String
} deriving (Show, Eq)

instance FromNamedRecord Trabajador where
    parseNamedRecord r = Trabajador
        <$> (r .: "cedula")
        <*> (r .: "nombre_completo")
        <*> (r .: "rol")

cargarTrabajadores :: FilePath -> IO (Either String [Trabajador])
cargarTrabajadores filePath = do
    csvData <- BL.readFile filePath
    case decodeByName csvData of
        Left err -> return $ Left $ show err
        Right (_, v) -> return $ Right (toList v)