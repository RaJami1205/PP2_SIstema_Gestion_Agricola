{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module OpcionesOperativas where

import Data.Csv (
    DefaultOrdered(..),
    ToNamedRecord(..),
    FromNamedRecord(..),
    encodeDefaultOrderedByName,
    decodeByName,
    (.=),
    namedRecord,
    (.:)  -- Operador para parsear campos
    )
import qualified Data.ByteString.Lazy as BL
import Data.Vector (fromList, toList)
import System.Directory (doesFileExist)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Tipo de dato para herramientas
data Herramienta = Herramienta {
    codigo :: Text,
    nombre :: Text,
    descripcion :: Text,
    tipo :: Text
} deriving (Show)

-- Instancias necesarias para CSV
instance DefaultOrdered Herramienta where
    headerOrder _ = fromList ["codigo", "nombre", "descripcion", "tipo"]

instance ToNamedRecord Herramienta where
    toNamedRecord h = namedRecord [
        "codigo" .= codigo h,
        "nombre" .= nombre h,
        "descripcion" .= descripcion h,
        "tipo" .= tipo h]

instance FromNamedRecord Herramienta where
    parseNamedRecord r = Herramienta
        <$> r .: "codigo"
        <*> r .: "nombre"
        <*> r .: "descripcion"
        <*> r .: "tipo"

-- Función principal para registrar herramientas 
registrarHerramientas :: IO ()
registrarHerramientas = do
    putStrLn "\n=== Registro de Herramientas ==="
    putStr "Ingrese la ruta del archivo TXT: "
    rutaTxt <- getLine
    
    existe <- doesFileExist rutaTxt
    if not existe
        then putStrLn "Error: El archivo no existe."
        else do
            contenido <- TIO.readFile rutaTxt
            let herramientas = map parsearLinea (T.lines contenido)
            BL.writeFile "data/herramientas.csv" (encodeDefaultOrderedByName herramientas)
            putStrLn "\nHerramientas registradas exitosamente\n"
            
            -- Mostrar todas las herramientas disponibles
            putStrLn "=== Herramientas Disponibles ==="
            mostrarHerramientasDisponibles

-- Función para mostrar herramientas
mostrarHerramientasDisponibles :: IO ()
mostrarHerramientasDisponibles = do
    csvData <- BL.readFile "data/herramientas.csv"
    case decodeByName csvData of
        Left err -> putStrLn $ "Error leyendo herramientas: " ++ err
        Right (_, herramientas) -> do
            mapM_ (putStrLn . formatHerramienta) (toList herramientas)
            putStrLn ""

-- Formatear una herramienta para mostrar
formatHerramienta :: Herramienta -> String
formatHerramienta h = 
    T.unpack $ T.intercalate " | " 
        [ "Código: " <> codigo h
        , "Nombre: " <> nombre h
        , "Tipo: " <> tipo h
        , "Descripción: " <> descripcion h
        ]

-- Función para parsear cada línea del TXT
parsearLinea :: Text -> Herramienta
parsearLinea linea =
    case T.splitOn "," linea of
        [c, n, d, t] -> Herramienta c n d t
        _ -> error $ "Formato inválido en línea: " ++ T.unpack linea