{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module OpcionesGenerales
    ( gestionCosechasMenu
    ) where

import GHC.Generics (Generic)
import Trabajadores (Trabajador(..), cedula)
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime)
import Data.List (find, isInfixOf, intercalate)
import System.IO
import qualified Data.Char as Char
import System.Random (randomRIO)
import qualified Data.ByteString.Lazy as BL
import Data.Csv ( encodeDefaultOrderedByName
                , encodeDefaultOrderedByNameWith
                , decodeByName
                , FromNamedRecord(..)
                , ToNamedRecord(..)
                , DefaultOrdered(..)
                , (.:)
                , (.=)
                , namedRecord
                , Header
                , defaultEncodeOptions
                , encIncludeHeader
                )
import qualified Data.Vector as V
import System.Directory (doesFileExist)
import Control.Monad (when, unless)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

data Parcela = Parcela
    { idParcela :: String
    , nombreParcela :: String
    , zonaParcela :: String
    , areaParcela :: Double
    , vegetalesParcela :: String  
    , herramientasParcela :: String
    } deriving (Show)

-- Instancias para CSV
instance FromNamedRecord Parcela where
    parseNamedRecord r = Parcela
        <$> r .: "id"
        <*> r .: "nombre"
        <*> r .: "zona"
        <*> r .: "area"
        <*> r .: "vegetales"  
        <*> r .: "herramientas"

data Cosecha = Cosecha
    { id :: String
    , trabajador :: String
    , parcela :: String
    , fecha_inicio :: String
    , fecha_fin :: String
    , vegetal :: String
    , cantidad :: Int
    , estado :: String  
    } deriving (Show, Generic)

instance ToNamedRecord Cosecha
instance FromNamedRecord Cosecha
instance DefaultOrdered Cosecha where
    headerOrder _ = cabecerasCosecha

splitOn :: Char -> String -> [String]
splitOn delim = words . map (\c -> if c == delim then ' ' else c)

-- Cabeceras para el CSV
cabecerasCosecha :: Header
cabecerasCosecha = V.fromList ["id", "trabajador", "parcela", "fecha_inicio", "fecha_fin", "vegetal", "cantidad", "estado"]

-- Generar ID de cosecha (6 caracteres alfanuméricos)
generarIdCosecha :: IO String
generarIdCosecha = do
    let chars = ['0'..'9'] ++ ['A'..'Z']
    sequence $ replicate 6 (randomRIO (0, length chars - 1) >>= return . (chars !!))

-- Verificar si un trabajador existe
verificarTrabajador :: String -> IO Bool
verificarTrabajador id = do
    exists <- doesFileExist "data/trabajadores.csv"
    if not exists then return False else do
        contenido <- BL.readFile "data/trabajadores.csv"
        case decodeByName contenido of
            Left _ -> return False
            Right (_, trabajadores) -> return $ any ((== id) . cedula) (V.toList trabajadores)

-- Verificar si una parcela existe
verificarParcela :: String -> IO Bool
verificarParcela id = do
    exists <- doesFileExist "data/parcelas.csv"
    if not exists then return False else do
        contenido <- BL.readFile "data/parcelas.csv"
        case decodeByName contenido of
            Left _ -> return False
            Right (_, parcelas) -> return $ any ((== id) . idParcela) (V.toList parcelas)

-- Obtener vegetales permitidos de una parcela
obtenerVegetalesPermitidos :: String -> IO [String]
obtenerVegetalesPermitidos parcelaId = do
    exists <- doesFileExist "data/parcelas.csv"
    if not exists then return [] else do
        contenido <- BL.readFile "data/parcelas.csv"
        case decodeByName contenido of
            Left _ -> return []
            Right (_, parcelas) -> 
                case find ((== parcelaId) . idParcela) (V.toList parcelas) of
                    Nothing -> return []
                    Just parcela -> return $ extraerNombresVegetales (vegetalesParcela parcela)
  where
    extraerNombresVegetales :: String -> [String]
    extraerNombresVegetales str = map (takeWhile (/= ':')) $ splitOn ',' str

-- verificar vegetales permitidos dentro de la parcela
verificarVegetal :: String -> String -> IO Bool
verificarVegetal parcelaId vegetal = do
    vegetales <- obtenerVegetalesPermitidos parcelaId
    return $ map Char.toLower vegetal `elem` map (map Char.toLower) vegetales

-- Verificar disponibilidad de parcela en fechas
verificarDisponibilidad :: String -> String -> String -> IO Bool
verificarDisponibilidad parcelaId inicio fin = do
    exists <- doesFileExist "data/cosechas.csv"
    if not exists then return True else do
        contenido <- BL.readFile "data/cosechas.csv"
        case decodeByName contenido of
            Left _ -> return True
            Right (_, cosechas) -> return $ not $ any (solapamiento inicio fin) (V.toList cosechas)
  where
    solapamiento i f c = 
        parcela c == parcelaId && 
        (fecha_inicio c `entre` (i, f)) || 
        fecha_fin c `entre` (i, f)
    
    entre fecha (inicio, fin) = fecha >= inicio && fecha <= fin

-- Guardar cosecha en CSV
guardarCosecha :: Cosecha -> IO ()
guardarCosecha cosecha = do 
    exists <- doesFileExist "data/cosechas.csv"
    if exists
        then BL.appendFile "data/cosechas.csv" $ encodeDefaultOrderedByNameWith 
                (defaultEncodeOptions { encIncludeHeader = False })  
                [cosecha]
        else BL.writeFile "data/cosechas.csv" $
                encodeDefaultOrderedByName [cosecha]  

-- Función principal de registro
registrarCosecha :: IO ()
registrarCosecha = do
    putStrLn "\n=== Registro de Nueva Cosecha ==="
    
    putStr "ID del trabajador: " >> hFlush stdout
    tid <- getLine
    existeTrab <- verificarTrabajador tid
    unless existeTrab $ do
        putStrLn "Error: Trabajador no registrado"
        return ()
    
    when existeTrab $ do
        putStr "ID de parcela: " >> hFlush stdout
        pid <- getLine
        existeParc <- verificarParcela pid
        unless existeParc $ do
            putStrLn "Error: Parcela no registrada"
            return ()
        
        when existeParc $ do
            vegetales <- obtenerVegetalesPermitidos pid
            putStrLn $ "\nVegetales permitidos: " ++ intercalate ", " vegetales
            when (null vegetales) $ do
                putStrLn "Error: Parcela no tiene vegetales configurados"
                return ()
            
            putStrLn $ "\nVegetales permitidos en esta parcela: " ++ show vegetales
            
            putStr "Fecha inicio (dd/mm/aaaa): " >> hFlush stdout
            fini <- getLine
            putStr "Fecha fin (dd/mm/aaaa): " >> hFlush stdout
            ffin <- getLine
            
            disponible <- verificarDisponibilidad pid fini ffin
            unless disponible $ do
                putStrLn "Error: Parcela no disponible en esas fechas"
                return ()
            
            when disponible $ do
                putStr "Tipo de vegetal: " >> hFlush stdout
                veg <- getLine
                let vegLower = map Char.toLower veg
                esValido <- verificarVegetal pid veg
                unless esValido $ do
                    putStrLn "Error: Vegetal no permitido en esta parcela"
                    return ()
                
                putStr "Cantidad a recolectar (kg): " >> hFlush stdout
                cantStr <- getLine
                case reads cantStr :: [(Int, String)] of
                    [(cant, "")] -> do
                        cid <- generarIdCosecha
                        let nuevaCosecha = Cosecha cid tid pid fini ffin vegLower cant "abierto"
                        guardarCosecha nuevaCosecha
                        putStrLn $ "\nCosecha registrada exitosamente!\nID: " ++ cid
                    _ -> putStrLn "Error: Cantidad debe ser un número positivo"

-- Menú de gestión de cosechas
gestionCosechasMenu :: IO ()
gestionCosechasMenu = do
    putStrLn "\n=================================="
    putStrLn "=      Gestión de Cosechas       ="
    putStrLn "=================================="
    putStrLn "1. Registrar nueva cosecha"
    putStrLn "2. Volver al menú anterior"
    putStr "Ingrese una opción: " >> hFlush stdout
    
    opcion <- getLine
    case opcion of
        "1" -> registrarCosecha >> gestionCosechasMenu
        "2" -> return ()
        _   -> do
            putStrLn "\nOpción inválida\n"
            gestionCosechasMenu