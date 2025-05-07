{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module InformeCosechas where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.List (sortOn, groupBy, sort, maximumBy)
import Data.Ord (Down(..))
import Data.Time (parseTimeM, defaultTimeLocale)
import Data.Maybe (fromMaybe, catMaybes)
import Text.Printf (printf)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)

-- Definición del tipo Cosecha
data Cosecha = Cosecha {
    cosechaId :: Text,
    trabajador :: Text,
    parcela :: Text,
    fechaInicio :: Text,
    fechaFin :: Text,
    vegetal :: Text,
    cantidad :: Int,
    estado :: Text
} deriving (Show)

-- Instancias para CSV
instance FromNamedRecord Cosecha where
    parseNamedRecord r = Cosecha
        <$> r .: "id"
        <*> r .: "trabajador"
        <*> r .: "parcela"
        <*> r .: "fecha_inicio"
        <*> r .: "fecha_fin"
        <*> r .: "vegetal"
        <*> r .: "cantidad"
        <*> r .: "estado"

-- Función principal para generar el informe
generarInformeCosechas :: IO ()
generarInformeCosechas = do
    putStrLn "\n=== Informe Completo de Cosechas ==="
    
    -- Leer todas las cosechas del archivo CSV
    todasCosechasEither <- leerTodasLasCosechas
    case todasCosechasEither of
        Left err -> putStrLn $ "Error leyendo cosechas: " ++ err
        Right [] -> putStrLn "\nNo hay cosechas registradas."
        Right todasCosechas -> do
            -- Mostrar información básica de todas las cosechas
            mostrarInformacionBasica todasCosechas
            
            -- Filtrar solo cosechas cerradas para estadísticas
            let cosechasCerradas = filter (esCosechaCerrada . estado) todasCosechas
            
            if null cosechasCerradas
                then putStrLn "\nNo hay cosechas cerradas para generar estadísticas."
                else generarEstadisticas cosechasCerradas

-- Función para leer todas las cosechas desde el archivo CSV
leerTodasLasCosechas :: IO (Either String [Cosecha])
leerTodasLasCosechas = do
    exists <- doesFileExist "data/cosechas.csv"
    if not exists 
        then return $ Left "No existe el archivo de cosechas"
        else do
            contenido <- BL.readFile "data/cosechas.csv"
            case decodeByName contenido of
                Left err -> return $ Left err
                Right (_, cosechas) -> return $ Right (V.toList cosechas)

-- Función auxiliar para verificar si una cosecha está cerrada
esCosechaCerrada :: Text -> Bool
esCosechaCerrada estado = T.toLower estado == "cerrado"

-- Mostrar información básica de todas las cosechas
mostrarInformacionBasica :: [Cosecha] -> IO ()
mostrarInformacionBasica cosechas = do
    putStrLn "\n=== Todas las Cosechas Registradas ==="
    putStrLn "Estado   ID      Parcela    Vegetal        Fecha Inicio  Fecha Fin      Cantidad (kg) Trabajador     "
    putStrLn "----------------------------------------------------------------------------------------------------"
    mapM_ mostrarCosecha cosechas
    putStrLn ""

mostrarCosecha :: Cosecha -> IO ()
mostrarCosecha Cosecha{..} = 
    printf "%-8s %-7s %-10s %-14s %-14s %-14s %-13d %s\n" 
        (T.unpack estado)
        (T.unpack cosechaId) 
        (T.unpack parcela)
        (T.unpack vegetal)
        (T.unpack fechaInicio)
        (T.unpack fechaFin)
        cantidad
        (T.unpack trabajador)

-- Generar todas las estadísticas
generarEstadisticas :: [Cosecha] -> IO ()
generarEstadisticas cosechasCerradas = do
    putStrLn "\n=== Estadísticas ==="
    
    -- 1. Parcela con mayor volumen de cosecha
    let parcelaMayorVolumen = obtenerParcelaMayorVolumen cosechasCerradas
    putStrLn $ "\n1. Parcela con mayor volumen de cosecha: " ++ parcelaMayorVolumen
    
    -- 2. Top 3 de parcelas con mayor venta
    let topParcelas = obtenerTopParcelas cosechasCerradas 3
    putStrLn "\n2. Top 3 parcelas con mayor volumen de cosecha:"
    mapM_ (\(p, v) -> putStrLn $ " - " ++ T.unpack p ++ ": " ++ show v ++ " kg") topParcelas
    
    -- 3. Trabajador con más cosechas realizadas
    let trabajadorMasCosechas = obtenerTrabajadorMasCosechas cosechasCerradas
    putStrLn $ "\n3. Trabajador con más cosechas realizadas: " ++ T.unpack trabajadorMasCosechas
    
    -- 4. Mes-Año con mayor recolección acumulada
    let mejorMesAnio = obtenerMejorMesAnio cosechasCerradas
    putStrLn $ "\n4. Mes-Año con mayor recolección: " ++ mejorMesAnio
    
    -- 5. Cosechas con subproducción y sobreproducción
    let (subproduccion, sobreproduccion) = obtenerProduccionExtrema cosechasCerradas
    putStrLn "\n5. Cosechas con producción extrema:"
    putStrLn " - Subproducción (menos de 1000 kg):"
    if null subproduccion 
        then putStrLn "   No hay cosechas con subproducción"
        else mapM_ mostrarCosecha subproduccion
    putStrLn "\n - Sobreproducción (más de 5000 kg):"
    if null sobreproduccion 
        then putStrLn "   No hay cosechas con sobreproducción"
        else mapM_ mostrarCosecha sobreproduccion

-- Funciones para calcular estadísticas

-- 1. Parcela con mayor volumen de cosecha
obtenerParcelaMayorVolumen :: [Cosecha] -> String
obtenerParcelaMayorVolumen cosechas =
    if null cosechas
        then "No hay datos"
        else let parcelasAgrupadas = Map.fromListWith (+) [(parcela c, cantidad c) | c <- cosechas]
                 (maxParcela, maxVolumen) = maximumBy (\a b -> compare (snd a) (snd b)) 
                                              (Map.toList parcelasAgrupadas)
             in T.unpack maxParcela ++ " (" ++ show maxVolumen ++ " kg)"

-- 2. Top N parcelas con mayor volumen de cosecha
obtenerTopParcelas :: [Cosecha] -> Int -> [(Text, Int)]
obtenerTopParcelas cosechas n =
    let parcelasAgrupadas = Map.fromListWith (+) [(parcela c, cantidad c) | c <- cosechas]
        sorted = sortOn (Down . snd) (Map.toList parcelasAgrupadas)
    in take n sorted

-- 3. Trabajador con más cosechas realizadas
obtenerTrabajadorMasCosechas :: [Cosecha] -> Text
obtenerTrabajadorMasCosechas cosechas =
    if null cosechas
        then "No hay datos"
        else let trabajadoresAgrupados = Map.fromListWith (+) [(trabajador c, 1) | c <- cosechas]
                 (maxTrabajador, _) = maximumBy (\a b -> compare (snd a) (snd b)) 
                                        (Map.toList trabajadoresAgrupados)
             in maxTrabajador

-- 4. Mes-Año con mayor recolección acumulada
obtenerMejorMesAnio :: [Cosecha] -> String
obtenerMejorMesAnio cosechas =
    if null cosechas
        then "No hay datos"
        else let
                 extraerMesAnio fecha = 
                     case T.splitOn "/" fecha of
                         [_d, m, a] -> T.unpack m ++ "/" ++ T.unpack a
                         _ -> "00/0000"
                 
                 mesesAgrupados = Map.fromListWith (+) 
                     [(extraerMesAnio (fechaFin c), cantidad c) | c <- cosechas]
                 
                 (mejorMes, mejorVolumen) = maximumBy (\a b -> compare (snd a) (snd b)) 
                     (Map.toList mesesAgrupados)
             in mejorMes ++ " (" ++ show mejorVolumen ++ " kg)"

-- 5. Cosechas con producción extrema
obtenerProduccionExtrema :: [Cosecha] -> ([Cosecha], [Cosecha])
obtenerProduccionExtrema cosechas =
    let subproduccion = filter (\c -> cantidad c < 1000) cosechas
        sobreproduccion = filter (\c -> cantidad c > 5000) cosechas
    in (subproduccion, sobreproduccion)