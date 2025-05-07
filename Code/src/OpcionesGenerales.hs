{-# LANGUAGE RecordWildCards #-} -- Permite usar la sintaxis (..) para desestructurar registros automáticamente
{-# LANGUAGE OverloadedStrings #-} -- Permite usar literales de texto como tipos distintos de String (como Text, ByteString, etc.)
{-# LANGUAGE DeriveGeneric #-} -- Permite derivar automáticamente instancias de la clase Generic, necesaria para serialización/deserialización

module OpcionesGenerales
    ( gestionCosechasMenu
    , cierreCosecha
    , consultaCosecha
    , cancelarCosecha
    , modificarCosecha
    , checkParcelAvailability
    ) where

import Prelude hiding (id)
import GHC.Generics (Generic)
import Trabajadores (Trabajador(..), cedula)
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime)
import Data.List (find, isInfixOf, intercalate, groupBy)
import Data.Char (toLower, isDigit)
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
                , encode
                , decode
                , HasHeader(..)
                )
import qualified Data.Vector as V
import System.Directory (doesFileExist)
import Control.Monad (when, unless)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Text.Printf (printf)

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

{-|
=======================================
         MÓDULO DE CREACIÓN
=======================================
-}

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
verificarDisponibilidad :: String -> String -> String -> Maybe String -> IO Bool
verificarDisponibilidad parcelaId inicio fin excluirId = do
    exists <- doesFileExist "data/cosechas.csv"
    if not exists 
        then return True 
        else do
            contenido <- BL.readFile "data/cosechas.csv"
            case decodeByName contenido of
                Left _ -> return True
                Right (_, cosechas) -> return $ not $ any (solapamiento inicio fin excluirId) (V.toList cosechas)
  where
    solapamiento i f e c = 
        parcela c == parcelaId && 
        maybe True (/= id c) e &&  
        (fecha_inicio c `entre` (i, f) || 
         fecha_fin c `entre` (i, f))
    
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
            when (null vegetales) $ do
                putStrLn "Error: Parcela no tiene vegetales configurados"
                return ()
            
            putStrLn $ "\nVegetales permitidos en esta parcela: " ++ show vegetales
            
            putStr "Fecha inicio (dd/mm/aaaa): " >> hFlush stdout
            fini <- getLine
            putStr "Fecha fin (dd/mm/aaaa): " >> hFlush stdout
            ffin <- getLine
            
            disponible <- verificarDisponibilidad pid fini ffin Nothing
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


{-|
=======================================
          MÓDULO DE CIERRE
=======================================
-}

leerCosechas :: IO (Either String (V.Vector Cosecha))
leerCosechas = do
    contenido <- BL.readFile "data/cosechas.csv"
    case decodeByName contenido of
        Left err -> return $ Left err
        Right (_, cosechas) -> return $ Right cosechas

actualizarCosecha :: V.Vector Cosecha -> String -> Int -> Either String (V.Vector Cosecha)
actualizarCosecha cosechas idCosecha cantidadRecolectada =
    case V.find ((== idCosecha) . id) cosechas of
        Nothing -> Left "Cosecha no encontrada"
        Just cosecha ->
            if estado cosecha /= "abierto"
                then Left "La cosecha ya se encuentra cerrada"
                else Right $ V.map (\c -> if id c == idCosecha 
                                         then c { estado = "cerrado", cantidad = cantidadRecolectada }
                                         else c) cosechas

guardarCosechaCerrada :: Cosecha -> IO ()
guardarCosechaCerrada cosecha = do
    exists <- doesFileExist "data/cosechasCerradas.csv"
    let options = defaultEncodeOptions { encIncludeHeader = not exists }
    BL.appendFile "data/cosechasCerradas.csv" $ encodeDefaultOrderedByNameWith options [cosecha]

-- Función principal de cierre
cierreCosecha :: IO ()
cierreCosecha = do
    putStrLn "\n=== Cierre de Cosecha ==="
    
    putStr "ID de la cosecha a cerrar: " >> hFlush stdout
    idCosecha <- getLine
    
    cosechasEither <- leerCosechas
    case cosechasEither of
        Left err -> putStrLn $ "Error leyendo cosechas: " ++ err
        Right cosechas -> do  
            putStr "Cantidad recolectada (kg): " >> hFlush stdout
            cantStr <- getLine
            
            case reads cantStr :: [(Int, String)] of
                [(cantidad, "")] | cantidad >= 0 -> do
                    case actualizarCosecha cosechas idCosecha cantidad of
                        Left err -> putStrLn err
                        Right nuevasCosechas -> do
                            case V.find ((== idCosecha) . id) nuevasCosechas of
                                Nothing -> putStrLn "Error interno: cosecha no encontrada"
                                Just cosechaCerrada -> do
                                    guardarCosechaCerrada cosechaCerrada
                                    BL.writeFile "data/cosechas.csv" $ encodeDefaultOrderedByName (V.toList nuevasCosechas)
                                    putStrLn "Cosecha cerrada exitosamente!"
                _ -> putStrLn "Error: La cantidad debe ser un número positivo"

{-|
=======================================
          MÓDULO DE Consulta
=======================================
-}

-- Función principal de consulta
consultaCosecha :: IO ()
consultaCosecha = do
    putStrLn "\n=== Consulta de Cosecha ==="
    
    -- Pedir ID de cosecha
    putStr "Ingrese el ID de la cosecha a consultar: " >> hFlush stdout
    idCosecha <- getLine
    
    -- Leer archivo de cosechas
    cosechasEither <- leerCosechas
    case cosechasEither of
        Left err -> putStrLn $ "Error al leer cosechas: " ++ err
        Right cosechas ->
            case V.find ((== idCosecha) . id) cosechas of
                Nothing -> putStrLn "No se encontró una cosecha con ese ID"
                Just cosecha -> mostrarInfoCosecha cosecha

-- Función auxiliar para mostrar la información
mostrarInfoCosecha :: Cosecha -> IO ()
mostrarInfoCosecha Cosecha{..} = do
    putStrLn "\n=== Información de la Cosecha ==="
    putStrLn $ "ID: " ++ id
    putStrLn $ "Trabajador: " ++ trabajador
    putStrLn $ "Parcela: " ++ parcela
    putStrLn $ "Fecha inicio: " ++ fecha_inicio
    putStrLn $ "Fecha fin: " ++ fecha_fin
    putStrLn $ "Vegetal: " ++ vegetal
    putStrLn $ "Cantidad (kg): " ++ show cantidad
    putStrLn $ "Estado: " ++ estado
    putStrLn ""

{-|
=======================================
        MÓDULO DE CANCELACIÓN
=======================================
-}

-- Función principal de Cancelación
cancelarCosecha :: IO ()
cancelarCosecha = do
    putStrLn "\n=== Cancelación de Cosecha ==="
    
    -- Pedir ID de cosecha
    putStr "Ingrese el ID de la cosecha a cancelar: " >> hFlush stdout
    idCosecha <- getLine
    
    -- Leer archivo de cosechas
    cosechasEither <- leerCosechas
    case cosechasEither of
        Left err -> putStrLn $ "Error al leer cosechas: " ++ err
        Right cosechas -> do
            -- Buscar la cosecha
            case V.find ((== idCosecha) . id) cosechas of
                Nothing -> putStrLn "No se encontró una cosecha con ese ID"
                Just cosecha ->
                    if estado cosecha == "cerrado"
                        then putStrLn "Error: No se puede cancelar una cosecha ya cerrada"
                        else confirmarYEliminar idCosecha cosechas

-- Función auxiliar para confirmar y eliminar
confirmarYEliminar :: String -> V.Vector Cosecha -> IO ()
confirmarYEliminar idCosecha cosechas = do
    putStrLn "\n¿Está seguro que desea cancelar esta cosecha? (s/n)"
    confirmacion <- getLine
    if map Char.toLower confirmacion == "s"
        then do
            let nuevasCosechas = V.filter ((/= idCosecha) . id) cosechas
            BL.writeFile "data/cosechas.csv" $ encodeDefaultOrderedByName (V.toList nuevasCosechas)
            putStrLn "Cosecha cancelada exitosamente!"
        else putStrLn "Cancelación abortada"

{-|
=======================================
        MÓDULO DE MODIFICACIÓN
=======================================
-}

-- Función principal de modificación
modificarCosecha :: IO ()
modificarCosecha = do
    putStrLn "\n=== Modificación de Cosecha ==="
    
    -- Pedir ID de cosecha
    putStr "Ingrese el ID de la cosecha a modificar: " >> hFlush stdout
    idCosecha <- getLine
    
    -- Leer archivo de cosechas
    cosechasEither <- leerCosechas
    case cosechasEither of
        Left err -> putStrLn $ "Error al leer cosechas: " ++ err
        Right cosechas ->
            case V.find ((== idCosecha) . id) cosechas of
                Nothing -> putStrLn "No se encontró una cosecha con ese ID"
                Just cosecha -> do
                    if estado cosecha == "cerrado"
                        then putStrLn "Error: No se puede modificar una cosecha ya cerrada"
                        else menuModificacion cosecha cosechas

-- Menú de modificación
menuModificacion :: Cosecha -> V.Vector Cosecha -> IO ()
menuModificacion cosecha cosechas = do
    putStrLn "\nSeleccione qué desea modificar:"
    putStrLn "1. Parcela"
    putStrLn "2. Fechas"
    putStrLn "3. Tipo de vegetal"
    putStrLn "4. Confirmar cambios"
    putStrLn "5. Cancelar modificación"
    putStr "Opción: " >> hFlush stdout
    
    opcion <- getLine
    case opcion of
        "1" -> modificarParcela cosecha cosechas
        "2" -> modificarFechas cosecha cosechas
        "3" -> modificarVegetal cosecha cosechas
        "4" -> confirmarCambios cosecha cosechas
        "5" -> putStrLn "Modificación cancelada"
        _   -> do
            putStrLn "Opción inválida"
            menuModificacion cosecha cosechas

-- Funciones específicas de modificación
modificarParcela :: Cosecha -> V.Vector Cosecha -> IO ()
modificarParcela cosecha cosechas = do
    putStr "Nueva parcela (actual: " >> putStr (parcela cosecha) >> putStr "): " >> hFlush stdout
    nuevaParcela <- getLine
    
    existe <- verificarParcela nuevaParcela
    if not existe
        then do
            putStrLn "Error: Parcela no existe"
            menuModificacion cosecha cosechas
        else do
            let cosechaMod = cosecha { parcela = nuevaParcela }
            menuModificacion cosechaMod cosechas

modificarFechas :: Cosecha -> V.Vector Cosecha -> IO ()
modificarFechas cosecha cosechas = do
    putStr "Nueva fecha inicio (actual: " >> putStr (fecha_inicio cosecha) >> putStr "): " >> hFlush stdout
    nuevaInicio <- getLine
    putStr "Nueva fecha fin (actual: " >> putStr (fecha_fin cosecha) >> putStr "): " >> hFlush stdout
    nuevaFin <- getLine
    
    disponible <- verificarDisponibilidad (parcela cosecha) nuevaInicio nuevaFin (Just (id cosecha))
    if not disponible
        then do
            putStrLn "Error: Parcela no disponible en esas fechas"
            menuModificacion cosecha cosechas
        else do
            let cosechaMod = cosecha { fecha_inicio = nuevaInicio, fecha_fin = nuevaFin }
            menuModificacion cosechaMod cosechas

modificarVegetal :: Cosecha -> V.Vector Cosecha -> IO ()
modificarVegetal cosecha cosechas = do
    putStr "Nuevo vegetal (actual: " >> putStr (vegetal cosecha) >> putStr "): " >> hFlush stdout
    nuevoVegetal <- getLine
    
    valido <- verificarVegetal (parcela cosecha) nuevoVegetal
    if not valido
        then do
            putStrLn "Error: Vegetal no permitido en esta parcela"
            menuModificacion cosecha cosechas
        else do
            let cosechaMod = cosecha { vegetal = nuevoVegetal }
            menuModificacion cosechaMod cosechas

-- Confirmar y guardar cambios
confirmarCambios :: Cosecha -> V.Vector Cosecha -> IO ()
confirmarCambios cosecha cosechas = do
    putStrLn "\n¿Confirmar cambios? (s/n)"
    confirmacion <- getLine
    
    if map Char.toLower confirmacion == "s"
        then do
            let nuevasCosechas = V.map (\c -> if id c == id cosecha then cosecha else c) cosechas
            BL.writeFile "data/cosechas.csv" $ encodeDefaultOrderedByName (V.toList nuevasCosechas)
            putStrLn "Cosecha modificada exitosamente!"
        else do
            putStrLn "Cambios descartados"
            menuModificacion cosecha cosechas

{-|
=============================================
    Consula de disponibilidad de Parcelas
============================================= -}
checkParcelAvailability :: IO ()
checkParcelAvailability = do
    putStrLn "\n=== Consulta de Disponibilidad de Parcelas ==="
    putStrLn "1. Listar parcelas disponibles en rango de fechas"
    putStrLn "2. Mostrar estado de parcelas en rango de fechas"
    putStrLn "3. Volver"
    putStr "Seleccione una opción: " >> hFlush stdout
    
    opt <- getLine
    case opt of
        "1" -> do
            mRange <- getDateRange
            case mRange of
                Nothing -> return ()
                Just (start, end) -> showAvailableParcels start end
            checkParcelAvailability
        "2" -> do
            mRange <- getDateRange
            case mRange of
                Nothing -> return ()
                Just (start, end) -> showDiaryStateParcels start end
            checkParcelAvailability
        "3" -> return ()
        _   -> do
            putStrLn "Opción inválida"
            checkParcelAvailability

-- Función auxiliar para ingreso de fechas con validación y opcion
dateInput :: String -> IO (Maybe String)
dateInput prompt = do
    putStr (prompt ++ " (o 'v' para volver): ") >> hFlush stdout
    input <- getLine
    case map toLower input of
        "v" -> return Nothing
        _ -> if correctDateFormat input
                then return (Just input)
                else do
                    putStrLn "Formato de fecha inválido. Use dd/mm/aaaa"
                    dateInput prompt

-- Validación de formato de fecha (dd/mm/aaaa)
correctDateFormat :: String -> Bool
correctDateFormat date =
    case splitOn '/' date of
        [d,m,a] -> length d == 2 && length m == 2 && length a == 4
                    && all isDigit (d++m++a)
        _       -> False
        
-- Función para obtener rango de fechas
getDateRange :: IO (Maybe (String, String))
getDateRange = do
    putStrLn "\nIngrese el rango de fechas (dd/mm/aaaa): "
    mStart <- dateInput "Fecha inicio"
    case mStart of
        Nothing -> return Nothing
        Just start -> do
            mEnd <- dateInput "Fecha fin"
            case mEnd of
                Nothing -> return Nothing
                Just end -> do
                    let startDays = dateToDays start
                        endDays = dateToDays end
                    if startDays <= endDays
                        then return (Just (start, end))
                        else do
                            putStrLn "Error: La fecha de inicio debe ser antes de la fecha fin"
                            getDateRange

-- Opción 1: Mostrar parcelas disponibles en alún día del juego
showAvailableParcels :: String -> String -> IO ()
showAvailableParcels start end = do
    parcels <- getAllParcels
    harvests <- getOpenedHarvests

    let usedParcels = map parcela $ filter (overlapsRange start end) harvests
    let availableParcels = filter (\p -> idParcela p `notElem` usedParcels) parcels

    putStrLn $ "\nParcelas disponibles entre " ++ start ++ " y " ++ end ++ ":"

    if null availableParcels
        then putStrLn "No hay parcelas disponibles en el rango especificado"
        else mapM_ (\p -> putStrLn $ idParcela p ++ " - " ++ nombreParcela p) availableParcels
  where
    dateToComparable :: String -> String
    dateToComparable date = 
        case splitOn '/' date of
            [d, m, y] -> y ++ m ++ d
            _ -> error "Formato de fecha inválido"
    overlapsRange rangeStart rangeEnd c =
        let startComp = dateToComparable rangeStart
            endComp = dateToComparable rangeEnd
            cStart = dateToComparable (fecha_inicio c)
            cEnd = dateToComparable (fecha_fin c)
        in cStart <= endComp && cEnd >= startComp

-- Opción 2: Mostrar estado diario de parcelas
showDiaryStateParcels :: String -> String -> IO ()
showDiaryStateParcels start end = do
    parcels <- getAllParcels
    harvests <- getOpenedHarvests

    let days = generateDays start end
    
    putStrLn $ "\nEstado diario de parcelas entre " ++ start ++ " y " ++ end ++ ":"
    
    mapM_ (\p -> do
        putStrLn $ "\n" ++ idParcela p ++ " - " ++ nombreParcela p
        let parcelHarvests = filter (\c -> parcela c == idParcela p) harvests
        mapM_ (\day -> do
            let used = any (\c -> isInDate day c) parcelHarvests
            putStrLn $ "    " ++ day ++ ": " ++ if used then "No disponible" else "Disponible"
            ) days
        ) parcels

-- Funciones auxiliares

getAllParcels :: IO [Parcela]
getAllParcels = do
    exists <- doesFileExist "data/parcelas.csv"
    if not exists then return [] else do
        contenido <- BL.readFile "data/parcelas.csv"
        case decodeByName contenido of
            Left _ -> return []
            Right (_, parcels) -> return $ V.toList parcels

getAllHarvests :: IO [Cosecha]
getAllHarvests = do
    exists <- doesFileExist "data/cosechas.csv"
    if not exists then return [] else do
        contenido <- BL.readFile "data/cosechas.csv"
        case decodeByName contenido of
            Left _ -> return []
            Right (_, harvests) -> return $ V.toList harvests

getOpenedHarvests :: IO [Cosecha]
getOpenedHarvests = do
    exists <- doesFileExist "data/cosechas.csv"
    if not exists then return [] else do
        contenido <- BL.readFile "data/cosechas.csv"
        case decodeByName contenido of
            Left _ -> return []
            Right (_, harvests) -> return $ filter ((== "abierto") . estado) $ V.toList harvests

isInRange :: String -> String -> Cosecha -> Bool
isInRange start end c =
    (fecha_inicio c >= start && fecha_inicio c <= end) ||
    (fecha_fin c >= start && fecha_fin c <= end)

dateToDays :: String -> Int
dateToDays date = 
    case splitOn '/' date of
        [d, m, y] -> 
            let day = read d
                month = read m
                year = read y
            in year * 365 + month * 30 + day
        _ -> error "Formato de fecha inválido"

isInDate :: String -> Cosecha -> Bool
isInDate date c = 
        let day = dateToDays date
            start = dateToDays (fecha_inicio c)
            end = dateToDays (fecha_fin c)
        in day >= start && day <= end

generateDays :: String -> String -> [String]
generateDays start end =
    if start == end
        then [start]
        else start : generateDays (nextDay start) end

nextDay :: String -> String
nextDay date =
    case splitOn '/' date of
        [d, m, y] ->
            let day = read d :: Int
                month = read m :: Int
                year = read y :: Int
                (newDay, newMonth, newYear) = addDay day month year
            in printf "%02d/%02d/%04d" newDay newMonth newYear
        _ -> error "Formato de fecha inválido"
  where
    addDay :: Int -> Int -> Int -> (Int, Int, Int)
    addDay d m y
        | d < daysInMonth m y = (d+1, m, y)
        | m < 12 = (1, m+1, y)
        | otherwise = (1, 1, y+1)
    
    daysInMonth :: Int -> Int -> Int
    daysInMonth m y
        | m `elem` [4,6,9,11] = 30
        | m == 2 = if isLeapYear y then 29 else 28
        | otherwise = 31
    
    isLeapYear :: Int -> Bool
    isLeapYear y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || y `mod` 400 == 0