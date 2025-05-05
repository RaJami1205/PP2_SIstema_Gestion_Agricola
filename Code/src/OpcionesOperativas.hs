{-# LANGUAGE OverloadedStrings #-} -- Permite usar literales de texto como tipos distintos de String (como Text, ByteString, etc.)
{-# LANGUAGE TypeApplications #-} -- Permite especificar tipos explícitamente usando @Tipo cuando la inferencia no es suficiente

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
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Vector (fromList, toList)
import System.Directory (doesFileExist)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Monad (replicateM)
import System.Random (randomIO)
import Numeric (showHex)
import Data.Word (Word8)
import Control.Monad (replicateM)
import Data.Char (toUpper)
import Data.Map (Map, fromList, toList)
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.List (find)
import Control.Monad (when)
import Text.Printf (printf)
import System.IO (hFlush, stdout)


-- Tipo de dato para herramientas
data Herramienta = Herramienta {
    codigo :: Text,
    nombre :: Text,
    descripcion :: Text,
    tipo :: Text
} deriving (Show)

-- Instancias necesarias para CSV
instance DefaultOrdered Herramienta where
    headerOrder _ = V.fromList ["codigo", "nombre", "descripcion", "tipo"]

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
    putStr "Ingrese la ruta del archivo TXT: " >> hFlush stdout
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
            putStrLn ""
            mostrarHerramientasDisponibles

-- Función para mostrar herramientas
mostrarHerramientasDisponibles :: IO ()
mostrarHerramientasDisponibles = do
    csvData <- BL.readFile "data/herramientas.csv"
    case decodeByName csvData of
        Left err -> putStrLn $ "Error leyendo herramientas: " ++ err
        Right (_, herramientas) -> do
            showToolsHeader  -- Muestra el encabezado de la tabla
            mapM_ (putStrLn . formatHerramienta) (V.toList herramientas)
            putStrLn ""

formatTableRow :: Text -> Text -> Text -> Text -> String
formatTableRow a b c d =
    printf "%-10s %-40s %-20s %-50s" 
        (T.unpack a) 
        (T.unpack b) 
        (T.unpack d) 
        (T.unpack c)

-- Función para formatear una herramienta como fila de tabla
formatHerramienta :: Herramienta -> String
formatHerramienta h = 
    formatTableRow (codigo h) (nombre h) (descripcion h) (tipo h)

-- Función para mostrar el encabezado de la tabla
showToolsHeader :: IO ()
showToolsHeader = do
    putStrLn $ formatTableRow "Código" "Nombre" "Descripción" "Tipo"
    putStrLn $ replicate 140 '-'

-- Función para parsear cada línea del TXT
parsearLinea :: Text -> Herramienta
parsearLinea linea =
    case T.splitOn "," linea of
        [c, n, d, t] -> Herramienta c n d t
        _ -> error $ "Formato inválido en línea: " ++ T.unpack linea


-- \\\\\\\\\\\\\\\\\\\\\     PARCELAS     //////////////////////////

-- =============================================
-- Definición del tipo Parcela
-- =============================================
data Parcel = Parcel {
    parcelId :: Text,               -- Identificador único
    name :: Text,                   -- Nombre descriptivo
    zone :: Text,                   -- Zona
    area :: Double,                 -- Área en m²
    vegetables :: Map Text Double,  -- Tipo de vegetal y precio por kilo
    asignedTools :: [Text]          -- código de Herramientas
} deriving (Show)

-- Instancias para CSV
instance DefaultOrdered Parcel where
    headerOrder _ = V.fromList ["id", "nombre", "zona", "area", "vegetales", "herramientas"]

instance ToNamedRecord Parcel where
    toNamedRecord p = namedRecord [
        "id" .= parcelId p,
        "nombre" .= name p,
        "zona" .= zone p,
        "area" .= area p,
        "vegetales" .= formatVegetales (vegetables p),
        "herramientas" .= T.intercalate "," (asignedTools p)
        ]

instance FromNamedRecord Parcel where
    parseNamedRecord r = do
        vegetablesStr <- r .: "vegetales"
        let vegetablesMap = parseVegetables vegetablesStr
        toolsStr <- r .: "herramientas"
        let tools = if T.null toolsStr then [] else T.splitOn "," toolsStr
        Parcel 
            <$> r .: "id"
            <*> r .: "nombre"
            <*> r .: "zona"
            <*> r .: "area"
            <*> pure vegetablesMap
            <*> pure tools

-- Función auxiliar para formatear vegetales (Map -> Text)
formatVegetales :: Map Text Double -> Text
formatVegetales = T.intercalate "," . map (\(v, p) -> v <> ":" <> T.pack (show p)) . Map.toList

-- Función auxiliar para parsear vegetales (Text -> Map Text Double)
parseVegetables :: Text -> Map Text Double
parseVegetables txt = Map.fromList $ map parsePair (T.splitOn "," txt)
    where parsePair entry = case T.splitOn ":" entry of
                                [v, p] -> (v, read (T.unpack p))
                                _ -> error "Formato inválido en vegetales"

-- Función auxiliar para leer vegetales en bucle
inputVegetales :: IO (Map Text Double)
inputVegetales = do
    putStrLn "\nIngrese los vegetales y sus precios por kilo (formato: vegetal:precio):"
    putStrLn "Ejemplo: tomate:2340.5"
    putStrLn "Presione Enter sin ingresar nada para terminar."
    loop Map.empty
  where
    loop :: Map Text Double -> IO (Map Text Double)
    loop currentMap = do
        putStr "> " >> hFlush stdout
        linea <- TIO.getLine
        if T.null linea
            then return currentMap  -- Terminar si la línea está vacía
            else case T.splitOn ":" linea of
                    [vegetal, precioText] -> 
                        case reads (T.unpack precioText) of
                            [(precio, "")] -> loop (Map.insert vegetal precio currentMap)
                            _ -> do
                                putStrLn "Error: Precio inválido. Use formato: vegetal:precio (ej. tomate:1.5)"
                                loop currentMap
                    _ -> do
                        putStrLn "Error: Formato inválido. Use vegetal:precio (ej. tomate:1.5)"
                        loop currentMap

-- Función auxiliar para seleccionar herramientas en bucle
selectTools :: [Herramienta] -> IO [Text]
selectTools availableTools = do
    putStrLn "\nSeleccione herramientas por código (una por línea):"
    putStrLn "Herramientas disponibles:"
    mapM_ (putStrLn . formatHerramienta) availableTools
    putStrLn "\nIngrese los códigos de las herramientas (presione Enter para terminar):"
    loop []
  where
    loop :: [Text] -> IO [Text]
    loop selected = do
        putStr "> " >> hFlush stdout
        code <- TIO.getLine
        if T.null code
            then return selected  -- Terminar si la línea está vacía
            else if any (\h -> codigo h == code) availableTools
                then do
                    let newSelected = if code `elem` selected 
                                      then selected 
                                      else code : selected
                    putStrLn $ "Herramienta añadida: " <> T.unpack code
                    loop newSelected
                else do
                    putStrLn "Error: Código no válido. Intente nuevamente."
                    loop selected

-- Función para crear un ID de 6 caracteres
generateShortId :: IO Text
generateShortId = do
    randomBytes <- replicateM 3 (randomIO :: IO Word8)
    let hexStr = map toUpper $ concatMap (\b -> showHex b "") randomBytes
    return $ T.pack $ take 6 hexStr  -- Aseguramos exactamente 6 caracteres

-- =============================================
-- Función principal para crear una parcela
-- =============================================
registerParcel :: IO ()
registerParcel = do
    putStrLn "\n=== Registrar Nueva Parcela ==="

    -- Generar el ID
    parcelId <- generateShortId

    -- Solicitar datos al usuario
    putStr "Nombre de la parcela: " >> hFlush stdout
    name <- TIO.getLine

    putStr "Zona de la parcela: " >> hFlush stdout
    zone <- TIO.getLine

    putStr "Área (m²): " >> hFlush stdout
    areaStr <- getLine
    let area = read areaStr :: Double

    -- Registrar Vegetales y Precios
    vegetables <- inputVegetales

    -- Selección de herramientas con bucle
    availableTools <- loadAvailableTools
    asignedTools <- if null availableTools
        then do
            putStrLn "\nNo hay herramientas disponibles."
            return []
        else selectTools availableTools

    -- Crear la parcela
    let newParcel = Parcel {
        parcelId = parcelId,
        name = name,
        zone = zone,
        area = area,
        vegetables = vegetables,
        asignedTools = asignedTools
    }

    -- Guardar en CSV
    appendParcelToFile newParcel
    putStrLn "\nParcela registrada exitosamente!"
    putStrLn $ "ID generado: " <> T.unpack parcelId

-- Función auxiliar para cargar herramientas disponibles
loadAvailableTools :: IO [Herramienta]
loadAvailableTools = do
    csvData <- BL.readFile "data/herramientas.csv"
    case decodeByName csvData of
        Left _ -> return []
        Right (_, tools) -> return (V.toList tools)

-- Función auxiliar para añadir parcela al archivo CSV
appendParcelToFile :: Parcel -> IO ()
appendParcelToFile parcel = do
    let filePath = "data/parcelas.csv"
    exists <- doesFileExist filePath
    if exists
        then do
            csvData <- BL.readFile filePath
            case decodeByName csvData of
                Left _ -> error "Error leyendo parcelas existentes"
                Right (_, parcels) -> do
                    let newParcels = V.toList parcels ++ [parcel]
                    BL.writeFile filePath (encodeDefaultOrderedByName newParcels)
        else BL.writeFile filePath (encodeDefaultOrderedByName [parcel])

-- =============================================
-- Consultar Parcela por ID
-- =============================================
searchParcel :: IO ()
searchParcel = do
    putStrLn "\n=== Consultar Parcela ==="
    putStr "Ingrese el ID de la parcela: " >> hFlush stdout
    parcelId <- TIO.getLine

    csvData <- BL.readFile "data/parcelas.csv"
    case findParcelById parcelId csvData of
        Left err -> putStrLn err
        Right Nothing -> putStrLn "Parcela no encontrada"
        Right (Just parcel) -> displayParcelInfo parcel

-- Función pura para buscar parcela por ID
findParcelById :: Text -> BL.ByteString -> Either String (Maybe Parcel)
findParcelById id csvData =
    case decodeByName csvData of
        Left err -> Left $ "Error leyendo parcelas: " ++ err
        Right (_, parcel) -> Right $ find (\p -> parcelId p == id) (V.toList parcel)

-- Función para mostrar la información de una parcela
displayParcelInfo :: Parcel -> IO ()
displayParcelInfo parcel = do
    putStrLn "\n=== Información de la Parcela ==="
    putStrLn $ "ID      : " ++ T.unpack (parcelId parcel)
    putStrLn $ "Nombre  : " ++ T.unpack (name parcel)
    putStrLn $ "Zona    : " ++ T.unpack (zone parcel)
    putStrLn $ "Área    : " ++ show (area parcel) ++ " m²"

    -- Mostrar vegetales y precios
    putStrLn "\nVegetales y Precios por Kg:"
    mapM_(\(v, p) -> putStrLn $ " - " ++ T.unpack v ++ ": " ++ show p ++ " ₡/kg") 
        (Map.toList (vegetables parcel))

    -- Mostrar herramientas asociadas
    putStrLn "\nHerramientas Asociadas:\n"
    if null (asignedTools parcel)
        then putStrLn " No hay herramientas asignadas"
        else do
            tools <- loadToolsDetails (asignedTools parcel)
            showToolsHeader
            mapM_ (putStrLn . formatHerramienta) tools
    putStrLn "\n"

-- Función auxiliar para cargar detalles de herramientas
loadToolsDetails :: [Text] -> IO [Herramienta]
loadToolsDetails toolCodes = do
    csvData <- BL.readFile "data/herramientas.csv"
    case decodeByName csvData of
        Left _ -> return []
        Right (_, tools) -> 
            return $ filter (\t -> codigo t `elem` toolCodes) (V.toList tools)