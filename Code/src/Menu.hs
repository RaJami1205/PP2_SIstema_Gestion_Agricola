{-# LANGUAGE LambdaCase #-}

-- src/Menu.hs
module Menu where
import Trabajadores (Trabajador(..), cargarTrabajadores)
import OpcionesOperativas (registrarHerramientas, registerParcel, searchParcel)
import OpcionesGenerales (gestionCosechasMenu)
import Data.List (find) 
import System.IO (hFlush, stdout)

-- Función de login
login :: IO (Maybe Trabajador)
login = do
    putStr "Ingrese su cédula: " 
    hFlush stdout -- Forzar a mostrar el mensaje antes de leer
    cedulaIngresada <- getLine
    trabajadores <- cargarTrabajadores "data/trabajadores.csv"
    case trabajadores of
        Right ts -> return $ find (\t -> cedula t == cedulaIngresada) ts
        Left _ -> do
            putStrLn "\n[ERROR] Cédula no registrada. Acceso denegado."
            return Nothing

-- Submenú de gestión de parcelas de cultivo
parcelMenu :: IO ()
parcelMenu = do
    putStrLn "=================================="
    putStrLn "=       Gestión de Parcelas      ="
    putStrLn "=================================="
    putStrLn "1. Registrar Parcela de Cultivo"
    putStrLn "2. Buscar Parcela"
    putStrLn "3. Volver"
    putStr "Ingrese una opción: "  >> hFlush stdout
    opt <- getLine

    case opt of
        "1" -> registerParcel >> opcionesOperativas
        "2" -> searchParcel >> opcionesOperativas
        "3" -> mainMenu
        _   -> do
            putStrLn "\nOpción no implementada aún.\n"
            parcelMenu

-- Submenú de opciones operativas 
opcionesOperativas :: IO ()
opcionesOperativas = do
    putStrLn "=================================="
    putStrLn "=      Opciones Operativas       ="
    putStrLn "=================================="
    putStrLn "1. Registrar Herramientas de Campo"
    putStrLn "2. Gestion de Parcelas de Cultivo"
    putStrLn "3. Informe de Cosechas"
    putStrLn "4. Volver"
    putStr "Ingrese una opción: "  >> hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> registrarHerramientas >> opcionesOperativas
        "2" -> parcelMenu
        "3" -> mainMenu
        "4" -> mainMenu
        _   -> do
            putStrLn "\nOpción no implementada aún.\n"
            opcionesOperativas

-- Submenú de opciones generales
opcionesGeneralesMenu :: IO ()
opcionesGeneralesMenu = do
    putStrLn "\n=================================="
    putStrLn "=       Opciones Generales       ="
    putStrLn "=================================="
    putStrLn "1. Gestión de cosechas"
    putStrLn "2. Cierre de cosecha"
    putStrLn "3. Consultar cosecha"
    putStrLn "4. Cancelación o modificación de cosecha"
    putStrLn "5. Consulta de disponibilidad de parcela"
    putStrLn "6. Volver"
    putStr "Ingrese una opción: " >> hFlush stdout
    
    opcion <- getLine
    case opcion of
        "1" -> gestionCosechasMenu >> opcionesGeneralesMenu
        "2" -> do putStrLn "\nProcesando cierre de cosecha...\n"; opcionesGeneralesMenu
        "3" -> do putStrLn "\nConsultando cosecha...\n"; opcionesGeneralesMenu
        "4" -> do putStrLn "\nCancelando/Modificando cosecha...\n"; opcionesGeneralesMenu
        "5" -> do putStrLn "\nConsultando disponibilidad...\n"; opcionesGeneralesMenu
        "6" -> return ()
        _   -> do putStrLn "\n¡Opción inválida!\n"; opcionesGeneralesMenu

mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn "\n"
    putStrLn "============================================="
    putStrLn "= Sistema de Gestion Finca Agricola Frucali ="
    putStrLn "============================================="
    putStrLn "1. Opciones Operativas"
    putStrLn "2. Opciones Generales"
    putStrLn "3. Salir"
    putStr "Ingrese una opción: "  >> hFlush stdout

-- Función Principal
mainMenu :: IO ()
mainMenu = do
    mostrarMenu
    opcion <- getLine
    case opcion of
        "1" -> login >>= \case
                Just trabajador -> do
                    putStrLn $ "\n¡Bienvenido, " ++ nombre trabajador ++ "!\n"
                    opcionesOperativas
                Nothing -> do
                    putStrLn "\nCédula no registrada. Acceso denegado.\n"
                    mainMenu
        "2" -> opcionesGeneralesMenu >> mainMenu
        "3" -> putStrLn "\nHasta Pronto\n"
        _   -> do putStrLn "\n¡Opción inválida!\n"; mainMenu