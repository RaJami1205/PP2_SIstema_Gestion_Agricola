{-# LANGUAGE LambdaCase #-}

-- src/Menu.hs
module Menu where
import Trabajadores (Trabajador(..), cargarTrabajadores)
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

-- Submenú de opciones operativas (ejemplo básico)
opcionesOperativas :: IO ()
opcionesOperativas = do
    putStrLn "============================="
    putStrLn "Opciones Operativas"
    putStrLn "============================="
    putStrLn "1. Registrar siembra"
    putStrLn "2. Registrar cosecha"
    putStrLn "3. Volver al menú principal"
    putStr "Ingrese una opción: "
    opcion <- getLine
    case opcion of
        "3" -> mainMenu
        _   -> do
            putStrLn "\nOpción no implementada aún.\n"
            opcionesOperativas

mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn "\n"
    putStrLn "========================================="
    putStrLn "Sistema de Gestion Finca Agricola Frucali"
    putStrLn "========================================="
    putStrLn "1. Opciones Operativas"
    putStrLn "2. Opciones Generales"
    putStrLn "3. Salir"
    putStr "Ingrese una opción: "

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
        "2" -> do putStrLn "\nEntrando a Opciones Generales...\n"; mainMenu
        "3" -> putStrLn "\nSaliendo del sistema...\n"
        _   -> do putStrLn "\n¡Opción inválida!\n"; mainMenu