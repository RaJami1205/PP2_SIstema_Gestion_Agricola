-- src/Menu.hs
module Menu where

mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn "\n"
    putStrLn "===================================="
    putStrLn "Bienvenido a Finca Agricola Frucali"
    putStrLn "===================================="
    putStrLn "1. Opciones Operativas"
    putStrLn "2. Opciones Generales"
    putStrLn "3. Salir"
    putStrLn "Prueba commit"
    putStr "Ingrese una opción: "

mainMenu :: IO ()
mainMenu = do
    mostrarMenu
    opcion <- getLine
    case opcion of
        "1" -> do putStrLn "\nEntrando a Opciones Operativas...\n"; mainMenu
        "2" -> do putStrLn "\nEntrando a Opciones Generales...\n"; mainMenu
        "3" -> putStrLn "\nSaliendo del sistema...\n"
        _   -> do putStrLn "\n¡Opción inválida!\n"; mainMenu